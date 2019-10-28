{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Authenticated
        ( AuthenticatedT(..)
        )
where

import           Data.Either.Combinators        ( rightToMaybe )
import           Control.Monad                  ( (>=>) )
import           Control.Monad.Trans            ( MonadTrans(..) )
import           Database.Persist ( PersistRecordBackend )
import           Database.Persist.Sqlite        ( SqlBackend )
import           LIO.HTTP.Server.Frankie
import           Prelude                 hiding ( log )
import           Data.Typeable                  ( Typeable )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Base64        as Base64
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text

data AuthenticatedT u m a = AuthenticatedT { runAuthenticatedT :: u -> m a }

instance Functor m => Functor (AuthenticatedT u m) where
        fmap f x = AuthenticatedT (fmap f . runAuthenticatedT x)

instance Applicative m => Applicative (AuthenticatedT u m) where
        pure x = AuthenticatedT $ const (pure x)
        f <*> x =
                AuthenticatedT
                        $ \user ->
                                  runAuthenticatedT f user
                                          <*> runAuthenticatedT x user

instance Monad m => Monad (AuthenticatedT u m) where
        x >>= f =
                AuthenticatedT
                        $ \user ->
                                  runAuthenticatedT x user
                                          >>= (`runAuthenticatedT` user)
                                          .   f

class Monad m => MonadAuthenticated u m | m -> u where
  getLoggedInUser :: m u

instance Monad m => MonadAuthenticated u (AuthenticatedT u m) where
        getLoggedInUser = AuthenticatedT pure

instance MonadTrans (AuthenticatedT u) where
        lift x = AuthenticatedT $ const x

instance WebMonad n => MonadController s n (AuthenticatedT u (Controller s n)) where
        request     = lift request
        respond     = lift . respond
        getAppState = lift getAppState
        putAppState = lift . putAppState
        log ll str = lift $ log ll str
        liftWeb = lift . liftWeb

class HasSqlBackend config where
  getSqlBackend :: config -> SqlBackend

backend :: (MonadController config w m, HasSqlBackend config) => m SqlBackend
backend = getSqlBackend <$> getAppState

-- instance forall m w a config u. (MonadIO w, Typeable m, Typeable a, RequestHandler (m a) config w, MonadIO m, WebMonad w) => RequestHandler (AuthenticatedT u m a) config w where
--         handlerToController = handlerToControllerAuthenticatedT
--         reqHandlerArgTy _ = reqHandlerArgTy (undefined :: m a) -- TODO: Fix this bad thing

handlerToControllerAuthenticatedT
        :: ( MonadIO w
           , Typeable m
           , Typeable a
           , RequestHandler (m a) config w
           , MonadIO m
           , WebMonad w
           , HasSqlBackend config
           )
        => [PathSegment]
        -> AuthenticatedT u m a
        -> Controller config w ()
handlerToControllerAuthenticatedT args handler = do
        authHeader <- requestHeader hAuthorization
        case authHeader >>= parseBasicAuthHeader of
                Just (username, _) -> do
                        back <- backend
                        user <- (`runReaderT` back) $ selectFirst
                                (userNameField ==. username ?: nilFL)
                        handlerToController
                                args
                                (runAuthenticatedT handler (fromJust user))
                Nothing -> respond $ requireBasicAuth "this website"

parseBasicAuthHeader :: ByteString -> Maybe (Text, Text)
parseBasicAuthHeader =
        ByteString.stripPrefix "Basic "
                >=> rightToMaybe
                .   Base64.decode
                >=> rightToMaybe
                .   Text.decodeUtf8'
                >=> splitAtChar ':'
    where
        splitAtChar :: Char -> Text -> Maybe (Text, Text)
        splitAtChar c text =
                let (before, after) = Text.break (== c) text
                in  do
                            (_, after') <- Text.uncons after
                            return (before, after')
