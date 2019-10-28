{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}

module Main where

import           Data.Int                       ( Int64 )
import           Model
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import qualified Database.Persist.Sql          as Sql
import           Database.Persist.Sqlite        ( SqlBackend
                                                , Migration
                                                , runMigration
                                                , runSqlite
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Data.Typeable
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Data.Text                     as Text
import           Database.Persist               ( entityKey
                                                , entityVal
                                                )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           LIO.HTTP.Server.Frankie
import           Text.Mustache                  ( (~>)
                                                , ToMustache(..)
                                                )
import qualified Text.Mustache.Types           as Mustache
import qualified Text.Mustache                 as Mustache
import qualified Database.Persist              as Persist
import           Control.Exception              ( try
                                                , evaluate
                                                )
import qualified Data.ByteString.Lazy          as ByteString
import           Data.Function                  ( (&) )
import qualified Data.HashMap.Strict           as HashMap
import           Infrastructure
import           Control.Concurrent.MVar

import           Frankie
import qualified Actions
import           Filters


data Config = Config
  { configBackend :: SqlBackend
  , configTemplateCache :: !(MVar Mustache.TemplateCache)
  }

setup :: MonadIO m => ReaderT SqlBackend m Config
setup = do
  templateCache <- liftIO $ newMVar mempty

  runMigration migrateAll

  rossId <- Persist.insert
    $ User "Ross Geller" "ross@friends.com" "New York" True
  monicaId <- Persist.insert
    $ User "Monica Geller" "monica@friends.com" "New York" False
  _       <- Persist.insert $ FriendRequest monicaId rossId True
  backend <- ask


  return
    $ Config { configBackend = backend, configTemplateCache = templateCache }


class ToMustache d => TemplateData d where
  templateFile :: FilePath

{-@ ignore getOrLoadTemplate @-}
getOrLoadTemplate
  :: (MonadController Config w m, MonadTIO m)
  => [FilePath]
  -> FilePath
  -> m Mustache.Template
getOrLoadTemplate searchDirs file = do
  cacheMVar <- configTemplateCache <$> getAppState
  oldCache  <- liftTIO $ TIO (readMVar cacheMVar)
  case HashMap.lookup file oldCache of
    Just template -> pure template
    Nothing       -> do
      liftTIO
        $   TIO
        $   Mustache.compileTemplateWithCache searchDirs oldCache file
        >>= \case
              Right template ->
                let updatedCache = HashMap.insert
                      (Mustache.name template)
                      template
                      (Mustache.partials template)
                in  do
                      modifyMVar_
                        cacheMVar
                        (\currentCache ->
                          evaluate $ currentCache <> updatedCache
                        )
                      pure template
              Left err ->
                error $ "Error parsing template " ++ file ++ ": " ++ show err

{-@ assume renderTemplate :: _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ @-}
{-@ ignore renderTemplate @-}
renderTemplate
  :: forall d w m
   . (MonadController Config w m, MonadTIO m, TemplateData d)
  => d
  -> TaggedT m Text
renderTemplate templateData = do
  template <- getOrLoadTemplate searchDirs file
  pure $ Mustache.substitute template templateData
 where
  file       = templateFile @d
  searchDirs = ["templates"]

guardNotFound :: MonadController Config w m => Maybe a -> m a
guardNotFound Nothing  = respond notFound
guardNotFound (Just x) = return x

data Profile = Profile {profileName :: Text, profileAddress :: Text}

instance TemplateData Profile where
  templateFile = "profile.html.mustache"

instance ToMustache Profile where
  toMustache (Profile name address) =
    Mustache.object ["name" ~> toMustache name, "address" ~> toMustache address]

instance HasSqlBackend Config where
  getSqlBackend = configBackend


getBackend :: MonadController Config w m => m SqlBackend
getBackend = configBackend <$> getAppState

profile :: Int64 -> TaggedT (AuthenticatedT (Controller Config TIO)) ()
profile uid = mapTaggedT (reading getBackend) $ do
  let userId = Sql.toSqlKey uid
  user <- Actions.get userId
  user <- guardNotFound user
  page <- renderTemplate Profile { profileName    = userName user
                                 , profileAddress = userAddress user
                                 }
  respondTagged . okHtml . ByteString.fromStrict . encodeUtf8 $ page

data FriendshipStatus = NotFriend | Pending | Friend deriving (Show)

data Person = Person {
  personName :: Text,
  personStatus :: FriendshipStatus
}

newtype People = People [Person]

instance TemplateData People where
  templateFile = "people.html.mustache"

instance ToMustache Person where
  toMustache (Person name status) =
    Mustache.object ["name" ~> name, "status" ~> show status]

instance ToMustache People where
  toMustache (People people) =
    Mustache.object ["people" ~> toMustache (map toMustache people)]

people :: TaggedT (AuthenticatedT (Controller Config TIO)) ()
people = mapTaggedT (reading getBackend) $ do
  loggedInUser <- getLoggedInUser
  let loggedInUserId = entityKey loggedInUser
  users          <- Actions.selectList (userIdField !=. loggedInUserId ?: nilFL)
  friendRequests <- Actions.selectList
    (friendRequestFromField ==. loggedInUserId ?: nilFL)
  let friendshipMap = friendRequests & map entityVal & map
        (\request -> (friendRequestTo request, request))
  let people = map
        (\user -> Person (userName $ entityVal user)
                         (friendshipStatus (entityKey user) friendshipMap)
        )
        users
  page <- renderTemplate (People people)
  respondTagged . okHtml . ByteString.fromStrict . encodeUtf8 $ page
 where
  friendshipStatus userId friendshipMap = case lookup userId friendshipMap of
    Nothing      -> NotFriend
    Just request -> if friendRequestAccepted request then Friend else Pending
getArgs' :: [TypeRep] -> [TypeRep]
getArgs' [a, b] = a : (getArgs' $ typeRepArgs b)
getArgs' _      = []


main :: IO ()
main = runSqlite ":memory:" $ do
  cfg <- setup
  liftIO . runFrankieServer "dev" $ do
    c <- reqHandlerArgTy a
    () <- return . unsafePerformIO . print $ c
    mode "dev" $ do
      host "localhost"
      port 3000
      appState cfg
    dispatch $ do
      -- get "/profile" profile
      -- get "/profile/:uid" profile
      get "/people"       people
      fallback $ respond notFound
