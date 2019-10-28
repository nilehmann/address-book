{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}

module Main where

import qualified Authenticated
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
import           Data.Typeable
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Data.Text                     as Text
import           Database.Persist        hiding ( get )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           LIO.HTTP.Server.Frankie
import           Text.Mustache                  ( (~>)
                                                , ToMustache(..)
                                                )
import qualified Text.Mustache                 as Mustache
import qualified Database.Persist              as Persist
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Wai
import           Control.Exception              ( try )
import qualified Data.ByteString.Lazy          as ByteString
import           Data.Function                  ( (&) )


newtype Config = Config
  { configBackend :: SqlBackend
  }

type AuthenticatedT = Authenticated.AuthenticatedT User

setup :: MonadIO m => ReaderT SqlBackend m Config
setup = do
        runMigration migrateAll

        aliceId <- insert
                $ User "Ross Geller" "ross@friends.com" "New York" True
        liftIO $ print aliceId
        bobId <- insert $ User "Monica Geller"
                               "monica@friends.com"
                               "New York"
                               False
        backend <- ask


        return $ Config { configBackend = backend }


class ToMustache d => TemplateData d where
  templateFile :: FilePath

renderTemplate :: forall d . (TemplateData d) => d -> IO Text
renderTemplate templateData = do
        let file = templateFile @d
        templateFile <- Mustache.automaticCompile ["templates"] file
        case templateFile of
                Right template ->
                        return $ Mustache.substitute template templateData
                Left err ->
                        error
                                $  "Error parsing template "
                                ++ file
                                ++ ": "
                                ++ show err

toWaiApplication :: Application IO -> Wai.Application
toWaiApplication app wReq wRespond = do
        resp <- liftIO $ app req
        wRespond $ toWaiResponse resp
    where
        req :: Request IO
        req = Request $ wReq { Wai.pathInfo = trimPath $ Wai.pathInfo wReq }
        toWaiResponse :: Response -> Wai.Response
        toWaiResponse (Response status headers body) =
                Wai.responseLBS status headers body

trimPath :: [Text] -> [Text]
trimPath path =
        if (not . null $ path) && Text.null (last path) then init path else path


instance WebMonad IO where
        data Request IO = Request { unRequestReader :: Wai.Request }
        reqMethod      = Wai.requestMethod . unRequestReader
        reqHttpVersion = Wai.httpVersion . unRequestReader
        reqPathInfo    = Wai.pathInfo . unRequestReader
        reqQueryString = Wai.queryString . unRequestReader
        reqHeaders     = Wai.requestHeaders . unRequestReader
        reqBody        = liftIO . Wai.strictRequestBody . unRequestReader
        tryWeb act = do
                er <- try act
                case er of
                        Left e -> return . Left . toException $ e
                        r      -> return r
        server port hostPref app =
                let settings =
                                    Wai.setHost hostPref
                                            $ Wai.setPort port
                                            $ Wai.setServerName
                                                      "lio-http-server"
                                                      Wai.defaultSettings
                in  Wai.runSettings settings $ toWaiApplication app

class HasSqlBackend config where
  getSqlBackend :: config -> SqlBackend

backend :: (MonadController config w m, HasSqlBackend config) => m SqlBackend
backend = getSqlBackend <$> getAppState

guardNotFound :: WebMonad m => Maybe a -> Controller Config m a
guardNotFound Nothing  = respond notFound
guardNotFound (Just x) = return x

data Profile = Profile {profileName :: Text, profileAddress :: Text}

instance TemplateData Profile where
        templateFile = "profile.html.mustache"

instance ToMustache Profile where
        toMustache (Profile name address) =
                Mustache.object
                        [ "name" ~> toMustache name
                        , "address" ~> toMustache address
                        ]

profile :: Int64 -> Controller Config IO ()
profile uid = do
        let userId = Sql.toSqlKey uid
        user <- readingBackend $ Persist.get userId
        user <- guardNotFound user
        page <- lift $ renderTemplate Profile
                { profileName    = userName user
                , profileAddress = userAddress user
                }
        respond . okHtml . ByteString.fromStrict . encodeUtf8 $ page

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

-- people :: AuthenticatedT (Controller Config IO) ()
-- people = do
--         users <- lift $ readingBackend $ selectList [] []
--         let people = users & map entityVal & map
--                     (\user -> Person (userName user) NotFriend)
--         page <- lift $ lift $ renderTemplate (People people)
--         respond . okHtml . ByteString.fromStrict . encodeUtf8 $ page

main :: IO ()
main = runSqlite ":memory:" $ do
        cfg <- setup
        liftIO . runFrankieServer "dev" $ do
                mode "dev" $ do
                        host "localhost"
                        port 3000
                        appState cfg
                dispatch $ do
                        get "/profile/:uid" profile
                        -- get "/people"       people
                        fallback $ respond notFound
