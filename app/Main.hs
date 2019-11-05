{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}

{-@ LIQUID "--no-pattern-inline" @-}

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

import           Core


data Config = Config
  { configBackend :: SqlBackend
  , configTemplateCache :: !(MVar Mustache.TemplateCache)
  }

setup :: MonadIO m => ReaderT SqlBackend m Config
setup = do
  templateCache <- liftIO $ newMVar mempty

  runMigration migrateAll

  jerryId <- Persist.insert $ User
    "Jerry Seinfeld"
    "jerry@seinfeld.com"
    "129 West 81st Street, Apt 5A, New York, NY"
    True
    10
  kramerId <- Persist.insert $ User
    "Cosmo Kramer"
    "cosmo@kramer.com"
    "129 West 81st Street, Apt 5B, New York, NY"
    True
    10
  newmanId <- Persist.insert $ User
    "Newman"
    "newman@newman.com"
    "129 West 81st Street, Apt 5E, New York, NY"
    True
    10
  georgeId <- Persist.insert $ User "George Constanza"
                                    "george@constanza.com"
                                    "Somewhere in New York"
                                    True
                                    10
  susanId <- Persist.insert
    $ User "Susan Ross" "susan@ross.com" "Somewhere in New York" False 10

  Persist.insert $ FriendRequest newmanId kramerId True

  Persist.insert $ FriendRequest kramerId jerryId True
  Persist.insert $ FriendRequest kramerId georgeId True
  Persist.insert $ FriendRequest kramerId newmanId True

  Persist.insert $ FriendRequest georgeId kramerId True
  Persist.insert $ FriendRequest georgeId jerryId True

  Persist.insert $ FriendRequest jerryId georgeId True
  Persist.insert $ FriendRequest jerryId kramerId True

  Persist.insert $ FriendRequest georgeId susanId False

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
    Nothing ->
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

data Profile = Profile {profileName :: Text, profileAddress :: Maybe Text, profileEmail :: Maybe Text}

instance TemplateData Profile where
  templateFile = "profile.html.mustache"

instance ToMustache Profile where
  toMustache (Profile name address email) = Mustache.object
    [ "name" ~> toMustache name
    , "address" ~> toMustache address
    , "email" ~> toMustache email
    ]

instance HasSqlBackend Config where
  getSqlBackend = configBackend


getBackend :: MonadController Config w m => m SqlBackend
getBackend = configBackend <$> getAppState


{-@ profile :: {v: Int64 | True} -> TaggedT<{\_ -> False}, {\_ -> True}> _ () @-}
profile :: Int64 -> TaggedT (AuthenticatedT (Controller Config TIO)) ()
profile uid = mapTaggedT (reading getBackend) $ do
  let userId = Sql.toSqlKey uid
  loggedInUser   <- getLoggedInUserTagged
  loggedInUserId <- Actions.project userIdField loggedInUser
  if userId == loggedInUserId
    then respondTagged (redirectTo "/my-profile")
    else do
      _         <- guardVerified loggedInUser
      maybeUser <- Actions.selectFirst (userIdField ==. userId ?: nilFL)
      user      <- case maybeUser of
        Nothing   -> respondTagged notFound
        Just user -> returnTagged user
      userName      <- Actions.project userNameField user
      friendRequest <-
        Actions.selectFirst
        $   (   friendRequestFromField
            ==. loggedInUserId
            ?:  friendRequestToField
            ==. userId
            ?:  nilFL
            )
        ||| (   friendRequestFromField
            ==. userId
            ?:  friendRequestToField
            ==. loggedInUserId
            ?:  nilFL
            )

      userAddress <- case friendRequest of
        Just friendRequest -> do
          accepted <- Actions.project friendRequestAcceptedField friendRequest
          if accepted
            then do
              userAddress <- Actions.project userAddressField user
              returnTagged $ Just userAddress
            else returnTagged Nothing
        Nothing -> returnTagged Nothing
      page <- renderTemplate $ Profile userName userAddress Nothing
      respondTagged . okHtml . ByteString.fromStrict . encodeUtf8 $ page


data FriendshipStatus = NotFriend | Pending | Friend deriving (Show)

data Person = Person {
  personId :: String,
  personName :: Text,
  personStatus :: FriendshipStatus
}

newtype People = People [Person]

instance TemplateData People where
  templateFile = "people.html.mustache"

instance ToMustache Person where
  toMustache (Person id name status) =
    Mustache.object ["id" ~> id, "name" ~> name, "status" ~> show status]

instance ToMustache People where
  toMustache (People people) =
    Mustache.object ["people" ~> toMustache (map toMustache people)]


{-@ guardVerified :: user: (Entity User) -> TaggedT<{\_ -> True}, {\u -> currentUser == u}> _ {v: () | userVerified (entityVal user) }@-}
guardVerified :: MonadController s w m => Entity User -> TaggedT m ()
guardVerified user = do
  verified <- Actions.project userVerifiedField user
  if verified then returnTagged () else respondTagged forbidden


{-@ people :: TaggedT<{\_ -> False}, {\_ -> True}> _ () @-}
people :: TaggedT (AuthenticatedT (Controller Config TIO)) ()
people = mapTaggedT (reading getBackend) $ do
  loggedInUser   <- getLoggedInUserTagged
  _              <- guardVerified loggedInUser
  loggedInUserId <- Actions.project userIdField loggedInUser
  users          <- Actions.selectList (userIdField !=. loggedInUserId ?: nilFL)
  friendRequests <-
    Actions.selectList (friendRequestFromField ==. loggedInUserId ?: nilFL)
  let friendshipMap = friendRequests & map entityVal & map
        (\request -> (friendRequestTo request, request))
  userNames <- Actions.projectList userNameField users
  userIds   <- Actions.projectList userIdField users
  let userNamesAndIds = zip userIds userNames
  let people = map
        (\(userId, userName) -> Person (show $ Sql.fromSqlKey userId)
                                       userName
                                       (friendshipStatus userId friendshipMap)
        )
        userNamesAndIds
  page <- renderTemplate (People people)
  respondTagged . okHtml . ByteString.fromStrict . encodeUtf8 $ page
 where
  friendshipStatus userId friendshipMap = case lookup userId friendshipMap of
    Nothing      -> NotFriend
    Just request -> if friendRequestAccepted request then Friend else Pending

{-@ myProfile :: TaggedT<{\_ -> False}, {\_ -> True}> _ _@-}
myProfile :: TaggedT (AuthenticatedT (Controller Config TIO)) ()
myProfile = mapTaggedT (reading getBackend) $ do
  loggedInUser <- getLoggedInUserTagged
  userName     <- Actions.project userNameField loggedInUser
  userAddress  <- Actions.project userAddressField loggedInUser
  userEmail    <- Actions.project userEmailField loggedInUser
  let profile = Profile userName (Just userAddress) (Just userEmail)
  page <- renderTemplate profile
  respondTagged . okHtml . ByteString.fromStrict . encodeUtf8 $ page


data Home = Home
instance TemplateData Home where
  templateFile = "home.html.mustache"

instance ToMustache Home where
  toMustache Home = Mustache.object []

{-@ home :: TaggedT<{\_ -> False}, {\_ -> True}> _ _@-}
home :: TaggedT (AuthenticatedT (Controller Config TIO)) ()
home = mapTaggedT (reading getBackend) $ do
  page <- renderTemplate Home
  respondTagged . okHtml . ByteString.fromStrict . encodeUtf8 $ page

{-@ ignore main @-}
main :: IO ()
main = runSqlite ":memory:" $ do
  cfg <- setup
  liftIO . runFrankieServer "dev" $ do
    mode "dev" $ do
      host "localhost"
      port 3000
      appState cfg
    dispatch $ do
      get "/"             home
      get "/people"       people
      get "/my-profile"   myProfile
      get "/profile/:uid" profile
      fallback $ respond notFound
