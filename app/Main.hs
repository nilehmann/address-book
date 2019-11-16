{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}

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
import           Text.Mustache                  ( (~>)
                                                , ToMustache(..)
                                                )
import qualified Text.Mustache.Types           as Mustache
import qualified Database.Persist              as Persist
import qualified Data.ByteString.Lazy          as ByteString
import qualified Control.Concurrent.MVar       as MVar

import           Frankie.Config
import           Frankie.Auth

import           Infrastructure
import           Templates
import           Frankie.Tagged
import qualified Actions
import           Filters
import           Core

data Config = Config
  { configBackend :: SqlBackend
  , configTemplateCache :: !(MVar.MVar Mustache.TemplateCache)
  , configAuthMethod :: !(AuthMethod (Entity User) Controller)
  }

type Controller
  = TaggedT (ReaderT SqlBackend (ConfigT Config (ControllerT TIO)))

instance HasTemplateCache Config where
  getTemplateCache = configTemplateCache

instance HasSqlBackend Config where
  getSqlBackend = configBackend

instance HasAuthMethod (Entity User) Controller Config where
  getAuthMethod = configAuthMethod

getBackend :: MonadConfig Config m => m SqlBackend
getBackend = configBackend <$> getConfig

setup :: MonadIO m => ReaderT SqlBackend m Config
setup = do
  templateCache <- liftIO $ MVar.newMVar mempty

  runMigration migrateAll

  jerryId <- Persist.insert $ User
    "Jerry Seinfeld"
    "jerry@seinfeld.com"
    "129 West 81st Street, Apt 5A, New York, NY"
    True
  kramerId <- Persist.insert $ User
    "Cosmo Kramer"
    "cosmo@kramer.com"
    "129 West 81st Street, Apt 5B, New York, NY"
    True
  newmanId <- Persist.insert $ User
    "Newman"
    "newman@newman.com"
    "129 West 81st Street, Apt 5E, New York, NY"
    True
  georgeId <- Persist.insert $ User "George Constanza"
                                    "george@constanza.com"
                                    "Somewhere in New York"
                                    True
  susanId <- Persist.insert
    $ User "Susan Ross" "susan@ross.com" "Somewhere in New York" False

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

  return $ Config { configBackend       = backend
                  , configTemplateCache = templateCache
                  , configAuthMethod    = httpAuthDb
                  }


{-@ guardVerified :: user: (Entity User) -> TaggedT<{\_ -> True}, {\u -> currentUser == u}> _ {v: () | userVerified (entityVal user) }@-}
guardVerified :: MonadController w m => Entity User -> TaggedT m ()
guardVerified user = do
  verified <- Actions.project userVerifiedField user
  if verified then returnTagged () else respondTagged forbidden


data Profile = Profile {profileName :: Text, profileAddress :: Maybe Text, profileEmail :: Maybe Text, friends :: [Person] }

instance TemplateData Profile where
  templateFile = "profile.html.mustache"

instance ToMustache Profile where
  toMustache (Profile name address email friends) = Mustache.object
    [ "name" ~> toMustache name
    , "address" ~> toMustache address
    , "email" ~> toMustache email
    , "friends" ~> toMustache friends
    ]


{-@ getFriends :: userId: UserId -> TaggedT<{\v -> ((friends (entityKey currentUser) userId) && currentUser == v) || (entityKey v) == userId}, {\_ -> False}> _ _ @-}
getFriends :: UserId -> Controller [Entity User]
getFriends userId = do
  requests1 <- Actions.selectList
    (friendRequestFromField ==. userId ?: friendRequestAcceptedField ==. True ?: nilFL)
  userIds1  <- Actions.projectList friendRequestToField requests1
  requests2 <- Actions.selectList
    (friendRequestToField ==. userId ?: friendRequestAcceptedField ==. True ?: nilFL)
  userIds2 <- Actions.projectList friendRequestFromField requests2
  users <- Actions.selectList (userIdField <-. (userIds1 ++ userIds2) ?: nilFL)
  returnTagged users


{-@ isFriendWith :: viewer: UserId -> userId: UserId -> TaggedT<{\v -> (entityKey v) == viewer}, {\_ -> False}> _ {v: Bool | v => friends viewer userId} @-}
isFriendWith :: UserId -> UserId -> Controller Bool
isFriendWith viewer userId = do
  friendRequest <- Actions.selectFirst
    (   friendRequestFromField
    ==. viewer
    ?:  friendRequestToField
    ==. userId
    ?:  friendRequestAcceptedField
    ==. True
    ?:  nilFL
    )
  case friendRequest of
    Just _  -> returnTagged True
    Nothing -> returnTagged False


{-@ profile :: {v: Int64 | True} -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
profile :: Int64 -> Controller ()
profile uid = do
  let userId = Sql.toSqlKey uid
  loggedInUser   <- requireAuthUser
  loggedInUserId <- Actions.project userIdField loggedInUser
  if userId == loggedInUserId
    then respondTagged (redirectTo "/my-profile")
    else do
      _         <- guardVerified loggedInUser
      maybeUser <- Actions.selectFirst (userIdField ==. userId ?: nilFL)
      user      <- case maybeUser of
        Nothing   -> respondTagged notFound
        Just user -> returnTagged user
      userName               <- Actions.project userNameField user

      areFriends             <- loggedInUserId `isFriendWith` userId
      (userAddress, friends) <- if areFriends
        then do
          userAddress <- Actions.project userAddressField user
          friends     <- getFriends userId
          friendIds   <- Actions.projectList userIdField friends
          friendNames <- Actions.projectList userNameField friends
          returnTagged
            ( Just userAddress
            , map
              (\(id, name) -> Person (show $ Sql.fromSqlKey id) name Friend)
              (zip friendIds friendNames)
            )
        else returnTagged (Nothing, [])
      page <- renderTemplate $ Profile userName userAddress Nothing friends
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

{-@ people :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
people :: Controller ()
people = do
  loggedInUser   <- requireAuthUser
  _              <- guardVerified loggedInUser
  loggedInUserId <- Actions.project userIdField loggedInUser
  users          <- Actions.selectList (userIdField !=. loggedInUserId ?: nilFL)
  friendRequests <- Actions.selectList
    (friendRequestFromField ==. loggedInUserId ?: nilFL)
  requestsTo       <- Actions.projectList friendRequestToField friendRequests
  requestsAccepted <- Actions.projectList friendRequestAcceptedField
                                          friendRequests
  let friendshipMap = zip requestsTo requestsAccepted
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
    Nothing    -> NotFriend
    Just True  -> Friend
    Just False -> Pending

{-@ myProfile :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
myProfile :: Controller ()
myProfile = do
  loggedInUser <- requireAuthUser
  userId       <- Actions.project userIdField loggedInUser
  userName     <- Actions.project userNameField loggedInUser
  userAddress  <- Actions.project userAddressField loggedInUser
  userEmail    <- Actions.project userEmailField loggedInUser
  userVerified <- Actions.project userVerifiedField loggedInUser

  friends      <- if userVerified
    then do
      friends     <- getFriends userId
      friendIds   <- Actions.projectList userIdField friends
      friendNames <- Actions.projectList userNameField friends
      let friends = map
            (\(id, name) -> Person (show $ Sql.fromSqlKey id) name Friend)
            (zip friendIds friendNames)
      returnTagged friends
    else returnTagged []

  let profile = Profile userName (Just userAddress) (Just userEmail) friends
  page <- renderTemplate profile
  respondTagged . okHtml . ByteString.fromStrict . encodeUtf8 $ page


data Home = Home
instance TemplateData Home where
  templateFile = "home.html.mustache"

instance ToMustache Home where
  toMustache Home = Mustache.object []

{-@ home :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
home :: Controller ()
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
      initWith $ configure cfg . reading backend . unTag
    dispatch $ do
      get "/"             home
      get "/people"       people
      get "/my-profile"   myProfile
      get "/profile/:uid" profile
      fallback $ respond notFound
