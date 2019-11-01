{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model where

import           Database.Persist               ( Key )
import           Database.Persist.TH            ( share
                                                , mkMigrate
                                                , mkPersist
                                                , sqlSettings
                                                , persistLowerCase
                                                )
import           Data.Text                      ( Text )
import qualified Database.Persist              as Persist
import Core

{-@
data EntityFieldWrapper record typ <policy :: Entity record -> Entity User -> Bool,
                                    selector :: Entity record -> typ -> Bool,
                                    flippedselector :: typ -> Entity record -> Bool> = EntityFieldWrapper _
@-}
data EntityFieldWrapper record typ = EntityFieldWrapper (Persist.EntityField record typ)
{-@ data variance EntityFieldWrapper covariant covariant contravariant invariant invariant @-}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  email Text
  address Text
  verified Bool
  deriving Show

FriendRequest
  from UserId
  to UserId
  accepted Bool
  deriving Show
|]

-- * User
{-@
data User = User
  { userName :: _
  , userEmail :: {v: _ | tlen v > 0}
  , userAddress :: {v: _ | tlen v > 0}
  , userVerified :: _
  }
@-}

{-@ assume userIdField :: EntityFieldWrapper <{\row viewer -> True}, {\row field -> field == entityKey row}, {\field row -> field == entityKey row}> User UserId @-}
userIdField :: EntityFieldWrapper User UserId
userIdField = EntityFieldWrapper UserId

-- TODO change policy so only vierified users can see this
{-@ assume userNameField :: EntityFieldWrapper <{\row viewer -> True}, {\row field -> field == userName (entityVal row)}, {\field row -> field == userName (entityVal row)}> User Text @-}
userNameField :: EntityFieldWrapper User Text
userNameField = EntityFieldWrapper UserName

{-@ assume userEmailField :: EntityFieldWrapper <{\row viewer -> True}, {\row field -> field == userEmail (entityVal row)}, {\field row -> field == userEmail (entityVal row)}> User Text @-}
userEmailField :: EntityFieldWrapper User Text
userEmailField = EntityFieldWrapper UserEmail

{-@ assume userAddressField :: EntityFieldWrapper <
      {\user viewer -> friends (entityKey viewer) (entityKey user) || (entityKey viewer) == (entityKey user)}
    , {\user field -> field == userAddress (entityVal user)}
    , {\field user -> field == userAddress (entityVal user)}
    > User Text @-}
userAddressField :: EntityFieldWrapper User Text
userAddressField = EntityFieldWrapper UserAddress

{-@ assume userVerifiedField :: EntityFieldWrapper <{\row viewer -> True}, {\row field -> field = userVerified (entityVal row)}, {\field row -> field = userVerified (entityVal row)}> User Bool @-}
userVerifiedField :: EntityFieldWrapper User Bool
userVerifiedField = EntityFieldWrapper UserVerified

-- * FriendRequest

{-@
data FriendRequest = FriendRequest
  { friendRequestFrom :: Key User
  , friendRequestTo :: Key User
  , friendRequestAccepted :: Bool
  }
@-}

{-@ assume friendRequestFromField :: EntityFieldWrapper <{\row viewer -> True}, {\row field -> field = friendRequestFrom (entityVal row)}, {\field row -> field = friendRequestFrom (entityVal row)}> FriendRequest UserId @-}
friendRequestFromField :: EntityFieldWrapper FriendRequest (Key User)
friendRequestFromField = EntityFieldWrapper FriendRequestFrom

{-@ assume friendRequestToField :: EntityFieldWrapper <{\row viewer -> True}, {\row field -> field = friendRequestTo (entityVal row)}, {\field row -> field = friendRequestTo (entityVal row)}> FriendRequest UserId @-}
friendRequestToField :: EntityFieldWrapper FriendRequest (Key User)
friendRequestToField = EntityFieldWrapper FriendRequestTo

{-@ assume friendRequestAcceptedField :: EntityFieldWrapper <{\row viewer -> True}, {\row field -> field = friendRequestAccepted (entityVal row)}, {\field row -> field = friendRequestAccepted (entityVal row)}> FriendRequest Bool @-}
friendRequestAcceptedField :: EntityFieldWrapper FriendRequest Bool
friendRequestAcceptedField = EntityFieldWrapper FriendRequestAccepted

-- TODO make this symmetric
{-@ measure friends :: Key User -> Key User -> Bool @-}
{-@ invariant {v:FriendRequest | friendRequestAccepted v => friends (friendRequestFrom v) (friendRequestTo v)} @-}
