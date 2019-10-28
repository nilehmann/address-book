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

userIdField :: EntityFieldWrapper User UserId
userIdField = EntityFieldWrapper UserId

userNameField :: EntityFieldWrapper User Text
userNameField = EntityFieldWrapper UserName

userAddressField :: EntityFieldWrapper User Text
userAddressField = EntityFieldWrapper UserAddress

friendRequestFromField :: EntityFieldWrapper FriendRequest (Key User)
friendRequestFromField = EntityFieldWrapper FriendRequestFrom

friendRequestToField :: EntityFieldWrapper FriendRequest (Key User)
friendRequestToField = EntityFieldWrapper FriendRequestTo
