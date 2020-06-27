{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.User where

import           Data.Text                      ( Text )
import           Data.Int                       ( Int64 )
import           Data.Maybe
import           Database.Persist.Sql           ( fromSqlKey
                                                , toSqlKey
                                                )
import           GHC.Generics

import           Binah.Core
import           Binah.Actions
import           Binah.Updates
import           Binah.Insert
import           Binah.Filters
import           Binah.Helpers
import           Binah.Infrastructure
import           Binah.Templates
import           Binah.Frankie

import           Controllers
import           Controllers.Invitation         ( InvitationCode(..) )
import           Model
import           JSON

----------------------------------------------------------------------------------------------------
-- | User List
----------------------------------------------------------------------------------------------------

{-@ userList :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
userList :: Controller ()
userList = do
  _     <- requireAuthUser
  users <- selectList trueF
  users <- mapMC extractUserData users
  respondJSON status200 users

extractUserData :: Entity User -> Controller UserData
extractUserData u = do
  id           <- project userId' u
  emailAddress <- project userEmailAddress' u
  firstName    <- project userFirstName' u
  lastName     <- project userLastName' u
  level        <- project userLevel' u
  group        <- project userGroup' u
  return $ _fixme -- UserData id emailAddress firstName lastName level group 

-- data UserData = UserData
--   { userId           :: UserId
--   , userEmailAddress :: Text
--   -- , userFirstName    :: Text
--   -- , userLastName     :: Text
--   , userLevel        :: String
--   -- , userGroup        :: Maybe GroupId
--   }
--   deriving Generic

instance ToJSON UserData where
  toEncoding = genericToEncoding (stripPrefix "user")

data UserNG = UserNG 
  { userFirstName :: Text
  , userLastName  :: Text
  , userGroup     :: Maybe GroupId
  }
  deriving Generic

data UserData 
  = UserStudent 
     { user      :: UserNG
     , grpBuffer :: Text
     }
  | UserInstructor 
     { user       :: UserNG 
     , allBuffers :: [Text]
     }
  | UserNone
  deriving Generic

----------------------------------------------------------------------------------------------------
-- | User Get
----------------------------------------------------------------------------------------------------

{-@ userGet :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
userGet :: Int64 -> Controller ()
userGet uid = do
  let userId = toSqlKey uid
  _        <- requireAuthUser
  user     <- selectFirstOr notFoundJSON (userId' ==. userId)
  userData <- extractUserData user
  respondJSON status200 userData

----------------------------------------------------------------------------------------------------
-- | User Update
----------------------------------------------------------------------------------------------------

{-@ userUpdateMe :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
userUpdateMe :: Controller ()
userUpdateMe = do
  user            <- requireAuthUser
  userId          <- project userId' user
  UserUpdate {..} <- decodeBody
  let up =          (userFirstName' `assign` userUpdateFirstName)
          `combine` (userLastName' `assign` userUpdateLastName)
  _        <- updateWhere (userId' ==. userId) up
  user     <- selectFirstOr notFoundJSON (userId' ==. userId)
  userData <- extractUserData user
  respondJSON status200 userData


data UserUpdate = UserUpdate
  { userUpdateFirstName :: Text
  , userUpdateLastName :: Text
  }
  deriving Generic

instance FromJSON UserUpdate where
  parseJSON = genericParseJSON (stripPrefix "userUpdate")
