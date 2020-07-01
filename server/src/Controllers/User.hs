{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.User where

import           Data.Text                      ( Text, pack )
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
import           Types

import qualified Debug.Trace 

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
  let uNG       = UserNG firstName lastName
  classes      <- extractUserClasses u
  return $ UserData uNG classes
  -- level        <- project userLevel' u
  -- group        <- project userGroup' u
  -- case level of
  --   "instructor"    -> extractInstructor u uNG
  --   _ {- student -} -> extractStudent    u uNG 

extractUserClasses :: Entity User -> Controller [ClassData]
extractUserClasses u = do
  instrClasses   <- extractInstrClasses u
  studentClasses <- extractStudentClasses u
  return (instrClasses ++ studentClasses)

extractInstrClasses :: Entity User -> Controller [ClassData]
extractInstrClasses u = do
  uId        <- project userId' u
  classes    <- selectList (classInstructor' ==. uId)
  mapMC (extractInstrData u) classes 

extractInstrData :: Entity User -> Entity Class -> Controller ClassData
extractInstrData u cls = do
  clsId     <- project classId' cls
  clsName   <- project className' cls
  allGroups <- selectList (groupClass' ==. clsId)
  allBufs   <- mapMC extractBuffer allGroups
  return (Instructor clsName allBufs)

extractStudentClasses :: Entity User -> Controller [ClassData]
extractStudentClasses = _fixme

-- extractInstructor :: Entity User -> UserNG -> Controller UserData
-- extractInstructor u user = do 
--   allGroups <- selectList trueF
--   allBufs   <- mapMC extractBuffer allGroups
--   return (Instructor user allBufs)

-- extractStudent :: Entity User -> UserNG -> Controller UserData
-- extractStudent u (user@UserNG {..}) = case userGroup of
--   Nothing  -> 
--     respondTagged $ errorResponse status401 (Just "Undefined group")
--   Just groupId -> do 
--     group <- selectFirstOr (errorResponse status401 (Just "Invalid group"))
--                (groupId' ==. groupId)
--     myBuf <- extractBuffer group
--     return (Student user myBuf)

extractBuffer :: Entity Group -> Controller Buffer
extractBuffer group = do
  bId   <- project groupId'         group
  bHash <- project groupEditorLink' group
  let bText  = "-- Code for group: " <> pack (show bId)
  return $ Buffer bId bHash bText

traceShow :: (Show a) => String -> a -> a 
traceShow msg x = Debug.Trace.trace (msg <> ": " <> (show x)) x

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
