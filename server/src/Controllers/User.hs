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

extractUserNG :: Entity User -> Controller UserNG
extractUserNG u = do
  firstName <- project userFirstName' u
  lastName  <- project userLastName' u
  return     $ UserNG firstName lastName 

extractUserData :: Entity User -> Controller UserData
extractUserData u = do
  emailAddress <- project userEmailAddress' u
  firstName    <- project userFirstName' u
  lastName     <- project userLastName' u
  let uNG       = UserNG firstName lastName
  classes      <- extractUserClasses u
  return $ UserData uNG classes

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
  allBufs   <- mapMC (extractBuffer clsName False) allGroups
  return (Instructor clsName allBufs)

extractStudentClasses :: Entity User -> Controller [ClassData]
extractStudentClasses u = do 
  uId       <- project userId' u
  uEnrolls  <- selectList (enrollStudent' ==. uId)
  mapMC (enrollClassData u) uEnrolls

enrollClassData :: Entity User -> Entity Enroll -> Controller ClassData
enrollClassData u enroll = do
  grpId   <- project enrollGroup' enroll
  grp     <- selectFirstOr notFoundJSON (groupId' ==. grpId)
  clsId   <- project groupClass' grp
  cls     <- selectFirstOr notFoundJSON (classId' ==. clsId)
  clsName <- project className' cls
  grpBuf  <- extractBuffer clsName True grp
  return (Student clsName grpBuf)

extractBuffer :: Text -> Bool -> Entity Group -> Controller Buffer
extractBuffer clsName isStudent group = do
  bName <- project groupName' group 
  bHash <- project groupEditorLink' group
  let bText = "-- Code for group: " <> bName
  let bDiv  = "editor-" <> clsName <> "-" <> bName 
  let bDiv' = if isStudent then bDiv <> "-" <> "student" else bDiv
  return $ Buffer bName bHash bText bDiv'


traceShow :: (Show a) => String -> a -> a 
traceShow msg x = Debug.Trace.trace (msg <> ": " <> (show x)) x

----------------------------------------------------------------------------------------------------
-- | User Get
----------------------------------------------------------------------------------------------------

{-@ userGet :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
userGet :: Int64 -> Controller ()
userGet _uid = do
  -- let userId = toSqlKey uid
  user     <- requireAuthUser
  -- user     <- selectFirstOr notFoundJSON (userId' ==. userId)
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
