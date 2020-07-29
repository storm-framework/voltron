{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Enroller
  ( addUser
  , addClass
  , addGroup
  , addEnroll
  , addRoster
  )
where

import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.ByteString.Base64.URL    as B64Url
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
import qualified Frankie.Log                   as Log
import           Controllers
import           Controllers.Invitation         ( InvitationCode(..) )
import           Model
import           JSON
import           Crypto
import           Crypto.Random                  ( getRandomBytes )
import           Types

-------------------------------------------------------------------------------
-- | Add a full roster of students to a class using an Roster -----------------
-------------------------------------------------------------------------------

addRoster :: Controller ()
addRoster = do
  instr         <- requireAuthUser
  r@Roster {..} <- decodeBody
  crUsers       <- mapM createUser rosterStudents
  mapM_ addUser   crUsers
  mapM_ addGroup  (rosterGroups r)
  mapM_ addEnroll (rosterEnrolls r)
  return ()

createUser :: EnrollStudent -> Controller CreateUser
createUser (EnrollStudent {..}) = do 
  password <- genRandomText
  let crUser = CreateUser esEmail password esFirstName esLastName
  return crUser 

{-@ genRandomText :: TaggedT<{\_ -> True}, {\_ -> False}> _ _@-}
genRandomText :: Controller T.Text
genRandomText = do
  bytes <- liftTIO (getRandomBytes 24)
  return $ T.decodeUtf8 $ B64Url.encode bytes

rosterGroups :: Roster -> [CreateGroup]
rosterGroups (Roster {..}) =
  [ CreateGroup rosterClass bufferId bufferHash | Buffer {..} <- rosterBuffers ]

rosterEnrolls :: Roster -> [CreateEnroll]
rosterEnrolls (Roster {..}) = 
  [ CreateEnroll esEmail rosterClass esGroup | EnrollStudent {..} <- rosterStudents ]

-------------------------------------------------------------------------------
-- | Add a user ---------------------------------------------------------------
-------------------------------------------------------------------------------
{-@ ignore addUser @-}
addUser :: CreateUser -> Controller (Maybe UserId)
addUser r@(CreateUser {..}) = do
  Log.log Log.INFO ("addUser: " ++ show r)
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 userPassword))
  let msg = "addUser: duplicate email " ++ T.unpack userEmail
  insertOrMsg msg $ mkUser userEmail encrypted userFirst userLast False

-------------------------------------------------------------------------------
-- | Add a class --------------------------------------------------------------
-------------------------------------------------------------------------------

{-@ ignore addClass @-}
addClass :: CreateClass -> Controller (Maybe ClassId)
addClass r@(CreateClass {..}) = do
  Log.log Log.INFO ("addClass: " ++ show r)
  instrId <- lookupUserId classInstructor
  let msg = "addClass: duplicate class" ++ show r
  insertOrMsg msg $ mkClass classInstitution className instrId

-------------------------------------------------------------------------------
-- | Add a group from cmd-line ------------------------------------------------
-------------------------------------------------------------------------------

{-@ ignore addGroup @-}
addGroup :: CreateGroup -> Controller (Maybe GroupId)
addGroup r@(CreateGroup {..}) = do
  Log.log Log.INFO ("addGroup: " ++ show r)
  clsId <- lookupClassId groupClass
  let msg = "addGroup: duplicate group " ++ show r
  insertOrMsg msg $ mkGroup groupName groupEditorLink clsId

-------------------------------------------------------------------------------
-- | Add an enroll, i.e. student to a group ----------------------------------- 
-------------------------------------------------------------------------------

{-@ ignore addEnroll @-}
addEnroll :: CreateEnroll -> Controller (Maybe EnrollId)
addEnroll r@(CreateEnroll {..}) = do
  Log.log Log.INFO ("addEnroll: " ++ show r)
  studentId <- lookupUserId enrollStudent
  groupId   <- lookupGroupId enrollGroup
  let msg = "addGroup: duplicate enroll" ++ show r
  insertOrMsg msg $ mkEnroll studentId groupId

lookupUserId :: T.Text -> Controller UserId
lookupUserId email = do
  r <- selectFirstOrCrash (userEmailAddress' ==. email)
  project userId' r

lookupGroupId :: T.Text -> Controller GroupId
lookupGroupId name = do
  r <- selectFirstOrCrash (groupName' ==. name)
  project groupId' r

lookupClassId :: T.Text -> Controller ClassId
lookupClassId name = do
  r <- selectFirstOrCrash (className' ==. name)
  project classId' r

