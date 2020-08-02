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
  , getRoster
  , genRandomText
  )
where

import qualified Data.HashMap.Strict           as M
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
-- | Respond with Current [EnrollStudent] for className -----------------------
-------------------------------------------------------------------------------

getRoster :: T.Text -> Controller ()
getRoster className = do
  instr   <- requireAuthUser
  -- TODO: check that `instr` is an instructor for className
  clsId   <- lookupClassId className
  enrolls <- selectList (enrollClass' ==. clsId)
  roster  <- mapMC enrollEnrollStudent enrolls
  respondJSON status200 roster 

enrollEnrollStudent :: Entity Enroll -> Controller EnrollStudent
enrollEnrollStudent enroll = do
  userId  <- project enrollStudent' enroll
  user    <- selectFirstOr notFoundJSON (userId' ==. userId)
  groupId <- project enrollGroup' enroll
  group   <- selectFirstOr notFoundJSON (groupId' ==. groupId)
  EnrollStudent
    `fmap` project userFirstName'    user
    <*>    project userLastName'     user
    <*>    project userEmailAddress' user
    <*>    project groupName'        group

  

-------------------------------------------------------------------------------
-- | Add a full roster of students to a class using an Roster -----------------
-------------------------------------------------------------------------------

addRoster :: Controller ()
addRoster = do
  instr         <- requireAuthUser
  r@Roster {..} <- decodeBody
  crUsers       <- mapM createUser rosterStudents

  clsId <- lookupClassId rosterClass
  mapM_ addUser   crUsers
  mapM_ (addGroup  clsId) (rosterGroups r)
  mapM_ (addEnroll clsId) (rosterEnrolls r)
  getRoster rosterClass 
  -- respondJSON status200 ("OK:addRoster" :: T.Text)

createUser :: EnrollStudent -> Controller CreateUser
createUser (EnrollStudent {..}) = do 
  password <- genRandomText
  let crUser = mkCreateUser esEmail password esFirstName esLastName
  return crUser 

{-@ genRandomText :: TaggedT<{\_ -> True}, {\_ -> False}> _ _@-}
genRandomText :: Controller T.Text
genRandomText = do
  bytes <- liftTIO (getRandomBytes 24)
  return $ T.decodeUtf8 $ B64Url.encode bytes

rosterGroups :: Roster -> [CreateGroup]
rosterGroups (Roster {..}) = [ g | (_, g) <- M.toList groupM ]
  where
    groupM = M.fromList [ (bufferId , group)
                          | Buffer {..} <- rosterBuffers 
                          , let group    = mkCreateGroup rosterClass bufferId bufferHash
                        ]

rosterEnrolls :: Roster -> [CreateEnroll]
rosterEnrolls (Roster {..}) = 
  [ mkCreateEnroll esEmail rosterClass esGroup | EnrollStudent {..} <- rosterStudents ]

-------------------------------------------------------------------------------
-- | Add a user ---------------------------------------------------------------
-------------------------------------------------------------------------------
{-@ ignore addUser @-}
-- addUser :: CreateUser -> Controller (Maybe UserId)
addUser :: (MonadTIO m) => CreateUser -> TasCon m (Maybe UserId)
addUser r@(CreateUser {..}) = do
  let email' = T.strip userEmail
  let first' = T.strip userFirst 
  Log.log Log.INFO ("addUser: " ++ show r)
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 userPassword))
  let msg = "addUser: duplicate email " ++ T.unpack userEmail
  insertOrMsg msg $ mkUser userEmail encrypted userFirst userLast False

-------------------------------------------------------------------------------
-- | Add a class --------------------------------------------------------------
-------------------------------------------------------------------------------

{-@ ignore addClass @-}
-- addClass :: CreateClass -> Controller (Maybe ClassId)
addClass :: (MonadTIO m) => CreateClass -> TasCon m (Maybe ClassId)
addClass r@(CreateClass {..}) = do
  Log.log Log.INFO ("addClass: " ++ show r)
  instrId <- lookupUserId classInstructor
  let msg = "addClass: duplicate class" ++ show r
  insertOrMsg msg $ mkClass classInstitution className instrId

-------------------------------------------------------------------------------
-- | Add a group from cmd-line ------------------------------------------------
-------------------------------------------------------------------------------

{-@ ignore addGroup @-}
addGroup :: ClassId -> CreateGroup -> Controller (Maybe GroupId)
addGroup clsId r@(CreateGroup {..}) = do
  Log.log Log.INFO ("addGroup: " ++ show r)
  let msg = "addGroup: duplicate group " ++ show r
  insertOrMsg msg $ mkGroup groupName groupEditorLink clsId

-------------------------------------------------------------------------------
-- | Add an enroll, i.e. student to a group ----------------------------------- 
-------------------------------------------------------------------------------

{-@ ignore addEnroll @-}
addEnroll :: ClassId -> CreateEnroll -> Controller (Maybe EnrollId)
addEnroll clsId r@(CreateEnroll {..}) = do
  Log.log Log.INFO ("addEnroll: " ++ show r)
  studentId <- lookupUserId enrollStudent
  groupId   <- lookupGroupId enrollGroup
  let msg = "addGroup: duplicate enroll" ++ show r
  insertOrMsg msg $ mkEnroll studentId clsId groupId

lookupUserId :: (MonadTIO m) => T.Text -> TasCon m UserId 
-- lookupUserId :: T.Text -> Controller UserId
lookupUserId email = do
  r <- selectFirstOrCrash (userEmailAddress' ==. email)
  project userId' r

lookupGroupId :: (MonadTIO m) => T.Text -> TasCon m GroupId 
-- lookupGroupId :: T.Text -> Controller GroupId
lookupGroupId name = do
  r <- selectFirstOrCrash (groupName' ==. name)
  project groupId' r


lookupClassId :: (MonadTIO m) => T.Text -> TasCon m ClassId
-- lookupClassId :: T.Text -> Controller ClassId
lookupClassId name = do
  r <- selectFirstOrCrash (className' ==. name)
  project classId' r

