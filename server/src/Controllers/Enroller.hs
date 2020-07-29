{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Enroller 
       ( addUser 
       , addClass
       , addGroup
       , addEnroll 
       , enrollStudents 
       )
       where

import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
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
import qualified Frankie.Log as Log
import           Controllers
import           Controllers.Invitation         ( InvitationCode(..) )
import           Model
import           JSON
import           Crypto
import           Types

import qualified Debug.Trace 


-- Add a full roster of students to a class using an Enrole -----------------
enrollStudents :: Controller ()
enrollStudents = do
  instr <- requireAuthUser
  roster@Enrole {..} <- decodeBody 
  mapM_ (insertUser   instr) (enroleUsers roster)
  mapM_ (insertGroup  instr) (enroleGroups roster)
  mapM_ (insertEnroll instr) (enroleEnrolls roster)
  return ()

insertUser :: Entity User -> CreateUser -> Controller () 
insertUser instr = _fixme

insertGroup :: Entity User -> CreateGroup -> Controller () 
insertGroup = _fixme

insertEnroll :: Entity User -> CreateEnroll -> Controller () 
insertEnroll = _fixme

enroleUsers :: Enrole -> [CreateUser]
enroleUsers = _fixme

enroleGroups :: Enrole -> [CreateGroup] 
enroleGroups = _fixme 

enroleEnrolls :: Enrole -> [CreateEnroll]
enroleEnrolls = _fixme 

-- Add a user from cmd-line -------------------------------------------------
{-@ ignore addUser @-}
addUser :: CreateUser -> Task (Maybe UserId)
addUser r@(CreateUser {..}) = do
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 userPassword))
  let msg = "addUser: duplicate email " ++ T.unpack userEmail 
  insertOrMsg msg $ mkUser userEmail encrypted userFirst userLast False

-- Add a class from cmd-line ------------------------------------------------
{-@ ignore addClass @-}
addClass :: CreateClass -> Task (Maybe ClassId)
addClass r@(CreateClass {..}) = do
  instrId <- lookupUserId classInstructor
  let msg =  "addClass: duplicate class" ++ show r 
  insertOrMsg msg $ mkClass classInstitution className instrId

-- Add a group from cmd-line ------------------------------------------------
{-@ ignore addGroup @-}
addGroup :: CreateGroup -> Task GroupId
addGroup (CreateGroup {..}) = do 
  clsId <- lookupClassId groupClass
  insert $ mkGroup groupName groupEditorLink clsId 

-- Add an enroll, i.e. student to a group from cmd-line ----------------------------------------------
{-@ ignore addEnroll @-}
addEnroll :: CreateEnroll -> Task EnrollId
addEnroll (CreateEnroll {..}) = do
  studentId <- lookupUserId  enrollStudent
  groupId   <- lookupGroupId enrollGroup 
  insert     $ mkEnroll studentId groupId

lookupUserId :: T.Text -> Task UserId
lookupUserId email = do
  r <- selectFirstOrCrash (userEmailAddress' ==. email)
  project userId' r

lookupGroupId :: T.Text -> Task GroupId
lookupGroupId name = do
  r <- selectFirstOrCrash (groupName' ==. name)
  project groupId' r

lookupClassId :: T.Text -> Task ClassId
lookupClassId name = do
  r <- selectFirstOrCrash (className' ==. name)
  project classId' r

