{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Class
  ( addUser
  , addClass
  , addGroup
  , addEnroll
  , addRoster
  , getRoster
  , setLanguage
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
import           Binah.SMTP
import qualified Frankie.Log                   as Log
import           Controllers
import           Model
import           JSON
import           Crypto
import           Crypto.Random                  ( getRandomBytes )
import           Types
import           Frankie.Log

-------------------------------------------------------------------------------
-- | Update the language-mode used for a given class --------------------------
-------------------------------------------------------------------------------

{-@ ignore setLanguage @-}
setLanguage :: Controller ()
setLanguage = do
  instr <- requireAuthUser
  ClassLangInfo {..} <- decodeBody
  cls <- selectFirstOr404 (className' ==. cliClass)
  -- TODO: check that `instr` is the instructor for className
  _  <- updateWhere
          (className' ==. cliClass)
          (classEditorLang' `assign` cliLanguage)
  respondJSON status200 ("OK: updated language for " <> cliClass <> " to " <> cliLanguage)

-------------------------------------------------------------------------------
-- | Respond with Current [EnrollStudent] for className -----------------------
-------------------------------------------------------------------------------

{-@ ignore getRoster @-}
getRoster :: T.Text -> Controller ()
getRoster className = do
  instr   <- requireAuthUser
  -- TODO: check that `instr` is an instructor for className
  clsId   <- lookupClassId className
  enrolls <- selectList (enrollClass' ==. clsId)
  roster  <- mapT enrollEnrollStudent enrolls
  respondJSON status200 roster

{-@ ignore enrollEnrollStudent @-}
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

{-@ ignore addRoster @-}
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

{-@ ignore addGroup @-}
addGroup :: ClassId -> CreateGroup -> Controller (Maybe GroupId)
addGroup clsId r@(CreateGroup {..}) = do
  id <- insertMaybe (mkGroup groupName groupEditorLink clsId)
  whenT (isNothing id) (logT Log.WARNING ("addGroup: skipping duplicate group " ++ show r))
  return id

{-@ ignore createUser @-}
createUser :: EnrollStudent -> Controller CreateUser
createUser (EnrollStudent {..}) = do
  password <- genRandomText
  let crUser = mkCreateUser esEmail password esFirstName esLastName "" ""
  return crUser

{-@ genRandomText :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
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
addUser :: (MonadTIO m) => CreateUser -> TasCon m (Maybe UserId)
addUser r@(CreateUser {..}) = do
  logT Log.INFO ("addUser: " ++ show r)
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 crUserPassword))
  maybeId <- insertMaybe (mkUser crUserEmail encrypted crUserFirst crUserLast "" "" False)
  whenT (isNothing maybeId) (logT Log.WARNING ("addUser: skipping duplicate user " ++ show r))
  return maybeId

-------------------------------------------------------------------------------
-- | Add a class --------------------------------------------------------------
-------------------------------------------------------------------------------

{-@ ignore addClass @-}
addClass :: CreateClass -> Task (Maybe ClassId)
addClass r@(CreateClass {..}) = do
  logT Log.INFO ("addClass: " ++ show r)
  maybeInstr <- selectFirst (userEmailAddress' ==. crClassInstructor)
  case maybeInstr of
    Just instr -> do
      instrId <- project userId' instr
      id      <- insertMaybe (mkClass crClassInstitution crClassName instrId crClassLanguage)
      whenT (isNothing id) (logT Log.WARNING ("addClass: skipping duplicate class " ++ show r))
      return id
    Nothing -> do
      logT Log.ERROR ("addClass: cannot find user " ++ show crClassInstructor)
      return Nothing

-------------------------------------------------------------------------------
-- | Add an enroll, i.e. student to a group -----------------------------------
-------------------------------------------------------------------------------

{-@ ignore addEnroll @-}
addEnroll :: ClassId -> CreateEnroll -> Controller EnrollId
addEnroll clsId r@(CreateEnroll {..}) = do
  logT Log.INFO ("addEnroll: " ++ show r)
  student   <- selectFirstOr notFoundJSON (userEmailAddress' ==. enrollStudent)
  studentId <- project userId' student
  groupId   <- lookupGroupId enrollGroup
  insert (mkEnroll studentId clsId groupId)

{-@ ignore lookupGroupId @-}
lookupGroupId :: T.Text -> Controller GroupId
lookupGroupId name = do
  r <- selectFirstOr notFoundJSON (groupName' ==. name)
  project groupId' r

{-@ ignore lookupClassId @-}
lookupClassId :: T.Text -> Controller ClassId
lookupClassId name = do
  r <- selectFirstOr notFoundJSON (className' ==. name)
  project classId' r
