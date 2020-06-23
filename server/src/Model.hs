{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--compile-spec" @-}

module Model
  ( EntityFieldWrapper(..)
  , migrateAll
  , BinahRecord
  , persistentRecord
  , mkInvitation
  , mkUser
  , mkGroup
  , Invitation
  , User
  , Group
  , invitationId'
  , invitationCode'
  , invitationEmailAddress'
  , invitationFirstName'
  , invitationLastName'
  , invitationAccepted'
  , invitationEmailStatus'
  , invitationEmailError'
  , userId'
  , userEmailAddress'
  , userPassword'
  , userFirstName'
  , userLastName'
  , userLevel'
  , userGroup'
  , groupId'
  , groupName'
  , groupEditorLink'
  , InvitationId
  , UserId
  , GroupId
  )

where

import           Database.Persist               ( Key )
import           Database.Persist.TH            ( share
                                                , mkMigrate
                                                , mkPersist
                                                , sqlSettings
                                                , persistLowerCase
                                                )
import           Data.Text                      ( Text )
import qualified Database.Persist              as Persist

import           Binah.Core

import Data.ByteString (ByteString)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Invitation
  code Text
  emailAddress Text
  firstName Text
  lastName Text
  accepted Bool
  emailStatus String
  emailError String Maybe
  

User
  emailAddress Text
  password ByteString
  firstName Text
  lastName Text
  level String
  group GroupId Maybe
  

Group
  name Text
  editorLink Text
  
|]

{-@
data EntityFieldWrapper record typ < querypolicy :: Entity record -> Entity User -> Bool
                                   , selector :: Entity record -> typ -> Bool
                                   , flippedselector :: typ -> Entity record -> Bool
                                   , capability :: Entity record -> Bool
                                   , updatepolicy :: Entity record -> Entity record -> Entity User -> Bool
                                   > = EntityFieldWrapper _
@-}

data EntityFieldWrapper record typ = EntityFieldWrapper (Persist.EntityField record typ)
{-@ data variance EntityFieldWrapper covariant covariant invariant invariant invariant invariant invariant @-}

{-@ measure currentUser :: Entity User @-}

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------

{-@ predicate IsSelf USER VIEWER = VIEWER == USER @-}

{-@ predicate IsInstructor USER = userLevel (entityVal USER) == "instructor" @-}

--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

{-@ data BinahRecord record <
    p :: Entity record -> Bool
  , insertpolicy :: Entity record -> Entity User -> Bool
  , querypolicy  :: Entity record -> Entity User -> Bool
  >
  = BinahRecord _
@-}
data BinahRecord record = BinahRecord record
{-@ data variance BinahRecord invariant covariant invariant invariant @-}

{-@ persistentRecord :: BinahRecord record -> record @-}
persistentRecord :: BinahRecord record -> record
persistentRecord (BinahRecord record) = record

{-@ measure getJust :: Key record -> Entity record @-}

-- * Invitation
{-@ mkInvitation ::
     x_0: Text
  -> x_1: Text
  -> x_2: Text
  -> x_3: Text
  -> x_4: Bool
  -> x_5: String
  -> x_6: (Maybe String)
  -> BinahRecord <
       {\row -> invitationCode (entityVal row) == x_0 && invitationEmailAddress (entityVal row) == x_1 && invitationFirstName (entityVal row) == x_2 && invitationLastName (entityVal row) == x_3 && invitationAccepted (entityVal row) == x_4 && invitationEmailStatus (entityVal row) == x_5 && invitationEmailError (entityVal row) == x_6}
     , {\invitation viewer -> not (invitationAccepted (entityVal invitation)) && IsInstructor viewer && emailStatus invitation == "not_sent"}
     , {\x_0 x_1 -> False}
     > Invitation
@-}
mkInvitation x_0 x_1 x_2 x_3 x_4 x_5 x_6 = BinahRecord (Invitation x_0 x_1 x_2 x_3 x_4 x_5 x_6)

{-@ invariant {v: Entity Invitation | v == getJust (entityKey v)} @-}



{-@ assume invitationId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > _ _
@-}
invitationId' :: EntityFieldWrapper Invitation InvitationId
invitationId' = EntityFieldWrapper InvitationId

{-@ measure invitationCode :: Invitation -> Text @-}

{-@ measure invitationCodeCap :: Entity Invitation -> Bool @-}

{-@ assume invitationCode' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationCode (entityVal row)}
  , {\field row  -> field == invitationCode (entityVal row)}
  , {\old -> invitationCodeCap old}
  , {\old _ _ -> invitationCodeCap old}
  > _ _
@-}
invitationCode' :: EntityFieldWrapper Invitation Text
invitationCode' = EntityFieldWrapper InvitationCode

{-@ measure invitationEmailAddress :: Invitation -> Text @-}

{-@ measure invitationEmailAddressCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailAddress' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationEmailAddress (entityVal row)}
  , {\field row  -> field == invitationEmailAddress (entityVal row)}
  , {\old -> invitationEmailAddressCap old}
  , {\old _ _ -> invitationEmailAddressCap old}
  > _ _
@-}
invitationEmailAddress' :: EntityFieldWrapper Invitation Text
invitationEmailAddress' = EntityFieldWrapper InvitationEmailAddress

{-@ measure invitationFirstName :: Invitation -> Text @-}

{-@ measure invitationFirstNameCap :: Entity Invitation -> Bool @-}

{-@ assume invitationFirstName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationFirstName (entityVal row)}
  , {\field row  -> field == invitationFirstName (entityVal row)}
  , {\old -> invitationFirstNameCap old}
  , {\old _ _ -> invitationFirstNameCap old}
  > _ _
@-}
invitationFirstName' :: EntityFieldWrapper Invitation Text
invitationFirstName' = EntityFieldWrapper InvitationFirstName

{-@ measure invitationLastName :: Invitation -> Text @-}

{-@ measure invitationLastNameCap :: Entity Invitation -> Bool @-}

{-@ assume invitationLastName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationLastName (entityVal row)}
  , {\field row  -> field == invitationLastName (entityVal row)}
  , {\old -> invitationLastNameCap old}
  , {\old _ _ -> invitationLastNameCap old}
  > _ _
@-}
invitationLastName' :: EntityFieldWrapper Invitation Text
invitationLastName' = EntityFieldWrapper InvitationLastName

{-@ measure invitationAccepted :: Invitation -> Bool @-}

{-@ measure invitationAcceptedCap :: Entity Invitation -> Bool @-}

{-@ assume invitationAccepted' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationAccepted (entityVal row)}
  , {\field row  -> field == invitationAccepted (entityVal row)}
  , {\old -> invitationAcceptedCap old}
  , {\x_0 x_1 x_2 -> ((not (invitationAccepted (entityVal x_0)) && invitationAccepted (entityVal x_1))) => (invitationAcceptedCap x_0)}
  > _ _
@-}
invitationAccepted' :: EntityFieldWrapper Invitation Bool
invitationAccepted' = EntityFieldWrapper InvitationAccepted

{-@ measure invitationEmailStatus :: Invitation -> String @-}

{-@ measure invitationEmailStatusCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailStatus' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationEmailStatus (entityVal row)}
  , {\field row  -> field == invitationEmailStatus (entityVal row)}
  , {\old -> invitationEmailStatusCap old}
  , {\x_0 x_1 x_2 -> ((IsInstructor viewer && (emailStatus x_1 == "sent" || emailStatus x_1 == "error"))) => (invitationEmailStatusCap x_0)}
  > _ _
@-}
invitationEmailStatus' :: EntityFieldWrapper Invitation String
invitationEmailStatus' = EntityFieldWrapper InvitationEmailStatus

{-@ measure invitationEmailError :: Invitation -> (Maybe String) @-}

{-@ measure invitationEmailErrorCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailError' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationEmailError (entityVal row)}
  , {\field row  -> field == invitationEmailError (entityVal row)}
  , {\old -> invitationEmailErrorCap old}
  , {\old _ _ -> invitationEmailErrorCap old}
  > _ _
@-}
invitationEmailError' :: EntityFieldWrapper Invitation (Maybe String)
invitationEmailError' = EntityFieldWrapper InvitationEmailError

-- * User
{-@ mkUser ::
     x_0: Text
  -> x_1: ByteString
  -> x_2: Text
  -> x_3: Text
  -> x_4: String
  -> x_5: (Maybe GroupId)
  -> BinahRecord <
       {\row -> userEmailAddress (entityVal row) == x_0 && userPassword (entityVal row) == x_1 && userFirstName (entityVal row) == x_2 && userLastName (entityVal row) == x_3 && userLevel (entityVal row) == x_4 && userGroup (entityVal row) == x_5}
     , {\new viewer -> IsInstructor viewer}
     , {\x_0 x_1 -> (x_1 == x_0)}
     > User
@-}
mkUser x_0 x_1 x_2 x_3 x_4 x_5 = BinahRecord (User x_0 x_1 x_2 x_3 x_4 x_5)

{-@ invariant {v: Entity User | v == getJust (entityKey v)} @-}



{-@ assume userId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > _ _
@-}
userId' :: EntityFieldWrapper User UserId
userId' = EntityFieldWrapper UserId

{-@ measure userEmailAddress :: User -> Text @-}

{-@ measure userEmailAddressCap :: Entity User -> Bool @-}

{-@ assume userEmailAddress' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userEmailAddress (entityVal row)}
  , {\field row  -> field == userEmailAddress (entityVal row)}
  , {\old -> userEmailAddressCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (userEmailAddressCap x_0)}
  > _ _
@-}
userEmailAddress' :: EntityFieldWrapper User Text
userEmailAddress' = EntityFieldWrapper UserEmailAddress

{-@ measure userPassword :: User -> ByteString @-}

{-@ measure userPasswordCap :: Entity User -> Bool @-}

{-@ assume userPassword' :: EntityFieldWrapper <
    {\x_0 x_1 -> (x_1 == x_0)}
  , {\row field  -> field == userPassword (entityVal row)}
  , {\field row  -> field == userPassword (entityVal row)}
  , {\old -> userPasswordCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (userPasswordCap x_0)}
  > _ _
@-}
userPassword' :: EntityFieldWrapper User ByteString
userPassword' = EntityFieldWrapper UserPassword

{-@ measure userFirstName :: User -> Text @-}

{-@ measure userFirstNameCap :: Entity User -> Bool @-}

{-@ assume userFirstName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userFirstName (entityVal row)}
  , {\field row  -> field == userFirstName (entityVal row)}
  , {\old -> userFirstNameCap old}
  , {\x_0 x_1 x_2 -> ((sSelf x_0 x_2)) => (userFirstNameCap x_0)}
  > _ _
@-}
userFirstName' :: EntityFieldWrapper User Text
userFirstName' = EntityFieldWrapper UserFirstName

{-@ measure userLastName :: User -> Text @-}

{-@ measure userLastNameCap :: Entity User -> Bool @-}

{-@ assume userLastName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userLastName (entityVal row)}
  , {\field row  -> field == userLastName (entityVal row)}
  , {\old -> userLastNameCap old}
  , {\x_0 x_1 x_2 -> ((sSelf x_0 x_2)) => (userLastNameCap x_0)}
  > _ _
@-}
userLastName' :: EntityFieldWrapper User Text
userLastName' = EntityFieldWrapper UserLastName

{-@ measure userLevel :: User -> String @-}

{-@ measure userLevelCap :: Entity User -> Bool @-}

{-@ assume userLevel' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userLevel (entityVal row)}
  , {\field row  -> field == userLevel (entityVal row)}
  , {\old -> userLevelCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (userLevelCap x_0)}
  > _ _
@-}
userLevel' :: EntityFieldWrapper User String
userLevel' = EntityFieldWrapper UserLevel

{-@ measure userGroup :: User -> (Maybe GroupId) @-}

{-@ measure userGroupCap :: Entity User -> Bool @-}

{-@ assume userGroup' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userGroup (entityVal row)}
  , {\field row  -> field == userGroup (entityVal row)}
  , {\old -> userGroupCap old}
  , {\old _ _ -> userGroupCap old}
  > _ _
@-}
userGroup' :: EntityFieldWrapper User (Maybe GroupId)
userGroup' = EntityFieldWrapper UserGroup

-- * Group
{-@ mkGroup ::
     x_0: Text
  -> x_1: Text
  -> BinahRecord <
       {\row -> groupName (entityVal row) == x_0 && groupEditorLink (entityVal row) == x_1}
     , {\_ viewer -> IsInstructor viewer}
     , {\x_0 x_1 -> False}
     > Group
@-}
mkGroup x_0 x_1 = BinahRecord (Group x_0 x_1)

{-@ invariant {v: Entity Group | v == getJust (entityKey v)} @-}



{-@ assume groupId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > _ _
@-}
groupId' :: EntityFieldWrapper Group GroupId
groupId' = EntityFieldWrapper GroupId

{-@ measure groupName :: Group -> Text @-}

{-@ measure groupNameCap :: Entity Group -> Bool @-}

{-@ assume groupName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == groupName (entityVal row)}
  , {\field row  -> field == groupName (entityVal row)}
  , {\old -> groupNameCap old}
  , {\x_0 x_1 x_2 -> ((IsInstructor x_2)) => (groupNameCap x_0)}
  > _ _
@-}
groupName' :: EntityFieldWrapper Group Text
groupName' = EntityFieldWrapper GroupName

{-@ measure groupEditorLink :: Group -> Text @-}

{-@ measure groupEditorLinkCap :: Entity Group -> Bool @-}

{-@ assume groupEditorLink' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == groupEditorLink (entityVal row)}
  , {\field row  -> field == groupEditorLink (entityVal row)}
  , {\old -> groupEditorLinkCap old}
  , {\x_0 x_1 x_2 -> ((IsInstructor x_2)) => (groupEditorLinkCap x_0)}
  > _ _
@-}
groupEditorLink' :: EntityFieldWrapper Group Text
groupEditorLink' = EntityFieldWrapper GroupEditorLink

--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------


