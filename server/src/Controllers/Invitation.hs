{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Invitation where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LT
import           Data.Int                       ( Int64 )
import           Data.Maybe
import           Data.Aeson.Types
import           Database.Persist.Sql           ( fromSqlKey
                                                , toSqlKey
                                                )
import           GHC.Generics
import           Text.Mustache                  ( (~>) )
import qualified Text.Mustache.Types           as Mustache
import qualified Network.Mail.Mime             as M


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
import           Crypto                         ( genRandomCodes )
import           Text.Read                      ( readMaybe )
import           SMTP
import           Exception
import           System.IO.Unsafe               ( unsafePerformIO )

--------------------------------------------------------------------------------
-- | Invitation Put (create invitations)
--------------------------------------------------------------------------------

{-@ invitationPut :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
invitationPut :: Controller ()
invitationPut = do
  viewer           <- requireAuthUser
  _                <- requireInstructor viewer
  (PutReq reqData) <- decodeBody
  codes            <- genRandomCodes (length reqData)
  let invitations = zipWith
        (\InvitationInsert {..} code -> mkInvitation code
                                                     insertEmailAddress
                                                     insertFirstName
                                                     insertLastName
                                                     -- insertInstitution
                                                     False
                                                     "not_sent"
                                                     Nothing
        )
        reqData
        codes
  ids <- insertMany invitations
  _   <- runTask (sendEmails ids)
  respondJSON status201 (object ["keys" .= map fromSqlKey ids])

data EmailData = EmailData
  { emailDataInvitationId :: InvitationId
  , emailDataInvitationCode :: Text
  , emailDataFirstName :: Text
  , emailDataLastName :: Text
  , emailDataFullName :: Text
  }

instance TemplateData EmailData where
  templateFile = "invitation.json.mustache"
  toMustache (EmailData id code firstName lastName fullName) = Mustache.object
    [ "invitationId" ~> id
    , "invitationCode" ~> code
    , "firstName" ~> firstName
    , "lastName" ~> lastName
    , "fullName" ~> fullName
    ]

data EmailRender = EmailRender
  { emailRenderBodyHtml :: LT.Text
  , emailRenderBodyPlain :: LT.Text
  , emailRenderSubject :: Text
  , emailRenderFrom :: Text
  }
  deriving (Generic, Show)

instance FromJSON EmailRender where
  parseJSON = genericParseJSON (stripPrefix "emailRender")

sendEmails :: [InvitationId] -> Task ()
sendEmails ids = do
  invitations <- selectList (invitationId' <-. ids)
  forMC invitations $ \invitation -> do
    id   <- project invitationId' invitation
    mail <- renderEmail invitation
    res  <- tryT $ renderSendMail mail
    case res of
      Left (SomeException e) -> do
        let up1 = invitationEmailError' `assign` Just (show e)
        let up2 = invitationEmailStatus' `assign` "error"
        updateWhere (invitationId' ==. id) (up1 `combine` up2)
      Right _ -> updateWhere (invitationId' ==. id) (invitationEmailStatus' `assign` "sent")
  return ()


renderEmail :: Entity Invitation -> Task Mail
renderEmail invitation = do
  id           <- project invitationId' invitation
  emailAddress <- project invitationEmailAddress' invitation
  code         <- project invitationCode' invitation
  firstName    <- project invitationFirstName' invitation
  lastName     <- project invitationLastName' invitation
  let fullName = T.strip $ firstName `T.append` " " `T.append` lastName
  raw <- renderTemplate (EmailData id code firstName lastName fullName)
  let EmailRender {..} = fromJust $ decode (LT.encodeUtf8 (LT.fromStrict raw))
  let to               = mkPublicAddress (Just fullName) emailAddress
  let from             = mkPublicAddress Nothing emailRenderFrom
  return $ simpleMail from
                      [to]
                      []
                      []
                      emailRenderSubject
                      [M.htmlPart emailRenderBodyHtml, M.plainPart emailRenderBodyPlain]


newtype PutReq = PutReq [InvitationInsert]
  deriving Generic

instance FromJSON PutReq where
  parseJSON = genericParseJSON defaultOptions

--------------------------------------------------------------------------------
-- | Invitation Get
--------------------------------------------------------------------------------

{-@ invitationGet :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
invitationGet :: Int64 -> Controller ()
invitationGet iid = do
  let id = toSqlKey iid :: InvitationId
  code <- listToMaybe <$> queryParams "code"
  case code of
    Nothing   -> respondError status400 (Just "missing code")
    Just code -> do
      invitation <- selectFirstOr
        notFoundJSON
        (invitationCode' ==. code &&: invitationId' ==. id &&: invitationAccepted' ==. False)
      res <- extractInvitationData invitation
      respondJSON status200 res

--------------------------------------------------------------------------------
-- | Invitation List
--------------------------------------------------------------------------------

{-@ invitationList :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ @-}
invitationList :: Controller ()
invitationList = do
  viewer      <- requireAuthUser
  _           <- requireInstructor viewer
  invitations <- selectList trueF
  res         <- mapMC extractInvitationData invitations
  respondJSON status200 res

--------------------------------------------------------------------------------
-- | Invitation Data
--------------------------------------------------------------------------------

data InvitationInsert = InvitationInsert
  { insertEmailAddress :: Text
  , insertFirstName    :: Text
  , insertLastName     :: Text
  -- , insertInstitution  :: Text
  }
  deriving Generic

instance FromJSON InvitationInsert where
  parseJSON = genericParseJSON (stripPrefix "insert")

data InvitationData = InvitationData
  { invitationId           :: InvitationId
  , invitationEmailAddress :: Text
  , invitationFirstName    :: Text
  , invitationLastName     :: Text
 -- , invitationInstitution  :: Text
  , invitationAccepted     :: Bool
  }
  deriving Generic

extractInvitationData :: Entity Invitation -> Controller InvitationData
extractInvitationData invitation =
  InvitationData
    <$> project invitationId'           invitation
    <*> project invitationEmailAddress' invitation
    <*> project invitationFirstName'    invitation
    <*> project invitationLastName'     invitation
    -- <*> project invitationInstitution'  invitation
    <*> project invitationAccepted'     invitation

instance ToJSON InvitationData where
  toEncoding = genericToEncoding (stripPrefix "invitation")

data InvitationCode = InvitationCode InvitationId Text deriving Generic

instance FromJSON InvitationCode where
  parseJSON = withText "InvitationCode" parse
   where
    parse t = case parseCode t of
      Nothing -> fail "Invalid invitation id"
      Just id -> return id

parseCode :: Text -> Maybe InvitationCode
parseCode text = case readMaybe (T.unpack h) of
  Nothing -> Nothing
  Just id -> Just $ InvitationCode (toSqlKey id) (T.drop 1 t)
  where (h, t) = T.breakOn "." text