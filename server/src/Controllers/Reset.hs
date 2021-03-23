{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Reset (reset, resetPass) where

import           Control.Monad.Time             ( MonadTime(..) )
import           Data.Text                      ( Text(..) )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Encoding            as T
import           Frankie.Config ( MonadConfig(getConfig) )
import qualified Frankie.Log                   as Log
import           Storm.Updates
import           Storm.Insert
import           Storm.Filters
import           Storm.Helpers
import           Storm.Frankie
import           Storm.SMTP ( publicAddress, sendMailWithLoginSTARTTLS, simpleMail' )
import Storm.Crypto
    ( encryptPassTIO', EncryptedPass(EncryptedPass), Pass(Pass) )
import           Storm.JSON
import           Controllers
import           Controllers.Class              ( sendMail, genRandomText )
import           Model
import           Types

-------------------------------------------------------------------------------
-- | Reset password : generate a random code and send to user's email
-------------------------------------------------------------------------------
{-@ reset :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
reset :: Controller ()
reset = do
  ResetInfo {..} <- decodeBody
  user           <- selectFirstOr
                      (errorResponse status401 (Just "Unknown email address"))
                      (userEmailAddress' ==. resetEmailAddress)
  _    <- updateWhere
            (resetPasswordEmail' ==. resetEmailAddress)
            (resetPasswordValid' `assign` False)
  code <- genRandomText
  insert (mkResetPassword resetEmailAddress code True)
  res <- sendMail resetSubject (resetBody code) resetEmailAddress
  case res of
    Right _ -> respondJSON status200  ("OK: Please check " <> resetEmailAddress)
    Left _  -> respondError status401 (Just "Error sending email!")

-- {-@ sendResetMail :: _ -> _ -> TaggedT<{\_ -> True}, {\_ -> True}> _ _ _ @-}
-- sendResetMail :: Text -> Text -> Controller ()
-- sendResetMail code userEmail = do
--   SMTPConfig{..} <- configSMTP <$> getConfig
--   let subject = "VOLTRON Password Reset Code"
--   let to      = publicAddress userEmail
--   let from    = publicAddress (T.pack smtpUser)
--   let mail    = simpleMail' to from subject (mkBody code)
--   res        <- sendMailWithLoginSTARTTLS smtpHost smtpUser smtpPass mail
--   logT Log.INFO ("reset email: " ++ show res)
--   case res of
--     Right _ -> return ()
--     Left _  -> respondError status401 (Just "Error sending email!")

-- mkBody :: Text -> LT.Text
-- mkBody code =
--   LT.fromStrict
--     $  "Please use the following code to reset your VOLTRON password: "
--     <> code

resetSubject :: Text
resetSubject = "VOLTRON Password Reset Code"

resetBody :: Text -> Text
resetBody code = "Please use the following code to reset your VOLTRON password: " <> code



-------------------------------------------------------------------------------
-- | `resetPass` actually resets the password using a previously mailed code
-------------------------------------------------------------------------------
{-@ resetPass :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
resetPass :: Controller ()
resetPass = do
  ResetPassInfo {..} <- decodeBody
  -- 1. Check if the code is valid
  _ <- selectFirstOr
        (errorResponse status403 (Just "invalid reset code"))
        ((resetPasswordCode'  ==. resetPassCode) &&:
         (resetPasswordEmail' ==. resetPassEmail) &&:
         (resetPasswordValid' ==. True))
  -- 2. Invalidate the code
  _  <- updateWhere
          (resetPasswordCode' ==. resetPassCode)
          (resetPasswordValid' `assign` False)
  -- 3. Update the user's information
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 resetPassPassword))
  _  <- updateWhere
          (userEmailAddress' ==. resetPassEmail)
          (userPassword' `assign` encrypted)
  respondJSON status200 ("OK: Login with new password" :: T.Text)
