module Crypto
  ( genRandomCodes
  , genRandomCode
  , encryptPassTIO'
  , module Crypto.Scrypt
  )
where

import           Control.Monad.Time             ( MonadTime(..) )
import           Crypto.JWT                     ( MonadRandom(..) )
import           Crypto.Scrypt
import           Data.Text                      ( Text(..) )
import qualified Data.Text.Encoding            as T
import qualified Data.ByteString.Base64.URL    as B64Url
import           Control.Monad                  ( replicateM )

import           Controllers
import           Binah.Infrastructure
import           Binah.Filters
import           Model

instance MonadRandom TIO where
  getRandomBytes x = TIO (getRandomBytes x)

instance MonadTime TIO where
  currentTime = TIO currentTime

{-@ ignore genRandomCodes @-}
{-@ genRandomCodes :: _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ @-}
genRandomCodes :: Int -> Controller [Text]
genRandomCodes n = replicateM n genRandomCode

{-@ ignore genRandomCode @-}
{-@ genRandomCode :: TaggedT<{\_ -> True}, {\_ -> False}> _ _@-}
genRandomCode :: Controller Text
genRandomCode = do
  bytes <- liftTIO (getRandomBytes 24)
  return $ T.decodeUtf8 $ B64Url.encode bytes

----------------------------------------------------------------------------------------------------
-- | Scrypto
----------------------------------------------------------------------------------------------------

encryptPassTIO' :: MonadTIO m => Pass -> m EncryptedPass
encryptPassTIO' = liftTIO . TIO . encryptPassIO'
