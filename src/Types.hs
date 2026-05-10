module Types
  ( Username (..), userValidate
  , MsgNo, toIdx, msgEnum, readMsgNo
  , UID (..), readUID
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Data.Char (isAlphaNum)

-- Username

newtype Username = Username { unUser :: String }

instance Show Username where
  show = show . unUser

userValidate :: ByteString -> Maybe Username
userValidate user
  | BS.all isAlphaNum user = Just $ Username (BS.unpack user)
  | otherwise              = Nothing

-- MsgNo

newtype MsgNo = MsgNo Int
  deriving (Ord, Eq)

instance Show MsgNo where
  show (MsgNo num) = show num

toIdx :: MsgNo -> Int
toIdx (MsgNo num) = num - 1

msgEnum :: [MsgNo]
msgEnum = MsgNo <$> [1..]

maxWord :: Integer
maxWord = toInteger (maxBound :: Int)

readMsgNo :: BS.ByteString -> Maybe MsgNo
readMsgNo bs = do
    (n, rest) <- BS.readInteger bs

    if BS.null rest
       && n > 0
       && n <= maxWord
    then Just (MsgNo $ fromInteger n)
    else Nothing

-- UID

newtype UID = UID { unUID :: String }

instance Show UID where
  show = show . unUID

readUID :: String -> Maybe UID
readUID = Just . UID
