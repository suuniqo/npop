module Config
  ( progName
  , listenBacklog
  , backoffMin
  , backoffMax
  , idleTimeout
  , readChunk
  , storageRoot
  , maxSizeQuery
  , maxSizeResponse
  ) where

-- Auxiliary

secInMicro :: Int
secInMicro = 1000000

minInMicro :: Int
minInMicro = 60 * secInMicro

-- Flexible (Admin)

progName :: String
progName = "nano-pop"

listenBacklog :: Int
listenBacklog = 16

backoffMin :: Int
backoffMin = secInMicro `div` 16

backoffMax :: Int
backoffMax = secInMicro * 16

readChunk :: Int
readChunk = 4096

storageRoot :: FilePath
storageRoot = "/Users/sunico/mail"

-- Fixed (Protocol)

maxSizeKeyword :: Int
maxSizeKeyword = 4

maxSizeArg :: Int
maxSizeArg = 40

maxSizeQuery :: Int
maxSizeQuery = maxSizeKeyword + 1 + maxSizeArg + 2

maxSizeResponse :: Int
maxSizeResponse = 512

idleTimeout :: Int
idleTimeout = minInMicro * 10
