module Pudding.Storage.Header
( Header
, byteHeader
, parseHeader
) where

import Data.ByteString (ByteString)

class Header a where
  byteHeader :: a -> ByteString
  parseHeader :: ByteString -> a
