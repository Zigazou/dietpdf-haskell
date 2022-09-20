{-# LANGUAGE OverloadedStrings #-}
module Codec.Filter.ASCIIHex
  ( encode
  , decode
  ) where

import qualified Data.ByteString               as BS
import           Data.Word                      ( Word8 )
import           Util.Errors                    ( UnifiedError )
import           Util.Ascii                     ( asciiGREATERTHANSIGN )

decode :: BS.ByteString -> Either UnifiedError BS.ByteString
decode = error "todo"

encode :: BS.ByteString -> Either UnifiedError BS.ByteString
encode bytes = return $ BS.snoc (BS.concatMap toHex bytes) asciiGREATERTHANSIGN
 where
  toHex :: Word8 -> BS.ByteString
  toHex byte = 
