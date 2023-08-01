-- |
-- This module allows to encode a PDF name.
module Util.Name
  ( fromName
  ) where

import qualified Data.ByteString               as BS
import           Data.Word                      ( Word8 )
import           Util.Ascii                     ( asciiCR
                                                , asciiFF
                                                , asciiGREATERTHANSIGN
                                                , asciiHT
                                                , asciiLEFTCURLYBRACKET
                                                , asciiLEFTPARENTHESIS
                                                , asciiLEFTSQUAREBRACKET
                                                , asciiLESSTHANSIGN
                                                , asciiLF
                                                , asciiNUL
                                                , asciiNUMBERSIGN
                                                , asciiPERCENTSIGN
                                                , asciiRIGHTCURLYBRACKET
                                                , asciiRIGHTPARENTHESIS
                                                , asciiRIGHTSQUAREBRACKET
                                                , asciiSOLIDUS
                                                , asciiSPACE
                                                )

escapeChar :: Word8 -> BS.ByteString
escapeChar byte | byte == asciiNUL = error "NUL char not allowed in name"
                | byte == asciiHT                 = "#09"
                | byte == asciiCR                 = "#0D"
                | byte == asciiLF                 = "#0A"
                | byte == asciiFF                 = "#0C"
                | byte == asciiSPACE              = "#20"
                | byte == asciiNUMBERSIGN         = "#23"
                | byte == asciiLEFTPARENTHESIS    = "#28"
                | byte == asciiRIGHTPARENTHESIS   = "#29"
                | byte == asciiLESSTHANSIGN       = "#3C"
                | byte == asciiGREATERTHANSIGN    = "#3E"
                | byte == asciiLEFTSQUAREBRACKET  = "#5B"
                | byte == asciiRIGHTSQUAREBRACKET = "#5D"
                | byte == asciiLEFTCURLYBRACKET   = "#7B"
                | byte == asciiRIGHTCURLYBRACKET  = "#7D"
                | byte == asciiSOLIDUS            = "#2F"
                | byte == asciiPERCENTSIGN        = "#25"
                | otherwise                       = BS.singleton byte

{-|
Escape all special characters from a PDF Name and insert a solidus at the
beginning.

The following characters are escaped:

 - `asciiHT`
 - `asciiCR`
 - `asciiLF`
 - `asciiFF`
 - `asciiSPACE`
 - `asciiNUMBERSIGN`
 - `asciiLEFTPARENTHESIS`
 - `asciiRIGHTPARENTHESIS`
 - `asciiLESSTHANSIGN`
 - `asciiGREATERTHANSIGN`
 - `asciiLEFTSQUAREBRACKET`
 - `asciiRIGHTSQUAREBRACKET`
 - `asciiLEFTCURLYBRACKET`
 - `asciiRIGHTCURLYBRACKET`
 - `asciiSOLIDUS`
 - `asciiPERCENTSIGN`

If the PDF Name contains an `asciiNUL` character, it generates an error.
-}
fromName :: BS.ByteString -> BS.ByteString
fromName = BS.cons asciiSOLIDUS . BS.concatMap escapeChar
