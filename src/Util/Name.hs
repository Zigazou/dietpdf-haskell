-- |
-- This module allows to encode a PDF name.
module Util.Name
  ( fromName
  ) where

import Data.ByteString qualified as BS
import Data.Word (Word8)

import Util.Ascii
    ( asciiSOLIDUS
    , pattern AsciiCR
    , pattern AsciiFF
    , pattern AsciiGREATERTHANSIGN
    , pattern AsciiHT
    , pattern AsciiLEFTCURLYBRACKET
    , pattern AsciiLEFTPARENTHESIS
    , pattern AsciiLEFTSQUAREBRACKET
    , pattern AsciiLESSTHANSIGN
    , pattern AsciiLF
    , pattern AsciiNUL
    , pattern AsciiNUMBERSIGN
    , pattern AsciiPERCENTSIGN
    , pattern AsciiRIGHTCURLYBRACKET
    , pattern AsciiRIGHTPARENTHESIS
    , pattern AsciiRIGHTSQUAREBRACKET
    , pattern AsciiSOLIDUS
    , pattern AsciiSPACE
    )

escapeChar :: Word8 -> BS.ByteString
escapeChar AsciiNUL                = error "NUL char not allowed in name"
escapeChar AsciiHT                 = "#09"
escapeChar AsciiCR                 = "#0D"
escapeChar AsciiLF                 = "#0A"
escapeChar AsciiFF                 = "#0C"
escapeChar AsciiSPACE              = "#20"
escapeChar AsciiNUMBERSIGN         = "#23"
escapeChar AsciiLEFTPARENTHESIS    = "#28"
escapeChar AsciiRIGHTPARENTHESIS   = "#29"
escapeChar AsciiLESSTHANSIGN       = "#3C"
escapeChar AsciiGREATERTHANSIGN    = "#3E"
escapeChar AsciiLEFTSQUAREBRACKET  = "#5B"
escapeChar AsciiRIGHTSQUAREBRACKET = "#5D"
escapeChar AsciiLEFTCURLYBRACKET   = "#7B"
escapeChar AsciiRIGHTCURLYBRACKET  = "#7D"
escapeChar AsciiSOLIDUS            = "#2F"
escapeChar AsciiPERCENTSIGN        = "#25"
escapeChar byte                    = BS.singleton byte

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
