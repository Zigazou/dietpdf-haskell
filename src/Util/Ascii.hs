{-|
The `Ascii` modules just defines constants for all the ASCII characters.

It avoids using literals.

The constants are of type `Word8` in order to make it easy to work with
`ByteString`.
-}
module Util.Ascii
  ( -- * Control characters
    asciiNUL
  , asciiSOH
  , asciiSTX
  , asciiETX
  , asciiEOT
  , asciiENQ
  , asciiACK
  , asciiBEL
  , asciiBS
  , asciiHT
  , asciiLF
  , asciiVT
  , asciiFF
  , asciiCR
  , asciiSO
  , asciiSI
  , asciiDLE
  , asciiDC1
  , asciiDC2
  , asciiDC3
  , asciiDC4
  , asciiNAK
  , asciiSYN
  , asciiETB
  , asciiCAN
  , asciiEM
  , asciiSUB
  , asciiESC
  , asciiFS
  , asciiGS
  , asciiRS
  , asciiUS
  , asciiDELETE
  ,

    -- * Punctuation and other signs
    asciiSPACE
  , asciiEXCLAMATIONMARK
  , asciiQUOTATIONMARK
  , asciiNUMBERSIGN
  , asciiDOLLARSIGN
  , asciiPERCENTSIGN
  , asciiAMPERSAND
  , asciiAPOSTROPHE
  , asciiLEFTPARENTHESIS
  , asciiRIGHTPARENTHESIS
  , asciiASTERISK
  , asciiPLUSSIGN
  , asciiCOMMA
  , asciiHYPHENMINUS
  , asciiFULLSTOP
  , asciiSOLIDUS
  , asciiCOLON
  , asciiSEMICOLON
  , asciiLESSTHANSIGN
  , asciiEQUALSSIGN
  , asciiGREATERTHANSIGN
  , asciiQUESTIONMARK
  , asciiCOMMERCIALAT
  , asciiLEFTSQUAREBRACKET
  , asciiREVERSESOLIDUS
  , asciiRIGHTSQUAREBRACKET
  , asciiCIRCUMFLEXACCENT
  , asciiLOWLINE
  , asciiGRAVEACCENT
  , asciiLEFTCURLYBRACKET
  , asciiVERTICALLINE
  , asciiRIGHTCURLYBRACKET
  , asciiTILDE
  ,

    -- * Digits
    asciiDIGITZERO
  , asciiDIGITONE
  , asciiDIGITTWO
  , asciiDIGITTHREE
  , asciiDIGITFOUR
  , asciiDIGITFIVE
  , asciiDIGITSIX
  , asciiDIGITSEVEN
  , asciiDIGITEIGHT
  , asciiDIGITNINE
  ,

    -- * Uppercase letters
    asciiUPPERA
  , asciiUPPERB
  , asciiUPPERC
  , asciiUPPERD
  , asciiUPPERE
  , asciiUPPERF
  , asciiUPPERG
  , asciiUPPERH
  , asciiUPPERI
  , asciiUPPERJ
  , asciiUPPERK
  , asciiUPPERL
  , asciiUPPERM
  , asciiUPPERN
  , asciiUPPERO
  , asciiUPPERP
  , asciiUPPERQ
  , asciiUPPERR
  , asciiUPPERS
  , asciiUPPERT
  , asciiUPPERU
  , asciiUPPERV
  , asciiUPPERW
  , asciiUPPERX
  , asciiUPPERY
  , asciiUPPERZ
  ,

    -- * Lowercase letters
    asciiLOWERA
  , asciiLOWERB
  , asciiLOWERC
  , asciiLOWERD
  , asciiLOWERE
  , asciiLOWERF
  , asciiLOWERG
  , asciiLOWERH
  , asciiLOWERI
  , asciiLOWERJ
  , asciiLOWERK
  , asciiLOWERL
  , asciiLOWERM
  , asciiLOWERN
  , asciiLOWERO
  , asciiLOWERP
  , asciiLOWERQ
  , asciiLOWERR
  , asciiLOWERS
  , asciiLOWERT
  , asciiLOWERU
  , asciiLOWERV
  , asciiLOWERW
  , asciiLOWERX
  , asciiLOWERY
  , asciiLOWERZ

    -- * Patterns
  , pattern AsciiNUL
  , pattern AsciiSOH
  , pattern AsciiSTX
  , pattern AsciiETX
  , pattern AsciiEOT
  , pattern AsciiENQ
  , pattern AsciiACK
  , pattern AsciiBEL
  , pattern AsciiBS
  , pattern AsciiHT
  , pattern AsciiLF
  , pattern AsciiVT
  , pattern AsciiFF
  , pattern AsciiCR
  , pattern AsciiSO
  , pattern AsciiSI
  , pattern AsciiDLE
  , pattern AsciiDC1
  , pattern AsciiDC2
  , pattern AsciiDC3
  , pattern AsciiDC4
  , pattern AsciiNAK
  , pattern AsciiSYN
  , pattern AsciiETB
  , pattern AsciiCAN
  , pattern AsciiEM
  , pattern AsciiSUB
  , pattern AsciiESC
  , pattern AsciiFS
  , pattern AsciiGS
  , pattern AsciiRS
  , pattern AsciiUS
  , pattern AsciiSPACE
  , pattern AsciiEXCLAMATIONMARK
  , pattern AsciiQUOTATIONMARK
  , pattern AsciiNUMBERSIGN
  , pattern AsciiDOLLARSIGN
  , pattern AsciiPERCENTSIGN
  , pattern AsciiAMPERSAND
  , pattern AsciiAPOSTROPHE
  , pattern AsciiLEFTPARENTHESIS
  , pattern AsciiRIGHTPARENTHESIS
  , pattern AsciiASTERISK
  , pattern AsciiPLUSSIGN
  , pattern AsciiCOMMA
  , pattern AsciiHYPHENMINUS
  , pattern AsciiFULLSTOP
  , pattern AsciiSOLIDUS
  , pattern AsciiCOLON
  , pattern AsciiSEMICOLON
  , pattern AsciiLESSTHANSIGN
  , pattern AsciiEQUALSSIGN
  , pattern AsciiGREATERTHANSIGN
  , pattern AsciiQUESTIONMARK
  , pattern AsciiCOMMERCIALAT
  , pattern AsciiLEFTSQUAREBRACKET
  , pattern AsciiREVERSESOLIDUS
  , pattern AsciiRIGHTSQUAREBRACKET
  , pattern AsciiCIRCUMFLEXACCENT
  , pattern AsciiLOWLINE
  , pattern AsciiGRAVEACCENT
  , pattern AsciiLEFTCURLYBRACKET
  , pattern AsciiVERTICALLINE
  , pattern AsciiRIGHTCURLYBRACKET
  , pattern AsciiTILDE
  , pattern AsciiDIGITZERO
  , pattern AsciiDIGITONE
  , pattern AsciiDIGITTWO
  , pattern AsciiDIGITTHREE
  , pattern AsciiDIGITFOUR
  , pattern AsciiDIGITFIVE
  , pattern AsciiDIGITSIX
  , pattern AsciiDIGITSEVEN
  , pattern AsciiDIGITEIGHT
  , pattern AsciiDIGITNINE
  , pattern AsciiUPPERA
  , pattern AsciiUPPERB
  , pattern AsciiUPPERC
  , pattern AsciiUPPERD
  , pattern AsciiUPPERE
  , pattern AsciiUPPERF
  , pattern AsciiUPPERG
  , pattern AsciiUPPERH
  , pattern AsciiUPPERI
  , pattern AsciiUPPERJ
  , pattern AsciiUPPERK
  , pattern AsciiUPPERL
  , pattern AsciiUPPERM
  , pattern AsciiUPPERN
  , pattern AsciiUPPERO
  , pattern AsciiUPPERP
  , pattern AsciiUPPERQ
  , pattern AsciiUPPERR
  , pattern AsciiUPPERS
  , pattern AsciiUPPERT
  , pattern AsciiUPPERU
  , pattern AsciiUPPERV
  , pattern AsciiUPPERW
  , pattern AsciiUPPERX
  , pattern AsciiUPPERY
  , pattern AsciiUPPERZ
  , pattern AsciiLOWERA
  , pattern AsciiLOWERB
  , pattern AsciiLOWERC
  , pattern AsciiLOWERD
  , pattern AsciiLOWERE
  , pattern AsciiLOWERF
  , pattern AsciiLOWERG
  , pattern AsciiLOWERH
  , pattern AsciiLOWERI
  , pattern AsciiLOWERJ
  , pattern AsciiLOWERK
  , pattern AsciiLOWERL
  , pattern AsciiLOWERM
  , pattern AsciiLOWERN
  , pattern AsciiLOWERO
  , pattern AsciiLOWERP
  , pattern AsciiLOWERQ
  , pattern AsciiLOWERR
  , pattern AsciiLOWERS
  , pattern AsciiLOWERT
  , pattern AsciiLOWERU
  , pattern AsciiLOWERV
  , pattern AsciiLOWERW
  , pattern AsciiLOWERX
  , pattern AsciiLOWERY
  , pattern AsciiLOWERZ
  , pattern AsciiDELETE
  ) where

import Data.Word (Word8)

-- $controlCharacters

-- | Null character '\\0'
asciiNUL :: Word8
asciiNUL = 0
pattern AsciiNUL :: Word8
pattern AsciiNUL = 0

-- | Start Of Heading
asciiSOH :: Word8
asciiSOH = 1
pattern AsciiSOH :: Word8
pattern AsciiSOH = 1

-- | Start Of Text
asciiSTX :: Word8
asciiSTX = 2
pattern AsciiSTX :: Word8
pattern AsciiSTX = 2

-- | End Of Text
asciiETX :: Word8
asciiETX = 3
pattern AsciiETX :: Word8
pattern AsciiETX = 3

-- | End Of Transmission
asciiEOT :: Word8
asciiEOT = 4
pattern AsciiEOT :: Word8
pattern AsciiEOT = 4

-- | ENQuiry
asciiENQ :: Word8
asciiENQ = 5
pattern AsciiENQ :: Word8
pattern AsciiENQ = 5

-- | ACKnowledge
asciiACK :: Word8
asciiACK = 6
pattern AsciiACK :: Word8
pattern AsciiACK = 6

-- | BELl '\\a'
asciiBEL :: Word8
asciiBEL = 7
pattern AsciiBEL :: Word8
pattern AsciiBEL = 7

-- | BackSpace '\\b'
asciiBS :: Word8
asciiBS = 8
pattern AsciiBS :: Word8
pattern AsciiBS = 8

-- | Horizontal Tab '\\t'
asciiHT :: Word8
asciiHT = 9
pattern AsciiHT :: Word8
pattern AsciiHT = 9

-- | Line Feed '\\n' (new line)
asciiLF :: Word8
asciiLF = 10
pattern AsciiLF :: Word8
pattern AsciiLF = 10

-- | Vertical Tab '\\v'
asciiVT :: Word8
asciiVT = 11
pattern AsciiVT :: Word8
pattern AsciiVT = 11

-- | Form Feed '\\f'
asciiFF :: Word8
asciiFF = 12
pattern AsciiFF :: Word8
pattern AsciiFF = 12

-- | Carriage Return '\\r'
asciiCR :: Word8
asciiCR = 13
pattern AsciiCR :: Word8
pattern AsciiCR = 13

-- | Shift Out
asciiSO :: Word8
asciiSO = 14
pattern AsciiSO :: Word8
pattern AsciiSO = 14

-- | Shift In
asciiSI :: Word8
asciiSI = 15
pattern AsciiSI :: Word8
pattern AsciiSI = 15

-- | Data Link Escape
asciiDLE :: Word8
asciiDLE = 16
pattern AsciiDLE :: Word8
pattern AsciiDLE = 16

-- | Device Control 1
asciiDC1 :: Word8
asciiDC1 = 17
pattern AsciiDC1 :: Word8
pattern AsciiDC1 = 17

-- | Device Control 2
asciiDC2 :: Word8
asciiDC2 = 18
pattern AsciiDC2 :: Word8
pattern AsciiDC2 = 18

-- | Device Control 3
asciiDC3 :: Word8
asciiDC3 = 19
pattern AsciiDC3 :: Word8
pattern AsciiDC3 = 19

-- | Device Control 4
asciiDC4 :: Word8
asciiDC4 = 20
pattern AsciiDC4 :: Word8
pattern AsciiDC4 = 20

-- | Negative Acknowledge
asciiNAK :: Word8
asciiNAK = 21
pattern AsciiNAK :: Word8
pattern AsciiNAK = 21

-- | Synchronous Idle
asciiSYN :: Word8
asciiSYN = 22
pattern AsciiSYN :: Word8
pattern AsciiSYN = 22

-- | End of Transmission Block
asciiETB :: Word8
asciiETB = 23
pattern AsciiETB :: Word8
pattern AsciiETB = 23

-- | Cancel
asciiCAN :: Word8
asciiCAN = 24
pattern AsciiCAN :: Word8
pattern AsciiCAN = 24

-- | End of Medium
asciiEM :: Word8
asciiEM = 25
pattern AsciiEM :: Word8
pattern AsciiEM = 25

-- | Substitute
asciiSUB :: Word8
asciiSUB = 26
pattern AsciiSUB :: Word8
pattern AsciiSUB = 26

-- | Escape
asciiESC :: Word8
asciiESC = 27
pattern AsciiESC :: Word8
pattern AsciiESC = 27

-- | File Separator
asciiFS :: Word8
asciiFS = 28
pattern AsciiFS :: Word8
pattern AsciiFS = 28

-- | Group Separator
asciiGS :: Word8
asciiGS = 29
pattern AsciiGS :: Word8
pattern AsciiGS = 29

-- | Record Separator
asciiRS :: Word8
asciiRS = 30
pattern AsciiRS :: Word8
pattern AsciiRS = 30

-- | Unit Separator
asciiUS :: Word8
asciiUS = 31
pattern AsciiUS :: Word8
pattern AsciiUS = 31

-- $visibleCharacters

-- | Space
asciiSPACE :: Word8
asciiSPACE = 32
pattern AsciiSPACE :: Word8
pattern AsciiSPACE = 32

-- | !
asciiEXCLAMATIONMARK :: Word8
asciiEXCLAMATIONMARK = 33
pattern AsciiEXCLAMATIONMARK :: Word8
pattern AsciiEXCLAMATIONMARK = 33

-- | "
asciiQUOTATIONMARK :: Word8
asciiQUOTATIONMARK = 34
pattern AsciiQUOTATIONMARK :: Word8
pattern AsciiQUOTATIONMARK = 34

-- | #
asciiNUMBERSIGN :: Word8
asciiNUMBERSIGN = 35
pattern AsciiNUMBERSIGN :: Word8
pattern AsciiNUMBERSIGN = 35

-- | \$
asciiDOLLARSIGN :: Word8
asciiDOLLARSIGN = 36
pattern AsciiDOLLARSIGN :: Word8
pattern AsciiDOLLARSIGN = 36

-- | %
asciiPERCENTSIGN :: Word8
asciiPERCENTSIGN = 37
pattern AsciiPERCENTSIGN :: Word8
pattern AsciiPERCENTSIGN = 37

-- | &
asciiAMPERSAND :: Word8
asciiAMPERSAND = 38
pattern AsciiAMPERSAND :: Word8
pattern AsciiAMPERSAND = 38

-- | '
asciiAPOSTROPHE :: Word8
asciiAPOSTROPHE = 39
pattern AsciiAPOSTROPHE :: Word8
pattern AsciiAPOSTROPHE = 39

-- | (
asciiLEFTPARENTHESIS :: Word8
asciiLEFTPARENTHESIS = 40
pattern AsciiLEFTPARENTHESIS :: Word8
pattern AsciiLEFTPARENTHESIS = 40

-- | )
asciiRIGHTPARENTHESIS :: Word8
asciiRIGHTPARENTHESIS = 41
pattern AsciiRIGHTPARENTHESIS :: Word8
pattern AsciiRIGHTPARENTHESIS = 41

-- | \*
asciiASTERISK :: Word8
asciiASTERISK = 42
pattern AsciiASTERISK :: Word8
pattern AsciiASTERISK = 42

-- | +
asciiPLUSSIGN :: Word8
asciiPLUSSIGN = 43
pattern AsciiPLUSSIGN :: Word8
pattern AsciiPLUSSIGN = 43

-- | ,
asciiCOMMA :: Word8
asciiCOMMA = 44
pattern AsciiCOMMA :: Word8
pattern AsciiCOMMA = 44

-- | \-
asciiHYPHENMINUS :: Word8
asciiHYPHENMINUS = 45
pattern AsciiHYPHENMINUS :: Word8
pattern AsciiHYPHENMINUS = 45

-- | .
asciiFULLSTOP :: Word8
asciiFULLSTOP = 46
pattern AsciiFULLSTOP :: Word8
pattern AsciiFULLSTOP = 46

-- | /
asciiSOLIDUS :: Word8
asciiSOLIDUS = 47
pattern AsciiSOLIDUS :: Word8
pattern AsciiSOLIDUS = 47

-- | 0
asciiDIGITZERO :: Word8
asciiDIGITZERO = 48
pattern AsciiDIGITZERO :: Word8
pattern AsciiDIGITZERO = 48

-- | 1
asciiDIGITONE :: Word8
asciiDIGITONE = 49
pattern AsciiDIGITONE :: Word8
pattern AsciiDIGITONE = 49

-- | 2
asciiDIGITTWO :: Word8
asciiDIGITTWO = 50
pattern AsciiDIGITTWO :: Word8
pattern AsciiDIGITTWO = 50

-- | 3
asciiDIGITTHREE :: Word8
asciiDIGITTHREE = 51
pattern AsciiDIGITTHREE :: Word8
pattern AsciiDIGITTHREE = 51

-- | 4
asciiDIGITFOUR :: Word8
asciiDIGITFOUR = 52
pattern AsciiDIGITFOUR :: Word8
pattern AsciiDIGITFOUR = 52

-- | 5
asciiDIGITFIVE :: Word8
asciiDIGITFIVE = 53
pattern AsciiDIGITFIVE :: Word8
pattern AsciiDIGITFIVE = 53

-- | 6
asciiDIGITSIX :: Word8
asciiDIGITSIX = 54
pattern AsciiDIGITSIX :: Word8
pattern AsciiDIGITSIX = 54

-- | 7
asciiDIGITSEVEN :: Word8
asciiDIGITSEVEN = 55
pattern AsciiDIGITSEVEN :: Word8
pattern AsciiDIGITSEVEN = 55

-- | 8
asciiDIGITEIGHT :: Word8
asciiDIGITEIGHT = 56
pattern AsciiDIGITEIGHT :: Word8
pattern AsciiDIGITEIGHT = 56

-- | 9
asciiDIGITNINE :: Word8
asciiDIGITNINE = 57
pattern AsciiDIGITNINE :: Word8
pattern AsciiDIGITNINE = 57

-- | :
asciiCOLON :: Word8
asciiCOLON = 58
pattern AsciiCOLON :: Word8
pattern AsciiCOLON = 58

-- | ;
asciiSEMICOLON :: Word8
asciiSEMICOLON = 59
pattern AsciiSEMICOLON :: Word8
pattern AsciiSEMICOLON = 59

-- | <
asciiLESSTHANSIGN :: Word8
asciiLESSTHANSIGN = 60
pattern AsciiLESSTHANSIGN :: Word8
pattern AsciiLESSTHANSIGN = 60

-- | =
asciiEQUALSSIGN :: Word8
asciiEQUALSSIGN = 61
pattern AsciiEQUALSSIGN :: Word8
pattern AsciiEQUALSSIGN = 61

-- | \>
asciiGREATERTHANSIGN :: Word8
asciiGREATERTHANSIGN = 62
pattern AsciiGREATERTHANSIGN :: Word8
pattern AsciiGREATERTHANSIGN = 62

-- | ?
asciiQUESTIONMARK :: Word8
asciiQUESTIONMARK = 63
pattern AsciiQUESTIONMARK :: Word8
pattern AsciiQUESTIONMARK = 63

-- | @
asciiCOMMERCIALAT :: Word8
asciiCOMMERCIALAT = 64
pattern AsciiCOMMERCIALAT :: Word8
pattern AsciiCOMMERCIALAT = 64

-- | A
asciiUPPERA :: Word8
asciiUPPERA = 65
pattern AsciiUPPERA :: Word8
pattern AsciiUPPERA = 65

-- | B
asciiUPPERB :: Word8
asciiUPPERB = 66
pattern AsciiUPPERB :: Word8
pattern AsciiUPPERB = 66

-- | C
asciiUPPERC :: Word8
asciiUPPERC = 67
pattern AsciiUPPERC :: Word8
pattern AsciiUPPERC = 67

-- | D
asciiUPPERD :: Word8
asciiUPPERD = 68
pattern AsciiUPPERD :: Word8
pattern AsciiUPPERD = 68

-- | E
asciiUPPERE :: Word8
asciiUPPERE = 69
pattern AsciiUPPERE :: Word8
pattern AsciiUPPERE = 69

-- | F
asciiUPPERF :: Word8
asciiUPPERF = 70
pattern AsciiUPPERF :: Word8
pattern AsciiUPPERF = 70

-- | G
asciiUPPERG :: Word8
asciiUPPERG = 71
pattern AsciiUPPERG :: Word8
pattern AsciiUPPERG = 71

-- | H
asciiUPPERH :: Word8
asciiUPPERH = 72
pattern AsciiUPPERH :: Word8
pattern AsciiUPPERH = 72

-- | I
asciiUPPERI :: Word8
asciiUPPERI = 73
pattern AsciiUPPERI :: Word8
pattern AsciiUPPERI = 73

-- | J
asciiUPPERJ :: Word8
asciiUPPERJ = 74
pattern AsciiUPPERJ :: Word8
pattern AsciiUPPERJ = 74

-- | K
asciiUPPERK :: Word8
asciiUPPERK = 75
pattern AsciiUPPERK :: Word8
pattern AsciiUPPERK = 75

-- | L
asciiUPPERL :: Word8
asciiUPPERL = 76
pattern AsciiUPPERL :: Word8
pattern AsciiUPPERL = 76

-- | M
asciiUPPERM :: Word8
asciiUPPERM = 77
pattern AsciiUPPERM :: Word8
pattern AsciiUPPERM = 77

-- | N
asciiUPPERN :: Word8
asciiUPPERN = 78
pattern AsciiUPPERN :: Word8
pattern AsciiUPPERN = 78

-- | O
asciiUPPERO :: Word8
asciiUPPERO = 79
pattern AsciiUPPERO :: Word8
pattern AsciiUPPERO = 79

-- | P
asciiUPPERP :: Word8
asciiUPPERP = 80
pattern AsciiUPPERP :: Word8
pattern AsciiUPPERP = 80

-- | Q
asciiUPPERQ :: Word8
asciiUPPERQ = 81
pattern AsciiUPPERQ :: Word8
pattern AsciiUPPERQ = 81

-- | R
asciiUPPERR :: Word8
asciiUPPERR = 82
pattern AsciiUPPERR :: Word8
pattern AsciiUPPERR = 82

-- | S
asciiUPPERS :: Word8
asciiUPPERS = 83
pattern AsciiUPPERS :: Word8
pattern AsciiUPPERS = 83

-- | T
asciiUPPERT :: Word8
asciiUPPERT = 84
pattern AsciiUPPERT :: Word8
pattern AsciiUPPERT = 84

-- | U
asciiUPPERU :: Word8
asciiUPPERU = 85
pattern AsciiUPPERU :: Word8
pattern AsciiUPPERU = 85

-- | V
asciiUPPERV :: Word8
asciiUPPERV = 86
pattern AsciiUPPERV :: Word8
pattern AsciiUPPERV = 86

-- | W
asciiUPPERW :: Word8
asciiUPPERW = 87
pattern AsciiUPPERW :: Word8
pattern AsciiUPPERW = 87

-- | X
asciiUPPERX :: Word8
asciiUPPERX = 88
pattern AsciiUPPERX :: Word8
pattern AsciiUPPERX = 88

-- | Y
asciiUPPERY :: Word8
asciiUPPERY = 89
pattern AsciiUPPERY :: Word8
pattern AsciiUPPERY = 89

-- | Z
asciiUPPERZ :: Word8
asciiUPPERZ = 90
pattern AsciiUPPERZ :: Word8
pattern AsciiUPPERZ = 90

-- | [
asciiLEFTSQUAREBRACKET :: Word8
asciiLEFTSQUAREBRACKET = 91
pattern AsciiLEFTSQUAREBRACKET :: Word8
pattern AsciiLEFTSQUAREBRACKET = 91

-- | \\
asciiREVERSESOLIDUS :: Word8
asciiREVERSESOLIDUS = 92
pattern AsciiREVERSESOLIDUS :: Word8
pattern AsciiREVERSESOLIDUS = 92

-- | ]
asciiRIGHTSQUAREBRACKET :: Word8
asciiRIGHTSQUAREBRACKET = 93
pattern AsciiRIGHTSQUAREBRACKET :: Word8
pattern AsciiRIGHTSQUAREBRACKET = 93

-- | \^
asciiCIRCUMFLEXACCENT :: Word8
asciiCIRCUMFLEXACCENT = 94
pattern AsciiCIRCUMFLEXACCENT :: Word8
pattern AsciiCIRCUMFLEXACCENT = 94

-- | _
asciiLOWLINE :: Word8
asciiLOWLINE = 95
pattern AsciiLOWLINE :: Word8
pattern AsciiLOWLINE = 95

-- | `
asciiGRAVEACCENT :: Word8
asciiGRAVEACCENT = 96
pattern AsciiGRAVEACCENT :: Word8
pattern AsciiGRAVEACCENT = 96

-- | a
asciiLOWERA :: Word8
asciiLOWERA = 97
pattern AsciiLOWERA :: Word8
pattern AsciiLOWERA = 97

-- | b
asciiLOWERB :: Word8
asciiLOWERB = 98
pattern AsciiLOWERB :: Word8
pattern AsciiLOWERB = 98

-- | c
asciiLOWERC :: Word8
asciiLOWERC = 99
pattern AsciiLOWERC :: Word8
pattern AsciiLOWERC = 99

-- | d
asciiLOWERD :: Word8
asciiLOWERD = 100
pattern AsciiLOWERD :: Word8
pattern AsciiLOWERD = 100

-- | e
asciiLOWERE :: Word8
asciiLOWERE = 101
pattern AsciiLOWERE :: Word8
pattern AsciiLOWERE = 101

-- | f
asciiLOWERF :: Word8
asciiLOWERF = 102
pattern AsciiLOWERF :: Word8
pattern AsciiLOWERF = 102

-- | g
asciiLOWERG :: Word8
asciiLOWERG = 103
pattern AsciiLOWERG :: Word8
pattern AsciiLOWERG = 103

-- | h
asciiLOWERH :: Word8
asciiLOWERH = 104
pattern AsciiLOWERH :: Word8
pattern AsciiLOWERH = 104

-- | i
asciiLOWERI :: Word8
asciiLOWERI = 105
pattern AsciiLOWERI :: Word8
pattern AsciiLOWERI = 105

-- | j
asciiLOWERJ :: Word8
asciiLOWERJ = 106
pattern AsciiLOWERJ :: Word8
pattern AsciiLOWERJ = 106

-- | k
asciiLOWERK :: Word8
asciiLOWERK = 107
pattern AsciiLOWERK :: Word8
pattern AsciiLOWERK = 107

-- | l
asciiLOWERL :: Word8
asciiLOWERL = 108
pattern AsciiLOWERL :: Word8
pattern AsciiLOWERL = 108

-- | m
asciiLOWERM :: Word8
asciiLOWERM = 109
pattern AsciiLOWERM :: Word8
pattern AsciiLOWERM = 109

-- | n
asciiLOWERN :: Word8
asciiLOWERN = 110
pattern AsciiLOWERN :: Word8
pattern AsciiLOWERN = 110

-- | o
asciiLOWERO :: Word8
asciiLOWERO = 111
pattern AsciiLOWERO :: Word8
pattern AsciiLOWERO = 111

-- | p
asciiLOWERP :: Word8
asciiLOWERP = 112
pattern AsciiLOWERP :: Word8
pattern AsciiLOWERP = 112

-- | q
asciiLOWERQ :: Word8
asciiLOWERQ = 113
pattern AsciiLOWERQ :: Word8
pattern AsciiLOWERQ = 113

-- | r
asciiLOWERR :: Word8
asciiLOWERR = 114
pattern AsciiLOWERR :: Word8
pattern AsciiLOWERR = 114

-- | s
asciiLOWERS :: Word8
asciiLOWERS = 115
pattern AsciiLOWERS :: Word8
pattern AsciiLOWERS = 115

-- | t
asciiLOWERT :: Word8
asciiLOWERT = 116
pattern AsciiLOWERT :: Word8
pattern AsciiLOWERT = 116

-- | u
asciiLOWERU :: Word8
asciiLOWERU = 117
pattern AsciiLOWERU :: Word8
pattern AsciiLOWERU = 117

-- | v
asciiLOWERV :: Word8
asciiLOWERV = 118
pattern AsciiLOWERV :: Word8
pattern AsciiLOWERV = 118

-- | w
asciiLOWERW :: Word8
asciiLOWERW = 119
pattern AsciiLOWERW :: Word8
pattern AsciiLOWERW = 119

-- | x
asciiLOWERX :: Word8
asciiLOWERX = 120
pattern AsciiLOWERX :: Word8
pattern AsciiLOWERX = 120

-- | y
asciiLOWERY :: Word8
asciiLOWERY = 121
pattern AsciiLOWERY :: Word8
pattern AsciiLOWERY = 121

-- | z
asciiLOWERZ :: Word8
asciiLOWERZ = 122
pattern AsciiLOWERZ :: Word8
pattern AsciiLOWERZ = 122

-- | {
asciiLEFTCURLYBRACKET :: Word8
asciiLEFTCURLYBRACKET = 123
pattern AsciiLEFTCURLYBRACKET :: Word8
pattern AsciiLEFTCURLYBRACKET = 123

-- | \|
asciiVERTICALLINE :: Word8
asciiVERTICALLINE = 124
pattern AsciiVERTICALLINE :: Word8
pattern AsciiVERTICALLINE = 124

-- | }
asciiRIGHTCURLYBRACKET :: Word8
asciiRIGHTCURLYBRACKET = 125
pattern AsciiRIGHTCURLYBRACKET :: Word8
pattern AsciiRIGHTCURLYBRACKET = 125

-- | ~
asciiTILDE :: Word8
asciiTILDE = 126
pattern AsciiTILDE :: Word8
pattern AsciiTILDE = 126

-- | DEL
asciiDELETE :: Word8
asciiDELETE = 127
pattern AsciiDELETE :: Word8
pattern AsciiDELETE = 127
