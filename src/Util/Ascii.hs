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
  ) where

import           Data.Word                      ( Word8 )

-- $controlCharacters

asciiNUL, asciiSOH, asciiSTX, asciiETX, asciiEOT, asciiENQ, asciiACK :: Word8
asciiBEL, asciiBS, asciiHT, asciiLF, asciiVT, asciiFF, asciiCR :: Word8
asciiSO, asciiSI, asciiDLE, asciiDC1, asciiDC2, asciiDC3, asciiDC4 :: Word8
asciiNAK, asciiSYN, asciiETB, asciiCAN, asciiEM, asciiSUB, asciiESC :: Word8
asciiFS, asciiGS, asciiRS, asciiUS :: Word8

-- | Null character '\\0'
asciiNUL = 0

-- | Start Of Heading
asciiSOH = 1

-- | Start Of Text
asciiSTX = 2

-- | End Of Text
asciiETX = 3

-- | End Of Transmission
asciiEOT = 4

-- | ENQuiry
asciiENQ = 5

-- | ACKnowledge
asciiACK = 6

-- | BELl '\\a'
asciiBEL = 7

-- | BackSpace '\\b'
asciiBS = 8

-- | Horizontal Tab '\\t'
asciiHT = 9

-- | Line Feed '\\n' (new line)
asciiLF = 10

-- | Vertical Tab '\\v'
asciiVT = 11

-- | Form Feed '\\f'
asciiFF = 12

-- | Carriage Return '\\r'
asciiCR = 13

-- | Shift Out
asciiSO = 14

-- | Shift In
asciiSI = 15

-- | Data Link Escape
asciiDLE = 16

-- | Device Control 1
asciiDC1 = 17

-- | Device Control 2
asciiDC2 = 18

-- | Device Control 3
asciiDC3 = 19

-- | Device Control 4
asciiDC4 = 20

-- | Negative AcKnowledge
asciiNAK = 21

-- | SYNchronous idle
asciiSYN = 22

-- | End of Transmission Block
asciiETB = 23

-- | CANcel
asciiCAN = 24

-- | End of Medium
asciiEM = 25

-- | SUBstitute
asciiSUB = 26

-- | ESCape
asciiESC = 27

-- | File Separator
asciiFS = 28

-- | Group Separator
asciiGS = 29

-- | Record Separator
asciiRS = 30

-- | Unit Separator
asciiUS = 31

-- $visibleCharacters

asciiSPACE, asciiEXCLAMATIONMARK, asciiQUOTATIONMARK, asciiNUMBERSIGN :: Word8
asciiDOLLARSIGN, asciiPERCENTSIGN, asciiAMPERSAND, asciiAPOSTROPHE :: Word8
asciiLEFTPARENTHESIS, asciiRIGHTPARENTHESIS, asciiASTERISK :: Word8
asciiPLUSSIGN, asciiCOMMA, asciiHYPHENMINUS, asciiFULLSTOP :: Word8
asciiSOLIDUS, asciiDIGITZERO, asciiDIGITONE, asciiDIGITTWO :: Word8
asciiDIGITTHREE, asciiDIGITFOUR, asciiDIGITFIVE, asciiDIGITSIX :: Word8
asciiDIGITSEVEN, asciiDIGITEIGHT, asciiDIGITNINE, asciiCOLON :: Word8
asciiSEMICOLON, asciiLESSTHANSIGN, asciiEQUALSSIGN :: Word8
asciiGREATERTHANSIGN, asciiQUESTIONMARK :: Word8

-- | Space
asciiSPACE = 32

-- | !
asciiEXCLAMATIONMARK = 33

-- | "
asciiQUOTATIONMARK = 34

-- | #
asciiNUMBERSIGN = 35

-- | \$
asciiDOLLARSIGN = 36

-- | %
asciiPERCENTSIGN = 37

-- | &
asciiAMPERSAND = 38

-- | '
asciiAPOSTROPHE = 39

-- | (
asciiLEFTPARENTHESIS = 40

-- | )
asciiRIGHTPARENTHESIS = 41

-- | \*
asciiASTERISK = 42

-- | +
asciiPLUSSIGN = 43

-- | ,
asciiCOMMA = 44

-- | \-
asciiHYPHENMINUS = 45

-- | .
asciiFULLSTOP = 46

-- | /
asciiSOLIDUS = 47

-- | 0
asciiDIGITZERO = 48

-- | 1
asciiDIGITONE = 49

-- | 2
asciiDIGITTWO = 50

-- | 3
asciiDIGITTHREE = 51

-- | 4
asciiDIGITFOUR = 52

-- | 5
asciiDIGITFIVE = 53

-- | 6
asciiDIGITSIX = 54

-- | 7
asciiDIGITSEVEN = 55

-- | 8
asciiDIGITEIGHT = 56

-- | 9
asciiDIGITNINE = 57

-- | :
asciiCOLON = 58

-- | ;
asciiSEMICOLON = 59

-- | <
asciiLESSTHANSIGN = 60

-- | =
asciiEQUALSSIGN = 61

-- | \>
asciiGREATERTHANSIGN = 62

-- | ?
asciiQUESTIONMARK = 63

asciiCOMMERCIALAT, asciiUPPERA, asciiUPPERB, asciiUPPERC, asciiUPPERD :: Word8
asciiUPPERE, asciiUPPERF, asciiUPPERG, asciiUPPERH, asciiUPPERI :: Word8
asciiUPPERJ, asciiUPPERK, asciiUPPERL, asciiUPPERM, asciiUPPERN :: Word8
asciiUPPERO, asciiUPPERP, asciiUPPERQ, asciiUPPERR, asciiUPPERS :: Word8
asciiUPPERT, asciiUPPERU, asciiUPPERV, asciiUPPERW, asciiUPPERX :: Word8
asciiUPPERY, asciiUPPERZ, asciiLEFTSQUAREBRACKET, asciiREVERSESOLIDUS :: Word8
asciiRIGHTSQUAREBRACKET, asciiCIRCUMFLEXACCENT, asciiLOWLINE :: Word8

-- | @
asciiCOMMERCIALAT = 64

-- | A
asciiUPPERA = 65

-- | B
asciiUPPERB = 66

-- | C
asciiUPPERC = 67

-- | D
asciiUPPERD = 68

-- | E
asciiUPPERE = 69

-- | F
asciiUPPERF = 70

-- | G
asciiUPPERG = 71

-- | H
asciiUPPERH = 72

-- | I
asciiUPPERI = 73

-- | J
asciiUPPERJ = 74

-- | K
asciiUPPERK = 75

-- | L
asciiUPPERL = 76

-- | M
asciiUPPERM = 77

-- | N
asciiUPPERN = 78

-- | O
asciiUPPERO = 79

-- | P
asciiUPPERP = 80

-- | Q
asciiUPPERQ = 81

-- | R
asciiUPPERR = 82

-- | S
asciiUPPERS = 83

-- | T
asciiUPPERT = 84

-- | U
asciiUPPERU = 85

-- | V
asciiUPPERV = 86

-- | W
asciiUPPERW = 87

-- | X
asciiUPPERX = 88

-- | Y
asciiUPPERY = 89

-- | Z
asciiUPPERZ = 90

-- | [
asciiLEFTSQUAREBRACKET = 91

-- | \\
asciiREVERSESOLIDUS = 92

-- | ]
asciiRIGHTSQUAREBRACKET = 93

-- | \^
asciiCIRCUMFLEXACCENT = 94

-- | _
asciiLOWLINE = 95

asciiGRAVEACCENT, asciiLOWERA, asciiLOWERB, asciiLOWERC, asciiLOWERD :: Word8
asciiLOWERE, asciiLOWERF, asciiLOWERG, asciiLOWERH, asciiLOWERI :: Word8
asciiLOWERJ, asciiLOWERK, asciiLOWERL, asciiLOWERM, asciiLOWERN :: Word8
asciiLOWERO, asciiLOWERP, asciiLOWERQ, asciiLOWERR, asciiLOWERS :: Word8
asciiLOWERT, asciiLOWERU, asciiLOWERV, asciiLOWERW, asciiLOWERX :: Word8
asciiLOWERY, asciiLOWERZ, asciiLEFTCURLYBRACKET, asciiVERTICALLINE :: Word8
asciiRIGHTCURLYBRACKET, asciiTILDE, asciiDELETE :: Word8

-- | `
asciiGRAVEACCENT = 96

-- | a
asciiLOWERA = 97

-- | b
asciiLOWERB = 98

-- | c
asciiLOWERC = 99

-- | d
asciiLOWERD = 100

-- | e
asciiLOWERE = 101

-- | f
asciiLOWERF = 102

-- | g
asciiLOWERG = 103

-- | h
asciiLOWERH = 104

-- | i
asciiLOWERI = 105

-- | j
asciiLOWERJ = 106

-- | k
asciiLOWERK = 107

-- | l
asciiLOWERL = 108

-- | m
asciiLOWERM = 109

-- | n
asciiLOWERN = 110

-- | o
asciiLOWERO = 111

-- | p
asciiLOWERP = 112

-- | q
asciiLOWERQ = 113

-- | r
asciiLOWERR = 114

-- | s
asciiLOWERS = 115

-- | t
asciiLOWERT = 116

-- | u
asciiLOWERU = 117

-- | v
asciiLOWERV = 118

-- | w
asciiLOWERW = 119

-- | x
asciiLOWERX = 120

-- | y
asciiLOWERY = 121

-- | z
asciiLOWERZ = 122

-- | {
asciiLEFTCURLYBRACKET = 123

-- | \|
asciiVERTICALLINE = 124

-- | }
asciiRIGHTCURLYBRACKET = 125

-- | ~
asciiTILDE = 126

-- | DEL
asciiDELETE = 127
