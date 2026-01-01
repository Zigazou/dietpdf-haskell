{-|
Optimization and external tool flags for dietpdf.

This module defines small enumerations and a record used to control optional
optimizations and integrations with external tools (GhostScript, pdf-to-cairo,
Zopfli). Helpers are provided to convert booleans to these flags.
-}
module Data.PDF.Settings
  ( Settings(Settings, sOptimizeGFX, sZopfli, sUseGhostScript, sUsePDFToCairo)
  , OptimizeGFX(OptimizeGFX, DoNotOptimizeGFX)
  , UseZopfli(UseZopfli, UseDeflate)
  , UseGhostScript(UseGhostScript, DoNotUseGhostScript)
  , UsePDFToCairo(UsePDFToCairo, DoNotUsePDFToCairo)
  , defaultSettings
  , toUseZopfli
  , toOptimizeGFX
  , toUseGhostScript
  , toUsePDFToCairo
  ) where

import Data.Kind (Type)

{-|
Flag indicating whether to use Zopfli compression or standard deflate.
-}
type UseZopfli :: Type
data UseZopfli = UseZopfli
               | UseDeflate
               deriving stock Eq

{-|
Convert a boolean to a 'UseZopfli' flag.

'True' enables Zopfli; 'False' selects standard deflate.
-}
toUseZopfli :: Bool -> UseZopfli
toUseZopfli True  = UseZopfli
toUseZopfli False = UseDeflate

{-|
Flag indicating whether to optimize graphics content.
-}
type OptimizeGFX :: Type
data OptimizeGFX = OptimizeGFX
                 | DoNotOptimizeGFX
                 deriving stock Eq

{-|
Convert a boolean to an 'OptimizeGFX' flag.

'True' enables optimization; 'False' disables it.
-}
toOptimizeGFX :: Bool -> OptimizeGFX
toOptimizeGFX True  = OptimizeGFX
toOptimizeGFX False = DoNotOptimizeGFX

{-|
Flag indicating whether to use GhostScript for certain optimizations.
-}
type UseGhostScript :: Type
data UseGhostScript = UseGhostScript
                    | DoNotUseGhostScript
                    deriving stock Eq

{-|
Convert a boolean to a 'UseGhostScript' flag.

'True' enables GhostScript; 'False' disables it.
-}
toUseGhostScript :: Bool -> UseGhostScript
toUseGhostScript True  = UseGhostScript
toUseGhostScript False = DoNotUseGhostScript

{-|
Flag indicating whether to use pdf-to-cairo for certain optimizations.
-}
type UsePDFToCairo :: Type
data UsePDFToCairo = UsePDFToCairo
                   | DoNotUsePDFToCairo
                   deriving stock Eq

{-|
Convert a boolean to a 'UsePDFToCairo' flag.

'True' enables pdf-to-cairo; 'False' disables it.
-}
toUsePDFToCairo :: Bool -> UsePDFToCairo
toUsePDFToCairo True  = UsePDFToCairo
toUsePDFToCairo False = DoNotUsePDFToCairo

{-|
Settings controlling optimizations and external tool usage.
-}
type Settings :: Type
data Settings = Settings
  { sOptimizeGFX    :: !OptimizeGFX    -- ^ Graphics optimization flag
  , sZopfli         :: !UseZopfli      -- ^ Zopfli compression flag
  , sUseGhostScript :: !UseGhostScript -- ^ GhostScript usage flag
  , sUsePDFToCairo  :: !UsePDFToCairo  -- ^ pdf-to-cairo usage flag
  }

{-|
Default settings.

By default: graphics optimization enabled, Zopfli enabled, GhostScript
disabled, pdf-to-cairo disabled.
-}
defaultSettings :: Settings
defaultSettings = Settings 
  { sOptimizeGFX    = OptimizeGFX
  , sZopfli         = UseZopfli
  , sUseGhostScript = DoNotUseGhostScript
  , sUsePDFToCairo  = DoNotUsePDFToCairo
  }
