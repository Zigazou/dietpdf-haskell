{-|
Optimization and external tool flags for dietpdf.

This module defines small enumerations and a record used to control optional
optimizations and integrations with external tools (GhostScript, pdf-to-cairo,
Zopfli). Helpers are provided to convert booleans to these flags.
-}
module Data.PDF.Settings
  ( Settings(Settings, sOptimizeGFX, sCompressor, sUseGhostScript, sUsePDFToCairo)
  , OptimizeGFX(OptimizeGFX, DoNotOptimizeGFX)
  , UseCompressor(UseZopfli, UseDeflate, UseBrotli)
  , UseGhostScript(UseGhostScript, DoNotUseGhostScript)
  , UsePDFToCairo(UsePDFToCairo, DoNotUsePDFToCairo)
  , defaultSettings
  , toUseCompressor
  , toOptimizeGFX
  , toUseGhostScript
  , toUsePDFToCairo
  ) where

import Data.Kind (Type)

{-|
Flag indicating whether to use Zopfli compression, standard deflate, or Brotli.
-}
type UseCompressor :: Type
data UseCompressor = UseZopfli -- ^ Use Zopfli compression
                   | UseDeflate -- ^ Use standard deflate compression
                   | UseBrotli -- ^ Use Brotli compression
                   deriving stock Eq

toUseCompressor :: Maybe String -> UseCompressor
toUseCompressor (Just "deflate") = UseDeflate
toUseCompressor (Just "brotli")  = UseBrotli
toUseCompressor _anyOtherCase    = UseZopfli

{-|
Flag indicating whether to optimize graphics content.
-}
type OptimizeGFX :: Type
data OptimizeGFX = OptimizeGFX -- ^ Enable graphics optimization
                 | DoNotOptimizeGFX -- ^ Disable graphics optimization
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
data UseGhostScript = UseGhostScript -- ^ Use GhostScript
                    | DoNotUseGhostScript -- ^ Do not use GhostScript
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
data UsePDFToCairo = UsePDFToCairo -- ^ Use pdf-to-cairo
                   | DoNotUsePDFToCairo -- ^ Do not use pdf-to-cairo
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
  , sCompressor     :: !UseCompressor  -- ^ Compressor flag
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
  , sCompressor     = UseZopfli
  , sUseGhostScript = DoNotUseGhostScript
  , sUsePDFToCairo  = DoNotUsePDFToCairo
  }
