module Data.PDF.Settings
  ( Settings(Settings, sOptimizeGFX, sZopfli, sUseGhostScript)
  , OptimizeGFX(OptimizeGFX, DoNotOptimizeGFX)
  , UseZopfli(UseZopfli, UseDeflate)
  , UseGhostScript(UseGhostScript, DoNotUseGhostScript)
  , defaultSettings
  , toUseZopfli
  , toOptimizeGFX
  , toUseGhostScript
  ) where

import Data.Kind (Type)

type UseZopfli :: Type
data UseZopfli = UseZopfli | UseDeflate deriving stock Eq

toUseZopfli :: Bool -> UseZopfli
toUseZopfli True  = UseZopfli
toUseZopfli False = UseDeflate

type OptimizeGFX :: Type
data OptimizeGFX = OptimizeGFX | DoNotOptimizeGFX deriving stock Eq

toOptimizeGFX :: Bool -> OptimizeGFX
toOptimizeGFX True  = OptimizeGFX
toOptimizeGFX False = DoNotOptimizeGFX

type UseGhostScript :: Type
data UseGhostScript = UseGhostScript | DoNotUseGhostScript deriving stock Eq

toUseGhostScript :: Bool -> UseGhostScript
toUseGhostScript True  = UseGhostScript
toUseGhostScript False = DoNotUseGhostScript

type Settings :: Type
data Settings = Settings
  { sOptimizeGFX    :: !OptimizeGFX
  , sZopfli         :: !UseZopfli
  , sUseGhostScript :: !UseGhostScript
  }

defaultSettings :: Settings
defaultSettings = Settings OptimizeGFX UseZopfli DoNotUseGhostScript
