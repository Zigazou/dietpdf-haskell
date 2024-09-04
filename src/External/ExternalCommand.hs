module External.ExternalCommand (externalCommand) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

import Util.UnifiedError (FallibleT, UnifiedError (ExternalCommandError))

externalCommand :: FilePath -> [String] -> FallibleT IO ()
externalCommand command args = do
  (exitCode, _, _) <- lift $ readProcessWithExitCode command args ""

  case exitCode of
    ExitSuccess    -> return ()
    ExitFailure rc -> throwE (ExternalCommandError "Command failed" rc)
