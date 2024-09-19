module External.ExternalCommand (externalCommand, externalCommandBuf) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.ByteString qualified as BS

import GHC.IO.Handle
    ( BufferMode (BlockBuffering)
    , Handle
    , hClose
    , hFlush
    , hSetBinaryMode
    )

import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (hSetBuffering)
import System.Process
    ( StdStream (CreatePipe)
    , proc
    , readProcessWithExitCode
    , waitForProcess
    , withCreateProcess
    )
import System.Process.Internals (CreateProcess (std_in, std_out), ProcessHandle)

import Util.UnifiedError (FallibleT, UnifiedError (ExternalCommandError))

externalCommand :: FilePath -> [String] -> FallibleT IO ()
externalCommand command args = do
  (exitCode, _, _) <- lift $ readProcessWithExitCode command args ""

  case exitCode of
    ExitSuccess    -> return ()
    ExitFailure rc -> throwE (ExternalCommandError "Command failed" rc)

externalCommandBuf :: FilePath -> [String] -> BS.ByteString -> FallibleT IO BS.ByteString
externalCommandBuf command args input = do
  let process =
        (proc command args) { std_in  = CreatePipe, std_out = CreatePipe }
  result <- lift $ withCreateProcess process (injectInput input)
  case result of
    Right output -> return output
    Left err     -> throwE err
 where
  injectInput
    :: BS.ByteString
    -> Maybe Handle
    -> Maybe Handle
    -> Maybe Handle
    -> ProcessHandle
    -> IO (Either UnifiedError BS.ByteString)
  injectInput input' (Just stdin) (Just stdout) _stderr ph = do
    -- Configure stdin
    hSetBinaryMode stdin True
    hSetBuffering stdin (BlockBuffering Nothing)

    -- Configure stdout
    hSetBinaryMode stdout True
    hSetBuffering stdout (BlockBuffering Nothing)

    -- Write input to stdin
    BS.hPut stdin input'
    hFlush stdin
    hClose stdin

    -- Read stdout and stderr
    output <- BS.hGetContents stdout
    exitCode <- waitForProcess ph

    case exitCode of
      ExitSuccess    -> return $ Right output
      ExitFailure rc -> return $ Left (ExternalCommandError "jpegtran" rc)
  injectInput _ _ _ _ _ =
    return $ Left (ExternalCommandError "jpegtran: invalid handles" 1)
