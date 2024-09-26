module External.ExternalCommand
  ( externalCommand
  , externalCommandBuf
  , externalCommandBuf'
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)
import Data.UnifiedError (UnifiedError (ExternalCommandError))

import GHC.IO.Handle
    ( BufferMode (BlockBuffering)
    , Handle
    , hClose
    , hSetBinaryMode
    , hSetBuffering
    )

import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO.Temp (withSystemTempFile)
import System.Process
    ( StdStream (CreatePipe)
    , proc
    , readProcessWithExitCode
    , waitForProcess
    , withCreateProcess
    )
import System.Process.Internals (CreateProcess (std_in, std_out), ProcessHandle)

externalCommand :: FilePath -> [String] -> FallibleT IO ()
externalCommand command args = do
  (exitCode, _, _) <- lift $ readProcessWithExitCode command args ""

  case exitCode of
    ExitSuccess    -> return ()
    ExitFailure rc -> throwE (ExternalCommandError "Command failed" rc)

externalCommandBuf :: FilePath -> [String] -> BS.ByteString -> FallibleT IO BS.ByteString
externalCommandBuf command args input = do
  let process = (proc command args) { std_in  = CreatePipe
                                    , std_out = CreatePipe
                                    }

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
    hClose stdin

    -- Read stdout and stderr
    output   <- BS.hGetContents stdout
    exitCode <- waitForProcess ph

    case exitCode of
      ExitSuccess    -> return $ Right output
      ExitFailure rc -> return $ Left (ExternalCommandError command rc)
  injectInput _ _ _ _ _ =
    return $ Left (ExternalCommandError (command ++ ": invalid handles") 1)

externalCommandBuf'
  :: FilePath
  -> [String]
  -> BS.ByteString
  -> FallibleT IO BS.ByteString
externalCommandBuf' command args input = do
  withSystemTempFile "dietpdf.temporary" $ \temp tempHandle -> do
    lift $ hClose tempHandle
    lift $ BS.writeFile temp input
    let process = (proc command (args ++ [temp])) { std_out = CreatePipe }

    result <- lift $ withCreateProcess process injectInput

    case result of
      Right output -> return output
      Left err     -> throwE err
 where
  injectInput
    :: Maybe Handle
    -> Maybe Handle
    -> Maybe Handle
    -> ProcessHandle
    -> IO (Either UnifiedError BS.ByteString)
  injectInput _stdin (Just stdout) _stderr ph = do
    -- Configure stdout
    hSetBinaryMode stdout True
    hSetBuffering stdout (BlockBuffering Nothing)

    -- Read stdout and stderr
    output   <- BS.hGetContents stdout
    exitCode <- waitForProcess ph

    case exitCode of
      ExitSuccess    -> return $ Right output
      ExitFailure rc -> return $ Left (ExternalCommandError command rc)
  injectInput _ _ _ _ =
    return $ Left (ExternalCommandError (command ++ ": invalid handles") 1)
