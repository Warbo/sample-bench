{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module SampleBench where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.IO.Class
import           CriterionPlus
import           Data.Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.IORef
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           System.Environment
import           System.Exit
import           System.IO      (stderr)
import           System.Process (shell, proc, CreateProcess)
import           System.Process.ByteString

jsonFromVar :: FromJSON a => String -> IO a
jsonFromVar name = do
  cmdS <- getEnv name
  case eitherDecodeStrict (BSC.pack cmdS) of
       Left  err -> error ("Couldn't read JSON from " ++ name ++ ": " ++ err)
       Right c   -> return c

-- | Read command from env var `BENCHMARK_COMMANDS`
getBenchCommand :: IO String
getBenchCommand = jsonFromVar "BENCHMARK_COMMAND"

-- | Read command for generating input, from env var `BENCHMARK_INPUT_SOURCE`
getInputCommand :: Int -> IO BS.ByteString
getInputCommand n = do
  cmd <- jsonFromVar "BENCHMARK_INPUT_SOURCE"
  runCommand (proc cmd [show n]) ""

-- | Run a process, taking stdin and returning stdout. Errors on non-zero
-- | exit code. Collects up command's stderr output and writes it to our stderr
-- | handle after the command finishes.
runCommand :: CreateProcess -> BS.ByteString -> IO BS.ByteString
runCommand command input = do
  (code, out, err) <- readCreateProcessWithExitCode command input
  BSC.hPut stderr err
  case code of
    ExitSuccess   -> return out
    ExitFailure n -> error (concat ["Command failed (code ", show n, ")"])

-- | `prependToRef r x` prepends the result of `x` to the contents of `r`
prependToRef :: NFData a => IORef [a] -> IO a -> IO a
prependToRef ref ioX = do
  x  <- ioX
  x' <- evaluate (force x)
  modifyIORef' ref (x':)
  head <$> readIORef ref

-- | `benchCmd i c` creates a benchmark for the shell command `c`. The stdin for
-- | `c` comes from `i`: the first iteration comes from `i 0`, the second from
-- | `i 1`, etc.
-- |
-- | Returns a pair `(b, r)` where `b` is the benchmark and `r` will return a
-- | list of stdout strings captured from any iterations of `b` that
-- | have been run. Hence you'll probably want to execute `r` *after*
-- | benchmarking `b`.
benchCmd :: String -> IO (Standoff (), IO [BS.ByteString])
benchCmd command = do
  -- Count how many iterations we've run
  count <- newIORef 0

  -- This will accumulate our command outputs
  outputs <- newIORef []

  return (subject (T.append "Running " (T.pack command)) $ do
            -- Pause benchmarking while we get the next input
            pause
            input <- liftIO $ do
              n     <- readIORef count
              input <- getInputCommand n
              modifyIORef' count (+1)
              evaluate (force input)

            -- Resume benchmarking to time the command
            continue
            nfIO (prependToRef outputs (runCommand (shell command) input)),

          -- Outputs are accumulated at the head, so reverse into correct order
          reverse <$> readIORef outputs)

defaultMain = do
  command  <- getBenchCommand

  -- For some reason CriterionPlus runs one more iteration than we ask for, so
  -- we take one off before asking
  --let iterations = length inputs - 1

  -- The only way to set the CriterionPlus iteration count appears to be via a
  -- commandline arg, so we fake it with a wrapper.
  --withArgs ["-s", show iterations] $
  do
    (b, getResult) <- benchCmd command
    benchmark (standoff "Benchmark" b)

    result <- getResult
    putStrLn "\nBEGIN RESULTS\n"
    LBSC.putStrLn (encode (map TE.decodeUtf8 result))
    putStrLn "\nEND RESULTS\n"
