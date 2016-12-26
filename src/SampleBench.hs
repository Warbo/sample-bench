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
import           System.Process (shell)
import           System.Process.ByteString

-- | Read command from env var `BENCHMARK_COMMANDS`
getCommand :: IO String
getCommand = do
  cmdS <- getEnv "BENCHMARK_COMMAND"
  case eitherDecodeStrict (BSC.pack cmdS) of
       Left err -> error ("Couldn't read benchmark command: " ++ err)
       Right c  -> return c

-- | Read sample data from filename given in commandline arg
getInputs :: IO [BS.ByteString]
getInputs = getArgs >>= \case
  []    -> error "Require filename as argument, for JSON input data"
  (f:_) -> do
    inputBS <- LBSC.toStrict <$> LBSC.readFile f
    case eitherDecodeStrict inputBS of
      Left  err -> error  ("Failed to read JSON from " ++ f ++ ": " ++ err)
      Right xs  -> return (map TE.encodeUtf8 xs)

-- | Run shell command, taking stdin and returning stderr. Errors on non-zero
-- | exit code. Collects up command's stderr output and writes it to our stderr
-- | handle after the command finishes.
runCommand :: String -> BS.ByteString -> IO BS.ByteString
runCommand command input = do
  (code, out, err) <- readCreateProcessWithExitCode (shell command) input
  BSC.hPut stderr err
  case code of
    ExitSuccess   -> return out
    ExitFailure n -> error (concat ["Command ", command, " failed (code ",
                                    show n])

-- | `prependToRef r x` prepends the result of `x` to the contents of `r`
prependToRef :: NFData a => IORef [a] -> IO a -> IO a
prependToRef ref ioX = do
  x  <- ioX
  x' <- evaluate (force x)
  modifyIORef' ref (x':)
  head <$> readIORef ref

-- | `benchCmd [s1, s2, ...] c` creates a benchmark for the command `c`, using
-- | `s1` as stdin on the first iteration, `s2` on the second, and so on.
-- | Returns a pair `(b, r)` where `b` is the benchmark and `r` will return the
-- | stdout captured from any iterations of `b` that have been run. Hence you'll
-- | probably want to execute `r` *after* `b`.
benchCmd :: [BS.ByteString] -> String -> IO (Standoff (), IO [BS.ByteString])
benchCmd inputLst command = do
  -- Keep track of remaining inputs
  inputs <- newIORef inputLst

  -- This will accumulate our command outputs
  outputs <- newIORef []

  return (subject (T.append "Running " (T.pack command)) $ do
            -- Pause benchmarking while we choose the next sample
            pause
            input <- liftIO $ do
              (x:xs) <- readIORef inputs
              writeIORef inputs xs
              evaluate (force x)

            -- Resume benchmarking to time the command
            continue
            nfIO (prependToRef outputs (runCommand command input)),
          reverse <$> readIORef outputs)

defaultMain = do
  command <- getCommand
  inputs  <- getInputs

  -- For some reason CriterionPlus runs one more iteration than we ask for, so
  -- we take one off before asking
  let iterations = length inputs - 1

  -- The only way to set the CriterionPlus iteration count appears to be via a
  -- commandline arg, so we fake it with a wrapper.
  withArgs ["-s", show iterations] $ do
    (b, getResult) <- benchCmd inputs command
    benchmark (standoff "Benchmark" b)

    result <- getResult
    putStrLn "\nBEGIN RESULTS\n"
    LBSC.putStrLn (encode (map TE.decodeUtf8 result))
    putStrLn "\nEND RESULTS\n"
