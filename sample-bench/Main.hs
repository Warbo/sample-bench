module Main where

import           CriterionPlus
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char as BSC
import           Data.IORef
import           System.Environment
import           System.IO (stderr)
import           System.Process.ByteString

-- | Read command from env var `BENCHMARK_COMMANDS`
getCommand :: IO String
getCommand = do
  cmdS <- getEnv "BENCHMARK_COMMAND"
  case eitherDecodeStrict (BSC.pack cmdS) of
       Left err -> error ("Couldn't read benchmark command: " ++ err)
       Right c  -> return c

-- | Read sample data from file given in env var `BENCHMARK_INPUTS`
getInputs :: IO [BS.ByteString]
getInputs = do
  inputF  <- getEnv "BENCHMARK_INPUTS"
  inputBS <- BS.readFile inputF
  case eitherDecodeStrict inputBS of
    Left err -> error ("Couldn't decode input data: " ++ err)
    Right xs -> return xs

-- | Run shell command, taking stdin and returning stderr. Errors on non-zero
-- | exit code. Collects up command's stderr output and writes it to our stderr
-- | handle after the command finishes.
runCommand :: String -> BS.ByteString -> IO BS.ByteString
runCommand command input = do
  (code, out, err) <- readCreateProcessWithExitCode command
  BSC.hPut stderr err
  case code of
    ExitSuccess   -> return out
    ExitFailure n -> error (concat ["Command ", command, " failed (code ",
                                    show n])

-- | `prependToRef r x` prepends the result of `x` to the contents of `r`
prependToRef :: IORef [a] -> IO a -> IO a
prependToRef ref ioX = do
  x  <- ioX
  x' <- evaluate (force x)
  modifyIORef' ref (x':)
  readIORef ref

-- | `benchCmd [s1, s2, ...] c` creates a benchmark for the command `c`, using
-- | `s1` as stdin on the first iteration, `s2` on the second, and so on.
-- | Returns a pair `(b, r)` where `b` is the benchmark and `r` will return the
-- | stdout captured from any iterations of `b` that have been run. Hence you'll
-- | probably want to execute `r` *after* `b`.
benchCmd :: [BS.ByteString] -> String -> (Standoff (), IO [BS.ByteString])
benchCmd inputs command = do
  -- Start at sample 0
  inputIndex <- newIORef 0

  -- This will accumulate our command outputs
  output <- newIORef []

  (subject ("Running " ++ command) $ do
      -- Pause benchmarking while we choose the next sample
      pause
      n     <- liftIO (readIORef inputIndex)
      input <- evaluate (force (inputs !! n))
      liftIO (modifyIORef' inputIndex (+1))

      -- Resume benchmarking to time the command
      continue
      nfIO (prependToRef output (runCommand command input)),
   reverse <$> readIORef output)

main = do
  command <- getCommand
  inputs  <- getInputs
  let inputLen = show (length xs)

  -- The only way to set iteration count appears to be via a commandline arg, so
  -- we fake it to match up with the number of samples we have.
  withArgs ["-s", inputLen] $ do
    let (b, getResult) = (benchCmd inputs command)
    benchmark (standoff ("Size " ++ inputLen) b)

    result <- getResult
    putStrLn "\nBEGIN RESULTS\n"
    BSC.putStrLn result
    putStrLn "\nEND RESULTS\n"
