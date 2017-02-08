{-# LANGUAGE LambdaCase, OverloadedStrings, GADTs #-}

module Main where

import           Control.Applicative
import           CriterionPlus
import           Data.Aeson
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.Char
import           Data.IORef
import           Data.List
import           SampleBench hiding (defaultMain)
import           System.Environment
import           System.IO
import           System.IO.Silently
import           System.Process (proc, shell)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Property
import           Test.Tasty (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Can parse command"        canGetBenchCommand
  , testProperty "JSON encode/decode"       jsonDecode
  , testProperty "Can prepend result"       canPrepend
  , testProperty "Benchmarks get stdio"     benchCat
  , testProperty "Benchmark times match up" benchSleep
  , testProperty "Input not benchmarked"    benchSetup
  ]

-- Tests
jsonDecode :: NonEmptyString -> NonEmptyString -> Property
jsonDecode (NES name) val = '=' `notElem` name ==> monadicIO $ do
  val' <- run $ withEnv [(name, show (JSON val))]
                        (jsonFromVar name)
  assert (val == val')

canGetBenchCommand (NES s) = monadicIO $ do
  s' <- run $ withEnv [("BENCHMARK_COMMAND", show (JSON s))] getBenchCommand
  assert (s == s')

canPrepend :: String -> [String] -> Property
canPrepend x xs = monadicIO $ do
  result <- run $ do ref <- newIORef xs
                     prependToRef ref (return x)
                     readIORef    ref
  assert (result == x:xs)

benchCat = once . monadicIO $ do
    (outErr, outputs) <- exec $ do
      (so, getOutputs) <- benchCmd (shell "cat")
      benchmark (standoff "Test" so)
      getOutputs
    debug "outputs" outputs
    assert (outputs == map (BSC.pack . (++ "\n") . show) [0..100])
  where exec = run           .
               captureOutErr .
               echoInput

benchSleep :: Property
benchSleep = once $ conjoin (map check [0..5])
  where check n = monadicIO $ do
          mean <- run . echoInput . sleepFor $ n
          debug "n, mean" (n, mean)
          assert (round mean == n)

benchSetup :: Int -> Property
benchSetup n' = once . monadicIO $ do
    -- Call `sleep` to get our input: will sleep for 0 seconds on the first
    -- iteration, 1 for the second, and so on.
    mean <- run . withEnv [("BENCHMARK_INPUT_SOURCE", show "sleep")]
                $ sleepFor n
    -- Benchmark should only count the `sleepFor` wait, not the input wait
    assert (round mean == n)
  where n = abs n' `mod` 5

-- Helpers

debug :: (Show a, Monad m) => String -> a -> PropertyM m ()
debug msg x = monitor . printTestCase . show $ (msg, x)

withEnv :: [(String, String)] -> IO a -> IO a
withEnv []               p = p
withEnv ((name, val):xs) p = do
  -- Get old val (if any)
  old    <- lookupEnv name

  -- Run with new value
  setEnv name val
  result <- withEnv xs p

  -- Set old value back
  case old of
       Nothing  -> setEnv name ""
       Just old -> setEnv name old
  return result

echoInput = withEnv [("BENCHMARK_INPUT_SOURCE", show ("echo" :: String))]

captureOutErr = hCapture [stdout, stderr]

-- | Benchmarks a `sleep` command, passing it the given number of seconds.
-- | Returns the mean time taken from the CriterionPlus output.
sleepFor n = do
  (output, _) <- captureOutErr . fewIterations $ do
    (b, _) <- benchCmd $ proc "sleep" [show n]
    benchmark (standoff "Test" b)
  return (parseMean output)

fewIterations = withIterations 3

-- Parse the mean time, in seconds, from the captured stdout
parseMean s = parseTime s (takeWhile floatChar (dropUntil "mean: " s))

-- See which unit appears after `num` in `s`, and scale `read num`
-- accordingly
parseTime s num =
  let n     = read num :: Float
      scale = case takeWhile (/= ',')
                             (dropUntil ("mean: " ++ num ++ " ") s) of
                   "ms" -> 1 / 1000
                   "s"  -> 1
                   x    -> error ("Unknown unit: " ++ x)
   in n * scale

-- Look for `p` in `s` and return whatever follows
dropUntil p "" = error ("Couldn't find '" ++ p ++ "'")
dropUntil p s  = if p `isPrefixOf` s
                    then drop (length p) s
                    else dropUntil p (tail s)

-- `True` for characters in a printed `Float` (i.e. 0-9 and .)
floatChar '.' = True
floatChar c   = isDigit c

newtype NonEmptyString = NES String deriving (Eq)

instance Show NonEmptyString where
  show (NES s) = s

instance ToJSON NonEmptyString where
  toJSON (NES s) = toJSON s

instance FromJSON NonEmptyString where
  parseJSON x = NES <$> parseJSON x

instance Arbitrary NonEmptyString where
  arbitrary = NES <$> listOf1 ascii
    where ascii    = arbitrary `suchThat` valid
          valid x  = isPrint x && isAscii x
  shrink (NES s) = case s of
    ""  -> error "Empty NonEmptyString"
    "a" -> []
    [_] -> [NES "a"]
    _   -> let hs = tail          (init (inits s))
               ts = tail (reverse (tail (tails s)))
            in map NES (hs ++ ts)

data JsonOf a where
  JSON :: (FromJSON a, ToJSON a) => a -> JsonOf a

instance (Arbitrary a, ToJSON a, FromJSON a) => Arbitrary (JsonOf a) where
  arbitrary = JSON <$> arbitrary
  shrink (JSON s) = map JSON (shrink s)

instance (Show a) => Show (JsonOf a) where
  show (JSON s) = LBSC.unpack (encode s)
