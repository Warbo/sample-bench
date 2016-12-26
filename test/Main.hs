module Main where

import Control.Applicative
import Data.Char
import Data.List
import SampleBench hiding (defaultMain)
import System.Environment
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty (defaultMain, testGroup, localOption)
import Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Can parse command" canGetCommand
  ]

-- Tests

canGetCommand :: Property
canGetCommand = forAllShrink mkStr shrnk go
  where go x = monadicIO $ do
          x' <- run $ withEnv [("BENCHMARK_COMMAND", show x)] getCommand
          assert (x == x')

        mkStr   = (filter valid <$> listOf1 arbitrary) `suchThat` (not . null)
        valid x = isPrint x && isAscii x

        shrnk "a"   = []
        shrnk (c:_) = ["a"]
        shrnk xs    = tail (tails xs)

-- Helper functions

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
