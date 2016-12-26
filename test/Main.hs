{-# LANGUAGE LambdaCase, OverloadedStrings #-}

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
    testProperty "Can parse command" canGetBenchCommand
  --, testProperty ""
  ]

-- Tests

canGetBenchCommand (JS s) = monadicIO $ do
  s' <- run $ withEnv [("BENCHMARK_COMMAND", show s)] getBenchCommand
  assert (s == s')

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

newtype JsonString = JS String

instance Arbitrary JsonString where
  arbitrary = JS <$> nonEmpty
    where nonEmpty = ascii `suchThat` (not . null)
          ascii    = filter valid <$> listOf1 arbitrary
          valid x  = isPrint x && isAscii x
  shrink (JS s) = case s of
    "a"    -> []
    (_:[]) -> [JS "a"]
    (_:cs) -> map JS (tails cs)

instance Show JsonString where
  show (JS s) = show s
