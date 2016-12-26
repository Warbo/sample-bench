{-# LANGUAGE LambdaCase, OverloadedStrings, GADTs #-}

module Main where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.Char
import           Data.List
import           SampleBench hiding (defaultMain)
import           System.Environment
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Can parse command"  canGetBenchCommand
  , testProperty "JSON encode/decode" jsonDecode
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

-- Helpers

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
    ""     -> error "Empty NonEmptyString"
    "a"    -> []
    (_:[]) -> [NES "a"]
    _      -> let hs = tail          (init (inits s))
                  ts = tail (reverse (tail (tails s)))
               in map NES (hs ++ ts)

data JsonOf a where
  JSON :: (FromJSON a, ToJSON a) => a -> JsonOf a

instance (Arbitrary a, ToJSON a, FromJSON a) => Arbitrary (JsonOf a) where
  arbitrary = JSON <$> arbitrary
  shrink (JSON s) = map JSON (shrink s)

instance (Show a) => Show (JsonOf a) where
  show (JSON s) = LBSC.unpack (encode s)
