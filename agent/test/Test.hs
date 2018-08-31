{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Mocks
import           Agent

import           Test.Hspec
import           Test.HUnit
import           Test.QuickCheck hiding (Success)

import           Network.Socket
import           System.Exit            (ExitCode(..))
import           System.Directory
import           System.IO
import           Control.Exception
import           Control.Monad
import           Data.Monoid

import           Data.Aeson
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM

import           Text.Read              (read)
import           Language.Haskell.TH    (mkName)
import           Data.Functor.Identity

--------------------------------------- Types -----------------------------------------------------

shouldLike :: (HasCallStack, Exception e) => IO (Either e a) -> Selector e -> Expectation
shouldLike action p = action >>= \case
   Right{} -> expectationFailure "did not get expected exception"
   Left e  -> p e `unless` expectationFailure ("predicate failed on expected exception " <> show e)

--------------------------------------- Main ------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "loadM" $ loadMTest
  describe "IO" $ it "IO" pending

loadMTest :: Spec
loadMTest = do
  it "zero"    $ loadM pure ([] :: [Maybe Int])                         == Identity Nothing
  it "nothing" $ loadM pure ([Nothing, Nothing] :: [Maybe Int])         == Identity Nothing
  it "one"     $ loadM pure [Nothing, Just 1, Nothing]                 == Identity (Just 1)
  it "many"    $ loadM pure [Nothing, Just 1, Just 2, Just 3, Nothing] == Identity (Just 1)

--------------------------------------- IO --------------------------------------------------------

{-
testReadWrite :: Spec
testReadWrite = around (\run -> do
  (path, handle) <- openBinaryTempFile "/tmp" "saas.test"
  prepare "normal"
  hSetBuffering handle NoBuffering
  res <- tryAny $ run handle
  hClose handle
  removeFile path
  either throwIO return res) $ do
    it "empty" $ \h -> shouldReturn (act h "") ""
    it "string" $ \h -> property $ \bs -> shouldReturn (act h $ BL.pack bs) (BL.pack bs)
  where
  act h s = msgWrite h s >> hSeek h AbsoluteSeek 0 >> msgRead h
-}
