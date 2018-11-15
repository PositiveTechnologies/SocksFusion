{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Test.Mocks
import           Test.Templates
import           Test.SaaS.Instances
import           Agent.Types
import           Agent.Console
import           Network.SocksFusion (timeout, wait)

import           Test.Hspec
import           Test.HUnit
import           Test.QuickCheck hiding (Success)

import           System.Exit            (ExitCode(..))
import           System.Directory
import           System.IO
import           System.Log.FastLogger
import           Control.Exception
import           Data.Time.Clock
import           Data.Time.Calendar

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.Monoid

import           Data.Aeson
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC

import           Text.Read              (read)
import           Text.PrettyPrint.ANSI.Leijen (text)
import           Language.Haskell.TH    (mkName)
import           Data.Functor.Identity

import           Control.Concurrent.STM
import           Control.Concurrent

--------------------------------------- Types ------------------------------------------

shouldLike :: (HasCallStack, Exception e) => IO (Either e a) -> Selector e -> Expectation
shouldLike action p = action >>= \case
   Right{} -> expectationFailure "did not get expected exception"
   Left e  -> p e `unless` expectationFailure ("predicate failed on expected exception " <> show e)

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> (ModifiedJulianDay . (2000 +) <$> arbitrary)
                      <*> (fromRational . toRational <$> choose (0 :: Double, 86400))

$(genericArbitrary ''Token)

deriving instance Show Token

--------------------------------------- Main -------------------------------------------

main :: IO ()
main = do
  ch <- atomically newTQueue
  let ?tlog = \f -> atomically $ writeTQueue ch $ f ""
  hspec $ do
    describe "JSON" $
      it "Token" $ property $ \(t :: Token) -> eitherDecode (encode t) === Right t
    describe "Dialog" (dialogs ch)

--------------------------------------- IO ---------------------------------------------

dialogs :: (?tlog :: TimedFastLogger) => TQueue LogStr -> Spec
dialogs ch = around (\run -> do
  input <- newInput
  output <- newOutput
  run (\r -> evalStateT (runReaderT r (output, input)) undefined)) $ do
    it "Timeout 1" $ \run -> do
      timeout 2 $ run $ dialog [ ("", logPretty $ text "correct")
                               , ("1", return ()) ]
                               (text "Test timeout: ") (Just 1)
      res <- atomically (flushTQueue ch)
      res `shouldBe` ["\n", "Test timeout: \n", "> ", "\n", "correct\n"]
    it "Timeout 2" $ \run -> do
      timeout 2 $ run $ dialog [ ("", logPretty $ text "correct")
                               , ("1", return ()) ]
                               (text "Test timeout: ") (Just 5)
      res <- atomically (flushTQueue ch)
      res `shouldBe` ["\n", "Test timeout: \n", "> "]
    it "Select unknown" $ \run -> do
      forkIO $ wait 1 >> withFile "/proc/self/fd/0" AppendMode (`hPutStr` "2\n")
      timeout 3 $ run $ dialog [ ("", logPretty $ text "empty")
                               , ("1", logPretty $ text "one") ]
                               (text "Test select: ") Nothing
      res <- atomically (flushTQueue ch)
      res `shouldBe` ["\n", "Test select: \n", "> ", "\n", "Please enter one: 1\n", "> "]
    it "Select one" $ \run -> do
      forkIO $ wait 1 >> withFile "/proc/self/fd/0" AppendMode (`hPutStr` "1\n")
      timeout 3 $ run $ dialog [ ("", logPretty $ text "empty")
                               , ("1", logPretty $ text "one") ]
                               (text "Test select: ") Nothing
      res <- atomically (flushTQueue ch)
      res `shouldBe` ["\n", "Test select: \n", "> ", "\n", "one\n"]
    it "Select case" $ \run -> do
      forkIO $ wait 1 >> withFile "/proc/self/fd/0" AppendMode (`hPutStr` "ONE\n")
      timeout 3 $ run $ dialog [ ("", logPretty $ text "empty")
                               , ("one", logPretty $ text "one") ]
                               (text "Test select: ") Nothing
      res <- atomically (flushTQueue ch)
      res `shouldBe` ["\n", "Test select: \n", "> ", "\n", "one\n"]
    it "Select letter" $ \run -> do
      forkIO $ wait 1 >> withFile "/proc/self/fd/0" AppendMode (`hPutStr` "o\n")
      timeout 3 $ run $ dialog [ ("", logPretty $ text "empty")
                               , ("one", logPretty $ text "one") ]
                               (text "Test select: ") Nothing
      res <- atomically (flushTQueue ch)
      res `shouldBe` ["\n", "Test select: \n", "> ", "\n", "one\n"]
