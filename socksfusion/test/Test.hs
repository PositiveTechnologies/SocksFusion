{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Prelude         hiding ((.))
import           Network.SocksFusion
import           Network.Socks5
import           Test.Templates
import           Test.SaaS.Instances    ()

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Success)

import           Data.List              (intercalate)
import           Control.Category
import           Control.Monad.Reader
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Monoid

--------------------------------------- Utils --------------------------------------------

encodeDecode :: (Binary a) => a -> a
encodeDecode = runGet get . runPut . put

simpleTest :: (Show a, Binary a, Eq a) => a -> Spec
simpleTest a = it (show a) $ encodeDecode a == a

instance Arbitrary Arrow where
  arbitrary = do
    NonNegative ah <- arbitrary
    Arrow ah <$> arbitrary

instance Arbitrary SocksRequest where
  arbitrary = do
    atype <- arbitrary
    addr <- case atype of
      SocksIPv4 -> do
        nums <- replicateM 4 arbitraryASCIIChar
        return $ BC.pack $ intercalate "." $ map (show . fromEnum) nums
      SocksDomain -> arbitrary
      _ -> fail "Unknown atyp"
    SocksRequest <$> arbitrary <*> pure atype <*> pure addr <*> arbitrary

instance Arbitrary SocksResponse where
  arbitrary = do
    atype <- arbitrary
    addr <- case atype of
      SocksIPv4 -> do
        nums <- replicateM 4 arbitraryASCIIChar
        return $ BC.pack $ intercalate "." $ map (show . fromEnum) nums
      SocksDomain -> arbitrary
      _ -> fail "Unknown atyp"
    SocksResponse <$> arbitrary <*> pure atype <*> pure addr <*> arbitrary

instance Arbitrary SocksAtyp where
  arbitrary = oneof [pure SocksIPv4, pure SocksDomain]

$(genericArbitrary ''AgentMessage)
$(genericArbitrary ''ProxyMessage)
$(genericArbitrary ''Siid)
$(genericArbitrary ''KeyResponse)
$(genericArbitrary ''ScanChange)
$(genericArbitrary ''ExtSocksSyn)
$(genericArbitrary ''SocksAck)
$(genericArbitrary ''SocksMethod)
$(genericArbitrary ''SocksCommand)
$(genericArbitrary ''SocksReply)

--------------------------------------- Main ---------------------------------------------

main :: IO ()
main = hspec $ do
  describe "Read" $ describe "AddrPort" addrPort
  describe "Binary" $ do
    prop "Arrow" (encodeTest :: Arrow -> Property)
    prop "ExtSocksSyn" (encodeTest :: ExtSocksSyn -> Property)
    prop "SocksAck" (encodeTest :: SocksAck -> Property)
    prop "SocksRequest" (encodeTest :: SocksRequest -> Property)
    prop "SocksResponse" (encodeTest :: SocksResponse -> Property)
    prop "SocksMethod" (encodeTest :: SocksMethod-> Property)
    prop "SocksCommand" (encodeTest :: SocksCommand -> Property)
    prop "SocksAtyp" (encodeTest :: SocksAtyp -> Property)
    prop "SocksReply" (encodeTest :: SocksReply -> Property)
    prop "AgentMessage" (encodeTest :: AgentMessage -> Property)
    prop "ProxyMessage" (encodeTest :: ProxyMessage -> Property)
    prop "KeyResponse" (encodeTest :: KeyResponse -> Property)
    prop "ScanChange" (encodeTest :: ScanChange -> Property)
    prop "Siid" (encodeTest :: Siid -> Property)
  describe "Puzzle" $ do
    describe "bitEq" bitEqTest
    prop "incr" $ \(NonNegative x) -> succ (toEnum x) == incr (toEnum x :: [Word8])
    prop "solvePuzzle" testSolve
    describe "testPuzzle" testPuzzleTest
  where
  encodeTest :: (Binary a, Eq a, Show a) => a -> Property
  encodeTest t = runGet get (runPut $ put t) === t

--------------------------------------- Read ---------------------------------------------

addrPort :: Spec
addrPort = mapM_ (\t -> it t $ readTest t) [ "[0a:ab:ce:ef:01:12]:1111"
                                           , "localhost:2222"
                                           , "127.0.0.1:3333"]
  where
  readTest :: String -> Bool
  readTest str = show (read str :: AddrPort) == str

--------------------------------------- Puzzle -------------------------------------------

instance Enum [Word8] where
  toEnum x = if x <= 0 then [] else toEnum ((x - 1) `mod` 256) : toEnum ((x - 1) `div` 256)
  fromEnum [] = 0
  fromEnum (x:xs) = fromEnum x + 256 * fromEnum xs + 1

testSolve :: BS.ByteString -> BS.ByteString -> Expectation
testSolve x1 x2 = ((\x -> runReader (testPuzzle (b1 <> b2) x) (10, 12))
              <$> solvePuzzle (BS.pack [10, 14] <> b1 <> b2)) `shouldReturn` True
  where
  (b1, b2) = (BS.take 10 (x1 <> "variation_"), BS.take 5 (x2 <> "compl"))

bitEqTest :: Spec
bitEqTest = do
  it "0" $ act [1,2,3] [4,5,6] 0
  it "1a" $ act [1,2,3] [4,5,6] 1
  it "1b" $ not $ act [1,2,3] [128,5,6] 1
  it "2b" $ not $ act [1,2,3] [65,5,6] 2
  it "18a" $ act [170,85,128] [170,85,191] 18
  it "18b" $ not $ act [170,85,128] [170,85,192] 18
  it "18c" $ not $ act [170,85,128] [170,86,128] 18
  where
  act l r len = runReader (bitEq (BS.pack l) (BS.pack r)) ((), len)

testPuzzleTest :: Spec
testPuzzleTest = it "puzzleTest" pending
