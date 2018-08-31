{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Prelude         hiding ((.))
import           Network.SocksFusion
import           Network.Socks5

import           Test.Hspec
import           Test.HUnit
import           Test.QuickCheck hiding (Success)

import           Network.Socket         (PortNumber)
import           Control.Exception      (evaluate)

import           Control.Category
import           Control.Monad.Reader
import           Data.Function          ((&))
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Monoid

--------------------------------------- Utils -----------------------------------------------------

encodeDecode :: (Binary a) => a -> a
encodeDecode = runGet get . runPut . put

simpleTest :: (Show a, Binary a, Eq a) => a -> Spec
simpleTest a = it (show a) $ encodeDecode a == a

instance Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> arbitrary

instance Arbitrary PortNumber where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Word16)

--------------------------------------- Main ------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "Read" $ describe "AddrPort" addrPort
  describe "Binary" $ do
    describe "Arrow"         arrows
    describe "ExtSocksSyn"   socksSyn
    describe "SocksAck"      socksAck
    describe "SocksRequest"  socksRequest
    describe "SocksResponse" socksResponse
    describe "SocksMethod"   socksMethod
    describe "SocksCommand"  socksCommand
    describe "SocksAtyp"     socksAtyp
    describe "SocksReply"    socksReply
  describe "Puzzle" $ do
    describe "bitEq" bitEqTest
    it "incr" $ property $ \(NonNegative x) -> succ (toEnum x) == incr (toEnum x :: [Word8])
    it "solvePuzzle" $ property $ \x1 x2 -> let (b1, b2) = (BS.take 10 (x1 <> "variation_"), BS.take 5 (x2 <> "compl")) in (solvePuzzle (BS.pack [10, 17] <> b1 <> b2) >>= \x -> return (runReader (testPuzzle (b1 <> b2) x) (10, 12))) `shouldReturn` True
--    describe "testPuzzle"  testPuzzleTest

--------------------------------------- Read ------------------------------------------------------

addrPort :: Spec
addrPort = mapM_ (\t -> it t $ readTest t) [ "[0a:ab:ce:ef:01:12]:1111"
                                          , "localhost:2222"
                                          , "127.0.0.1:3333"]
  where
  readTest :: String -> Bool
  readTest str = show (read str :: AddrPort) == str

--------------------------------------- Binary ----------------------------------------------------

arrows :: Spec
arrows = it "Arrow" $ property $ \ah sh -> let x = Arrow (getNonNegative ah) sh in encodeDecode x == x

socksSyn :: Spec
socksSyn = do
  it "Info" $ property $ \str -> let x = SocksInfo str in encodeDecode x == x
  it "Methods 0" $ let x = SocksSyn mempty in encodeDecode x == x
  it "Methods 1" $ let x = SocksSyn [SocksNoAuth] in encodeDecode x == x
  it "Methods 3" $ let x = SocksSyn [SocksNoAuth, SocksMethodOther, SocksMethodOther] in encodeDecode x == x

socksAck :: Spec
socksAck = it "Ack" $ let x = SocksAck SocksNoAuth in encodeDecode x == x

socksRequest :: Spec
socksRequest = do
  it "Request ipv4" $ property $ \port ->
    let x = SocksRequest SocksConnect SocksIPv4 "127.0.0.1" port in
      encodeDecode x == x
  it "Request ipv6" $ property $ \port ->
    let x = SocksRequest SocksBind SocksIPv6 "a1:23:45:67:89:10" port in
      evaluate (encodeDecode x) `shouldThrow` errorCall "ipv6 is not implemented"
  it "Request domain" $ property $ \host port ->
    let x = SocksRequest SocksAssociate SocksDomain host port in
      encodeDecode x == x

socksResponse :: Spec
socksResponse = do
  it "Response ipv4" $ property $ \port ->
    let x = SocksResponse SocksSucceeded SocksIPv4 "127.0.0.1" port in
      encodeDecode x == x
  it "Response ipv6" $ property $ \port ->
    let x = SocksResponse SocksUnreachable SocksIPv6 "a1:23:45:67:89:10" port in
      evaluate (encodeDecode x) `shouldThrow` errorCall "ipv6 is not implemented"
  it "Response domain" $ property $ \host port ->
    let x = SocksResponse SocksRefused SocksDomain host port
      in encodeDecode x == x

socksMethod :: Spec
socksMethod = do
  simpleTest SocksNoAuth
  simpleTest SocksMethodOther

socksCommand :: Spec
socksCommand = do
  simpleTest SocksConnect
  simpleTest SocksBind
  simpleTest SocksAssociate

socksAtyp :: Spec
socksAtyp = do
  simpleTest SocksIPv4
  simpleTest SocksIPv6
  simpleTest SocksDomain

socksReply :: Spec
socksReply = do
 simpleTest SocksSucceeded
 simpleTest SocksUnreachable
 simpleTest SocksRefused
 simpleTest SocksReplyOther

--------------------------------------- Puzzle ----------------------------------------------------

instance Enum [Word8] where
  toEnum x = if x <= 0 then [] else toEnum ((x - 1) `mod` 256) : toEnum ((x - 1) `div` 256)
  fromEnum [] = 0
  fromEnum (x:xs) = fromEnum x + 256 * fromEnum xs + 1

bitEqTest :: Spec
bitEqTest = do
  it "0" $ runReader (bitEq (BS.pack [1,2,3]) (BS.pack [4,5,6])) ((), 0)
  it "1a" $ runReader (bitEq (BS.pack [1,2,3]) (BS.pack [4,5,6])) ((), 1)
  it "1b" $ not (runReader (bitEq (BS.pack [1,2,3]) (BS.pack [128,5,6])) ((), 1))
  it "2b" $ not (runReader (bitEq (BS.pack [1,2,3]) (BS.pack [65,5,6])) ((), 2))
  it "18a" $ runReader (bitEq (BS.pack [170,85,128]) (BS.pack [170,85,191])) ((), 18)
  it "18b" $ not (runReader (bitEq (BS.pack [170,85,128]) (BS.pack [170,85,192])) ((), 18))
  it "18c" $ not (runReader (bitEq (BS.pack [170,85,128]) (BS.pack [170,86,128])) ((), 18))
