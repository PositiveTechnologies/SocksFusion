{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Network.SocksFusion (
   -- * Utils
   pVersion
 , wait
 , BinaryException(..)
   -- * Network
 , AddrPort(..)
 , ProtocolException
 , Disposable(..)
 , (<@)
 , (@<)
 , (.@.)
   -- * Multiplexing
 , Quiver
 , defaultQuiver
 , getTMVar
 , contextRun
 , Arrow(..)
 , arrowsPair
 , cOMMAND
 , hEARTBEATIN
 , hEARTBEATOUT
   -- * Challenge
 , createPuzzle
 , solvePuzzle
 , testPuzzle
 , incr
 , bitEq
 ) where

import           Prelude         hiding (all, log)

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Control.Exception      (IOException, Exception, SomeException, try, handle, throwIO, throw)

import           Data.Char
import           Data.Bits
import           Data.List              (elemIndices,find)
import qualified Data.IntMap.Strict  as IM
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Control.Arrow          (first, (&&&))
import           Data.Version           (Version(..))

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           Network.Socket
import qualified Network.TLS         as T
import qualified Data.ByteArray      as B
import           Crypto.Random.Types    (MonadRandom(..))
import           Crypto.Hash

import qualified System.Timeout
import           System.IO

--------------------------------------- Constants -------------------------------------------------

-- | Protocol version.
pVersion :: Version
pVersion = Version [1, 3] mempty

cOMMAND :: Int
cOMMAND = 0
hEARTBEATIN :: Int
hEARTBEATIN = 1
hEARTBEATOUT :: Int
hEARTBEATOUT = 2

--------------------------------------- Utils -----------------------------------------------------

-- | Sleep for @n@ seconds.
wait :: Int -> IO ()
wait = threadDelay . (* 1000000)

timeout :: Int -> IO a -> IO (Maybe a)
timeout x = System.Timeout.timeout (x * 1000000)

try_ :: IO () -> IO ()
try_ a = void (try a :: IO (Either SomeException ()))

--------------------------------------- Network ---------------------------------------------------

type Host = ByteString

data AddrPort = !Host :@: !PortNumber
instance Show AddrPort where
  show (host :@: port)
    | B.null host      = p
    | BC.elem ':' host = "[" <> h <> "]:" <> p
    | otherwise                = h <> ":" <> p
    where
    h = BC.unpack host
    p = show port
instance Read AddrPort where
  readsPrec p s = case reverse $! elemIndices ':' s of
    []    -> all s
    (0:_) -> all $! drop 1 s
    (i:_) -> do
      let (x,y) = first (dropWhile isSpace) $ splitAt i s
      (a,_) <- readsPrec p $! "\"" <> filter (\c -> c /= '[' && ']' /= c) x <> "\""
      first ((:@:) a) <$> (readsPrec p $! tail y)
    where
    all s' = first ((:@:) B.empty) <$> readsPrec p s'

--------------------------------------- Peers

newtype ProtocolException = Silence [SockAddr]
instance Show ProtocolException where
  show (Silence xs) = foldl (\a b -> a <> " " <> show b) "Target is unreachable:" xs
instance Exception ProtocolException

-- | Open socket server on given address.
(@<) :: AddrPort -> IO Socket
(@<) (a :@: p) = do
  (fam, addr) <- (addrFamily &&& addrAddress) . preferIpv4
            <$> getAddrInfo (Just $ defaultHints { addrFlags = [ AI_PASSIVE, AI_NUMERICHOST ]
                                                 , addrSocketType = Stream })
                            (if B.null a then Nothing else Just $ BC.unpack a)
                            (Just $ show p)
  s <- socket fam Stream 0x6
  setSocketOption s ReuseAddr 1 >> setSocketOption s KeepAlive 1
  bind s addr
  listen s maxListenQueue
  return s
  where
  preferIpv4 xs = fromMaybe (head xs) $ find ((/= AF_INET6) . addrFamily) xs

-- | Accept and configure incoming socket.
(<@) :: Socket -> IO Socket
(<@) s = do
  (c,_) <- accept s
  setSocketOption c KeepAlive 1
  return c

-- | Try to connect to giver host and port.
(.@.) :: Host -> PortNumber -> IO Socket
h .@. p = do
  addrs <- getAddrInfo (Just $ defaultHints { addrSocketType = Stream })
                      (Just $ BC.unpack h)
                      (Just $ show p)
  -- Connect to all addresses and throw an error if nothing is success.
  foldl (\x -> handle (\(_ :: IOException) -> x) . c)
        (throwIO . Silence $ map addrAddress addrs)
        addrs
  where
  c a = do
    s <- socket (addrFamily a) Stream 0x6
    setSocketOption s KeepAlive 1
    timeout 3 (s `connect` addrAddress a) >>= \case
      Nothing -> dispose s >> throw (Silence [addrAddress a])
      Just _  -> return s
infixl 2 .@.

class Disposable a where
  dispose :: a -> IO ()
instance Disposable Socket where
  dispose s = try_ (shutdown s ShutdownBoth) >> try_ (close s)
instance Disposable Handle where
  dispose = try_ . hClose
instance Disposable T.Context where
  dispose ctx = try_ $ T.bye ctx

--------------------------------------- Arrows ----------------------------------------------------

data Arrow = Arrow { arrowhead :: {-# UNPACK #-} !Int          -- ^ Identifier for multiplexor.
                   , shaft     :: {-# UNPACK #-} !ByteString   -- ^ Actual message.
                   } deriving (Eq, Show)

instance Binary Arrow where
  get = Arrow <$> fmap fromIntegral getWord32le <*> (getWord32le >>= getByteString . fromIntegral)
  put (Arrow ah sh) = putWord32le (fromIntegral ah)
                   >> putWord32le (fromIntegral $ B.length sh)
                   >> putByteString sh

newtype BinaryException = ErrorBinary String
instance Show BinaryException where
  show (ErrorBinary s) = s
instance Exception BinaryException

--------------------------------------- Quivers ---------------------------------------------------

type Quiver = TVar (IM.IntMap (TMVar ByteString))
defaultQuiver :: STM Quiver
defaultQuiver = do
  cmd <- newEmptyTMVar
  h1 <- newEmptyTMVar
  h2 <- newTMVar "Waiting"
  newTVar $ IM.fromList [(cOMMAND, cmd), (hEARTBEATIN, h1), (hEARTBEATOUT, h2)]

-- | Blocking function returns mvar by it's arrowhead.
getTMVar :: Int -> Quiver -> STM (TMVar ByteString)
getTMVar index quiver = IM.lookup index <$> readTVar quiver >>= maybe retry return

--------------------------------------- Multiplexing ----------------------------------------------

finThread :: MVar () -> a -> IO ()
finThread control = const $ void $ tryPutMVar control ()

-- | Run secured proxy <-> agent tunnel.
contextRun :: Quiver
           -> MVar Arrow               -- ^ Second argument for outcoming messages,
           -> T.Context
           -> (Arrow -> Int -> IO Int)   -- ^ Action for unknown arroheads. Return's max global ah.
           -> (ByteString -> IO ())     -- ^ Logger action.
           -> IO ()
contextRun quiver back ctx notFound log = do
  control <- newEmptyMVar
  (hin, hout) <- atomically $ pure (,) <*> getTMVar hEARTBEATIN quiver
                                      <*> getTMVar hEARTBEATOUT quiver
  let forkControl = flip forkFinally (finThread control)
  s_tid  <- forkControl  sendC
  r_tid  <- forkControl (recvC decoder 0)
  hr_tid <- forkControl (recvH hin mempty)
  hs_tid <- forkControl (sendH hout mempty)
  void $ takeMVar control
  killThread s_tid >> killThread r_tid >> killThread hr_tid >> killThread hs_tid
  where
  decoder = runGetIncremental get
  delay = 60
  sendC = do
    arr@(Arrow ah sh) <- takeMVar back
    T.sendData ctx $ runPut $ put arr
    unless (ah == 0 && B.null sh) sendC
  recvC (Fail _ _ m) _ =
    log (BC.pack m)
  recvC (Done left _ arr@(Arrow ah sh)) top = do
    new <- atomically $ do
      x <- IM.lookup ah <$> readTVar quiver
      maybe (return ()) (`putTMVar` sh) x
      return x
    case new of
      Nothing -> notFound arr top >>= recvC (decoder `pushChunk` left)
      Just{}  -> recvC (decoder `pushChunk` left) top
  recvC (Partial k) top = do
    m <- T.recvData ctx
    unless (B.null m) (recvC (k $ Just m) top)
  sendH mvar prev = do
    msg <- atomically $ fromMaybe prev <$> tryTakeTMVar mvar
    putMVar back (Arrow hEARTBEATIN msg) -- IN because of symmetric protocol
    wait delay
    sendH mvar msg
  recvH mvar prev = timeout (2 * delay) (atomically $ takeTMVar mvar) >>= \case
    Nothing   -> return ()
    Just smth -> when (prev /= smth) (log smth) >> recvH mvar smth

-- | Sockets pair used for external connection.
arrowsPair :: Handle -> Int -> MVar Arrow -> TMVar ByteString -> IO ()
arrowsPair h ah back message = do
  control <- newEmptyMVar
  s_tid <- forkFinally sendC (finThread control)
  r_tid <- forkFinally recvC (finThread control)
  void $ takeMVar control
  killThread s_tid >> killThread r_tid
  void $ tryPutMVar back (Arrow ah B.empty)
  where
  chunk = 4 * 1024
  sendC = do
    m <- atomically $ takeTMVar message
    unless (B.null m) $ do
    BC.hPut h m
    sendC
  recvC = do
    m <- BC.hGetSome h chunk
    unless (B.null m) $ do
    putMVar back (Arrow ah m)
    recvC

--------------------------------------- Challenge -------------------------------------------------

-- | Bitwise test for bytestrings for global complexity.
bitEq :: (B.ByteArrayAccess b1, B.ByteArrayAccess b2, MonadReader (a, Int) m) => b1 -> b2 -> m Bool
bitEq b1 b2 = do
  (units, modulo) <- asks (flip divMod 8 . snd)
  return $ B.takeView b1 units `B.eq` B.takeView b2 units
         && countLeadingZeros (B.index b1 units `xor` B.index b2 units) >= modulo

-- | Generate puzzle for global parameters.
-- Answer should start with given bytes and sha256 of it should start with given bits
createPuzzle :: (B.ByteArray b) => Int -> Int -> IO b
createPuzzle variation complexity = getRandomBytes (variation + (complexity `quot` 8) + 1)

-- | Find the string satisfies global parameters.
-- This function will fail if parameters are to big.
-- TODO: use threaded version
solvePuzzle :: ByteString -> IO ByteString
solvePuzzle test = do
  answer <- newEmptyMVar
  tids <- forM [0..3] (forkIO . flip runReaderT ((), compl) . withPrefix answer [] . B.singleton)
  sol <- takeMVar answer
  mapM_ killThread tids
  return sol
  where
  (var, b1) = car &&& BC.tail $ test
  (compl, b2) = car &&& BC.tail $ b1
  (start, hstart) = B.splitAt var b2
  car = toEnum . fromEnum . BC.head
  withPrefix :: MVar ByteString -> [Word8] -> ByteString -> ReaderT (a, Int) IO ()
  withPrefix mvar af pref = hstart `bitEq` B.dropView (hashWith SHA256 sol) 0 >>= \case
    True  -> lift (void $ tryPutMVar mvar sol)
    False -> withPrefix mvar (incr af) pref
    where
    sol = start <> pref <> B.pack af

-- | Enumerate lists in alphabetic order.
incr :: (Bounded a, Enum a, Eq a) => [a] -> [a]
incr [] = [minBound]
incr (x:xs) = if x == maxBound then minBound:incr xs else succ x:xs

-- | Check that the answer satisfies the test.
testPuzzle :: (B.ByteArray b, MonadReader (Int, Int) m) => b -> b -> m Bool
testPuzzle test answer = do
    (variation, _) <- ask
    if not (B.takeView test variation `B.eq` B.takeView answer variation)
      then return False
      else B.dropView test variation `bitEq` B.dropView (hashWith SHA256 answer) 0
