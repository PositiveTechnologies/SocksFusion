{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Network.SocksFusion (
   -- * Utils
   pVersion
 , wait
 , timeout
 , BinaryException(..)
   -- * Network
 , Host
 , AddrPort(..)
 , ProtocolException
 , Disposable(..)
 , (<@)
 , (@<)
 , (.@.)
   -- * Multiplexing
 , cOMMAND
 , hEARTBEAT
 , iNFO
 , Quiver
 , Arrow(..)
 , AgentMessage(..)
 , ProxyMessage(..)
 , Siid(..)
 , ScanChange(..)
 , KeyResponse(..)
 , defaultQuiver
 , getTMVar
 , contextRun
 , arrowsPair
   -- * Challenge
 , createPuzzle
 , solvePuzzle
 , testPuzzle
 , incr
 , bitEq
 ) where

import           Prelude         hiding (all, log)
import qualified Paths_SocksFusion

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Control.Exception      (IOException, Exception, SomeException, try, catch, handle, throwIO)

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

--------------------------------------- Constants ----------------------------------------

-- | ID of command channel in quivers.
cOMMAND :: Int
cOMMAND = 0

-- | ID of heartbeat channel in quivers.
hEARTBEAT :: Int
hEARTBEAT = 1

-- | ID of info channel in quivers.
iNFO :: Int
iNFO = 2

--------------------------------------- Utils --------------------------------------------

-- | Protocol version.
pVersion :: Version
pVersion = Paths_SocksFusion.version

-- | Sleep for @n@ seconds.
wait :: Int -> IO ()
wait = threadDelay . (* 1000000)

-- | Returns `Nothing` if an action doesn't finish in `n` seconds.
timeout :: Int -> IO a -> IO (Maybe a)
timeout x = System.Timeout.timeout (x * 1000000)

try_ :: IO () -> IO ()
try_ a = void (try @SomeException a)

finThread :: MVar () -> a -> IO ()
finThread control = const $ void $ tryPutMVar control ()

newtype BinaryException = ErrorBinary String

instance Exception BinaryException
instance Show BinaryException where
  show (ErrorBinary s) = s

--------------------------------------- Network ------------------------------------------

type Host = ByteString
data AddrPort = !Host :@: !PortNumber                       deriving Eq
data ProtocolException = Refused [SockAddr] | Timeout [SockAddr]
                       | Unknown Host | Other String

-- | Accept and configure incoming socket.
(<@) :: Socket -> IO Socket
(<@) s = do
  (c,_) <- accept s
  setSocketOption c KeepAlive 1
  return c

-- | Open socket server on given address. Can throw ProtocolException.
(@<) :: AddrPort -> IO Socket
(@<) (a :@: p) = do
  (fam, addr) <- (addrFamily &&& addrAddress) . preferIpv4
             <$> getAddrInfo (Just $ defaultHints { addrFlags = [ AI_PASSIVE
                                                                , AI_NUMERICHOST ]
                                                  , addrSocketType = Stream })
                             (if BC.null a then Nothing else Just $ BC.unpack a)
                             (Just $ show p)
  s <- socket fam Stream 0x6
  setSocketOption s ReuseAddr 1 >> setSocketOption s KeepAlive 1
  bind s addr
  listen s maxListenQueue
  return s
  where
  preferIpv4 xs = fromMaybe (head xs) $ find ((/= AF_INET6) . addrFamily) xs

-- | Connect to giver host and port.
(.@.) :: Host -> PortNumber -> IO Handle
h .@. p = do
  addrs <- getAddrInfo (Just $ defaultHints { addrSocketType = Stream })
                       (Just $ BC.unpack h)
                       (Just $ show p) `catch` const @_ @IOError (throwIO $ Unknown h)
  -- Connect to all addresses and throw an error if nothing is success.
  foldl (\x -> handle (\(_ :: IOException) -> x) . c)
        (throwIO . Refused $ map addrAddress addrs)
        addrs
  where
  c a = do
    s <- socket (addrFamily a) Stream 0x6
    setSocketOption s KeepAlive 1
    timeout 3 (s `connect` addrAddress a) >>= \case
      Nothing -> dispose s >> throwIO (Timeout [addrAddress a])
      Just{}  -> socketToHandle s ReadWriteMode

-- | Class of resources with custom finaliser.
class Disposable a where
  dispose :: a -> IO ()
instance Disposable Socket where
  dispose s = try_ (shutdown s ShutdownBoth) >> try_ (close s)
instance Disposable Handle where
  dispose = try_ . hClose
instance Disposable T.Context where
  dispose ctx = try_ $ T.bye ctx

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

instance Exception ProtocolException
instance Show ProtocolException where
  show (Refused xs) = "Connection refused:" <> unwords (map show xs)
  show (Timeout xs) = "Target is unreachable: " <> unwords (map show xs)
  show (Unknown h)  = "Cannot resolve address: " <> BC.unpack h
  show (Other str)  = "Unknown socket error: " <> str

--------------------------------------- Multiplexing -------------------------------------

-- | Container of channels. Each TMVar is used to read a message on given channel.
type Quiver = TVar (IM.IntMap (TMVar ByteString))

-- | Structure keeps message with channel identifier.
data Arrow = Arrow { arrowhead :: {-# UNPACK #-} !Int          -- ^ Message identifier.
                   , shaft     :: {-# UNPACK #-} !ByteString   -- ^ Actual message.
                   }                                        deriving (Eq, Show)

-- | Scan internal id.
newtype Siid = Siid Int                           deriving (Binary, Eq, Show)

data AgentMessage = AgentInfo ByteString | KeyOffer ByteString  deriving (Eq, Show)

data ProxyMessage = ProxyInfo ByteString
                  | KeyResponse KeyResponse
                  | ScanChange Siid ScanChange
                  | ScanProgress Siid Double      deriving (Eq, Show)

data ScanChange = ScanStarted | ScanFinished | ScanStopped      deriving (Enum, Eq, Show)
data KeyResponse = KeyAccepted | CloudMaintenance
                 | KeyUnknown | KeyDuplicate      deriving (Enum, Eq, Show)

-- | Empty Quiver with installed command, heartbeat, info channels.
defaultQuiver :: STM Quiver
defaultQuiver = do
  command   <- newEmptyTMVar
  heartbeat <- newEmptyTMVar
  progress  <- newEmptyTMVar
  newTVar $ IM.fromList [(cOMMAND, command), (hEARTBEAT, heartbeat), (iNFO, progress)]

-- | Blocking function returns mvar by it's arrowhead.
getTMVar :: Int -> Quiver -> STM (TMVar ByteString)
getTMVar index quiver = IM.lookup index <$> readTVar quiver >>= maybe retry return

-- | Run secured proxy <-> agent tunnel.
contextRun :: Quiver
           -> MVar Arrow
           -> T.Context
           -> (Arrow -> Int -> IO Int) -- ^ Action for new heads. Returns max global head.
           -> IO ()
contextRun quiver back ctx notFound = do
  control <- newEmptyMVar
  heartbeat <- atomically $ getTMVar hEARTBEAT quiver
  let forkControl = flip forkFinally (finThread control)
  h_tid <- forkControl (sendH heartbeat)
  s_tid <- forkControl  sendC
  r_tid <- forkControl (recvC decoder 0)
  void $ takeMVar control
  killThread s_tid >> killThread r_tid >> killThread h_tid
  where
  decoder = runGetIncremental get
  delay = 30
  sendC = do
    arr@(Arrow ah sh) <- takeMVar back
    T.sendData ctx $ runPut $ put arr
    unless (ah == 0 && B.null sh) sendC
  recvC Fail{} _ = return ()
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
  sendH mvar = timeout (2 * delay) (atomically $ takeTMVar mvar) >>= \case
    Nothing  -> return ()
    Just msg -> do
      putMVar back (Arrow hEARTBEAT msg)
      wait delay
      sendH mvar

-- | Worker on sockets pair established with external resource.
arrowsPair :: Handle -> Int -> MVar Arrow -> TMVar ByteString -> IO ()
arrowsPair h ah back message = do
  control <- newEmptyMVar
  s_tid <- forkFinally sendC (finThread control)
  r_tid <- forkFinally recvC (finThread control)
  void $ takeMVar control
  killThread s_tid >> killThread r_tid
  void $ tryPutMVar back (Arrow ah B.empty)
  where
  chunk :: Int
  chunk = 65535
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

instance Binary Arrow where
  get = Arrow <$> (fromIntegral <$> getWord32le)
              <*> (getWord16le >>= getByteString . fromIntegral)
  put (Arrow ah sh) = do
    putWord32le (fromIntegral ah)
    let len = toEnum $ B.length sh
    putWord16le len
    -- Double conversion to trunk the size of message
    putByteString (B.take (fromEnum len) sh)

instance Binary AgentMessage where
  get = getWord8 >>= \case
    0 -> AgentInfo <$> get
    1 -> KeyOffer <$> get
    _ -> fail "Wrong AgentMessage"
  put (AgentInfo msg) = putWord8 0 >> put msg
  put (KeyOffer key) = putWord8 1 >> put key

instance Binary ProxyMessage where
  get = getWord8 >>= \case
    0 -> ProxyInfo <$> get
    1 -> KeyResponse <$> get
    2 -> ScanChange <$> get <*> get
    3 -> ScanProgress <$> get <*> get
    _ -> fail "Wrong ProxyMessage"
  put (ProxyInfo msg) = putWord8 0 >> put msg
  put (KeyResponse key) = putWord8 1 >> put key
  put (ScanChange siid ch) = putWord8 2 >> put siid >> put ch
  put (ScanProgress siid prog) = putWord8 3 >> put siid >> put prog

instance Binary ScanChange where
  get = toEnum . fromEnum <$> getWord16le
  put = putWord16le . toEnum . fromEnum

instance Binary KeyResponse where
  get = toEnum . fromEnum <$> getWord16le
  put = putWord16le . toEnum . fromEnum

--------------------------------------- Challenge ----------------------------------------

-- | Generate puzzle with given parameters.
-- Answer should start with given bytes and sha256 of it should start with given bits.
createPuzzle :: (B.ByteArray b) => Int -> Int -> IO b
createPuzzle variation complexity = getRandomBytes (variation + (complexity `quot` 8) + 1)

-- | Find the string satisfies global parameters.
-- This function will fail if parameters are to big.
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

-- | Check that the answer satisfies the test.
testPuzzle :: (B.ByteArray b, MonadReader (Int, Int) m) => b -> b -> m Bool
testPuzzle test answer = do
    variation <- asks fst
    if not (B.takeView test variation `B.eq` B.takeView answer variation)
      then return False
      else B.dropView test variation `bitEq` B.dropView (hashWith SHA256 answer) 0

-- | Enumerate lists in alphabetic order.
incr :: (Bounded a, Enum a, Eq a) => [a] -> [a]
incr [] = [minBound]
incr (x:xs) = if x == maxBound then minBound:incr xs else succ x:xs

-- | Two bytestrings are equivalent if first `n` bits are equivalent.
bitEq :: (B.ByteArrayAccess b1, B.ByteArrayAccess b2, MonadReader (a, Int) m)
      => b1 -> b2 -> m Bool
bitEq b1 b2 = do
  (units, modulo) <- asks (flip divMod 8 . snd)
  return $ B.takeView b1 units `B.eq` B.takeView b2 units
         && countLeadingZeros (B.index b1 units `xor` B.index b2 units) >= modulo
