{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, PostfixOperators, TupleSections, CPP #-}

module Network.SocksFusion where

import           Prelude         hiding ((++),length,last,init,all,log)

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad          (void,when,unless)
import qualified Control.Exception as X
import           Control.Arrow          (first)

import           Data.Char
import           Data.Word
import           Data.List              (elemIndices,(++),find)
import qualified Data.Map.Strict as M
import           Data.ByteString.Char8  (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe             (fromMaybe)
import           Data.Either            (either)
import           Data.Monoid            ((<>))

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)

import           Network.Socket  hiding (recv,send)
import qualified Network.TLS as T

import           System.Timeout
import           System.IO       hiding (hGetLine,hPutStr,hGetContents)
import           System.IO.Unsafe
import           System.Exit            (exitWith, ExitCode(..))

import           Foreign.Storable
import           Foreign.Marshal.Array
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.StablePtr

---------------------------------------------------------------------------------------------UTILITY

type Key = ByteString

type Seconds = Int
secs     :: Int -> Seconds;                  secs         = (* 1000000)
wait     :: Seconds -> IO ();                wait         = threadDelay . secs
schedule :: Seconds -> IO () -> IO ThreadId; schedule s a = forkIO $! wait s >> a

{-# INLINE (=>>) #-}; infixr 0 =>>; (=>>) :: Monad m => m a -> (a -> m b) -> m a
a =>> f = do r <- a; _ <- f r; return r

type ErrorIO = IO
tryRun :: IO () -> IO ()
tryRun = tryWith (\(x :: ProtocolException) -> do log (show x); wait 2)

(???) :: ErrorIO a -> [IO a] -> IO a
e ??? as = foldr (?>) e as
  where x ?> y = x `X.catch` (\(_ :: X.SomeException) -> y)

tryWith     -- Transfered from corsis/splice, cause we need only two functions from it
  :: (X.Exception e)
  => (e -> IO a)             -- ^ exception handler.
  -> IO a                    -- ^ action to run which can throw /any/ exception.
  -> IO a                    -- ^ new action where all exceptions are handled by the single handler.
tryWith h a = X.try a >>= \r -> case r of Left x -> h x; Right y -> return y

try_
  :: IO ()                   -- ^ action to run which can throw /any/ exception.
  -> IO ()                   -- ^ new action where exceptions are silenced.
try_ a = void (X.try a :: IO (Either X.SomeException ()))

type ChunkSize = Int

--------------------------------------------------------------------------------------------ADDRPORT

type Host = ByteString
type Port = PortNumber

data AddrPort = !Host :@: !Port
instance Show AddrPort where
  show (a:@:p) = if B.null     a then show p else f ++ ":" ++ show p
    where f    = if B.elem ':' a then "[" ++ B.unpack a ++ "]" else B.unpack a
instance Read AddrPort where
  readsPrec p s =
    case reverse $! elemIndices ':' s of
      [] -> all s
      (0:_) -> all $! drop 1 s
      (i:_) -> one i
    where all s' = first ((:@:) "") <$> readsPrec p s'
          one i  = do
            let (x,y) = first (dropWhile isSpace) $ splitAt i s
            (a,_) <- readsPrec p $! "\"" ++ filter (\c -> c /= '[' && ']' /= c) x ++ "\""
            first ((:@:) a) <$> (readsPrec p $! tail y)

(?:) :: AddrPort -> IO (Family, SockAddr)
(?:) (a :@: p)= f . c <$> getAddrInfo (Just hints) n (Just $! show p)
  where hints = defaultHints { addrFlags = [ AI_PASSIVE, AI_NUMERICHOST ], addrSocketType = Stream }
        n     = if B.null a then Nothing else Just $! B.unpack a
        c  xs = fromMaybe (head xs) $ find ((/= AF_INET6) . addrFamily) xs
        f  x  = (addrFamily x, addrAddress x)

-----------------------------------------------------------------------------------------------PEERS

data Peer = !Socket :!: !Handle
newtype SPeer = SPeer T.Context

newtype ProtocolException = Silence [SockAddr]
instance Show ProtocolException where
  show (Silence xs) = foldl (\a b -> a ++ " " ++ show b) "Target is unreachable:" xs
instance X.Exception ProtocolException where

faf :: Family -> String
faf x = case x of
  AF_INET6 -> sf
  AF_UNSPEC -> sf
  AF_INET -> "IPv4"
  _ -> show x
  where sf = "IPv6(+4?)"

configure :: Socket -> IO ()
configure s   = m RecvBuffer c >> m SendBuffer c >> setSocketOption s KeepAlive 1
  where m o u = do v <- getSocketOption s o; when (v < u) $! setSocketOption s o u
        c     = fromIntegral chunk

chunk :: ChunkSize
chunk = 4 * 1024

(@<) :: AddrPort -> IO Socket
(@<) ap = do
  (f,a) <- (ap ?:)
  s <- socket f Stream 0x6 =>> \s -> mapM_ (\o -> setSocketOption s o 1) [ ReuseAddr, KeepAlive ]
  bind s a; listen s maxListenQueue
  log $ show $ Listen :^: (faf f, ap)
  return s

(<@) :: Socket -> IO Socket
(<@) s = do
  (c,_) <- accept s
  configure c
  return c

(.@.) :: Host -> Port -> IO Socket
h .@. p = getAddrInfo hint host port >>= \as -> e as ??? map c as
  where hint = Just $! defaultHints { addrSocketType = Stream }
        host = Just $! B.unpack h
        port = Just $! show     p
        e as = X.throwIO . Silence $! map addrAddress as
        c a  = do s <-      socket (addrFamily  a) Stream 0x6 =>> configure
                  r <- timeout (secs 3) $ s `connect`  addrAddress a
                  case r of
                    Nothing -> do dispose s; X.throw $! Silence [addrAddress a]
                    Just _  -> return s

(!@)  :: Socket ->                                    IO  Peer; (!@) s     = (:!:) s <$> socketToHandle s ReadWriteMode -- return (socket :!: handle)
(~@)  :: (T.TLSParams t, T.HasBackend b) => t -> b -> IO SPeer; (~@) c s   = SPeer <$> T.contextNew s c -- return tls_peer
(!<@) :: Socket ->                                    IO  Peer; (!<@)  l   = (l <@) >>= (!@) -- accept socket and return peer
(!)   :: Host   -> Port ->                            IO  Peer; (!)  h p   = h .@. p >>= (!@)  -- connect to host, port and return peer
(~<)  :: (T.TLSParams t) => t -> Socket ->            IO SPeer; (~<) c l   = (l <@) >>= (c ~@)-- accept socket, wrap in tls context and return peer
(!~)  :: (T.TLSParams t) => t -> Host -> Port ->      IO SPeer; (!~) c h p = h .@. p >>= (c ~@)-- connect to host, port, wrap in tls context and return peer

class    Disposable a       where dispose :: a -> IO ()
instance Disposable Socket  where
  dispose s = do
    try_ $! shutdown s ShutdownBoth
    try_ $! close   s
instance Disposable Peer          where
  dispose (s :!: h) = dispose s >> dispose h
instance Disposable SPeer         where
  dispose (SPeer c) = T.backendClose $ T.ctxConnection c  -- We don't send Close Notify here
instance Disposable Handle        where dispose = try_ . hClose
instance Disposable T.Context     where dispose = T.bye
instance Disposable (StablePtr a) where dispose = freeStablePtr
instance Disposable (      Ptr a) where dispose = free

----------------------------------------------------------------------------------------------EVENTS

data ServiceAction = Listen | Watch | Drop                                   deriving Show
data    PeerAction = Accept | Open  | Close | Receive Request | Send Request

data Event = ServiceAction :^: (String, AddrPort)                            deriving Show

------------------------------------------------------------------------------------------------WIRE

data Task = ProxyTask AddrPort AddrPort | AgentTask (Port, Host) Key

type Request = AddrPort

-----------------------------------------------------------------------------------------PORTVECTORS

data Vectors = V {-# UNPACK #-} !(Ptr            Word16 )                      -- number of clients
                 {-# UNPACK #-} !(Ptr (StablePtr Socket))                      -- server socket
                 {-# UNPACK #-} !(Ptr            Word16 )                      -- watch thread
portVectors :: MVar Vectors
portVectors = unsafePerformIO $! newEmptyMVar

initialized :: MVar Bool
initialized = unsafePerformIO $! newMVar False

initialize  :: IO ()
initialize  = initialized `modifyMVar_` \mvar ->
  unless mvar (new >>= putMVar portVectors) >> return True
    where new = let pc = 65536 in V <$> mallocArray pc <*> mallocArray pc <*> mallocArray pc

{-# INLINE (|.) #-}; (|.)::Storable a=>Ptr a -> Int -> IO a         ; (|.) = peekElemOff
{-# INLINE (|^) #-}; (|^)::Storable a=>Ptr a -> Int ->    a -> IO (); (|^) = pokeElemOff

(-@<) :: AddrPort -> IO Socket
(-@<) ap@(_ :@: p') = let p = fromIntegral p' in
  withMVar portVectors $ \(V !c !s !t) -> do
    t |^ p =<< (1 +) <$> t |. p
    n <- c |. p
    case compare n 0 of
      GT -> do
        c |^ p $! n+1
        s |. p >>= deRefStablePtr
      EQ -> do
        l <- (ap @<)
        newStablePtr l >>= s |^ p
        c |^ p $! n+1
        return l
      LT -> log "-@< FAULT" >> exitWith (ExitFailure 10)

(-✖) :: AddrPort -> IO ()
(-✖) ap@(_ :@: p') = let p = fromIntegral p' in
  withMVar portVectors $ \(V !c _ _) -> do
    n <- c |. p
    case compare n 1 of
      GT -> c |^ p $! n-1
      EQ -> watch p
      LT -> log "-x  FAULT" >> exitWith (ExitFailure 11)
  where
  watch p = void . forkIO $! withMVar portVectors $ \(V _ _ !t') -> do
    tp <- t' |. p
    log $ show $ Watch :^: (show tp, ap)
    void . schedule 10 $!
      withMVar portVectors $ \(V !c !s !t) -> do
        n   <- c |. p
        tp' <- t |. p
        if n == 1 && tp == tp'
          then do log $ show $ Drop :^: (faf AF_UNSPEC, ap)
                  c |^ p $! n-1
                  sv <- s |. p
                  deRefStablePtr sv >>= dispose
                  dispose sv
          else when (n == 1) $! watch p

--------------------------------------- Working with controllers -----------------------------------

newtype FDMessage = FDMessage { fdMsg :: B.ByteString }

instance Binary FDMessage where
  put (FDMessage m) = do
    putWord32le $ fromIntegral $ B.length m
    putByteString m
  get = FDMessage <$> (getWord32le >>= getByteString . fromIntegral)

--------------------------------------- Multiplexing ------------------------------------------------

data Arrow = Arrow { arrowhead :: !Word32, shaft :: !ByteString }
instance Binary Arrow where
  get = Arrow <$> getWord32le <*> (getWord32le >>= getByteString . fromIntegral)
  put (Arrow ah s) = putWord32le ah >> putWord32le (fromIntegral $ B.length s) >> putByteString s

sendArrow :: T.Context -> Arrow -> IO ()
sendArrow ctx = T.sendData ctx . runPut . put

type Quiver = TVar (M.Map Word32 (MVar B.ByteString))

emptyQuiver :: Quiver -> IO ()
emptyQuiver quiver = atomically (M.elems <$> readTVar quiver) >>= mapM_ (`tryPutMVar` mempty)

contextRun :: Quiver -> MVar Arrow -> T.Context -> (Arrow -> IO ()) -> IO ()
contextRun quiver back ctx action = do
  control <- newEmptyMVar
  s_tid <- forkFinally sendC (const $ void $ tryPutMVar control False)
  r_tid <- forkFinally (recvC decoder) (const $ void $ tryPutMVar control False)
  void $ takeMVar control
  killThread s_tid >> killThread r_tid
    where
    decoder = runGetIncremental get
    sendC = do
      arr@(Arrow ah sh) <- takeMVar back
      sendArrow ctx arr
      unless (ah == 0 && B.null sh) sendC
    recvC (Fail left _ _) =
      log ("SSL decode failed " <> B.unpack left) >> exitWith (ExitFailure 5)
    recvC (Done left _ arr@(Arrow ah sh)) = do
      record <- atomically $ M.lookup ah <$> readTVar quiver
      case record of
        Nothing -> action arr
        Just mvar -> putMVar mvar sh
      recvC (decoder `pushChunk` left)
    recvC (Partial k) = do
      m <- T.recvData ctx
      unless (B.null m) (recvC $ k $ Just m)

arrowsPair :: Peer -> Word32 -> MVar Arrow -> MVar B.ByteString -> IO ()
arrowsPair (_ :!: h) ah back message = do
  control <- newEmptyMVar
  s_tid <- forkFinally sendC (const $ void $ tryPutMVar control False)
  r_tid <- forkFinally recvC (const $ void $ tryPutMVar control True)
  notify <- takeMVar control
  killThread s_tid >> killThread r_tid
  when notify (void $ tryPutMVar back (Arrow ah ""))
  void $ tryTakeMVar message
    where
    sendC = do
      m <- takeMVar message
      unless (B.null m) $ do
      res <- X.try $ B.hPut h m :: IO (Either X.SomeException ())
      either (const $ putMVar back (Arrow ah "")) (const sendC) res
    recvC = do
      m <- B.hGetSome h chunk
      unless (B.null m) $ do
      putMVar back (Arrow ah m)
      recvC

heartbeat :: MVar B.ByteString -> MVar Arrow -> IO ()
heartbeat message back = do
  control <- newEmptyMVar
  s_tid <- forkIO sendC
  r_tid <- forkFinally recvC (const $ void $ tryPutMVar control ())
  void $ takeMVar control
  killThread s_tid >> killThread r_tid
  void $ tryPutMVar back (Arrow 0 "")
    where
    sendC = do
      m <- takeMVar message
      log $ B.unpack m
      sendC
    recvC = do
      wait 60
      putMVar back (Arrow 0 "Running")
      recvC

--------------------------------------- Logging ------------------------------------------------------

log :: String -> IO ()
log a = do
  t <- getCurrentTime
  putStrLn $! formatTime defaultTimeLocale "%b %d %X - " t <> a
