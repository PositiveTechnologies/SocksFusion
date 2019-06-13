{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Network.Socks5 (
   ExtSocksSyn(..)
 , SocksAck(..)
 , SocksRequest(..)
 , SocksResponse(..)
 , SocksMethod(..)
 , SocksCommand(..)
 , SocksAtyp(..)
 , SocksReply(..)
 ) where

import           Data.List              (intersperse)
import           Control.Monad          (replicateM, join)
import           Network.Socket         (PortNumber)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString     as B
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Default.Class     (Default(..))

--------------------------------------- Types ---------------------------------------------

-- | Extended Socks Syn message.
data ExtSocksSyn = SocksProgress Int Double -- ^ Inform Agent about scan progress.
                 | SocksSyn { socksSynMethods :: [SocksMethod] }

newtype SocksAck = SocksAck { socksAckMethod :: SocksMethod }

data SocksRequest = SocksRequest { requestCommand :: SocksCommand
                                 , requestAtyp    :: SocksAtyp
                                 , requestAddr    :: B.ByteString
                                 , requestPort    :: PortNumber }

data SocksResponse = SocksResponse { responseReply :: SocksReply
                                   , responseAtyp  :: SocksAtyp
                                   , responseAddr  :: B.ByteString
                                   , responsePort  :: PortNumber }

data SocksMethod = SocksNoAuth | SocksMethodOther
data SocksCommand = SocksConnect | SocksBind | SocksAssociate
data SocksAtyp = SocksIPv4 | SocksIPv6 | SocksDomain
data SocksReply = SocksSucceeded | SocksFailure | SocksNotAllowed | SocksNetUnreachable
                | SocksHostUnreachable | SocksRefused | SocksTTLExpired
                | SocksCommNotSupported | SocksAddrNotSupported | SocksReplyOther

--------------------------------------- Instances -----------------------------------------

deriving instance Eq ExtSocksSyn
deriving instance Eq SocksAck
deriving instance Eq SocksRequest
deriving instance Eq SocksResponse
deriving instance Eq SocksMethod
deriving instance Eq SocksCommand
deriving instance Eq SocksAtyp
deriving instance Eq SocksReply
deriving instance Show ExtSocksSyn
deriving instance Show SocksAck
deriving instance Show SocksRequest
deriving instance Show SocksResponse
deriving instance Show SocksMethod
deriving instance Show SocksCommand
deriving instance Show SocksAtyp
deriving instance Show SocksReply
deriving instance Enum SocksReply

instance Binary ExtSocksSyn where
  get = do
    v <- getWord8
    case v of
      4 -> SocksProgress <$> get <*> get
      5 -> SocksSyn <$> (getWord8 >>= flip replicateM get . fromIntegral)
      x -> fail ("Unknown socks request " ++ show x)
  put (SocksProgress siid prog) = putWord8 4 >> put siid >> put prog
  put (SocksSyn methods) = do
    putWord8 5
    putWord8 $ fromIntegral $ length methods
    mapM_ put methods

instance Binary SocksAck where
  get = do
    _ <- getWord8
    SocksAck <$> get
  put (SocksAck m) = putWord8 5 >> put m

instance Binary SocksRequest where
  get = do
    _    <- getWord8
    cmd  <- get
    _    <- getWord8
    atyp <- get
    addr <- case atyp of
      SocksIPv4 -> BC.pack . join . intersperse "." . map show . B.unpack <$> getByteString 4
      SocksIPv6 -> fail "ipv6 is not implemented"
      SocksDomain -> getWord8 >>= getByteString . fromIntegral
    port <- fromIntegral <$> getWord16be
    return $ SocksRequest cmd atyp addr port
  put (SocksRequest cmd atyp addr port) = do
    putWord8 5
    put cmd
    putWord8 0
    put atyp
    case atyp of
      SocksIPv4 -> mapM_ (putWord8 . read . BC.unpack) $ BC.split '.' addr
      SocksIPv6 -> fail "ipv6 is not implemented"
      SocksDomain -> putWord8 (fromIntegral $ B.length addr) >> putByteString addr
    putWord16be $ fromIntegral port

instance Binary SocksResponse where
  get = do
    _ <- getWord8
    reply <- get
    _ <- getWord8
    atyp <- get
    addr <- case atyp of
      SocksIPv4   -> BC.pack . join . intersperse "." . map show . B.unpack <$> getByteString 4
      SocksDomain -> getWord8 >>= getByteString . fromIntegral
      SocksIPv6   -> fail "ipv6 is not implemented"
    port <- fromIntegral <$> getWord16be
    return $ SocksResponse reply atyp addr port
  put (SocksResponse reply atyp addr port) = do
    putWord8 5
    put reply
    putWord8 0
    put atyp
    case atyp of
      SocksIPv4   -> mapM_ (putWord8 . read . BC.unpack) $ BC.split '.' addr
      SocksDomain -> putWord8 (fromIntegral $ B.length addr) >> putByteString addr
      SocksIPv6   -> fail "ipv6 is not implemented"
    putWord16be $ fromIntegral port
instance Default SocksResponse where
  def = SocksResponse def def B.empty 0

instance Binary SocksMethod where
  get = do
    r <- getWord8
    case r of
      0 -> return SocksNoAuth
      _ -> return SocksMethodOther
  put SocksNoAuth      = putWord8 0
  put SocksMethodOther = putWord8 128
instance Default SocksMethod where
  def = SocksNoAuth

instance Binary SocksCommand where
  get = do
    r <- getWord8
    case r of
      1 -> return SocksConnect
      2 -> return SocksBind
      3 -> return SocksAssociate
      x -> fail ("Unknown command " ++ show x)
  put SocksConnect   = putWord8 1
  put SocksBind      = putWord8 2
  put SocksAssociate = putWord8 3

instance Binary SocksAtyp where
  get = do
    r <- getWord8
    case r of
      1 -> return SocksIPv4
      3 -> return SocksDomain
      4 -> return SocksIPv6
      x -> fail ("Unknown atyp " ++ show x)
  put SocksIPv4   = putWord8 1
  put SocksDomain = putWord8 3
  put SocksIPv6   = putWord8 4
instance Default SocksAtyp where
  def = SocksDomain

instance Binary SocksReply where
  get = do
    r <- getWord8
    return $ case () of
      _ | r >= 0 && r <= 8 -> toEnum $ fromIntegral r
        | otherwise -> SocksReplyOther
  put = putWord8 . fromIntegral . fromEnum
instance Default SocksReply where
  def = SocksSucceeded
