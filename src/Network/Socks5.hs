module Network.Socks5 where

import           Network.SocksFusion (Host, Port)

import           Data.List (intersperse)
import           Data.Foldable (fold)
import           Control.Monad (replicateM)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Default.Class (Default, def)

data RequestCommand = SocksConnect | SocksBind | SocksAssociate deriving Show
instance Enum RequestCommand where
  toEnum 1 = SocksConnect
  toEnum 2 = SocksBind
  toEnum 3 = SocksAssociate
  toEnum _ = error "Unknown command"
  fromEnum SocksConnect = 1
  fromEnum SocksBind = 2
  fromEnum SocksAssociate = 3
instance Binary RequestCommand where
  get = toEnum . fromIntegral <$> getWord8
  put = putWord8 . fromIntegral . fromEnum

data SocksAtyp = SocksIPv4 | SocksIPv6 | SocksDomain deriving Show
instance Enum SocksAtyp where
  toEnum 1 = SocksIPv4
  toEnum 3 = SocksDomain
  toEnum 4 = SocksIPv6
  toEnum _ = error "Unknown atyp"
  fromEnum SocksIPv4 = 1
  fromEnum SocksDomain = 3
  fromEnum SocksIPv6 = 4
instance Binary SocksAtyp where
  get = toEnum . fromIntegral <$> getWord8
  put = putWord8 . fromIntegral . fromEnum
instance Default SocksAtyp where
  def = SocksDomain

data ResponseReply = SocksSucceeded | SocksUnreachable | SocksRefused | SocksReplyOther deriving Show
instance Enum ResponseReply where
  toEnum 0 = SocksSucceeded
  toEnum 3 = SocksUnreachable
  toEnum 5 = SocksRefused
  toEnum _ = SocksReplyOther
  fromEnum SocksSucceeded = 0
  fromEnum SocksUnreachable = 3
  fromEnum SocksRefused = 5
  fromEnum SocksReplyOther = 1
instance Binary ResponseReply where
  get = toEnum . fromIntegral <$> getWord8
  put = putWord8 . fromIntegral . fromEnum
instance Default ResponseReply where
  def = SocksSucceeded

data SocksMethod = SocksNoAuth | SocksMethodOther deriving Show
instance Enum SocksMethod where
  toEnum 0 = SocksNoAuth
  toEnum _ = SocksMethodOther
  fromEnum SocksNoAuth = 0
  fromEnum SocksMethodOther = 128
instance Binary SocksMethod where
  get = toEnum . fromIntegral <$> getWord8
  put = putWord8 . fromIntegral . fromEnum
instance Default SocksMethod where
  def = SocksNoAuth

newtype SocksSyn = SocksSyn { socksSynMethods :: [SocksMethod] } deriving Show
instance Binary SocksSyn where
  get = do
    v <- getWord8
    case v of
      5 -> SocksSyn <$> (getWord8 >>= flip replicateM get . fromIntegral)
      _ -> error "Unknown socks request"
  put = undefined -- not needed

newtype SocksAck = SocksAck { socksAckMethod :: SocksMethod } deriving Show
instance Binary SocksAck where
  get = undefined -- not needed
  put (SocksAck m) = putWord8 5 >> putWord8 (fromIntegral $ fromEnum m)

data SocksRequest = SocksRequest { requestCommand :: RequestCommand
                                 , requestAtyp :: SocksAtyp
                                 , requestAddr :: Host
                                 , requestPort :: Port } deriving Show
instance Binary SocksRequest where
  get = do
    _    <- getWord8
    cmd  <- get
    _    <- getWord8
    atyp <- get
    addr <- case atyp of
              SocksIPv4 -> (BC.pack . fold . intersperse "." . map show . B.unpack) <$> getByteString 4
              SocksIPv6 -> error "ipv6 not implemented"
              SocksDomain -> getWord8 >>= getByteString . fromIntegral
    port <- toEnum . fromEnum <$> getWord16be
    return $ SocksRequest cmd atyp addr port
  put = undefined -- not needed

data SocksResponse = SocksResponse { responseReply :: ResponseReply
                                   , responseAtyp :: SocksAtyp
                                   , responseAddr :: Host
                                   , responsePort :: Port } deriving Show
instance Binary SocksResponse where
  get = undefined -- not needed
  put (SocksResponse reply atyp addr port) = do
    putWord8 5
    put reply
    putWord8 0
    put atyp
    case atyp of
      SocksIPv4   -> error "TODO"
      SocksDomain -> (putWord8 . toEnum . B.length) addr >> putByteString addr
      SocksIPv6   -> error "ipv6 not implemented"
    putWord16be $ toEnum $ fromEnum port
instance Default SocksResponse where
  def = SocksResponse def def B.empty 0
