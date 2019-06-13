{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

------------------------------------------------------------------------------------------
-- |
-- Module: Agent.Types
--
-- Constants, configuration data types, and internal communication protocol.
--
------------------------------------------------------------------------------------------

module Agent.Types (
 -- * Types
 -- ** Config
   Config(..)
 , RunConfig(..)
 , APIConfig(..)
 , TokenConfig(..)
 , ProxyConfig(..)
 -- ** Other types
 , LogProgress(..)
 , SafeURL(..)
 , APICode(..)
 , TokenPath(..)
 , Token(..)
 , TokenSource(..)
 , PrettyConsole(..)
 -- * Interaction
 , BackChannel(..)
 , FrontChannel(..)
 , FrontMessage(..)
 , BackMessage(..)
 , UserInput(..)
 , newChannels
 , readBack
 , writeBack
 , readFront
 , writeFront
 , logO
 -- * Constants
 -- $Constants
 , pROXYADDR
 , aPIURL
 , certificate
 , tOKENFILE
 , aVersion
 , pVersion
 , buildVersion
 , buildDate
 -- * Error codes
 -- $ErrorCodes
 , errorConfiguration
 , errorInterrupt
 , errorTLS
 , errorNotRegistered
 , errorDuplicate
 , errorPermissions
 , errorNetwork
 , errorObsolete
 , errorPuzzle
 , errorJSON
 , errorInternal
 , errorUnknown
 -- * Lenses
 -- $Lenses
 , apiConfig
 , proxyConfig
 , runConfig
 , tokenConfig
 , debugConn
 , ignoreSSL
 , oneRun
 , runTutorial
 , skipAll
 , apiCode
 , apiURL
 , tokenPath
 , proxyAddr
 , proxyToken
 ) where

import           Prelude

import           Data.Version
import qualified Paths_agent            (version)
import qualified Network.SocksFusion    (pVersion)

import           Network.SocksFusion hiding (pVersion)
import           Network.HTTP.Client    (parseUrlThrow)

import           Lens.Micro.Platform    (makeLenses)
import           Language.Haskell.TH    (runIO, stringE)
import           Data.FileEmbed         (embedFile)
import           System.Process         (readProcess)
import           System.Exit
import           System.FilePath        (normalise)

import           Data.Time.Clock        (UTCTime)
import           Data.Time.Format       (formatTime, defaultTimeLocale)
import           System.Environment     (lookupEnv)

import           Control.Monad.Reader
import           Data.Aeson             (FromJSON(..), ToJSON(..), withObject, (.:), object, (.=))
import           Control.Concurrent.STM
import           Data.Default.Class

import           Data.ByteString.Char8  (ByteString)
import           Data.Text              (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Text.Read              (Read(..), readPrec_to_P, readP_to_Prec)
import           Text.ParserCombinators.ReadP (ReadP, string, count, get)
import           Data.String            (words)
import           Data.Bifunctor         (bimap)
import           Data.Semigroup         (Sum(..))

-- Orphan instances because we need Integral instance.
deriving instance Real a => Real (Sum a)
deriving instance Enum a => Enum (Sum a)
deriving instance Integral a => Integral (Sum a)

--------------------------------------- Constants ----------------------------------------

-- $Constants
-- All constants except of tOKENFILE are generated at compile time and configurated for
-- BBS project cloud.

-- | Default BBS proxy server.
pROXYADDR :: AddrPort
pROXYADDR = read C_PROXY

-- | Default BBS API server.
aPIURL :: SafeURL
aPIURL = read C_API

-- | BBS proxy certificate.
certificate :: ByteString
certificate = $(embedFile "res/rootCert.pem")

-- | Default token file.
tOKENFILE :: String
tOKENFILE = "RemoteAgent.token"

-- | Generated at compile time.
buildDate :: String
buildDate = $(runIO (readProcess "date" ["+%d %b %G"] "") >>= stringE)

-- | Generated at compile time.
buildVersion :: Maybe String
buildVersion = $(runIO (lookupEnv "DEVOPS_BUILD_VERSION") >>= \case
  Nothing -> [|Nothing|]
  Just s -> [|Just s|])

-- | Agent version.
aVersion :: Version
aVersion = Paths_agent.version

-- | Protocol version.
pVersion :: Version
pVersion = Network.SocksFusion.pVersion

--------------------------------------- Error codes --------------------------------------

-- $ErrorCodes
-- Agent can fail at some actions. These error codes will be thrown in different
-- contexts and logged for future investigation.
--

errorConfiguration :: Int
errorConfiguration = 1
errorInterrupt :: Int
errorInterrupt = 2
errorTLS :: Int
errorTLS = 3
errorNotRegistered :: Int
errorNotRegistered = 4
errorDuplicate :: Int
errorDuplicate = 5
errorPermissions :: Int
errorPermissions = 6
errorNetwork :: Int
errorNetwork = 7
errorObsolete :: Int
errorObsolete = 8
errorPuzzle :: Int
errorPuzzle = 9
errorJSON :: Int
errorJSON = 10
errorInternal :: Int
errorInternal = 11
errorUnknown :: Int
errorUnknown = 40

--------------------------------------- Types --------------------------------------------

-- | Wrapper for custom Show/Read instances.
newtype SafeURL = SafeURL { safeURL :: String }             deriving Eq
-- | Wrapper for custom Show/Read instances.
newtype APICode = AC String
-- | Wrapper for custom Show/Read instances.
newtype TokenPath = TP FilePath
-- | Wrapper for custom Read instance.
newtype LogProgress = LP Int

-- | Used for incapsulating data in TextBlocks.
class PrettyConsole a where
  render :: Integral n
         => n        -- ^ Width of the frame.
         -> a        -- ^ Data.
         -> [Text]   -- ^ Output lines.

-- | Agent identification token.
data Token = Token { tokenString :: ByteString
                   , accessTime :: UTCTime
                   , createTime :: UTCTime
                   , tokenSource :: TokenSource }

-- | Type of the token.
data TokenSource = Loaded    -- ^ Token was found in the local storage.
                 | Explicit  -- ^ Token was given explicitly.
                 | Received  -- ^ Token was just received via web API.


-- | Agent configuration.
data Config = Config { _runConfig :: RunConfig
                     , _apiConfig :: APIConfig
                     , _tokenConfig :: TokenConfig
                     , _proxyConfig :: ProxyConfig }

-- | Run configuration.
data RunConfig = RunConfig {
   _skipAll :: Bool               -- ^ Run agent without user interaction.
 , _runTutorial :: Bool           -- ^ Start tutorial mode.
 , _ignoreSSL :: Bool             -- ^ Ignore SSL errors and show them as warnings.
 , _debugConn :: Bool             -- ^ Log proxy requests.
 , _oneRun :: Bool                -- ^ Exit agent after a scan or any error.
 }

-- | API configuration.
data APIConfig = APIConfig {
   _apiURL :: SafeURL             -- ^ URL of the API server.
 , _apiCode :: APICode            -- ^ Activation code obtained on the website.
 }

-- | Local tokens configuration.
data TokenConfig = TokenConfig {
   _tokenPath :: TokenPath        -- ^ Path to the local agents file.
 }

-- | Proxying configuration.
data ProxyConfig = ProxyConfig {
   _proxyAddr :: AddrPort         -- ^ BBS Proxy address.
 , _proxyToken :: Maybe Token     -- ^ Token string used for scanning.
 }

instance Default RunConfig where
  def = RunConfig { _skipAll = False
                  , _runTutorial = False
                  , _ignoreSSL = False
                  , _debugConn = False
                  , _oneRun = False }

--------------------------------------- Interactive --------------------------------------

-- | FIFO channel for backend tasks.
newtype BackChannel = BackChannel (TQueue BackMessage)
-- | FIFO channel for frontend tasks.
newtype FrontChannel = FrontChannel (TQueue FrontMessage)

-- | Representation of backend tasks.
data BackMessage =
   ConfigChange Config  -- ^ Update configuration storen in backend memory.
 | SaveTokens [Token]   -- ^ Merge new tokens in the local token file.
 | StopProxy            -- ^ Cancel proxying.
 | RunAPI               -- ^ Reach API server and exchange an activation code for a token.
 | RunToken             -- ^ Read tokens from the local storage.
 | RunProxy             -- ^ Run proxying.

-- | Representation of frontend tasks.
data FrontMessage =
   LogMessage Text                         -- ^ Show message to the user.
 | Progress Double                         -- ^ Update progress.
 | Abort ExitCode                          -- ^ Stop interaction and throw the exit code.
 | FinishAPI (Either ExitCode Token)       -- ^ API request was finished.
 | FinishToken (Either ExitCode [Token])   -- ^ Tokens loading was finished.
 | FinishProxy (Either ExitCode ())        -- ^ Proxying task was finished.
 | UserInput UserInput                     -- ^ User causes new event.

-- | Possible user events.
data UserInput = ResizeWindow Int Int | KeyChar Char | KeyNum Int
               | KeyBackspace | KeyDelete | KeyEnter | KeyEsc
               | KeyUp | KeyDown | KeyLeft | KeyRight

-- | Init empty channels.
newChannels :: IO (BackChannel, FrontChannel)
newChannels = curry (bimap BackChannel FrontChannel) <$> atomically newTQueue
                                                     <*> atomically newTQueue

-- | Take a message from backend channel blocking.
readBack :: (MonadIO m, MonadReader (BackChannel, a) m) => m BackMessage
readBack = asks fst >>= (\(BackChannel ch) -> liftIO $ atomically $ readTQueue ch)
-- | Send a message on the backend.
writeBack :: (MonadIO m, MonadReader (a, BackChannel) m) => BackMessage-> m ()
writeBack msg = asks snd >>= (\(BackChannel ch) -> liftIO $ atomically $ writeTQueue ch msg)

-- | Take a message from frontend channel blocking.
readFront :: (MonadIO m, MonadReader (FrontChannel, a) m) => m FrontMessage
readFront = asks fst >>= (\(FrontChannel ch) -> liftIO $ atomically $ readTQueue ch)
-- | Send a message on the frontend.
writeFront :: (MonadIO m, MonadReader (a, FrontChannel) m) => FrontMessage -> m ()
writeFront msg = asks snd >>= (\(FrontChannel ch) -> liftIO $ atomically $ writeTQueue ch msg)

-- | This function is used for avoiding MonadIO in high load proxy code.
logO :: FrontChannel -> Text -> IO ()
logO (FrontChannel output) = atomically . writeTQueue output . LogMessage

--------------------------------------- Lenses -------------------------------------------

-- $Lenses
-- Autogenerated lens functions.

makeLenses ''Config
makeLenses ''RunConfig
makeLenses ''APIConfig
makeLenses ''TokenConfig
makeLenses ''ProxyConfig

--------------------------------------- Instances ----------------------------------------

instance Read LogProgress where
  readPrec = readP_to_Prec $ \d -> do
    void $ count 18 get                 -- skip timestamp
    void $ string "Scan progress: "
    p <- readPrec_to_P readPrec d :: ReadP Double
    return $ LP $ round p

instance Eq Token where
  tx == ty = tokenString tx == tokenString ty

instance Ord Token where
  tx `compare` ty = accessTime ty `compare` accessTime tx

instance ToJSON Token where
  toJSON token = object [ "token" .= decodeUtf8 (tokenString token)
                        , "atime" .= accessTime token
                        , "ctime" .= createTime token ]

instance FromJSON Token where
  parseJSON = withObject "dict" $ \v -> Token <$> (encodeUtf8 <$> v .: "token")
                                              <*>  v .: "atime"
                                              <*>  v .: "ctime"
                                              <*> pure Loaded

instance Read SafeURL where
  readsPrec _ s = do
    void $ either (fail . show) pure $ parseUrlThrow s
    pure (SafeURL s, "")

instance Show SafeURL where
  show (SafeURL s) = s

instance Read APICode where
  readsPrec _ s = pure $ case words s of
    [] -> (AC "", "")
    x:_ -> (AC x, "")

instance Show APICode where
  show (AC s) = s

instance Show TokenPath where
  show (TP s) = s

instance Read TokenPath where
  readsPrec _ s = pure (TP $ normalise s, "")

instance PrettyConsole Bool where
  render _ = pure . T.pack . show

instance PrettyConsole AddrPort where
  render _ = pure . T.pack . show

instance PrettyConsole SafeURL where
  render _ = pure . T.pack . show

instance PrettyConsole TokenPath where
  render _ = pure . T.pack . show

instance PrettyConsole APICode where
  render _ = pure . T.pack . show

instance PrettyConsole String where
  render n = render n . T.pack

instance PrettyConsole Text where
  render n = pure . T.take (fromIntegral n)

instance PrettyConsole [Text] where
  render n ts = ts >>= render n

instance PrettyConsole Token where
  render n (Token str atime ctime src)
    | n >= 95   = pure $ T.intercalate ", " total
    | otherwise = T.take (fromIntegral n) <$> total
    where
    total = ["Token: " `T.append` decodeUtf8 str, formatC, formatA]
    formatC = T.pack $ formatTime defaultTimeLocale "Created: %b %d %X" ctime
    formatA = case src of
        Loaded -> T.pack $ formatTime defaultTimeLocale "Last active: %b %d %X" atime
        Explicit -> "Explicit"
        Received -> "Just received from API"
