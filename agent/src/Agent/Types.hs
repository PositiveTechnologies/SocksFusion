{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Agent.Types (
 -- * Configs
   Config(..)
 , RunConfig(..)
 , ApiConfig(..)
 , TokenConfig(..)
 , ProxyConfig(..)
 , Token(..)
 , Changeable(..)
 , SafeURL
 , safeURL
 , basicConfig
 , advancedConfig
 -- * Interaction
 , InputChannel
 , OutputChannel
 , Output(..)
 , Input(..)
 , newInput
 , readInput
 , writeInput
 , newOutput
 , readOutput
 , writeOutput
 , logO
 -- * Constants
 , pROXYADDR
 , aPIURL
 , certificate
 , tOKENFILE
 , aVersion
 , pVersion
 , buildVersion
 , buildDate
 -- * Error codes
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
 ) where

import           Prelude

import           Data.Version
import qualified Paths_agent            (version)
import qualified Network.SocksFusion    (pVersion)

import           Network.SocksFusion hiding (pVersion)
import           Network.HTTP.Client    (parseUrlThrow)

import           Language.Haskell.TH    (runIO, stringE)
import           Data.FileEmbed         (embedFile)
import           System.Process         (readProcess)

import           Data.Time.Clock        (UTCTime)
import           Data.Time.Format       (formatTime, defaultTimeLocale)
import           System.Exit            (ExitCode)
import           System.Environment     (lookupEnv)

import           Data.Bool              (bool)
import           Control.Monad.Reader
import           Data.Aeson             (FromJSON(..), ToJSON(..), withObject, (.:), object, (.=))
import           Control.Concurrent.STM
import           Data.Default.Class


import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Text.Read              (Read(..))
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), bool)

--------------------------------------- Constants ----------------------------------------

-- | Default BBS proxy server
pROXYADDR :: AddrPort
pROXYADDR = read PROXY

-- | Default BBS Api server
aPIURL :: SafeURL
aPIURL = read API

-- | BBS proxy certificate
certificate :: ByteString
certificate = $(embedFile "res/rootCert.pem")

-- | Default token file
tOKENFILE :: String
tOKENFILE = "RemoteAgent.token"

buildDate :: String
buildDate = $(runIO (readProcess "date" ["+%d %b %G"] "") >>= stringE)

buildVersion :: Maybe String
buildVersion = $(runIO (lookupEnv "DEVOPS_BUILD_VERSION") >>= \case
  Nothing -> [|Nothing|]
  Just s -> [|Just s|])

-- | Agent version
aVersion :: Version
aVersion = Paths_agent.version

-- | Protocol version
pVersion :: Version
pVersion = Network.SocksFusion.pVersion

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

--------------------------------------- Config -------------------------------------------

-- | Helper to prevent invalid urls.

newtype SafeURL = SafeURL String                  deriving Eq

safeURL :: SafeURL -> String
safeURL (SafeURL s) = s

instance Read SafeURL where
  readsPrec _ s = do
    void $ either (fail . show) pure $ parseUrlThrow s
    pure (SafeURL s, "")

instance Show SafeURL where
  show (SafeURL s) = show s

-- | Helper for State monad
class Changeable b a | a -> b where
  change :: (a -> a) -> b -> b

data Config = Config { runConfig :: RunConfig
                     , apiConfig :: ApiConfig
                     , tokenConfig :: TokenConfig
                     , proxyConfig :: ProxyConfig }

data RunConfig = RunConfig { skipAll :: Bool
                           , runTutorial :: Bool
                           , ignoreSSL :: Bool
                           , debugConn :: Bool
                           , oneRun :: Bool }

data ApiConfig = ApiConfig { apiURL :: SafeURL
                           , apiCode :: ByteString }

data TokenConfig = TokenConfig { tokenPath :: FilePath }

data ProxyConfig = ProxyConfig { proxyAddr :: AddrPort
                               , proxyToken :: Maybe ByteString }

instance Default RunConfig where
  def = RunConfig { skipAll = False
                  , runTutorial = False
                  , ignoreSSL = False
                  , debugConn = False
                  , oneRun = False }

instance Changeable Config RunConfig where
  change f c = let x = runConfig c in c { runConfig = f x }

instance Changeable Config ApiConfig where
  change f c = let x = apiConfig c in c { apiConfig = f x }

instance Changeable Config TokenConfig where
  change f c = let x = tokenConfig c in c { tokenConfig = f x }

instance Changeable Config ProxyConfig where
  change f c = let x = proxyConfig c in c { proxyConfig = f x }

--------------------------------------- Interactive --------------------------------------

newtype InputChannel = InputChannel (TQueue Input)
newtype OutputChannel = OutputChannel (TQueue Output)

data Input = Exit ExitCode | ConfigChange Config | SaveTokens [Token]
           | RunApi | RunToken | RunProxy

data Output = ShowMessage Text
            | FailRun ExitCode
            | FinishApi Token
            | FinishToken [Token]
            | FinishProxy ExitCode

newInput :: IO InputChannel
newInput = InputChannel <$> atomically newTQueue
newOutput :: IO OutputChannel
newOutput = OutputChannel <$> atomically newTQueue

readInput :: (MonadIO m, MonadReader (InputChannel, a) m) => m Input
readInput = asks fst >>= (\(InputChannel ch) -> liftIO $ atomically $ readTQueue ch)
writeInput :: (MonadIO m, MonadReader (a, InputChannel) m) => Input -> m ()
writeInput msg = asks snd >>= (\(InputChannel ch) -> liftIO $ atomically $ writeTQueue ch msg)

readOutput :: (MonadIO m, MonadReader (OutputChannel, a) m) => m Output
readOutput = asks fst >>= (\(OutputChannel ch) -> liftIO $ atomically $ readTQueue ch)
writeOutput :: (MonadIO m, MonadReader (a, OutputChannel) m) => Output -> m ()
writeOutput msg = asks snd >>= (\(OutputChannel ch) -> liftIO $ atomically $ writeTQueue ch msg)

-- | To avoid MonadIO in high load proxy code
logO :: OutputChannel -> Text -> IO ()
logO (OutputChannel output) = atomically . writeTQueue output . ShowMessage

--------------------------------------- Tokens -------------------------------------------

data Token = Token { tokenString :: ByteString
                   , accessTime :: UTCTime
                   , createTime :: UTCTime }

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

--------------------------------------- Pretty -------------------------------------------

prettyIgnore :: RunConfig -> Doc
prettyIgnore = ("Ignore ssl errors:" <+>) . bool (pretty False) (bold $ pretty True) . ignoreSSL

prettyDebug :: RunConfig -> Doc
prettyDebug = ("Debug mode:" <+>) . bool (pretty False) (bold $ pretty True) . debugConn

prettyOnce :: RunConfig -> Doc
prettyOnce = ("Exit after scan:" <+>) . pretty . oneRun

prettyApiUrl :: ApiConfig -> Doc
prettyApiUrl = ("API server:" <+>) . (bool bold id . (== aPIURL) <*> text . safeURL) . apiURL

prettyCode :: ApiConfig -> Doc
prettyCode api = "Activation code:" <+> bold (case BC.unpack (apiCode api) of
                                                "" -> dquotes empty
                                                c -> text c)

prettyPath :: TokenConfig -> Doc
prettyPath = ("Token path:" <+>) . bold . text . tokenPath

prettyProxyUrl :: ProxyConfig -> Doc
prettyProxyUrl = ("Proxy server:" <+>) . (bool bold id . (== pROXYADDR) <*> text . show) . proxyAddr

prettyToken :: ProxyConfig -> Doc
prettyToken = ("Using token:" <+>) . bold . maybe "undefined" (text . show) . proxyToken

basicConfig :: Config -> Doc
basicConfig (Config _ api token _) =
  "Basic configuration:" <> line <> indent 4 (vsep $ map (\(n, s) -> parens (int n) <+> s)
                                                   $ zip [1..] [ prettyCode api
                                                               , prettyPath token ])

advancedConfig :: Config -> Doc
advancedConfig (Config run api token proxy) =
  "Advanced configuration:" <> line <> indent 4 (vsep $ map (\(n, s) -> parens (int n) <+> s)
                                                      $ zip [1..] [ prettyCode api
                                                                  , prettyPath token
                                                                  , prettyToken proxy
                                                                  , prettyApiUrl api
                                                                  , prettyProxyUrl proxy
                                                                  , prettyIgnore run
                                                                  , prettyDebug run
                                                                  , prettyOnce run ])

instance Pretty Config where
  pretty config =
    "Configuration:" <> line <> indent 4 (vsep [ pretty (runConfig config)
                                               , pretty (tokenConfig config)
                                               , pretty (apiConfig config)
                                               , pretty (proxyConfig config) ])

instance Pretty RunConfig where
  pretty run = "Agent configuration:" <> line <> indent 4 (vsep [ prettyIgnore run
                                                                , prettyDebug run
                                                                , prettyOnce run ])

instance Pretty TokenConfig where
  pretty token = "Token configuration: " <> line <> indent 4 (prettyPath token)

instance Pretty ApiConfig where
  pretty api = "API configuration: " <> line <> indent 4 (vsep [ prettyApiUrl api
                                                               , prettyCode api ])

instance Pretty ProxyConfig where
  pretty proxy = "Proxy configuration: " <> line <> indent 4 (vsep [ prettyProxyUrl proxy
                                                                   , prettyToken proxy ])

instance Pretty Token where
  pretty token = vsep [
      fill 20 "Token:" <+> text (BC.unpack $ tokenString token)
    , fill 20 "Created:" <+> showT (createTime token)
    , fill 20 "Last active:" <+> showT (accessTime token)
    ]
    where
    showT = text . formatTime defaultTimeLocale "%b %d %X"
  prettyList tokens = "(*)"
                  <+> align (vsep (map (\(x, y) -> parens (int x) <+> align (pretty y))
                                 $ zip [1..] tokens))
