{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import           Prelude             hiding (interact, log)
import           Agent.Types
import           Agent.Backend
import           Agent.Frontend

import           System.Directory
import           Data.Time.Clock
import           Control.Concurrent

import qualified Data.ByteString.Char8 as BC

import           Options.Applicative hiding (command, Alternative(..))
import           Control.Monad
import           Control.Monad.State.Lazy
import           Control.Monad.Reader
import           Data.Version

#if MIN_VERSION_base(4,11,0)
#else
import           Data.Monoid
#endif

--------------------------------------- Configuration ------------------------------------

parseRun :: Parser RunConfig
parseRun = do
  tutorial <- switch $ long "tutorial"
                    <> help "Run interactive tutorial"
  skip <- switch $ long "skip"
                <> short 's'
                <> help "Skip configuration menu"
  ignore <- switch $ long "insecure"
                  <> short 'i'
                  <> help "Keep going if SSL certificate cannot be verified"
  debug <- switch $ long "debug"
                 <> short 'd'
                 <> help "Show packet debugging information"
  once <- switch $ long "once"
                <> short 'o'
                <> help "Exit when scanning completes"
  pure $ RunConfig { _runTutorial = tutorial
                   , _skipAll = skip
                   , _ignoreSSL = ignore
                   , _debugConn = debug
                   , _oneRun = once }

parseAPI :: Parser APIConfig
parseAPI = do
  code <- argument auto $ value (AC "")
                       <> metavar "string"
                       <> help "Specify activation code"
  url <- option auto $ long "api"
                    <> short 'a'
                    <> showDefault
                    <> value aPIURL
                    <> metavar "URL"
                    <> help "Use indicated API server address"
  pure $ APIConfig url code

parseToken :: FilePath -> Parser TokenConfig
parseToken appdata = do
  tokenP <- option auto $ long "token-path"
                       <> showDefaultWith (\(TP x) -> x)
                       <> action "filenames"
                       <> value (TP $ appdata <> "/" <> tOKENFILE)
                       <> metavar "filepath"
                       <> help "Use indicated path to read/write token"
  pure $ TokenConfig tokenP

parseProxy :: UTCTime -> Parser ProxyConfig
parseProxy time = do
  url <- option auto $ long "proxy"
                    <> short 'p'
                    <> showDefault
                    <> value pROXYADDR
                    <> metavar "host:port"
                    <> help "Use indicated proxy server"
  token <- strOption $ long "token"
                    <> value ""
                    <> metavar "string"
                    <> help "Use indicated token"
  pure $ ProxyConfig url (if BC.null token then Nothing
                                           else Just $ Token token time time Explicit)

options :: FilePath -> UTCTime -> Parser Config
options appdata time = do
  apiC <- parseAPI
  runC <- parseRun
  tokenC <- parseToken appdata
  proxyC <- parseProxy time
  _ <- infoOption (unlines $ [ "Agent version: " <> showVersion aVersion
                             , "Protocol version: " <> showVersion pVersion ]
                          <> maybe [] (pure . ("Build version: " <>)) buildVersion
                          <> [ "Build date: " <> buildDate ])
                $ long "version"
               <> short 'v'
               <> help "Show agent version"
  pure $ Config runC apiC tokenC proxyC

parserInfo :: FilePath -> UTCTime -> ParserInfo Config
parserInfo appdata time = info (options appdata time <**> helper)
                              $ header "SocksFusion forwarding agent"
                             <> fullDesc
                             <> failureCode errorConfiguration

--------------------------------------- Functions ----------------------------------------

main :: IO ()
main = do
  config <- parserInfo <$> getAppUserDataDirectory "bbs" <*> getCurrentTime >>= execParser
  (back, front) <- newChannels
  void $ forkIO (evalStateT (runReaderT backend (back, front)) config)
  frontend config front back
