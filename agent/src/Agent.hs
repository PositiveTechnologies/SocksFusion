{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

#ifdef TEST
module Agent () where
#endif

import           Prelude             hiding (interact, log)
import           Agent.Types
import           Agent.Console
--import           Agent.Graphics

import           Network.SocksFusion hiding (pVersion)
import           Network.Socks5

import           System.IO           hiding (interact)
import           System.IO.Error
import           System.Exit
import           System.Directory
import           System.FilePath            (takeDirectory)
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Time.Clock.TAI

import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.STM

import           Network.TLS
import           Network.TLS.Extra
import           Data.X509.CertificateStore (makeCertificateStore)
import           Data.X509.Memory       (readSignedObjectFromMemory)
import           Data.X509.Validation   (validateDefault)
import qualified Network.HTTP.Client as HTTP
import qualified Network.Connection  as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.Header as HTTP

import           Options.Applicative hiding (command, Alternative(..))

import           Data.Aeson
import           Data.Aeson.Types       (parse)
import           Data.Aeson.Parser      (decodeWith)
import qualified Data.Binary         as B (Binary(..))
import           Data.Binary.Get hiding (skip)
import           Data.Binary.Put

import qualified Data.IntMap.Strict  as IM
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text              (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Text.Printf

import           Control.Monad
import           Control.Arrow          ((&&&), (>>>))
import           Control.Monad.State.Lazy
import           Control.Monad.Reader
import           Data.Functor
import           Data.Default.Class
import           Data.Version
import           Data.Monoid
import           Data.List              (nub)
import           Data.Char              (isAlphaNum)

--------------------------------------- Configuration ------------------------------------

parseRun :: Parser RunConfig
parseRun = do
  tutorial <- switch $ long "tutorial"
                    <> help "Run interactive tutorial"
  skip <- switch $ long "skip"
                <> short 's'
                <> help "Use default configuration"
  ignore <- switch $ long "insecure"
                  <> short 'i'
                  <> help "Keep going if SSL certificate cannot be verified"
  debug <- switch $ long "debug"
                 <> short 'd'
                 <> help "Show packet debugging information"
  once <- switch $ long "once"
                <> short 'o'
                <> help "Exit when scanning completes"
  pure $ RunConfig { runTutorial = tutorial
                   , skipAll = skip
                   , ignoreSSL = ignore
                   , debugConn = debug
                   , oneRun = once }

parseApi :: Parser ApiConfig
parseApi = do
  code <- strArgument $ value ""
                     <> metavar "string"
                     <> help "Specify activation code"
  url <- option auto $ long "api"
                    <> short 'a'
                    <> showDefault
                    <> value aPIURL
                    <> metavar "URL"
                    <> help "Use indicated API server address"
  pure $ ApiConfig url code

parseToken :: FilePath -> Parser TokenConfig
parseToken appdata = do
  tokenP <- strOption $ long "token-path"
                     <> showDefaultWith id
                     <> action "filenames"
                     <> value (appdata <> "/" <> tOKENFILE)
                     <> metavar "filepath"
                     <> help "Use indicated path to read/write token"
  pure $ TokenConfig tokenP

parseProxy :: Parser ProxyConfig
parseProxy = do
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
  pure $ ProxyConfig url (if BC.null token then Nothing else Just token)

options :: FilePath -> Parser Config
options appdata = do
  apiC <- parseApi
  runC <- parseRun
  tokenC <- parseToken appdata
  proxyC <- parseProxy
  _ <- infoOption (unlines $ [ "Agent version: " <> showVersion aVersion
                             , "Protocol version: " <> showVersion pVersion ]
                          <> maybe [] (pure . ("Build version: " <>)) buildVersion
                          <> [ "Build date: " <> buildDate ])
                $ long "version"
               <> short 'v'
               <> help "Show agent version"
  pure $ Config runC apiC tokenC proxyC

parserInfo :: FilePath -> ParserInfo Config
parserInfo appdata = info (options appdata <**> helper)
                        $ header "SocksFusion forwarding agent"
                       <> fullDesc
                       <> failureCode errorConfiguration

--------------------------------------- Serialisation ------------------------------------

putArrow :: B.Binary a => MVar Arrow -> Int -> a -> IO ()
putArrow mvar ah = putMVar mvar . Arrow ah . BL.toStrict . runPut . B.put

-- | Deserialise bytestring from the handle with the format:
--
-- > +----------------------------+------------+
-- > | 4 bit little-endian length | bytestring |
-- > +----------------------------+------------+
msgRead :: Handle -> IO BL.ByteString
msgRead h = go "" 4 >>= go "" . fromIntegral . runGet getWord32le
  where
  go :: BL.ByteString -> Int -> IO BL.ByteString
  go initial 0 = return initial
  go initial len = do
    msg <- BL.hGet h len
    if BL.length msg == 0
      then throwIO $ mkIOError eofErrorType "" (Just h) Nothing
      else go (initial <> msg) (len - fromIntegral (BL.length msg))

-- | Serialise bytestring into the handle with the format:
--
-- > +----------------------------+------------+
-- > | 4 bit little-endian length | bytestring |
-- > +----------------------------+------------+
msgWrite :: Handle -> BL.ByteString -> IO ()
msgWrite h msg = BL.hPut h $ runPut $ putWord32le (fromIntegral $ BL.length msg)
                                   >> putLazyByteString msg

--------------------------------------- Utils --------------------------------------------

tryIO :: (MonadIO m) => IO a -> m (Either IOException a)
tryIO = liftIO . try

showT :: Show s => s -> Text
showT = T.pack . show

type Interactive = (InputChannel, OutputChannel)
type Logger = Text -> IO ()

log :: (MonadIO m, MonadReader Interactive m) => Text -> m ()
log = writeOutput . ShowMessage

--------------------------------------- Functions ----------------------------------------

client :: Logger -> HostName -> Bool -> ClientParams
client logIO host ignore = (defaultParamsClient host "") {
   clientShared    = def { sharedCAStore = makeCertificateStore ca }
 , clientHooks     = def { onServerCertificate = validate }
 , clientSupported = def { supportedVersions = [TLS12]
                         , supportedCiphers  = ciphersuite_default
                         , supportedSession  = False }}
  where
  ca = readSignedObjectFromMemory certificate
  validate cs vc sid cc = do
    errs <- validateDefault cs vc sid cc
    if ignore then mapM_ (\e -> logIO ("Warning: " <> showT e)) errs $> mempty
              else mapM  (\e -> logIO ("Error: " <> showT e) $> e) errs

saveTokens :: (MonadIO m, MonadState Config m, MonadReader Interactive m)
           => [Token] -> m ()
saveTokens tokens = do
  path <- gets (tokenConfig >>> tokenPath)
  loadTokens >>= \case
    Right tokens' -> tryIO (createDirectoryIfMissing False (takeDirectory path)
                         >> BL.writeFile path (encode $ nub $ tokens <> tokens'))
                 >>= either (log . showT)
                            (const $ log $ "Tokens was saved in " <> T.pack path)
    Left e -> log $ "Cannot save tokens, " <> showT e

runApi :: (MonadIO m, MonadState Config m, MonadReader Interactive m) => m ()
runApi = do
  ignore <- gets (runConfig >>> ignoreSSL)
  (url, code) <- gets (apiConfig >>> apiURL &&& apiCode)
  eBody <- liftIO $ try $ do
    errReq <- HTTP.parseUrlThrow $ safeURL url
    let req = errReq { HTTP.method = "POST"
                     , HTTP.path = "/_xhr/activate-agent"
                     , HTTP.requestHeaders = [(HTTP.hContentType, "application/x-www-form-urlencoded")]
                     , HTTP.requestBody = HTTP.RequestBodyBS ("code=" <> code) }
    let mngr = HTTP.mkManagerSettings (HTTP.TLSSettingsSimple ignore False False) Nothing
    HTTP.responseBody <$> (HTTP.newManager mngr >>= HTTP.httpLbs req)
  case eBody of
    Left e -> logHttp e >> writeOutput (FailRun $ ExitFailure errorNetwork)
    Right body -> case decodeWith json (parse parser) body of
      Nothing -> do
        log ("Cannot parse response: " <> T.decodeUtf8 (BL.toStrict body))
        writeOutput $ FailRun $ ExitFailure errorNetwork
      Just token -> liftIO getCurrentTime
                >>= writeOutput . FinishApi . join (Token token)
  where
  parser = withObject "dict" $ (.: "token") >=> withText "string" (pure . T.encodeUtf8)
  logHttp (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException s _)) = do
    let HTTP.Status cd msg = HTTP.responseStatus s
    log $ "Fail to connect API server: " <> case cd of
      404 -> "URL wasn't found"
      418 -> "Activation code is invalid"
      _   -> showT cd <> " " <> T.decodeUtf8 msg
  logHttp (HTTP.HttpExceptionRequest _ (HTTP.ConnectionFailure _)) =
    log "Fail to connect API server: target is unreachable"
  logHttp (HTTP.HttpExceptionRequest _ err) =
    log $ "Fail to connect API server: " <> showT err
  logHttp (HTTP.InvalidUrlException _ err) =
    log $ "Fail to connect API server: " <> showT err

loadTokens :: (MonadIO m, MonadReader Interactive m, MonadState Config m) => m (Either ExitCode [Token])
loadTokens = do
  path <- gets (tokenConfig >>> tokenPath)
  tryIO (BL.readFile path) >>= \case
    Left e ->
      return $ if isDoesNotExistError e
        then Right mempty
        else Left (ExitFailure errorPermissions)
    Right content ->
      if BL.length content == 32 && BL.all isAlphaNum content
        then do
          log "Found old token file. Token will be converted in the new format."
          time <- liftIO getCurrentTime
          return $ Right [Token (BL.toStrict content) time time]
        else case eitherDecode content of
          Left{} -> return $ Left (ExitFailure errorJSON)
          Right tokens -> return $ Right tokens

runToken :: (MonadIO m, MonadReader Interactive m, MonadState Config m) => m ()
runToken = do
  res <- loadTokens
  case res of
    Left ec | ec == ExitFailure errorJSON -> log "Token file is not well formatted"
            | ec == ExitFailure errorPermissions -> log "Cannot read file"
            | otherwise -> log "Unknown error while reading file"
    Right{} -> return ()
  writeOutput $ either FailRun FinishToken res

runProxy :: (MonadIO m, MonadReader Interactive m, MonadState Config m) => m ()
runProxy = do
  (ignore, debug) <- gets (runConfig >>> ignoreSSL &&& debugConn)
  (addr, mToken) <- gets (proxyConfig >>> proxyAddr &&& proxyToken)
  case mToken of
    Nothing -> do
      log "No token was provided"
      writeOutput $ FailRun (ExitFailure errorConfiguration)
    Just token -> do
      logIO <- logO <$> asks snd
      res <- liftIO (try $ run logIO addr token ignore debug)
      writeOutput $ FinishProxy $ either id id res

run :: Logger -> AddrPort -> BC.ByteString -> Bool -> Bool -> IO a
run logIO _ "" _ _ = logIO "Token cannot be empty" >> throwIO (ExitFailure errorConfiguration)
run logIO (fh :@: fp) token ignore debug = bracket connAcquire dispose $ \h -> do
  testVersion h
  puzzle h
  bracket (ctxAcquire h (client logIO (BC.unpack fh) ignore)) dispose $ \ctx -> do
    (quiver, command) <- atomically $ do
      q <- defaultQuiver
      (q,) <$> getTMVar cOMMAND q
    back <- newEmptyMVar
    void $ forkFinally (contextRun quiver back ctx (addNew logIO debug quiver back fh)
                                                   (logIO . T.decodeUtf8))
                       (const $ void $ atomically $ tryPutTMVar command "error")

    logIO "Delivering token..."
    putMVar back (Arrow cOMMAND token)
    atomically (takeTMVar command) >>= \case
      "unknown" -> do
        logIO "This agent is not registered"
        throwIO (ExitFailure errorNotRegistered)
      "duplicate" -> do
        logIO "Another agent already connected"
        throwIO (ExitFailure errorDuplicate)
      "maintenance" -> do
        logIO "PTBBS is in maintenance mode"
        throwIO (ExitFailure errorInternal)
      "accepted" -> do
        logIO "Agent is ready, you can start scan now"
        void (atomically (takeTMVar command))
        logIO "Scan finished"
        throwIO ExitSuccess
      _ -> do
        logIO "Connection is broken"
        throwIO (ExitFailure errorNetwork)
  where
  testVersion :: Handle -> IO ()
  testVersion h = do
    logIO "Sending version..."
    msgWrite h $ runPut $ B.put pVersion
    timeout 60 (msgRead h) >>= \case
      Nothing -> logIO "Timeout error" >> throwIO (ExitFailure errorNetwork)
      Just "matched" -> return ()
      Just{} -> do
        logIO "This agent is outdated, download a new version from the official website"
        throwIO (ExitFailure errorObsolete)

  puzzle :: Handle -> IO ()
  puzzle h = do
    test <- BL.toStrict <$> msgRead h
    answer <- BL.fromStrict <$> solvePuzzle test
    msgWrite h answer
    timeout 60 (msgRead h) >>= \case
      Nothing -> logIO "Timeout error" >> throwIO (ExitFailure errorNetwork)
      Just "solved" -> return ()
      Just{} -> do
        logIO "Handshake error"
        throwIO (ExitFailure errorPuzzle)

  connAcquire :: IO Handle
  connAcquire = go 3
    where
    go :: Int -> IO Handle
    go 0 = throwIO (ExitFailure errorNetwork)
    go n = do
      logIO "Connecting to server..."
      catch (fh .@. fp) (\(e :: SomeException) -> logIO (showT e) >> wait 1 >> go (n - 1))

  ctxAcquire :: Handle -> ClientParams -> IO Context
  ctxAcquire h cli = do
    logIO "Performing handshake..."
    ctx <- contextNew h cli
    handshake ctx
    return ctx

addNew :: Logger -> Bool -> Quiver -> MVar Arrow -> BC.ByteString -> Arrow -> Int -> IO Int
addNew logIO debug quiver back fh (Arrow ah sh) top
  | top >= ah = do
    putArrow back ah def { responseReply = SocksRefused, responseAddr = fh }
    return top
  | BC.null sh = return ah
  | otherwise = do
    mvar <- atomically newEmptyTMVar
    atomically $ modifyTVar quiver (IM.insert ah mvar)
    tStart <- getTime
    void $ forkFinally (go tStart mvar $ runGetIncremental B.get `pushChunk` sh)
                       (either (logIO . showT)
                               (const (atomically $ modifyTVar quiver (IM.delete ah))))
    return ah
  where
  getTime = if debug then systemToTAITime <$> getSystemTime else return taiEpoch
  debugLog x = when debug $ logIO (showT ah <> ": " <> x)
  formatLog t1 t2 = T.pack (printf ", %.6f Connect()/%.6f HTTP" t1 t2 :: String)

  diffT :: AbsoluteTime -> AbsoluteTime -> Double
  diffT t1 t2 = fromIntegral (diffTimeToPicoseconds $ diffAbsoluteTime t2 t1) * 1e-12

  go t mvar (Partial k) = atomically (takeTMVar mvar) >>= go t mvar . k . Just
  go tStart mvar (Done left _ (SocksRequest _ _ addr port)) =
    try (addr .@. port) >>= \case
      -- TODO: parse exception
      Left (e :: ProtocolException) -> do
        tCon <- getTime
        debugLog (showT e <> formatLog (diffT tCon tStart) (0 :: Int))
        putArrow back ah def { responseReply = SocksRefused, responseAddr = fh }
      Right h -> do
        tCon <- getTime
        unless (BC.null left) (atomically (putTMVar mvar left))
        putArrow back ah def { responseAddr = fh }
        arrowsPair h ah back mvar
        dispose h
        tFin <- getTime
        debugLog ("Request performed" <> formatLog (diffT tStart tCon) (diffT tCon tFin))
  go _ _ (Fail s _ e) = do
    putArrow back ah def { responseReply = SocksRefused, responseAddr = fh }
    logIO (showT ah <> ": unexpected error, please report to the developers: " <> T.pack e)
    logIO (showT ah <> ": " <> showT (BC.take 30 s))

--------------------------------------- Main ---------------------------------------------

main :: IO ()
main = do
  config <- getAppUserDataDirectory "bbs" >>= execParser . parserInfo
  input <- newInput
  output <- newOutput
  self <- myThreadId
  i_tid <- forkFinally (frontend config input output)
                       (either (void . tryIO . throwTo self) pure)
  res <- catches (evalStateT (runReaderT interact (input, output)) config)
                 (handlers $ logO output)
  throwTo i_tid res
  unless (res == ExitFailure errorInterrupt) $ wait 4
  wait 1

handlers :: Logger -> [Handler ExitCode]
handlers logIO = [
   Handler (\(e :: ExitCode) -> pure e)
 , Handler (\(e :: AsyncException) -> logIO (showT e) $> ExitFailure errorInterrupt)
 , Handler (\(e :: TLSException) -> logIO (showT e) $> ExitFailure errorTLS)
 , Handler (\(e :: SomeException) -> logIO (showT e)  $> ExitFailure errorUnknown)
 ]

interact :: (MonadIO m, MonadState Config m, MonadReader Interactive m) => m a
interact = readInput >>= \case
  Exit code -> liftIO $ throwIO code
  ConfigChange config -> put config >> interact
  SaveTokens tokens -> saveTokens tokens >> interact
  RunApi -> runApi >> interact
  RunToken -> runToken >> interact
  RunProxy -> runProxy >> interact
