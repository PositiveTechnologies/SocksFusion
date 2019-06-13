{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide, prune #-}

------------------------------------------------------------------------------------------
-- |
-- Module Agent.Backend
--
-- Agent backend.
--
------------------------------------------------------------------------------------------

module Agent.Backend (
   backend
 ) where

import           Prelude             hiding (interact, log)
import           Agent.Types

import           Network.SocksFusion hiding (pVersion)
import           Network.Socks5

import           System.IO
import           System.IO.Error
import           System.Exit
import           System.Directory
import           System.FilePath        (takeDirectory)
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Time.Clock.TAI

import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.IORef

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

import           Lens.Micro.Platform

import           Data.Aeson
import           Data.Aeson.Types       (parse)
import           Data.Aeson.Parser      (decodeWith)
import qualified Data.Binary         as B (Binary(..))
import           Data.Binary.Get hiding (skip)
import           Data.Binary.Put

import qualified Data.IntMap.Strict  as IM
import qualified Data.Map.Strict     as M
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text              (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Text.Printf

import           Control.Monad
import           Control.Monad.State.Lazy
import           Control.Monad.Reader
import           Data.Functor
import           Data.Default.Class
import           Data.List              (nub)
import           Data.Char              (isAlphaNum)
import           Data.Bifunctor         (second)
import           Data.Tuple             (swap)

#if MIN_VERSION_base(4,11,0)
#else
import           Data.Monoid
#endif

--------------------------------------- Serialisation ------------------------------------

-- | Put arrow message in the outcoming queue.
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

type Interactive = (BackChannel, FrontChannel)
type Logger = Text -> IO ()

log :: (MonadIO m, MonadReader Interactive m) => Text -> m ()
log = writeFront . LogMessage

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

saveTokens :: (MonadIO m, MonadReader Interactive m) => [Token] -> Config -> m ()
saveTokens tokens config = do
  let (TP path) = config ^. tokenConfig.tokenPath
  loadTokens path >>= \case
    Right tokens' -> tryIO (createDirectoryIfMissing False (takeDirectory path)
                         >> BL.writeFile path (encode $ nub $ tokens <> tokens'))
                 >>= either (log . showT)
                            (const $ log $ "Tokens was saved in " <> T.pack path)
    Left e -> log $ "Cannot save tokens, " <> showT e

runAPI :: (MonadIO m, MonadReader Interactive m) => Config -> m ()
runAPI config = do
  let ignore = config ^. runConfig.ignoreSSL
  let (url, AC code) = (config ^. apiConfig.apiURL, config ^. apiConfig.apiCode)
  eBody <- liftIO $ try $ do
    errReq <- HTTP.parseUrlThrow $ safeURL url
    let req = errReq { HTTP.method = "POST"
                     , HTTP.path = "/_xhr/activate-agent"
                     , HTTP.requestHeaders = [(HTTP.hContentType, "application/x-www-form-urlencoded")]
                     , HTTP.requestBody = HTTP.RequestBodyBS ("code=" <> BC.pack code) }
    let mngr = HTTP.mkManagerSettings (HTTP.TLSSettingsSimple ignore False False) Nothing
    HTTP.responseBody <$> (HTTP.newManager mngr >>= HTTP.httpLbs req)
  case eBody of
    Left e -> logHttp e >> writeFront (FinishAPI $ Left $ ExitFailure errorNetwork)
    Right body -> case decodeWith json (parse parser) body of
      Nothing -> do
        log ("Cannot parse response: " <> T.decodeUtf8 (BL.toStrict body))
        writeFront $ FinishAPI $ Left $ ExitFailure errorNetwork
      Just token -> do
        time <- liftIO getCurrentTime
        writeFront $ FinishAPI $ Right $ Token token time time Received
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

loadTokens :: (MonadIO m, MonadReader Interactive m) => FilePath -> m (Either ExitCode [Token])
loadTokens path = tryIO (BL.readFile path) >>= \case
  Left e ->
    return $ if isDoesNotExistError e
      then Right mempty
      else Left (ExitFailure errorPermissions)
  Right content ->
    if BL.length content == 32 && BL.all isAlphaNum content
      then do
        log "Found old token file. Token will be converted in the new format."
        time <- liftIO getCurrentTime
        return $ Right [Token (BL.toStrict content) time time Loaded]
      else case eitherDecode content of
        Left{} -> return $ Left (ExitFailure errorJSON)
        Right tokens -> return $ Right tokens

runToken :: (MonadIO m, MonadReader Interactive m) => Config -> m ()
runToken config = do
  let (TP path) = config ^. tokenConfig.tokenPath
  res <- loadTokens path
  case res of
    Left ec | ec == ExitFailure errorJSON -> log "Token file is not well formatted"
            | ec == ExitFailure errorPermissions -> log "Cannot read file"
            | otherwise -> log "Unknown error while reading file"
    Right{} -> return ()
  writeFront $ FinishToken res

runProxy :: (MonadIO m, MonadReader Interactive m) => Config -> m ()
runProxy config = do
  let (ignore, debug) = (config ^. runConfig.ignoreSSL, config ^. runConfig.debugConn)
  let (addr, mToken) = (config ^. proxyConfig.proxyAddr, config ^. proxyConfig.proxyToken)
  case mToken of
    Nothing -> do
      log "No token was provided"
      liftIO $ wait 1
      writeFront $ FinishProxy $ Left $ ExitFailure errorConfiguration
    Just (Token token _ _ _) -> do
      ch <- asks snd
      res <- liftIO $ try $ proxy ch addr token ignore debug
      liftIO $ wait 1
      writeFront $ FinishProxy res

proxy :: FrontChannel -> AddrPort -> BC.ByteString -> Bool -> Bool -> IO ()
proxy front _ "" _ _ = logO front "Token cannot be empty" >> throwIO (ExitFailure errorConfiguration)
proxy (FrontChannel ch) (fh :@: fp) token ignore debug = bracket connAcquire dispose $ \h -> do
  testVersion h
  puzzle h
  bracket (agentAcquire h (client logIO (BC.unpack fh) ignore)) agentRelease $ \(command, _, _) -> runGetOrFail B.get . BL.fromStrict <$> atomically (takeTMVar command) >>= \case
    Right (_, _, KeyResponse KeyUnknown) -> do
      logIO "This agent is not registered"
      throwIO (ExitFailure errorNotRegistered)
    Right (_, _, KeyResponse KeyDuplicate) -> do
      logIO "Another agent already connected"
      throwIO (ExitFailure errorDuplicate)
    Right (_, _, KeyResponse CloudMaintenance) -> do
      logIO "PTBBS is in maintenance mode"
      throwIO (ExitFailure errorInternal)
    Right (_, _, KeyResponse KeyAccepted) -> do
      logIO "Agent is ready, you can start scan now"
      void (atomically (takeTMVar command))
    _ -> do
      logIO "Connection is broken"
      throwIO (ExitFailure errorNetwork)
  where
  logIO = logO (FrontChannel ch)
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

  agentAcquire :: Handle -> ClientParams -> IO (TMVar BC.ByteString, Context, ThreadId)
  agentAcquire h cli = do
    logIO "Performing handshake..."
    ctx <- contextNew h cli
    handshake ctx
    (quiver, command, info') <- atomically $ do
      q <- defaultQuiver
      (q,,) <$> getTMVar cOMMAND q <*> getTMVar iNFO q
    back <- newEmptyMVar
    i_tid <- forkIO $ forever (runGetOrFail B.get . BL.fromStrict <$> atomically (takeTMVar info') >>= \case
      Right (_, _, ProxyInfo msg) -> logIO $ T.decodeUtf8 msg
      Right (_, _, ScanProgress _ prog) -> atomically $ writeTQueue ch (Progress prog)
      _ -> return ())
    void $ forkFinally (contextRun quiver back ctx (addNew logIO debug quiver back fh))
                       (const $ void $ atomically $ tryPutTMVar command "error")
    logIO "Delivering token..."
    putMVar back (Arrow cOMMAND token)
    return (command, ctx, i_tid)

  agentRelease :: (TMVar BC.ByteString, Context, ThreadId) -> IO ()
  agentRelease (_, ctx, tid) = dispose ctx >> killThread tid

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
  formatLog :: Double -> Double -> T.Text
  formatLog = (T.pack .) . printf ", %.6f Connect()/%.6f HTTP"

  diffT :: AbsoluteTime -> AbsoluteTime -> Double
  diffT t1 t2 = fromIntegral (diffTimeToPicoseconds $ diffAbsoluteTime t2 t1) * 1e-12

  go t mvar (Partial k) = atomically (takeTMVar mvar) >>= go t mvar . k . Just
  go tStart mvar (Done left _ (SocksRequest _ _ addr port)) =
    try (addr .@. port) >>= \case
      -- TODO: parse exception
      Left (e :: ProtocolException) -> do
        tCon <- getTime
        debugLog (showT e <> formatLog (diffT tCon tStart) 0)
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

data Task = ProxyTask | SaveTokensTask | APITask | TokenTask          deriving Eq
type Tasks = M.Map ThreadId Task

-- | Run backend tasks in given context.
backend :: (MonadIO m, MonadState Config m, MonadReader Interactive m) => m a
backend = do
  tasks <- liftIO $ newIORef mempty
  forever $ readBack >>= \case
    ConfigChange config -> put config
    SaveTokens tokens -> addTask (saveTokens tokens, SaveTokensTask) tasks
    RunAPI -> addTask (runAPI, APITask) tasks
    RunToken -> addTask (runToken, TokenTask) tasks
    RunProxy -> addTask (runProxy, ProxyTask) tasks
    StopProxy -> killTask ProxyTask tasks

addTask :: (MonadIO m, MonadReader Interactive m, MonadState Config m)
        => (Config -> ReaderT Interactive IO (), Task) -> IORef Tasks -> m ()
addTask (run, typo) tasks = do
  config <- get
  interactive <- ask
  tid <- liftIO $ forkIO $ runReaderT (modified config) interactive
  liftIO $ atomicModifyIORef' tasks ((,()) . M.insert tid typo)
  where
  modified config = do
    run config
    tid <- liftIO myThreadId
    liftIO $ atomicModifyIORef' tasks ((,()) . M.delete tid)

-- Convert dropped keys to List type to prevent possible logic errors
killTask :: (MonadIO m) => Task -> IORef Tasks -> m ()
killTask typo tasks = liftIO $ atomicModifyIORef' tasks f >>= mapM_ killThread
  where
  f :: Tasks -> (Tasks, [ThreadId])
  f = second M.keys . swap . M.partition (== typo)
