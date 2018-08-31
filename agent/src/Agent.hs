{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

#ifdef TEST
module Agent (loadM) where
#endif

import           Prelude         hiding (log)
import qualified Paths_agent            (version)

import           Network.SocksFusion hiding (pVersion)
import qualified Network.SocksFusion    (pVersion)
import           Network.Socks5

import           Language.Haskell.TH    (runIO, stringE)
import           Data.FileEmbed         (embedFile)
import           System.IO
import           System.IO.Error
import           System.Exit            (exitSuccess, ExitCode(..), exitWith)
import           System.Process         (readProcess)
import           System.Directory       (createDirectoryIfMissing, getAppUserDataDirectory)
import           System.FilePath.Posix  ((</>))
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Time.Clock.TAI

import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.STM

import           Network.Socket         (socketToHandle)
import           Network.TLS     hiding (Version)
import           Network.TLS.Extra
import           Data.X509.CertificateStore (makeCertificateStore)
import           Data.X509.Memory       (readSignedObjectFromMemory)
import           Data.X509.Validation   (validateDefault)
import qualified Network.HTTP.Client as HTTP
import qualified Network.Connection  as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))
import           Options.Applicative hiding (command, Alternative(..))
import           System.Log.FastLogger

import           Data.Aeson             (decode, Value(..))
import qualified Data.HashMap.Strict as HM
import           Data.Binary     hiding (decode)
import           Data.Binary.Get
import           Data.Binary.Put

import qualified Data.IntMap.Strict  as IM
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString     as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Encoding  as T
import           Control.Monad
import           Data.Functor
import           Data.Default.Class
import           Data.Version
import           Text.Printf

--------------------------------------- Constants -------------------------------------------------

proxyUrl :: AddrPort
proxyUrl = read PROXY

apiUrl :: ApiURL
apiUrl = API

certificate :: B.ByteString
certificate = $(embedFile "res/rootCert.pem")

buildDate :: String
buildDate = $(runIO (readProcess "date" ["+%d %b %G"] "") >>= stringE)

version :: String
version = showVersion Paths_agent.version

pVersion :: Version
pVersion = Network.SocksFusion.pVersion

tokenFile :: String
tokenFile = "RemoteAgent.token"

errorConfiguration :: Int
errorConfiguration = 1
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
errorUnknown :: Int
errorUnknown = 40

--------------------------------------- Configuration ---------------------------------------------

type Ignore = Bool
type Once = Bool
type Debug = Bool
type ApiURL = String

data Configuration = Run AddrPort ApiURL (Maybe B.ByteString) (Maybe FilePath) Ignore Once Debug
                   | ShowBuildInfo

options :: Parser Configuration
options = do
  proxy  <- option auto (long "proxy"    <> short 'p' <> value proxyUrl <> metavar "HOST:PORT"
                     <> hidden <> help "Use custom Proxy server")
  api    <- strOption   (long "api"      <> short 'a' <> value apiUrl   <> metavar "URL"
                     <> hidden <> help "Use custom API address")
  token  <- strOption   (long "token"                <> value ""       <> metavar "STRING"
                     <> hidden <> help "Use custom token")
  tokenP <- strOption   (long "token-path"           <> value ""       <> metavar "PATH"
                     <> hidden <> help "Use custom token file")
  insec  <- switch     (long "insecure"  <> short 'i'
                     <> hidden <> help "Do not verify TLS certificate")
  once   <- switch     (long "once"      <> short 'o'
                              <> help "Exit agent after any break")
  debug  <- switch     (long "debug"     <> short 'd'
                              <> help "Print debug info")
  si     <- switch     (long "show-info" <> short 's'
                              <> help "Show agent info")
  pure $ if si then ShowBuildInfo
               else Run proxy api (if null token then Nothing else Just $ BC.pack token)
                                  (if null tokenP then Nothing else Just tokenP) insec once debug

parserInfo :: ParserInfo Configuration
parserInfo = info (options <**> helper) (header "SocksFusion forwarding agent"
                                       <> fullDesc <> failureCode errorConfiguration)

--------------------------------------- Utils -----------------------------------------------------

client :: (?log :: LogStr -> IO ()) => HostName -> Ignore -> ClientParams
client host ignore = (defaultParamsClient host "") {
   clientShared    = def { sharedCAStore = makeCertificateStore ca }
 , clientHooks     = def { onServerCertificate = validate }
 , clientSupported = def { supportedVersions = [TLS12]
                         , supportedCiphers  = ciphersuite_default
                         , supportedSession  = False }}
  where
  ca = readSignedObjectFromMemory certificate
  validate cs vc sid cc = do
    errs <- validateDefault cs vc sid cc
    if ignore then mapM_ (\e -> ?log ("Warning: " <> toLogStr (show e))) errs $> mempty
              else mapM  (\e -> ?log ("Error: " <> toLogStr (show e)) $> e) errs

putArrow :: Binary a => MVar Arrow -> Int -> a -> IO ()
putArrow mvar ah = putMVar mvar . Arrow ah . BL.toStrict . runPut . put

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

--------------------------------------- Main ------------------------------------------------------

loadLocal :: FilePath -> IO (Maybe (B.ByteString, FilePath))
loadLocal path = catch (pure . (,path) <$> B.readFile path) (\(_ :: SomeException) -> return mempty)
loadM :: (Foldable t, Monad m) => (a -> m (Maybe b)) -> t a -> m (Maybe b)
loadM f = foldM (\case { Nothing -> f; x -> const $ return x }) Nothing
loadRemote :: (?log :: LogStr -> IO ()) => ApiURL -> B.ByteString -> Ignore -> IO B.ByteString
loadRemote url code ignore = do
  errReq <- HTTP.parseUrlThrow url
  let req = errReq { HTTP.method = "POST"
                   , HTTP.path = "/_xhr/activate-agent"
                   , HTTP.requestBody = HTTP.RequestBodyBS ("code=" <> code) }
  let manager = HTTP.mkManagerSettings (HTTP.TLSSettingsSimple ignore False False) Nothing
  resp <- HTTP.newManager manager >>= HTTP.httpLbs req
  let body = HTTP.responseBody resp
  let decodeError = ?log ("Cannot parse response: " <> toLogStr body)
                 >> exitWith (ExitFailure errorNetwork)
  case decode body of
    Just (Object dict) -> case HM.lookup "token" dict of
      Just (String token) -> return $ T.encodeUtf8 token
      _                   -> decodeError
    _                  -> decodeError

getToken :: (?log :: LogStr -> IO ())
         => TimedFastLogger -> Maybe FilePath -> ApiURL -> Ignore -> IO B.ByteString
getToken tlog mpath url ignore = (do
  appdata <- getAppUserDataDirectory "bbs"
  maybe (loadM loadLocal $ map (</> tokenFile) [appdata, "."]) loadLocal mpath >>= \case
    Just (token, loc) -> do
      ?log ("Found saved token in " <> toLogStr loc)
      return token
    Nothing    -> do
      ?log "Token file was not found, connecting to API server"
      tlog (const "Enter activation code: ")
      hFlush stdout
      code <- B.getLine
      token <- loadRemote url code ignore
      try (saveAs appdata token `catch` (\(_ :: SomeException) -> saveAs "." token)) >>= \case
        Right path               -> ?log ("Token was saved in " <> toLogStr path) $> token
        Left (e :: SomeException) -> ?log "Cannot save token here nor in home directory"
                                >> ?log (toLogStr $ show e)
                                >> exitWith (ExitFailure errorPermissions)) `catches` [
    Handler (\(e :: ExitCode) -> throw e)
    -- TODO: pretty print of socket error
  , Handler (\case
                HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException s _) -> do
                  ?log ("Fail to connect API server: " <> toLogStr (show $ HTTP.responseStatus s))
                  exitWith (ExitFailure errorNetwork)
                HTTP.HttpExceptionRequest _ e -> do
                  ?log ("Fail to connect API server: " <> toLogStr (show e))
                  exitWith (ExitFailure errorNetwork)
                e -> do
                  ?log ("Fail to connect API server: " <> toLogStr (show e))
                  exitWith (ExitFailure errorNetwork)) ]
  where
  saveAs path token = do
    createDirectoryIfMissing False path
    B.writeFile (path </> tokenFile) token
    return $ path </> tokenFile

run :: (?log :: LogStr -> IO ()) => AddrPort -> B.ByteString -> ClientParams -> Once -> Debug -> IO ()
run (fh :@: fp) token cli once debug = forever (bracket connAcquire dispose $ \h -> do
  testVersion h
  puzzle h
  bracket (ctxAcquire h) dispose $ \ctx -> do
    (quiver, command) <- atomically $ do
      q <- defaultQuiver
      (q,) <$> getTMVar cOMMAND q
    back <- newEmptyMVar
    tid <- forkFinally (contextRun quiver back ctx (addNew ?log debug quiver back fh)
                                                  (?log . toLogStr))
                      (const $ void $ atomically $ tryPutTMVar command "error")

    ?log "Delivering token..."
    putMVar back (Arrow cOMMAND token)
    atomically (takeTMVar command) >>= \case
      "unknown" -> do
        ?log "This agent is not registered"
        when once $ exitWith (ExitFailure errorNotRegistered)
      "duplicate" -> do
        ?log "Another agent already connected"
        when once $ exitWith (ExitFailure errorDuplicate)
      "accepted" -> do
        ?log "Scan started"
        void $ atomically $ takeTMVar command
        ?log "Scan finished"
        when once exitSuccess
      _ -> ?log "Connection was broken"
    killThread tid
    wait 5) `catches` [
      Handler (\(e :: AsyncException) -> ?log (toLogStr $ show e) >> exitWith (ExitFailure 2))
    , Handler (\(e :: TLSException)   -> ?log (toLogStr $ show e) >> exitWith (ExitFailure errorTLS))
    , Handler (\(e :: ExitCode)       -> throw e)
    , Handler (\(e :: SomeException)  -> ?log (toLogStr $ show e) >> exitWith (ExitFailure errorUnknown)) ]
  where
  testVersion :: (?log :: LogStr -> IO ()) => Handle -> IO ()
  testVersion h = do
    ?log "Sending version..."
    msgWrite h $ runPut $ put pVersion
    msgRead h >>= \case
      "matched" -> return ()
      _ -> do
        ?log "This agent is outdated, download a new version from the official website"
        exitWith (ExitFailure errorObsolete)
  puzzle :: (?log :: LogStr -> IO ()) => Handle -> IO ()
  puzzle h = do
    test <- BL.toStrict <$> msgRead h
    answer <- BL.fromStrict <$> solvePuzzle test
    msgWrite h answer
    msgRead h >>= \case
      "solved" -> return ()
      _ -> do
        ?log "Handshake error"
        exitWith (ExitFailure errorPuzzle)
  connAcquire :: (?log :: LogStr -> IO ()) => IO Handle
  connAcquire = do
    ?log "Connecting to server..."
    fh .@. fp >>= (`socketToHandle` ReadWriteMode)

  ctxAcquire :: (?log :: LogStr -> IO ()) => Handle -> IO Context
  ctxAcquire h = do
    ?log "Performing handshake..."
    ctx <- contextNew h cli
    handshake ctx
    return ctx

-- TODO: add tests for this function
addNew :: (LogStr -> IO ()) -> Debug -> Quiver -> MVar Arrow -> B.ByteString -> Arrow -> Int -> IO Int
addNew log debug quiver back fh (Arrow ah sh) top
  | top >= ah = do
    debugLog "race condition caught"
    putArrow back ah def { responseReply = SocksRefused, responseAddr = fh, responsePort = 0 }
    return top
  | B.null sh = return ah
  | otherwise = do
    -- call fork later or two `addNew` can be called with the same arrowhead
    -- we're ignoring port here, fix it if pycurl will row
    mvar <- atomically newEmptyTMVar
    atomically $ modifyTVar quiver (IM.insert ah mvar)
    tStart <- getTime
    void $ forkFinally (go tStart mvar $ runGetIncremental get `pushChunk` sh)
                       (either (log . toLogStr . show)
                               (const (atomically $ modifyTVar quiver (IM.delete ah))))
    return ah
  where
  getTime = if debug then systemToTAITime <$> getSystemTime else return taiEpoch
  debugLog x = when debug $ log (toLogStr (show ah) <> ": " <> x)
  formatLog t1 t2 = toLogStr (printf ", %.6f Connect()/%.6f HTTP" t1 t2 :: String)
  diffT t1 t2 = fromIntegral (diffTimeToPicoseconds $ diffAbsoluteTime t2 t1) * 1e-12 :: Double
  go _ _ (Fail s _ e) = do
    putArrow back ah def { responseReply = SocksRefused, responseAddr = fh, responsePort = 0 }
    log (toLogStr (show ah) <> ": unexpected error, please report to developers: " <> toLogStr e)
    log (toLogStr (show ah) <> ": " <> toLogStr (B.take 30 s))
  go t mvar (Partial k) =
    atomically (takeTMVar mvar) >>= go t mvar . k . Just
  go tStart mvar (Done left _ (SocksRequest _ _ addr port)) =
    try (addr .@. port >>= flip socketToHandle ReadWriteMode) >>= \case
      Left (e :: ProtocolException) -> do
        tCon <- getTime
        debugLog (toLogStr (show e) <> formatLog (diffT tCon tStart) (0 :: Int))
        putArrow back ah def { responseReply = SocksRefused, responseAddr = fh, responsePort = 0 }
      Right p -> do
        tCon <- getTime
        unless (B.null left) (atomically (putTMVar mvar left))
        putArrow back ah def { responseAddr = fh, responsePort = 0 }
        arrowsPair p ah back mvar
        dispose p
        tFin <- getTime
        debugLog ("Request performed" <> formatLog (diffT tStart tCon) (diffT tCon tFin))

main :: IO ()
main = hSetBuffering stdout LineBuffering >> execParser parserInfo >>= \case
  ShowBuildInfo -> putDoc $ vsep [ text ("Version: " <> version)
                                , text ("Protocol version: " <> showVersion pVersion)
                                , text ("Build date: " <> buildDate) ]
  Run target@(fh :@: _) api mToken mPath ignore once debug -> do
    (tlog, clean) <- newTimeCache "%b %d %X - " >>= flip newTimedFastLogger (LogStdout defaultBufSize)
    let ?log = \x -> tlog (\t -> toLogStr t <> x <> "\n") in handleExit clean $ do
      ?log "Start agent"
      token <- case mToken of
        Just token -> return token
        Nothing    -> getToken tlog mPath api ignore
      let cli = client (BC.unpack fh) ignore
      run target token cli once debug
  where
  handleExit :: (?log :: LogStr -> IO ()) => IO () -> IO () -> IO ()
  handleExit clean = handle $ \(e :: ExitCode) -> do
    ?log "Exit in 5 seconds"
    ?log (toLogStr (show e))
    clean
    wait 5
    exitWith e
