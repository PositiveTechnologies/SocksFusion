{-# LANGUAGE PostfixOperators, TemplateHaskell, OverloadedStrings, ScopedTypeVariables, CPP #-}

import           Prelude             hiding (log)
import           Network.SocksFusion
import           Network.Socks5
import           Config

import           Network.Socket             (withSocketsDo)

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           Data.Default.Class         (def)

import           Control.Concurrent
import           Control.Concurrent.STM

import           Network.TLS
import           Network.TLS.Extra
import           Data.X509.CertificateStore (makeCertificateStore)
import           Data.X509.Memory           (readSignedObjectFromMemory)

import           Language.Haskell.TH        (Exp(..), Lit(..), runIO)
import           System.Process             (readProcess)
import           System.IO
import           System.Exit                (exitWith, ExitCode(..))
#ifdef CMD
import           System.Environment
#endif
import           Control.Monad              (forever, void, unless)
import qualified Control.Exception as X

client :: Maybe (SessionID, SessionData) -> ClientParams
client sess = ClientParams { clientUseMaxFragmentLength = Just MaxFragment4096
                           , clientServerIdentification = (\(_, fh) -> (B.unpack fh, "")) proxy
                           , clientUseServerNameIndication = True
                           , clientWantSessionResume = sess
                           , clientShared = def { sharedCAStore = makeCertificateStore ca }
                           , clientHooks = def
                           , clientSupported = def { supportedVersions = [TLS12]
                                                   , supportedCiphers = [cipher_AES256_SHA256]
                                                   , supportedSession = False
                                                   , supportedEmptyPacket = True
                                                   , supportedSecureRenegotiation = True }
                           , clientDebug = def }
  where ca = readSignedObjectFromMemory certificate

run :: Task -> IO ()
run ProxyTask{} = error "ProxyTask here?"
run (AgentTask (fp, fh) key) =
  forever . tryRun $ do
    log "Connecting to server..."
    target <- fh .@. fp
    client Nothing ~@ target `X.bracketOnError` dispose $ \(SPeer ctx) -> do
      handshake ctx
      log "Connection established"
      sendArrow ctx (Arrow 0 key)
      res <- runGetOrFail get . BL.fromStrict <$> recvData ctx
      case res of
        Left{} -> log "Connection is broken"
        Right (_, _, Arrow _ sh) -> case sh of
          "1" -> do
            log "This agent is not registered"
            dispose ctx
            wait 5
            exitWith (ExitFailure 1)
          "2" -> do
            log "Another agent already connected"
            dispose ctx
            wait 5
          _   -> do
            log "Scan is started"
            (SPeer new_ctx) <- client Nothing ~@ target
            core new_ctx key fh (read $ B.unpack sh)
            log "Finish session"
            wait 1

core :: Context -> Key -> Host -> Port -> IO ()
core ctx key fh fp = do
  handshake ctx
  sendArrow ctx (Arrow 0 key)
  back <- newEmptyMVar
  m_heartbeat <- newEmptyMVar
  quiver <- atomically $ newTVar $ M.singleton 0 m_heartbeat
  _ <- forkIO $ heartbeat m_heartbeat back
  let notFound = addNew quiver back
  contextRun quiver back ctx notFound
  dispose ctx
  emptyQuiver quiver
    where
    addNew quiver back (Arrow ah sh) = unless (B.null sh) $ do
      -- do forkIO later or two `addNew` could be called with the same arrowhead
      mvar <- newEmptyMVar
      atomically $ readTVar quiver >>= writeTVar quiver . M.insert ah mvar
      void $ forkFinally (go mvar $ runGetIncremental get `pushChunk` sh)
                         (const $ atomically (readTVar quiver >>= writeTVar quiver . M.delete ah))
        where
        go _ Fail{} =
          putMVar back (Arrow ah $! BL.toStrict $ runPut $ put def { responseReply = SocksRefused
                                                                   , responseAddr = fh
                                                                   , responsePort = fp })
        go mvar (Partial k) = takeMVar mvar >>= go mvar . k . Just
        go mvar (Done left _ (SocksRequest _ _ addr port)) = do
          pp <- X.try (addr ! port) :: IO (Either ProtocolException Peer)
          case pp of
            Left x -> log (show x) >> putMVar back (Arrow ah $! BL.toStrict
                                                  $ runPut $ put def { responseReply = SocksRefused
                                                                     , responseAddr = fh
                                                                     , responsePort = fp })
            Right p -> unless (B.null left) (putMVar mvar left)
                    >> putMVar back (Arrow ah $! BL.toStrict
                                              $ runPut $ put def { responseAddr = fh
                                                                 , responsePort = fp })
                    >> arrowsPair p ah back mvar
                    >> dispose p

--------------------------------------------------------------------------------------------------MAIN

main :: IO ()
main = withSocketsDo $! tryWith (\(e :: X.SomeException) -> log $ show e) $! do
  hSetBuffering stdout LineBuffering
  putStrLn ("Build date: " ++ $(LitE . StringL <$> runIO (readProcess "bash" ["-c", "date +\"%d %b %G\""] "")))
#ifdef CMD
  args <- getArgs
  let task = (\[fh, fp, key] -> AgentTask (read fp, B.pack fh) (B.pack key)) $ take 3 args
#else
  let task = AgentTask proxy token
#endif
  run task
