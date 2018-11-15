{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Console (
   frontend
#ifdef TEST
 , dialog
 , logPretty
#endif
 ) where

import           Prelude         hiding (log, interact)
import           Agent.Types
import           Network.SocksFusion    (timeout, wait)

import           System.IO              (stdout, BufferMode(LineBuffering), hSetBuffering)
import           System.Log.FastLogger
import           Control.Exception      (catch, bracket, throwIO)
import           Data.Time.Clock        (getCurrentTime)
import           System.Environment     (getArgs)
import           System.Exit

import           Data.Text              (Text)
import qualified Data.Text.IO          as T
import qualified Data.Text             as T
import qualified Data.ByteString.Char8 as BC

import qualified Data.Map.Lazy         as M

import           Control.Monad.State.Lazy
import           Control.Monad.Reader
import           Control.Arrow          ((>>>), (&&&))
import           Data.List              (sort, intercalate)
import           Data.Monoid
import           Data.Maybe

import           Text.Read              (readMaybe)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

--------------------------------------- Utils --------------------------------------------

type Interactive = (OutputChannel, InputChannel)

showT :: Show s => s -> Text
showT = T.pack . show

showP :: Doc -> a -> LogStr
showP s = const $ toLogStr $ displayS (renderPretty 1.0 80 s) ""

logTimed :: (?tlog :: TimedFastLogger, MonadIO m) => Text -> m ()
logTimed x = liftIO $ ?tlog (\t -> toLogStr t <> toLogStr x <> toLogStr "\n")

logPretty :: (?tlog :: TimedFastLogger, MonadIO m, Pretty s) => s -> m ()
logPretty = liftIO . ?tlog . showP . (<> line) . pretty

--------------------------------------- Dialogs ------------------------------------------

type Dialog = (?tlog :: TimedFastLogger) => ReaderT Interactive (StateT Config IO) ()

dialog :: (?tlog :: TimedFastLogger, MonadIO m, MonadReader Interactive m, MonadState Config m, Pretty s)
       => [(String, m ())] -> s -> Maybe Int -> m ()
dialog answers msg mTime = do
  logPretty ""
  logPretty msg
  liftIO $ ?tlog $ const $ toLogStr "> "
  input <- liftIO $ fromMaybe mempty <$> timeout (fromMaybe (-1) mTime) T.getLine
  case M.lookup (T.toLower input) graph of
    Just next -> logPretty "" >> next
    Nothing -> dialog answers ("Please enter one: " <> filtered) Nothing
  where
  filtered = intercalate ", " $ filter (not . null) $ map (take 1 . fst) answers
  graph = M.fromList $ answers >>= \case
    ("", p) -> [(mempty, p)]
    (h:t, p) -> [(T.pack (h:t), p), (T.singleton h, p)]

initDialog :: Dialog
initDialog = do
  tokens <- writeInput RunToken >> waitTokens
  args <- liftIO getArgs
  when (null args && null tokens) $ do
    logPretty $ indent 4 $ enclose line line $ fillSep $ map text $ words
      "It seems that you are using Agent for the first time. The application will be run in the tutorial mode. Next time, you can launch the tutorial mode by using the --tutorial option."
    modify (change $ \x -> x { runTutorial = True })
  logTimed $ T.pack "Start"
  (tutorial, skip) <- gets (runConfig >>> runTutorial &&& skipAll)
  case () of _ | tutorial -> tutorialDialog
               | skip -> skipDialog
               | otherwise -> mainDialog (Just 5)
  where
  waitTokens = readOutput >>= \case
    ShowMessage m -> logTimed m >> waitTokens
    FailRun code -> liftIO $ throwIO code
    FinishToken tokens -> return tokens
    _ -> waitTokens

skipDialog :: Dialog
skipDialog = do
  c <- gets (apiConfig >>> apiCode)
  t <- gets (proxyConfig >>> proxyToken)
  case () of _ | isJust t -> writeInput RunProxy
               | not (BC.null c) -> writeInput RunApi
               | otherwise -> writeInput RunToken
  forever $ readOutput >>= \case
    ShowMessage m -> logTimed m
    FailRun code -> liftIO $ throwIO code
    FinishApi token -> do
      modify (change $ \x -> x { proxyToken = Just (tokenString token)})
      get >>= writeInput . ConfigChange
      writeInput (SaveTokens [token])
      writeInput RunProxy
    FinishToken tokens -> case sort tokens of
      [] -> do
        logPretty (enclose line line $ bold $ text "No tokens found")
        mainDialog Nothing
      token:cdr -> do
        time <- liftIO getCurrentTime
        writeInput $ SaveTokens (token { accessTime = time }:cdr)
        modify (change $ \x -> x { proxyToken = Just (tokenString token)})
        get >>= writeInput . ConfigChange
        writeInput RunProxy
    FinishProxy code -> finishProxy code

tutorialDialog :: Dialog
tutorialDialog = do
  logPretty $ indent 4 $ enclose line line $ fillSep $ map text $ words
    "Agent is the application that proxies scanner requests up to your target. To bypass NAT and network filter, Agent must establish an outgoing connection with the PTBBS cloud.  First, you need to obtain an activation code in the PTBBS web interface."
  readCode
  logPretty $ indent 4 $ enclose line line $ fillSep $ map text $ words
    "By using the activation code, Agent obtains an identification token in the cloud."
  get >>= writeInput . ConfigChange
  writeInput RunApi
  forever go
  where
  readCode = do
    liftIO $ ?tlog $ showP $ bold $ text "Enter activation code: "
    code <- liftIO getLine
    if null code
      then logPretty "Activation code cannot be empty" >> readCode
      else modify (change $ \x -> x { apiCode = BC.pack code })
  go = readOutput >>= \case
    ShowMessage m -> logTimed m
    FailRun code -> liftIO $ throwIO code
    FinishToken [] -> liftIO $ throwIO $ ExitFailure errorPermissions
    FinishToken (token:_) -> do
      modify (change $ \x -> x { proxyToken = Just (tokenString token) })
      get >>= writeInput . ConfigChange
      logPretty $ indent 4 $ enclose line line $ fillSep $ map text $ words
        "Token is a unique line that Agent sends when connecting to the cloud. Next time, the same token or the latest obtained will be used. Agent can now establish the connection and start proxying traffic from the PTBBS scanner."
      writeInput RunProxy
    FinishApi token -> do
      writeInput (SaveTokens [token])
      writeInput RunToken
    FinishProxy ExitSuccess -> do
      logPretty $ indent 4 $ enclose line line $ fillSep $ map text $ words
        "Congratulations, your first scanning is complete. Agent will now establish a new connection, in case you want to perform another scanning. You can change Agent settings in the main menu or using command line keys (use --help to learn more)."
      liftIO $ wait 2
      writeInput RunProxy
    FinishProxy code -> finishProxy code

mainDialog :: Maybe Int -> Dialog
mainDialog mt = do
  get >>= logPretty . basicConfig
  dialog [ ("", return ())
         , ("1", apiCodeDialog >> mainDialog Nothing)
         , ("2", tokenPathDialog >> mainDialog Nothing)
         , ("advanced", advancedDialog) ]
         (text "Enter number of option you want to change or type (a)dvanced for expert mode. "
      </> text "Press enter"
      <+> maybe mempty (\t -> text "or wait" <+> int t <+> text "seconds ") mt
       <> text "to use this configuration.")
         mt
  get >>= writeInput . ConfigChange
  code <- gets (apiConfig >>> apiCode)
  token <- gets (proxyConfig >>> proxyToken)
  unless (BC.null code) $ writeInput RunApi >> waitCode
  unless (isJust token) $ writeInput RunToken >> waitToken
  writeInput RunProxy >> waitProxy
  where
  waitToken = readOutput >>= \case
    ShowMessage m -> logTimed m >> waitToken
    FailRun code -> liftIO $ throwIO code
    FinishToken tokens -> case sort tokens of
      [] -> do
        logPretty (enclose line line $ bold $ text "No tokens found")
        mainDialog Nothing
      [token] -> do
        logPretty "Found one token:"
        logPretty token
        let next = do
              atime <- liftIO getCurrentTime
              modify (change $ \x -> x { proxyToken = Just (tokenString token) })
              writeInput $ SaveTokens [token { accessTime = atime }]
              get >>= writeInput . ConfigChange
              writeInput RunProxy
        dialog [("yes", next), ("", next), ("no", mainDialog Nothing)]
               "Do you want to use this token (Y/n)? Skip in 5 seconds"
               (Just 5)
      _ -> do
        manyTokensDialog tokens
        get >>= writeInput . ConfigChange
        writeInput RunProxy
    _ -> waitToken

  waitCode = readOutput >>= \case
    ShowMessage m -> logTimed m >> waitCode
    FailRun{} -> do
      logPretty (enclose line line $ bold $ text "Failed to use activation code")
      mainDialog Nothing
    FinishApi token -> do
      writeInput (SaveTokens [token])
      modify (change $ \x -> x { proxyToken = Just (tokenString token) })
      get >>= writeInput . ConfigChange
    _ -> waitCode

  waitProxy = forever $ readOutput >>= \case
    ShowMessage m -> logTimed m
    FailRun code -> liftIO $ throwIO code
    FinishProxy code -> finishProxy code
    _ -> return ()

advancedDialog :: Dialog
advancedDialog = do
  get >>= logPretty . advancedConfig
  dialog [ ("", return ())
         , ("1", apiCodeDialog >> advancedDialog)
         , ("2", tokenPathDialog >> advancedDialog)
         , ("3", proxyTokenDialog >> advancedDialog)
         , ("4", apiUrlDialog >> advancedDialog)
         , ("5", proxyAddrDialog >> advancedDialog)
         , ("6", modify (change $ \x -> x { ignoreSSL = not $ ignoreSSL x }) >> advancedDialog)
         , ("7", modify (change $ \x -> x { debugConn = not $ debugConn x }) >> advancedDialog)
         , ("8", modify (change $ \x -> x { oneRun = not $ oneRun x }) >> advancedDialog) ]
         "Enter number of option you want to change. Press enter to skip this dialog."
         Nothing

tokenPathDialog :: Dialog
tokenPathDialog = do
  logPretty "Enter tokens path (enter nothing to go back): "
  path <- liftIO getLine
  unless (null path) $ modify (change $ \x -> x { tokenPath = path })

proxyAddrDialog :: Dialog
proxyAddrDialog = do
  logPretty "Enter preferred proxy address (enter nothing to go back): "
  mAddr <- liftIO getLine
  unless (null mAddr) $ case readMaybe mAddr of
    Just addr -> modify (change $ \x -> x { proxyAddr = addr })
    Nothing -> logPretty "Enter address in <host>:<port> format" >> proxyAddrDialog

apiUrlDialog :: Dialog
apiUrlDialog = do
  liftIO $ ?tlog $ const $ toLogStr "Enter preferred api url (enter nothing to go back): "
  url <- liftIO getLine
  unless (null url) $ case readMaybe url of
    Nothing -> logPretty "Enter valid url" >> apiUrlDialog
    Just u -> modify (change $ \x -> x { apiURL = u })

proxyTokenDialog :: Dialog
proxyTokenDialog = do
  liftIO $ ?tlog $ const $ toLogStr "Enter preferred token (enter nothing to go back): "
  token <- liftIO getLine
  unless (null token) $ modify (change $ \x -> x { proxyToken = Just $ BC.pack token })

apiCodeDialog :: Dialog
apiCodeDialog = do
  liftIO $ ?tlog $ const $ toLogStr "Enter activation code (enter nothing to go back): "
  code <- liftIO getLine
  unless (null code) $ modify (change $ \x -> x { apiCode = BC.pack code })

manyTokensDialog :: [Token] -> Dialog
manyTokensDialog tokens = do
  logPretty "Found several tokens:"
  logPretty tokens
  logPretty $ text "Which one do you want to use? "
         <$$> text "Press enter or wait 5 seconds to use the last one."
  go
  where
  errorMessage = "Please enter valid number [1-" <> show (length tokens) <> "]"
  go :: Dialog
  go = do
    n <- liftIO (timeout 5 getLine) >>= \case
      Nothing -> return 1
      Just "" -> return 1
      Just smth -> case readMaybe smth of
        Just n -> return n
        Nothing -> return maxBound
    case splitAt (n - 1) tokens of
      (_, []) -> logPretty errorMessage >> go
      (h, token:t) -> do
        atime <- liftIO getCurrentTime
        writeInput $ SaveTokens (token { accessTime = atime }:h <> t)
        modify (change $ \x -> x { proxyToken = Just (tokenString token) })

finishProxy :: ExitCode -> Dialog
finishProxy code = do
  once <- gets (runConfig >>> oneRun)
  if not once && (code == ExitSuccess || code == ExitFailure errorInternal)
    then liftIO (wait 2) >> writeInput RunProxy
    else liftIO (exitWith code)

--------------------------------------- Console ------------------------------------------

frontend :: Config -> InputChannel -> OutputChannel -> IO ()
frontend config input output = bracket acquire snd $ \(tlog,_) -> let ?tlog = tlog in
  catch (evalStateT (runReaderT initDialog (output, input)) config)
        (\(e :: ExitCode) -> logTimed (showT e))

acquire :: IO (TimedFastLogger, IO ())
acquire = do
  hSetBuffering stdout LineBuffering
  newTimeCache (BC.pack "%b %d %X - ") >>= (`newTimedFastLogger` LogStdout defaultBufSize)
