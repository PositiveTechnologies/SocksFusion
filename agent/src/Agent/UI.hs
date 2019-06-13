-- TODO: Replace free monad with extensible-effects

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_HADDOCK prune #-}

------------------------------------------------------------------------------------------
-- |
-- Module: Agent.UI
--
-- This module contains interpreter based on free monad and all screens used in
-- user interface.
--
------------------------------------------------------------------------------------------

module Agent.UI (
 -- * OptionsBlock
   OptionsBlock
 , optionAction
 -- * Interface
 -- ** Data
 , InterfaceF(..)
 , Interface
 , mainScreen
 , skipScreen
 , tutorialScreen
 , writeEvent
 , logMessage
 , updateProgress
 , stopInteractive
 ) where

import           Prelude
import           Agent.Types
import           Agent.UI.Core

import           GHC.Exts               (IsString(..))
import           System.Exit            (ExitCode(..))

import           Lens.Micro.Platform
import           Control.Arrow
import           Control.Monad.Free
import           Control.Monad
import           Data.List              (genericLength)

import           Data.Maybe             (isJust)
import           Data.Either
import           Data.Bool
import           Data.Text              (Text)
import qualified Data.Text           as T

#if MIN_VERSION_base(4,11,0)
#else
import           Data.Semigroup  hiding (getOption)
#endif

--------------------------------------- Types --------------------------------------------

-- | Scrollbuffer represents map from the strings to actions.
type OptionsBlock = ScrollBuffer (Wrap PrettyConsole, Interface ())

instance UIBlock OptionsBlock where
  minSize width = min 5 . genericLength . foldMap (render width . fst)
  maxSize width = genericLength . foldMap (render width . fst)

-- | Representation of UI interpreter.
data InterfaceF next where
  -- | Clear the screen and init the new one.
  InitInterface :: next -> InterfaceF next
  -- | Block until any events will happen.
  WaitEvent :: (FrontMessage -> next) -> InterfaceF next
  -- | Send a message to the backend.
  WriteEvent :: BackMessage -> next -> InterfaceF next
  -- | Append a block to the frame tree.
  NewBlock :: UIBlock a => a -> (Either String (UIPtr a) -> next) -> InterfaceF next
  -- | Update the block by the given ptr with the given function.
  UpdateBlock :: UIBlock a => (a -> a) -> UIPtr a -> next -> InterfaceF next
  -- | Append a bordered frame to the frame tree.
  NewFrame :: (UIPtr FrameBlock -> next) -> InterfaceF next
  -- | Append a new block into the frame choosen by the given ptr.
  FillFrame :: UIBlock a => UIPtr FrameBlock -> a -> (Either String (UIPtr a) -> next) -> InterfaceF next
  -- | Extract the choosen option from the block.
  GetOption :: UIPtr OptionsBlock -> (Either String (Interface ()) -> next) -> InterfaceF next
  -- | Extract the input value from the block.
  GetInput :: Read a => UIPtr InputBlock -> (Either String a -> next) -> InterfaceF next
  -- | Append a new message to the logs.
  UpdateLog :: Text -> next -> InterfaceF next
  -- | Update scan progress.
  SetProgress :: Double -> next -> InterfaceF next
  -- | Update scan progress. Notify user.
  UpdateProgress :: Double -> next -> InterfaceF next
  -- | Get current configuration.
  GetConfig :: (Config -> next) -> InterfaceF next
  -- | Update current configuration.
  UpdateConfig :: (Config -> Config) -> next -> InterfaceF next
  -- | Exit with given code.
  StopInteractive :: ExitCode -> InterfaceF next

deriving instance Functor InterfaceF

-- | Actual free monad based on InterfaceF protocol.
type Interface = Free InterfaceF

initInterface :: Interface ()
initInterface = liftF $ InitInterface ()
waitEvent :: Interface FrontMessage
waitEvent = liftF $ WaitEvent id
writeEvent :: BackMessage -> Interface ()
writeEvent msg = liftF $ WriteEvent msg ()
newBlock :: UIBlock a => a -> Interface (Either String (UIPtr a))
newBlock b = liftF $ NewBlock b id
updateBlock :: UIBlock a => (a -> a) -> UIPtr a -> Interface ()
updateBlock f ptr = liftF $ UpdateBlock f ptr ()
newFrame :: Interface (UIPtr FrameBlock)
newFrame = liftF $ NewFrame id
fillFrame :: UIBlock a => UIPtr FrameBlock -> a -> Interface (Either String (UIPtr a))
fillFrame f b = liftF $ FillFrame f b id
getOption :: UIPtr OptionsBlock -> Interface (Either String (Interface ()))
getOption ptr = liftF $ GetOption ptr id
getInput :: (Read a) => UIPtr InputBlock -> Interface (Either String a)
getInput ptr = liftF $ GetInput ptr id
logMessage :: Text -> Interface ()
logMessage l = liftF $ UpdateLog l ()
updateProgress :: Double -> Interface ()
updateProgress p = liftF $ UpdateProgress p ()
getConfig :: Interface Config
getConfig = liftF $ GetConfig id
updateConfig :: (Config -> Config) -> Interface ()
updateConfig f = liftF $ UpdateConfig f ()
stopInteractive :: ExitCode -> Interface a
stopInteractive ec = liftF $ StopInteractive ec

replaceMain :: Interface ()
replaceMain = initInterface

exitWith :: ExitCode -> String -> Interface a
exitWith e s = logMessage (T.pack s) >> stopInteractive e

--------------------------------------- Tokens -------------------------------------------

appendToken :: (?isFallback :: Bool) => Token -> UIPtr OptionsBlock -> Interface ()
appendToken token = updateBlock (|> (Wrap token, replaceMain >> useToken token mainScreen))

useToken :: (?isFallback :: Bool) => Token -> Interface () -> Interface ()
useToken token nextScreen = do
  writeEvent $ SaveTokens [token]
  updateConfig (proxyConfig.proxyToken ?~ token { tokenSource = Explicit })
  progressScreen nextScreen

clearCode :: Interface ()
clearCode = updateConfig (\x -> x & apiConfig.apiCode .~ AC "")

--------------------------------------- Options ------------------------------------------

-- | Get action of the choosen element of the options block.
optionAction :: OptionsBlock -> Interface ()
optionAction = snd . currentScroll

--------------------------------------- Inputs -------------------------------------------

backspaceInput :: UIPtr InputBlock -> Interface ()
backspaceInput = updateBlock (\(InputBlock txt str)
                             -> InputBlock txt $ if null str then str else init str)

appendInput :: Char -> UIPtr InputBlock -> Interface ()
appendInput c = updateBlock (\(InputBlock txt str) -> InputBlock txt (str <> [c]))

flash :: String -> UIPtr TextBlock -> Interface ()
flash = updateBlock . const . fromString

showT :: Show s => s -> Text
showT = T.pack . show

--------------------------------------- Screens ------------------------------------------

mainScreen :: (?isFallback :: Bool) => Interface ()
mainScreen = if ?isFallback then skipScreen else do
  tutorial <- (^. runConfig.runTutorial) <$> getConfig
  if tutorial then tutorialScreen else do
    initInterface
    void $ newBlock $ TextBlock ""
                             <> "  Use the arrow keys to select an option."
                             <> "Press enter to activate the option."
    code <- (^. apiConfig.apiCode) <$> getConfig
    frame <- newFrame
    block <- fillFrame frame $ first Wrap <$> [
        ("Run with current configuration", skipScreen)
      , ("Activation code: " <> showT code, activationCodeScreen)
      , ("Change configuration", advancedScreen 1)
      , ("Manually choose token", loadTokensScreen)
      , ("Exit agent", stopInteractive ExitSuccess)
      ]
    either (exitWith (ExitFailure errorUnknown)) react block
  where
  react block = waitEvent >>= \case
    UserInput m -> case m of
      KeyUp -> updateBlock goBackward block >> react block
      KeyDown -> updateBlock goForward block >> react block
      KeyEnter -> getOption block >>= fromRight (react block)
      _ -> react block
    _ -> react block

loadTokensScreen :: (?isFallback :: Bool) => Interface ()
loadTokensScreen = do
  AC code <- (^. apiConfig.apiCode) <$> getConfig
  unless (null code) $ writeEvent RunAPI
  writeEvent RunToken
  replaceMain
  void $ newBlock $ TextBlock ""
                           <> "  Use the arrow keys to select a token."
                           <> "Press enter to connect to the proxy."
  frame <- newFrame
  let back = [(Wrap $ T.pack "Go back", mainScreen)]
  block <- (^. proxyConfig.proxyToken) <$> getConfig >>= \case
    Nothing -> fillFrame frame back
    Just token -> do
      logMessage "Using explicit token"
      fillFrame frame $ back |> (Wrap token, useToken token mainScreen)
  either (exitWith (ExitFailure errorUnknown)) react block
  where
  react block = waitEvent >>= \case
    FinishAPI (Left err) -> logMessage (showT err) >> react block
    FinishAPI (Right token) -> clearCode >> appendToken token block >> react block
    FinishToken (Left err) -> logMessage (showT err) >> react block
    FinishToken (Right tokens) -> mapM_ (`appendToken` block) tokens >> react block
    UserInput m -> case m of
      KeyUp -> updateBlock goBackward block >> react block
      KeyDown -> updateBlock goForward block >> react block
      KeyEnter -> getOption block >>= fromRight (react block)
      KeyEsc -> mainScreen
      _ -> react block
    _ -> react block

activationCodeScreen :: (?isFallback :: Bool) => Interface ()
activationCodeScreen = do
  replaceMain
  void $ newBlock $ TextBlock ""
                           <> "  Type new value."
                           <> "Press Enter to start agent, press Esc to go back."
  frame <- newFrame
  block <- (^. apiConfig.apiCode) <$> getConfig >>= fillFrame frame . InputBlock "Activation code: " . show
  either (exitWith (ExitFailure errorUnknown)) react block
  where
  react block = waitEvent >>= \case
    UserInput m -> case m of
      KeyEsc -> mainScreen
      KeyEnter -> getInput block >>= \case
        Right value -> updateConfig (\x -> x & apiConfig.apiCode .~ value) >> skipScreen
        Left err -> error ("Activation code can be any string: " <> err)
      KeyBackspace -> backspaceInput block >> react block
      KeyChar c -> appendInput c block >> react block
      KeyNum c -> appendInput (head $ show c) block >> react block
      _ -> react block
    _ -> react block

progressScreen :: (?isFallback :: Bool) => Interface () -> Interface ()
progressScreen nextScreen = do
  writeEvent RunProxy
  liftF $ SetProgress 0 () -- Do not notify about progress update
  frame <- newFrame
  void $ fillFrame frame (TextBlock "Scan progress. Press Esc to interrupt the scan.")
  void $ fillFrame frame ProgressBlock
  react
  where
  restartAgent err = do
    once <- (^. runConfig.oneRun) <$> getConfig
    case (err, once) of
      (_, True) -> stopInteractive err
      (ExitSuccess, _) -> replaceMain >> nextScreen
      (ExitFailure code, _)
        |   code == errorTLS || code == errorDuplicate
         || code == errorPuzzle || code == errorInternal || code == errorUnknown
                    -> replaceMain >> nextScreen
        | otherwise -> stopInteractive err
  react = waitEvent >>= \case
    FinishAPI (Left err) -> logMessage (showT err) >> react
    FinishToken (Left err) -> logMessage (showT err) >> react
    FinishProxy (Left err) -> logMessage (showT err) >> restartAgent err
    FinishProxy (Right ()) -> logMessage "Scan finished" >> restartAgent ExitSuccess
    UserInput KeyEsc -> writeEvent StopProxy
                     >> logMessage "Scan cancelled"
                     >> updateConfig (proxyConfig.proxyToken .~ Nothing)
                     >> mainScreen
    _ -> react

advancedScreen :: (?isFallback :: Bool) => Int -> Interface ()
advancedScreen n = do
  replaceMain
  void $ newBlock $ TextBlock ""
                           <> "  Use the arrow keys to select an option."
                           <> "Press enter to change the option. Press ESC to go to main screen."
  config <- getConfig
  frame <- newFrame
  block <- fillFrame frame $ setPosition n $ options config |> ("Go back", mainScreen)
  either (exitWith (ExitFailure errorUnknown)) react block
  where
  react block = waitEvent >>= \case
    UserInput m -> case m of
      KeyUp -> updateBlock goBackward block >> react block
      KeyDown -> updateBlock goForward block >> react block
      KeyEnter -> getOption block >>= fromRight (react block)
      KeyEsc -> mainScreen
      _ -> react block
    _ -> react block
  options :: Config -> ScrollBuffer (Wrap PrettyConsole, Interface ())
  options config = [
      toOption "Activation code: " (apiConfig.apiCode) 1
    , toOption "Token path: " (tokenConfig.tokenPath) 2
--    , toOption "Using token: " (proxyConfig.proxyToken)
    , toOption "API server: " (apiConfig.apiURL) 3
    , toOption "Proxy server: " (proxyConfig.proxyAddr) 4
    , toBoolOption "Ignore ssl errors: " (runConfig.ignoreSSL) 5
    , toBoolOption "Debug mode: " (runConfig.debugConn) 6
    , toBoolOption "Exit after scan: " (runConfig.oneRun) 7
    ]
    where
    -- TODO: fix render here
    toOption :: (PrettyConsole a, Show a, Read a) => Text -> Lens' Config a -> Int -> (Wrap PrettyConsole, Interface ())
    toOption x f k = (Wrap $ x <> head (render (100 :: Int) $ config ^. f)
                    , changeScreen x f >> advancedScreen k)
    toBoolOption :: Text -> Lens' Config Bool -> Int -> (Wrap PrettyConsole, Interface ())
    toBoolOption x f k = (Wrap $ x <> head (render (100 :: Int) $ config ^. f)
                        , changeBool f >> advancedScreen k)

changeBool :: Lens' Config Bool -> Interface ()
changeBool sel = do
  cur <- (^. sel) <$> getConfig
  updateConfig (sel .~ not cur)

changeScreen :: (Show a, Read a) => Text -> Lens' Config a -> Interface ()
changeScreen name sel = do
  replaceMain
  void $ newBlock $ TextBlock ""
                           <> "  Type new value."
                           <> "Press Enter to save it, or Esc to leave it unchanged."
  frame <- newFrame
  eBlock <- (^. sel) <$> getConfig >>= fillFrame frame . InputBlock name . show
  flashErr <- either (\_ _ -> return ()) (flip flash) <$> newBlock (TextBlock mempty)
  either (exitWith (ExitFailure errorUnknown)) (react flashErr) eBlock
  where
  react flashErr block = waitEvent >>= \case
    UserInput m -> case m of
      KeyEsc -> return ()
      KeyEnter -> getInput block >>= \case
        Right value -> updateConfig (\x -> x & sel .~ value)
        Left err -> flashErr err >> react flashErr block
      KeyBackspace -> backspaceInput block >> react flashErr block
      KeyChar c -> appendInput c block >> react flashErr block
      KeyNum c -> appendInput (head $ show c) block >> react flashErr block
      _ -> react flashErr block
    _ -> react flashErr block

skipScreen :: (?isFallback :: Bool) => Interface ()
skipScreen = do
  replaceMain
  explicit <- (^. proxyConfig.proxyToken) <$> getConfig
  if isJust explicit then progressScreen mainScreen else do
    AC code <- (^. apiConfig.apiCode) <$> getConfig
    if not (null code) then writeEvent RunAPI >> waitCode else do
      writeEvent RunToken
      waitPath
  where
  waitCode = waitEvent >>= \case
    FinishAPI (Left err) -> logMessage (showT err)
                         >> bool mainScreen (stopInteractive err) ?isFallback
    FinishAPI (Right token) -> clearCode >> useToken token mainScreen
    _ -> waitCode
  waitPath = waitEvent >>= \case
    FinishToken (Left err) -> logMessage (showT err)
                           >> bool mainScreen (stopInteractive err) ?isFallback
    FinishToken (Right []) -> logMessage "No local tokens found"
                           >> bool mainScreen (return ()) ?isFallback
    FinishToken (Right (token:_)) -> useToken token mainScreen
    _ -> waitPath

tutorialScreen :: (?isFallback :: Bool) => Interface ()
tutorialScreen = do
  updateConfig (\x -> x & runConfig.runTutorial .~ False)
  replaceMain
  frame1 <- newFrame
  void $ fillFrame frame1 $ textBlock "  Agent is the application that proxies scanner requests up to your target. To bypass NAT and network filter, Agent must establish an outgoing connection with the PTBBS cloud.  First, you need to obtain an activation code in the PTBBS web interface."
  frame2 <- newFrame
  code <- (^. apiConfig.apiCode) <$> getConfig
  codeBlock <- fillFrame frame2 $ InputBlock "Activation code: " (show code)
  either (exitWith (ExitFailure errorUnknown)) waitCode codeBlock

  writeEvent RunAPI
  replaceMain
  frame3 <- newFrame
  void $ fillFrame frame3 $ textBlock "  By using the activation code, Agent obtains an identification token in the cloud."
  waitToken
  where
  waitCode block = waitEvent >>= \case
    UserInput m -> case m of
      KeyEsc -> stopInteractive ExitSuccess
      KeyEnter -> getInput block >>= \case
        Right value -> updateConfig (\x -> x & apiConfig.apiCode .~ value)
        Left err -> error ("Activation code can be any string: " <> err)
      KeyBackspace -> backspaceInput block >> waitCode block
      KeyChar c -> appendInput c block >> waitCode block
      KeyNum c -> appendInput (head $ show c) block >> waitCode block
      _ -> waitCode block
    _ -> waitCode block
  waitToken = waitEvent >>= \case
    FinishAPI (Left err) -> logMessage (showT err) >> tutorialScreen
    FinishAPI (Right token) -> do
      clearCode
      replaceMain
      void $ newBlock $ HiddenBlock 1
      frame <- newFrame
      void $ fillFrame frame $ textBlock "  Token is a unique line that Agent sends when connecting to the cloud. Next time, the same token or the latest obtained will be used. Agent can now establish the connection and start proxying traffic from the PTBBS scanner."
      useToken token secondScan
    _ -> waitToken
  secondScan = do
    replaceMain
    void $ newBlock $ HiddenBlock 1
    frame <- newFrame
    void $ fillFrame frame $ textBlock "  Congratulations, your first scanning is complete. Agent will now establish a new connection, in case you want to perform another scanning. You can change Agent settings in the main menu or using command line keys (use --help to learn more)."
    progressScreen restScans
  restScans = do
    replaceMain
    void $ newBlock $ HiddenBlock 3
    progressScreen restScans
