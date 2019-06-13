{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

------------------------------------------------------------------------------------------
-- |
-- Module Agent.Frontend.Console
--
-- UI interpreter implemention for terminals and actual rendering functions.
--
------------------------------------------------------------------------------------------

module Agent.Frontend.Console (
   console
 -- * Rendering
 -- ** Class
 , MonadConsole(..)
 , ConsoleRender(..)
 -- ** Fallback
 , FallbackUI
 -- ** Interactive
 , Patch(..)
 , ConsoleUI(..)
 , ConsoleTree
 , ConsoleFrame
 , ConsoleNode
 , XDim
 , YDim
 , Position
 , Size
 , renderCUI
 , appendBlockCUI
 , updateBlockCUI
 , newFrameCUI
 , fillFrameCUI
 , readInputCUI
 , readOptionsCUI
 , findBlock
 , findFrame
 , resizeTree
 , branchSize
 , evalBuffer
 , gui
 ) where

import           Prelude         hiding (log, interact, lines, lookup, replicate, take, length, id, (.), putChar)
import           Agent.Types
import           Agent.UI
import           Agent.UI.Core
import           Agent.Frontend.Console.Internal (ansiSupported, withConsole, handleUserInput, enableQuickEdit, disableQuickEdit)

import           Data.Time              (getZonedTime, defaultTimeLocale, formatTime)
import           System.IO              (hFlush, stdout)
import qualified System.IO
import           System.Exit            (ExitCode(..))
import           Control.Exception      (try, throwIO, SomeException, Exception(..))
import           GHC.Stack

import           Data.Text              (Text)
import qualified Data.Text.IO        as T
import qualified Data.Text           as T

import           Data.Typeable          (Typeable, eqT, cast, (:~:)(..))
import           Data.Maybe             (fromMaybe)
import           Data.Function          ((&))

import           Lens.Micro.Platform
import           Control.Category
import qualified Control.Monad
import           Control.Monad.Free
import           Control.Monad.State.Lazy hiding (replicateM_)
import           Control.Monad.Reader hiding (replicateM_)
import           Control.Concurrent.STM
import           Control.Concurrent
import           Control.Arrow   hiding (left, right)
import           Data.Monoid            (First(..), Sum(..))
import           Data.Tuple

import qualified Data.String         as S
import           Text.Read              (readEither)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified System.Console.ANSI
import           System.Console.ANSI    (setSGRCode, SGR(..))
import qualified System.Console.Terminal.Size as T
import qualified Data.Sequence as S
import           Data.Foldable   hiding (length)
import qualified Data.List

import           Numeric                (showFFloat)

#if MIN_VERSION_base(4,11,0)
#else
import           Data.Semigroup  hiding (getOption, First(..))
#endif

--------------------------------------- Utils --------------------------------------------

take :: Integral n => n -> [a] -> [a]
take = Data.List.take . fromIntegral

length :: Num n => [a] -> n
length = fromIntegral . Data.List.length

replicate :: Integral n => n -> a -> [a]
replicate n = Data.List.replicate (fromIntegral n)

replicateM_ :: (Applicative m, Integral n) => n -> m a -> m ()
replicateM_ n = Control.Monad.replicateM_ (fromIntegral n)

extQ :: (Typeable a, Typeable b) => (a -> r) -> (b -> r) -> (a -> r)
extQ f g x = maybe (f x) g $ cast x

third :: (c -> d) -> (a, b, c) -> (a, b, d)
third f (a, b, c) = (a, b, f c)

setCursorColumn :: MonadIO m => Position -> m ()
setCursorColumn = liftIO . System.Console.ANSI.setCursorColumn . fromIntegral

cursorDown :: MonadIO m => Position -> m ()
cursorDown = liftIO . System.Console.ANSI.cursorDown . fromIntegral

setCursorPosition :: MonadIO m => Size -> Size -> m ()
setCursorPosition h w = liftIO $ System.Console.ANSI.setCursorPosition (getSum h)
                                                                       (getSum w)
putChar :: MonadIO m => Char -> m ()
putChar = liftIO . System.IO.putChar

getTimeStamp :: MonadIO m => m String
getTimeStamp = liftIO $ formatTime defaultTimeLocale "%b %d %X - " <$> getZonedTime

---------------------------------------- UI ----------------------------------------------

-- | Abstraction over frontend.
class Monad m => MonadConsole m where
  clearScreen :: m ()
  resizeWindow :: Size -> Size -> m ()
  stopRendering :: ExitCode -> m a
  updateLogs :: Text -> m ()
  setProgress :: Double -> m ()
  renderUI :: m ()
  appendBlockUI :: UIBlock b => b -> m (Either String (UIPtr b))
  updateBlockUI :: UIBlock b => (b -> b) -> UIPtr b -> m ()
  newFrameUI :: m (UIPtr FrameBlock)
  fillFrameUI :: UIBlock b => UIPtr FrameBlock -> b -> m (Either String (UIPtr b))
  readInputUI :: Read a => UIPtr InputBlock -> m (Either String a)
  readOptionsUI :: UIPtr OptionsBlock -> m (Either String (Interface ()))

instance MonadConsole m => MonadConsole (ReaderT r m) where
  clearScreen = lift clearScreen
  resizeWindow h = lift . resizeWindow h
  stopRendering = lift . stopRendering
  updateLogs = lift . updateLogs
  setProgress = lift . setProgress
  renderUI = lift renderUI
  appendBlockUI = lift . appendBlockUI
  updateBlockUI f = lift . updateBlockUI f
  newFrameUI = lift newFrameUI
  fillFrameUI ptr = lift . fillFrameUI ptr
  readInputUI = lift . readInputUI
  readOptionsUI = lift . readOptionsUI

wrapText :: TextBlock -> Wrap ConsoleRender
wrapText = Wrap
wrapInput :: InputBlock -> Wrap ConsoleRender
wrapInput = Wrap
wrapOptions :: OptionsBlock -> Wrap ConsoleRender
wrapOptions = Wrap
wrapProgress :: ProgressBlock -> Wrap ConsoleRender
wrapProgress = Wrap
wrapHidden :: HiddenBlock -> Wrap ConsoleRender
wrapHidden = Wrap
wrapAny :: UIBlock b => b -> Maybe (Wrap ConsoleRender)
wrapAny = let p f = pure . f in const Nothing
                         `extQ` p wrapText
                         `extQ` p wrapInput
                         `extQ` p wrapOptions
                         `extQ` p wrapProgress
                         `extQ` p wrapHidden

---------------------------------------- Fallback

-- | Console state in fallback mode.
data FallbackUI = FUI

instance MonadIO m => MonadConsole (StateT FallbackUI m) where
  clearScreen = return ()
  resizeWindow _ _ = return ()
  stopRendering code = liftIO (throwIO code)
  updateLogs msg = liftIO (T.putStrLn msg)
  setProgress _ = return ()
  renderUI = return ()
  appendBlockUI _ = return (Left "Fallback mode")
  updateBlockUI _ _ = return ()
  newFrameUI = return (UIPtr $ toEnum 0)
  fillFrameUI _ _ = return (Left "Fallback mode")
  readInputUI _ = return (Left "Fallback mode")
  readOptionsUI _ = return (Left "Fallback mode")

---------------------------------------- Console

class UIBlock a => ConsoleRender a where
  consoleRender :: (MonadState ConsoleUI m, MonadIO m) => XDim -> YDim -> a -> m ()

instance UIBlock (Wrap ConsoleRender) where
  minSize w (Wrap block) = minSize w block
  maxSize w (Wrap block) = maxSize w block

instance PrettyConsole RenderFunction where
  render width (RF f) = f width

newtype RenderFunction = RF (forall n. Integral n => n -> [Text])

-- | Patch rendering function for specific environment in run-time.
newtype Patch = Patch { getPatch :: Wrap ConsoleRender -> Wrap ConsoleRender }

-- | Special render for old windows console.
patchOptions :: Bool -> OptionsBlock -> Wrap ConsoleRender
patchOptions True = Wrap
patchOptions False = Wrap . fmap f
  where
  f :: (Wrap PrettyConsole, a) -> (Wrap PrettyConsole, a)
  f (pretty, action) = (Wrap $ RF $ \w -> ch (render (w - 2) pretty), action)
  ch [] = []
  ch (t:ts) = "? " <> t:fmap("  " <>) ts

patchAny :: IO Patch
patchAny = do
  colored <- ansiSupported
  return $ Patch $ \(Wrap b) -> (Wrap `extQ` patchOptions colored) b

type ConsoleTree = UITree ConsoleFrame ConsoleNode
type ConsoleNode = (XDim, YDim, Wrap ConsoleRender)
type ConsoleFrame = (XDim, YDim, FrameBlock)
type XDim = (Position, Size)
type YDim = (Position, Size)
type Position = Sum Int
type Size = Sum Int

-- | Represents current console state.
data ConsoleUI = CUI {
   cWidth :: Size          -- ^ Current terminal width.
 , cHeight :: Size         -- ^ Current terminal height.
 , cProgress :: Double     -- ^ Current scan progress.
 , cBlocks :: ConsoleTree  -- ^ UI frame tree.
 , cLogs :: S.Seq Text     -- ^ Log history.
 , cPatch :: Patch         -- ^ Modifier used for change objects during rendering.
 }

instance MonadIO m => MonadConsole (StateT ConsoleUI m) where
  clearScreen = do
    modify (\cui -> cui { cBlocks = empty })
    liftIO System.Console.ANSI.clearScreen
  resizeWindow height width =
    modify (\cui -> cui { cWidth = width, cHeight = height })
  stopRendering code = do
    setCursorPosition 0 0
    gets (S.reverse . cLogs) >>= mapM_ (\m -> renderString m
                                           >> cursorDown 1
                                           >> setCursorColumn 0) . toList
    liftIO (throwIO code)
  updateLogs text = do
    modify (\cui -> cui { cLogs = text S.:<| cLogs cui })
    renderLogs
  setProgress new = do
    modify $ \cui -> cui { cProgress = new }
    renderProgress
  renderUI = liftIO System.Console.ANSI.clearScreen >> renderCUI
  appendBlockUI = appendBlockCUI
  updateBlockUI = updateBlockCUI
  newFrameUI = newFrameCUI
  fillFrameUI = fillFrameCUI
  readInputUI = readInputCUI
  readOptionsUI = readOptionsCUI

---------------------------------------- Console rendering

-- | Render all elements.
renderCUI :: (MonadState ConsoleUI m, MonadIO m) => m ()
renderCUI = do
  (w, h) <- gets (cWidth &&& cHeight)
  new_blocks <- resizeTree (1, w) (0, h) <$> gets cBlocks
  modify $ \cui -> cui { cBlocks = new_blocks }
  patch <- gets (getPatch . cPatch)
  bimapMTree_ renderFrame (renderBlock . third patch) new_blocks
  renderLogs

-- | Append new at the end of the tree.
appendBlockCUI :: (MonadState ConsoleUI m, UIBlock b) => b -> m (Either String (UIPtr b))
appendBlockCUI b = state $ \cui@(CUI width height _ bs _ _) -> wrapAny b &
   second (\nbs -> cui { cBlocks = nbs })
 . maybe (Left "Unknown block type", bs)
         (pure . UIPtr *** resizeTree (1, width) (0, height)
      <<< flip appendReturn bs . ((0, 0), (0, 0), ))

-- | Update the block with given function by given pointer.
updateBlockCUI :: forall a m. (MonadState ConsoleUI m, UIBlock a)
               => (a -> a) -> UIPtr a -> m ()
updateBlockCUI f (UIPtr key) = modify $ \cui@(CUI width height _ bs _ _) ->
  cui { cBlocks = app $ updateAndReturn (f' width height) key id bs }
  where
  f' w h b@(x@(_, xSize), y, Wrap (block :: ax)) = fromMaybe (id, b) $ do
    Refl <- eqT :: Maybe (ax :~: a)
    newBlock <- wrapAny $ f block
    return (if (minSize xSize block, maxSize xSize block)
            /= (minSize xSize newBlock, maxSize xSize newBlock)
              then id else resizeTree (1, w) (0, h)
          , (x, y, newBlock))

-- | Add new frame at the end of the top level.
newFrameCUI :: MonadState ConsoleUI m => m (UIPtr FrameBlock)
newFrameCUI = state go
  where
  frame k = Branch k ((0, 0), (0, 0), BorderedFrame) []
  go cui@(CUI width height _ bs _ _) = UIPtr *** (\nbs -> cui { cBlocks = resizeTree (1, width) (0, height) nbs }) $ case bs of
    Leaf{} -> (UIKey "", frame mempty)
    Branch kx vx nodes -> let ns = reverse nodes
                              key = case ns of
                                [] -> downKey kx
                                Branch ky _ _:_ -> succ ky
                                Leaf ky _:_ -> succ ky in
                          (key, Branch kx vx (reverse $ frame key:ns))

-- | Append the block into the frame by given pointer.
fillFrameCUI :: (MonadState ConsoleUI m, UIBlock b)
             => UIPtr FrameBlock -> b -> m (Either String (UIPtr b))
fillFrameCUI (UIPtr kx) b = state $ \cui@(CUI width height _ blocks _ _) ->
  either ((,cui) . Left) (first Right) $ do
    block <- maybe (Left "Unknown block type") pure $ wrapAny b
    (tree, ptr) <- maybe (Left "Frame not found") pure $ getFirst $ sequence $ swap $ go block blocks
    pure (ptr, cui { cBlocks = resizeTree (1, width) (0, height) tree })
  where
  go _ x@Leaf{} = (mempty, x)
  go b' y@(Branch ky vy nodes)
    | kx == ky = first (pure . UIPtr) $ appendReturn ((0, 0), (0, 0), b') y
    | kx `leq` ky = Branch ky vy <$> foldMap (second pure . go b') nodes
    | otherwise = (mempty, Branch ky vy nodes)

-- | Try to read an input in the block by given pointer.
readInputCUI :: (MonadState ConsoleUI m, Read a)
             => UIPtr InputBlock -> m (Either String a)
readInputCUI ptr = gets cBlocks >>= (findBlock ptr >>> \case
  Nothing -> return (Left "Options block wasn't found")
  Just (_, _, InputBlock _ x) -> return $ readEither x)

-- | Try to get choosen option in the block by given pointer.
readOptionsCUI :: MonadState ConsoleUI m
               => UIPtr OptionsBlock -> m (Either String (Interface ()))
readOptionsCUI ptr = gets cBlocks >>= (findBlock ptr >>> \case
  Nothing -> return (Left "Options block wasn't found")
  Just (_, _, b) -> return (Right $ optionAction b))

-- | Lookup UIBlock by the pointer in the tree.
findBlock :: forall a. UIBlock a => UIPtr a -> ConsoleTree -> Maybe (XDim, YDim, a)
findBlock (UIPtr key) tree = lookup key tree >>= \case
  Left{} -> Nothing
  Right (x, y, Wrap (b :: b)) -> (\Refl -> (x, y, b)) <$> (eqT :: Maybe (a :~: b))

-- | Lookup Frame by the pointer in the tree.
findFrame :: UIPtr FrameBlock -> ConsoleTree -> Maybe (XDim, YDim, FrameBlock)
findFrame (UIPtr key) tree = lookup key tree >>= \case
  Left x -> Just x
  Right{} -> Nothing

-- | Resize all elements for given dimensions
resizeTree :: XDim -> YDim -> ConsoleTree -> ConsoleTree
resizeTree xDim@(_, xSize) (yPos, ySize) node =
  case resizeAt (xDim, (yPos, rest), []) node of
    (_, _, []) -> error "Empty result of resizeTree"
    (_, _, [n]) -> n
    _ -> error "Incorrect result of resizeTree"
  where
  rest = let (mx, mn) = branchSize xSize node in min ySize mx - mn

resizeAt :: (XDim, YDim, [ConsoleTree]) -> ConsoleTree -> (XDim, YDim, [ConsoleTree])
resizeAt ((xPos, xSize), (yPos, rest), res) node@(Leaf k (_, _, block)) =
  ( (xPos, xSize)
  , (yPos + bSize + 1, bRest)
  , res <> [Leaf k ((xPos, xSize), (yPos, bSize), block)])
  where
  (bMax, bMin) = branchSize xSize node
  bSize = min bMax (bMin + rest)
  bRest = rest - (bSize - bMin)

resizeAt ((xPos, xSize), (yPos, rest), res) node@(Branch k (_, _, frame) nodes) =
  ( (xPos, xSize)
  , (yNext, fRest)
  , res <> [Branch k ((xPos, xSize), (yPos, fSize), frame) nodes'] )
  where
  nodes' = (\(_, _, ns) -> ns) $ foldl resizeAt ((xPos', xSize'), (yPos', rest), []) nodes
  (fMax, fMin) = branchSize xSize node
  fRest = rest - (fSize - fMin)
  yNext = if fSize == 0 then yPos else yPos + fSize + 1
  fSize = min fMax (fMin + rest)
  (xPos', xSize', yPos') = case frame of
    BorderedFrame -> (xPos + 2, xSize - 4, yPos + 1)
    _ -> (xPos, xSize, yPos)

-- | Calculate the size of the whole branch with given width.
branchSize :: Size -> ConsoleTree -> (Sum Int, Sum Int)
branchSize xSize (Leaf _ (_, _, v)) = (maxSize xSize v, minSize xSize v)
branchSize _ (Branch _ (_, _, HiddenFrame) _) = (0, 0)
branchSize xSize (Branch _ (_, _, v) ns) =
     join (,) (frameSize v)
  <> max (0, 0)
         (foldl (\s n -> let s' = branchSize width n in if s' == (0, 0)
                                                          then s
                                                          else s <> s' <> (1,1))
                (-1, -1)
                ns)
  where
  width = case v of { BorderedFrame -> xSize - 2; _ -> xSize }

--------------------------------------- Rendering ----------------------------------------

instance ConsoleRender TextBlock where
  consoleRender (xPos, xSize) (yPos, _) (TextBlock doc) = liftIO $ do
    setCursorPosition yPos xPos
    forM_ (S.lines $ PP.displayS (PP.renderPretty 1.0 (getSum xSize - 2) doc) "") $ \l -> do
      putStr l
      cursorDown 1
      setCursorColumn xPos

instance ConsoleRender InputBlock where
  consoleRender (xPos, xSize) (yPos, _) (InputBlock doc str) = do
    liftIO enableQuickEdit
    setCursorPosition yPos xPos
    let string = doc <> if length str + T.length doc + 2 > getSum xSize
                          then "..." <> T.takeEnd (getSum xSize - 5 - T.length doc) (T.pack str)
                          else T.pack str
    renderString string
    renderReversedString " "

instance ConsoleRender OptionsBlock where
  consoleRender (xPos, xSize) (yPos, ySize) opts = do
    liftIO disableQuickEdit
    let total = length $ foldMap (render xSize . fst) opts
    let approximate = let h = toRational ySize in ceiling (h * h / toRational total)
    let takenC = render xSize $ fst $ currentScroll opts
    let rollerC = case total - ySize of
          0 -> ySize
          1 -> ySize - 1
          _ -> max 1 (min (ySize - 2) approximate)
    let (rollerL, takenL, takenR) =
          evalBuffer xSize (ySize - length takenC, ySize - rollerC) opts
    setCursorPosition (yPos + rollerL) 0
    replicateM_ rollerC $ do
      setCursorColumn (xPos + xSize - 2)
      putChar '#'
      cursorDown 1
    setCursorPosition yPos xPos
    mapM_ (renderOption False) takenL
    mapM_ (renderOption True) takenC
    mapM_ (renderOption False) takenR
    where
    -- | Special render for old windows console.
    renderOption reversed opt = do
      let string = if T.length opt + 2 > getSum xSize
                     then T.take (getSum xSize - 5) opt <> "..."
                     else opt
      let (car, cdr) = T.splitAt 2 string
      case (car == "? ", reversed) of
        (False, True) -> renderReversedString string
        (False, False) -> renderString string
        (True, True) -> renderString $ T.pack "* " <> cdr
        (True, False) -> renderString $ T.pack ". " <> cdr
      cursorDown 1
      setCursorColumn xPos

instance ConsoleRender ProgressBlock where
  consoleRender (xPos, xSize) (yPos, ySize) _ = gets cProgress >>= \p -> liftIO $ do
    disableQuickEdit
    let barWidth = getSum $ xSize - 11
    let percents = reverse $ take (6 :: Int) $ reverse (showFFloat (Just 2) p "%") <> repeat ' '
    let (leftProg, rightProg) = ((barWidth * round p) `div` 100, barWidth - leftProg)
    setCursorPosition (yPos + min 2 ySize - 1) xPos
    putStr $ "[" <> replicate leftProg '#' <> replicate rightProg '.' <> "] " <> percents

instance ConsoleRender HiddenBlock where
  consoleRender _ _ _ = return ()

-- | Calculate parameters for roller size and position.
evalBuffer :: Size                    -- ^ Width of the options block.
           -> (Size, Size)            -- ^ Number of lines visible on the screen and
                                      -- number of lines not allocated by the roller.
           -> OptionsBlock            -- ^ The options block
           -> (Size, [Text], [Text])  -- ^ Space before the roller,
                                      -- number of non-marked lines before and
                                      -- after the choosen option.
evalBuffer width (Sum scrollLeft, Sum rollerLeft) opts = (Sum rollerL, takenL, takenR)
  where
  left = leftScroll opts >>= reverse . render width . fst
  right = rightScroll opts >>= render width . fst

  half = toRational scrollLeft / 2
  (halfL, halfR) = (floor half, ceiling half) :: (Int, Int)
  (takenL, takenR) = first reverse $ case () of
    _ | halfL > length left  -> (left, take (scrollLeft - length left) right)
      | halfR > length right -> (take (scrollLeft - length right) left, right)
      | otherwise            -> (take halfL left, take halfR right)

  skipL = length left - length takenL
  rollerL' = floor $ toRational rollerLeft * toRational skipL
                   / toRational (skipL + length right - length takenR)

  rollerL = case rollerLeft of
    0 -> 0
    1 -> skipL
    _ -> if skipL == 0 then 0 else max 1 rollerL'

renderString :: MonadIO m => Text -> m ()
renderString = liftIO . T.putStr

renderReversedString :: MonadIO m => Text -> m ()
renderReversedString str = do
  liftIO $ putStr $ setSGRCode [SetSwapForegroundBackground True]
  renderString str
  liftIO $ putStr $ setSGRCode [Reset]

renderBlock :: (MonadState ConsoleUI m, MonadIO m) => ConsoleNode -> m ()
renderBlock (x, y, Wrap b) = consoleRender x y b

renderFrame :: MonadIO m => ConsoleFrame -> m ()
renderFrame (_, _, HiddenFrame) = return ()
renderFrame (_, _, NormalFrame) = return ()
renderFrame ((xPos, xSize), (yPos, ySize), BorderedFrame) = liftIO $ do
  setCursorPosition yPos xPos
  putStr $ "+" <> replicate (xSize - 4) '-' <> "+"
  replicateM_ (ySize - 2) $ do
    cursorDown 1
    setCursorColumn xPos
    putStr $ "|" <> replicate (xSize - 4) ' ' <> "|"
  cursorDown 1
  setCursorColumn xPos
  putStr $ "+" <> replicate (xSize - 4) '-' <> "+"

renderLogs :: (MonadState ConsoleUI m, MonadIO m) => m ()
renderLogs = gets (\cui -> (cWidth cui, cHeight cui, cLogs cui)) >>= \(w, h, ls) -> do
  let lines = Sum $ min 8 (getSum h `quot` 2 - 4)
  renderFrame ((1, w), (h - lines - 2, lines + 2), BorderedFrame)
  setCursorPosition (h - lines - 2) 3
  forM_ (S.reverse $ S.take (getSum lines) ls) $ \l -> liftIO $ do
    cursorDown 1
    setCursorColumn 3
    renderString (T.take (getSum $ w - 6) l)
  liftIO $ hFlush stdout

renderProgress :: (MonadState ConsoleUI m, MonadIO m) => m ()
renderProgress = do
  blocks <- gets cBlocks
  patch <- gets (getPatch . cPatch)
  mapM_ (\node@(_, _, Wrap (_ :: b)) ->
    maybe (return ())
          (\_ -> renderBlock (third patch node) >> liftIO (hFlush stdout))
          (eqT :: Maybe (b :~: ProgressBlock))) blocks

--------------------------------------- Main ---------------------------------------------

console :: Config -> FrontChannel -> BackChannel -> IO ()
console config (FrontChannel front) (BackChannel back) = withConsole sigHandler wait $ do
  r_tid <- forkIO $ handleUserInput (atomically . writeTQueue front . UserInput)
  let ?isFallback = not wait
  let ?front = front
  let ?back = back
  let fall msg = let ?isFallback = True in ((`runReaderT` config)
                                        >>> (`evalStateT` FUI)
                                        >>> try) (gui (logMessage msg >> mainScreen))
  res <- liftIO T.size >>= \case
    Nothing ->
      fall "No console was found. Start fallback mode."
    Just (T.Window height width)
      | ?isFallback ->
        fall "Start fallback mode."
      | height < 19 || width <= 30 ->
        fall "Console windows is too small. Start fallback mode."
      | otherwise -> do
        cui <- CUI width height 0 empty mempty <$> patchAny
        try (evalStateT (runReaderT (gui mainScreen) config) cui)
  killThread r_tid
  case res of
    Left (err :: SomeException) -> do
      print err
      stack <- whoCreated err
      case (fromException err, stack) of
        (Just ExitSuccess, _) -> return ()
        (_, []) -> return ()
        (_, [_]) -> return ()
        _ -> mapM_ putStrLn stack
      throwIO err
    Right{} -> return ()
  where
  wait = not $ config ^. runConfig.skipAll
  sigHandler = atomically $ writeTQueue front $ Abort (ExitFailure errorInterrupt)

writeEventRun :: (?back :: TQueue BackMessage, MonadIO m)
              => BackMessage -> (forall a. Interface a -> m a) -> Interface b -> m b
writeEventRun event action next = do
  liftIO (atomically $ writeTQueue ?back event)
  action next

-- | Default interpreter.
gui :: (?isFallback :: Bool, ?front :: TQueue FrontMessage, ?back :: TQueue BackMessage
      , MonadState ctx m
      , MonadReader Config m
      , MonadIO m
      , MonadConsole m)
    => Interface a -> m a
gui (Pure x) = pure x
gui (Free (InitInterface next)) = clearScreen >> renderUI >> gui next
gui (Free (StopInteractive code)) = clearScreen >> stopRendering code

gui (Free (WriteEvent event next)) = writeEventRun event gui next
gui x@(Free (WaitEvent next)) = do
  renderUI
  liftIO (atomically $ readTQueue ?front) >>= \case
    Abort code -> gui (stopInteractive code)
    LogMessage m -> gui (logMessage m >> x)
    Progress p -> gui (updateProgress p >> x)
    UserInput (ResizeWindow h w) -> resizeWindow (Sum h) (Sum w) >> gui x
    event -> gui (next event)

gui (Free (NewBlock block next)) = appendBlockUI block >>= gui . next
gui (Free (UpdateBlock f ptr next)) = updateBlockUI f ptr >> gui next
gui (Free (NewFrame next)) = newFrameUI >>= gui . next
gui (Free (FillFrame ptr block next)) = fillFrameUI ptr block >>= gui . next

gui (Free (GetInput ptr next)) = readInputUI ptr >>= gui . next
gui (Free (GetOption ptr next)) = readOptionsUI ptr >>= gui . next

gui (Free (UpdateLog text next)) = do
  current_time <- getTimeStamp
  updateLogs (T.pack current_time <> text)
  gui next

gui (Free (SetProgress new next)) = do
  setProgress new >> gui next

gui (Free (UpdateProgress new next)) = do
  gui (logMessage $ "Scan progress: " <> T.pack (showFFloat (Just 15) new ""))
  gui (Free (SetProgress new next))

gui (Free (GetConfig next)) = ask >>= gui . next
gui (Free (UpdateConfig f next)) = do
  new <- reader f
  gui (writeEvent $ ConfigChange new)
  local (const new) (gui next)
