{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           SaaS.Prelude.Common hiding (Alternative(..), withFile, toList)
import           Test.Templates
import           Test.SaaS.Instances    ()
import           Agent.Types
import           Agent.UI
import           Agent.UI.Core
import           Agent.Frontend.Console
import           Agent.Frontend.Console.Internal
import           Network.SocksFusion    (wait)

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Success, Args(..), output)
import           System.Random

import           System.IO
import           System.Exit            (ExitCode(..))
import           Data.Time

import           GHC.Exts               (IsList(..), IsString(..))
import           GHC.Err                (error)
import           Data.Typeable
import           Control.DeepSeq

import           Control.Monad.State
import           Data.Monoid     hiding ((<>))
import           Data.Proxy
import           Data.List              (init, reverse, sortOn)
import qualified Data.Sequence       as S
import           Data.Default.Class

import           Lens.Micro.Platform
import           Data.Aeson

import qualified Data.Text           as T
import           Text.Read              (read, readMaybe)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

--------------------------------------- Utils --------------------------------------------

shouldLike :: (HasCallStack, Exception e) => IO (Either e a) -> Selector e -> Expectation
shouldLike action p = action >>= \case
   Right{} -> expectationFailure "did not get expected exception"
   Left e  -> p e `unless` expectationFailure ("predicate failed on expected exception "
                                            <> show e)

isThreadKilled :: Selector AsyncException
isThreadKilled = (== ThreadKilled)

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> (ModifiedJulianDay . (2000 +) <$> arbitrary)
                      <*> (fromRational . toRational <$> choose (0 :: Double, 86400))

$(genericArbitrary ''TokenSource)
$(genericArbitrary ''Token)
deriving instance Eq UserInput
deriving instance Eq FrontMessage
deriving instance Eq LogProgress
deriving instance Show FrontMessage
deriving instance Show TokenSource
deriving instance Show Token
deriving instance Show LogProgress
instance Show UserInput where
  show (ResizeWindow h w) = "ResizeWindow " <> show h <> " " <> show w
  show (KeyChar c) = [c]
  show (KeyNum a) = show a
  show KeyBackspace = "\DEL"
  show KeyDelete = "\ESC[3"
  show KeyEnter = "\n"
  show KeyEsc = "\ESC"
  show KeyUp = "\ESC[A"
  show KeyDown = "\ESC[B"
  show KeyRight = "\ESC[C"
  show KeyLeft = "\ESC[D"

instance Arbitrary UserInput where
  arbitrary = arbitrary >>= \case
    Small 0 -> oneof $ map pure [ KeyBackspace, KeyDelete, KeyEsc, KeyEnter, KeyUp
                                , KeyDown, KeyRight, KeyLeft ]
    (_ :: Small Int) -> do
      c <- suchThat arbitraryPrintableChar (< '\128')
      return $ maybe (KeyChar c) KeyNum $ readMaybe [c]

--------------------------------------- IO -----------------------------------------------

readInputTest :: TQueue FrontMessage -> Spec
readInputTest ch = do
  it "One letter" $ do
    withFile "/proc/self/fd/0" AppendMode (\h -> hPutStr h "a" >> hFlush h)
    wait 1
    res <- atomically (flushTQueue ch)
    res `shouldBe` [UserInput $ KeyChar 'a']
  it "Two letters" $ do
    withFile "/proc/self/fd/0" AppendMode (\h -> hPutStr h "ab" >> hFlush h)
    wait 1
    res <- atomically (flushTQueue ch)
    res `shouldBe` map UserInput [KeyChar 'a', KeyChar 'b']
  prop "Random" $ \ks -> do
    withFile "/proc/self/fd/0" AppendMode (\h -> hPutStr h (foldMap show ks) >> hFlush h)
    wait 1
    res <- atomically (flushTQueue ch)
    res `shouldBe` map UserInput ks

--------------------------------------- ScrollBuffer -------------------------------------

deriving instance (Eq a) => Eq (ScrollBuffer a)
deriving instance (Show a) => Show (ScrollBuffer a)

instance (Arbitrary a) => Arbitrary (ScrollBuffer a) where
  arbitrary = ScrollBuffer <$> arbitrary <*> arbitrary <*> arbitrary

instance IsString (Wrap PrettyConsole, Interface ()) where
  fromString str = (Wrap str, pure ())

scrollBufferTests :: Spec
scrollBufferTests = modifyMaxSuccess (* 100) $ do
  prop "IsList" $ \(NonEmpty x) -> toList (asProxyTypeOf (fromList x) proxy) === x
  prop "Length" $ \(NonEmpty x) -> length (asProxyTypeOf (fromList x) proxy) === length x
  prop "|>" $ \(x, y :: Small Int) -> toList (x |> y) === toList x <> [y]
  prop "<|" $ \(x, y :: Small Int) -> toList (y <| x) === [y] <> toList x
  prop "Stability" $ \(x :: ScrollBuffer (Small Int), y, z) ->
          toList x === toList (goBackward x)
     .&&. toList x === toList (goForward x)
     .&&. toList x === toList (setPosition y x)
     .&&. toList x === toList (setPosition z (setPosition y x))
  prop "Foldable" $ \(x :: ScrollBuffer (Small Int)) -> sum x === sum (toList x)
  prop "Set position" $ \(x, y :: Small Int, z) ->
    let scroll = setPosition (length x + 1) $ fromList (x <> (y:z)) in
      counterexample (show scroll) (y === currentScroll scroll)
  prop "Go forward" $ \(NonEmpty x, y :: Small Int, z) ->
    let scroll = fromList (x <> (y:z)) in
      currentScroll (goForward $ setPosition (length x) scroll) === y
  prop "Go backward" $ \(x, y :: Small Int, NonEmpty z) ->
    let scroll = fromList (x <> (y:z)) in
      currentScroll (goBackward $ setPosition (length x + 2) scroll) === y
  describe "Eval buffer" $ do
    it "Fits" $ evalBuffer 100 (7, 0) (setPosition 4 advancedConfig)
            === (0, ["1", "2", "3"], ["5", "6", "7", "8"])
    it "Lack of one 1" $ evalBuffer 100 (6, 1) (setPosition 4 advancedConfig)
                     === (0, ["1", "2", "3"], ["5", "6", "7"])
    it "Lack of one 2" $ evalBuffer 100 (6, 1) (setPosition 5 advancedConfig)
                     === (1, ["2", "3", "4"], ["6", "7", "8"])
  where
  proxy = Proxy :: Proxy (ScrollBuffer (Small Int))
  advancedConfig = ["1", "2", "3", "4", "5", "6", "7", "8"]

--------------------------------------- UITree -------------------------------------------

type TestTree = UITree (Sum (Small Int)) (Sum (Small Int))

newtype PrettyChar = PrettyChar { getPrettyChar :: Char }   deriving Enum
newtype RandRange = RandRange Double                        deriving Random
newtype TestPair f l = TestPair (UIKey, UITree f l)         deriving Show

instance NFData UIKey where
  rnf (UIKey a) = a `deepseq` ()
instance (NFData a) => NFData (Small a) where
  rnf (Small a) = a `deepseq` ()
instance (NFData f, NFData l) => NFData (UITree f l) where
  rnf (Leaf k v) = k `deepseq` v `deepseq` ()
  rnf (Branch k v ns) = k `deepseq` v `deepseq` ns `deepseq` ()

instance Bounded PrettyChar where
  minBound = PrettyChar '0'
  maxBound = PrettyChar 'Z'

instance Arbitrary UIKey where
  arbitrary = UIKey . map getPrettyChar <$> liftArbitrary arbitraryBoundedEnum
  shrink (UIKey []) = []
  shrink (UIKey (x:xs)) = [UIKey (x:init xs)]

instance Bounded RandRange where
  maxBound = RandRange 1
  minBound = RandRange 0

instance Arbitrary RandRange where
  arbitrary = arbitraryBoundedRandom

instance (Arbitrary f, Arbitrary l) => Arbitrary (UITree f l) where
  arbitrary = snd <$> arbitraryTreeWithKey
  shrink = shrinkUITree

instance (Arbitrary f, Arbitrary l) => Arbitrary (TestPair f l) where
  arbitrary = TestPair <$> arbitraryTreeWithKey
  shrink _ = []

arbitraryTreeWithKey :: (Arbitrary f, Arbitrary l) => Gen (UIKey, UITree f l)
arbitraryTreeWithKey = do
  list <- arbitrary :: Gen [()]
  v <- arbitrary
  (\(_, key, tree) -> (key, tree)) <$> foldM go (1, UIKey [], Branch (UIKey []) v []) list
  where
  third f (a, b, c) = (a, b, f c)
  roll :: Double -> a -> a -> Gen a
  roll count v1 v2 = do
    RandRange r <- arbitrary
    pure $ if 1 / (count + 1) > r then v1 else v2
  go (count, res, Leaf k _) () = do
    v1 <- arbitrary
    v2 <- arbitrary
    let key = downKey k
    res' <- roll count res key
    pure (count + 1, res', Branch k v1 [Leaf key v2])
  go (count, res, Branch k v ns) () = do
    v1 <- arbitrary
    v2 <- arbitrary
    case reverse ns of
      [] -> do
        let key = downKey k
        res' <- roll count res key
        pure (count + 1, res', Branch k v [Leaf key v2])
      n'@Leaf{}:ns' -> do
        let key = succ $ getKey n'
        res' <- roll count res key
        let mBranch = Branch (getKey n') v1 []
        (c, r, t) <- oneof [ pure (count + 1, res', Leaf key v2:n':ns')
                           , third (:ns') <$> go (count, res, mBranch) () ]
        pure (c, r, Branch k v $ reverse t)
      n'@Branch{}:ns' -> do
        let key = succ $ getKey n'
        res' <- roll count res key
        (c, r, l) <- oneof [ pure (count + 1, res', Leaf key v2:n':ns')
                           , pure (count + 1, res', Branch key v1 []:n':ns')
                           , third (:ns') <$> go (count, res, n') () ]
        pure (c, r, Branch k v $ reverse l)

instance Show (Sum (Small Int) -> Sum (Small Int)) where
  show f = show (f 0)

shrinkUITree :: UITree f l -> [UITree f l]
shrinkUITree Leaf{} = []
shrinkUITree (Branch _ _ []) = []
shrinkUITree (Branch k v (_:ns)) = [Branch k v ns]

treeToList :: UITree f l -> [(UIKey, Either f l)]
treeToList = go []
  where
  go xs (Leaf k v) = xs <> [(k, Right v)]
  go xs (Branch k v ns) = xs <> [(k, Left v)] <> (ns >>= treeToList)

uiTreeTests :: Spec
uiTreeTests = modifyMaxSuccess (* 100) $ do
  describe "UIKey" $
    prop "Enum" $ \x -> pred (x <> UIKey ['5']) === x <> UIKey ['4']
                   .&&. succ (x <> UIKey ['5']) === x <> UIKey ['6']
                   .&&. succ (pred x) === x
  describe "UITree" $ parallel $ do
    modifyMaxSuccess (`div` 100) $ prop "Timed" $ within timeFrame $ \key tree val ->
      total ( member key (tree :: TestTree)
            , delete key tree
            , append val tree
            , updateWithKeyAndReturn (\k v -> (succ k, pred v)) key mempty tree )
    prop "Length" $ \(x :: TestTree) -> length x === length (treeToList x)
    prop "Order" $ \(x :: TestTree) -> treeToList x === sortOn fst (treeToList x)
    prop "Foldable" $ \(x :: TestTree) -> let fun = either (const 0) id . snd in
      foldl (+) 0 x === foldr (+) 0 x .&&. sum x === foldMap fun (treeToList x)
    prop "Common" $ \(tree :: TestTree) -> let list = treeToList tree in
      all (\(k, v) -> lookup k tree == Just v) list
    prop "Delete" $ \(TestPair (key, tree :: TestTree)) ->
      member key tree .&&. (key == UIKey "" .||. not (member key $ delete key tree))
    prop "Append" $ \(tree :: TestTree) f l -> let list = treeToList tree
                                                   (keyF, treeF) = appendBranch f tree
                                                   (keyL, treeL) = appendReturn l tree in
           counterexample (show treeF) (treeToList treeF === list <> [(keyF, Left f)])
      .&&. counterexample (show treeL) (treeToList treeL === list <> [(keyL, Right l)])
    prop "Update" $ \(TestPair (key, tree :: TestTree)) ->
      let (res, tree') = updateWithKeyAndReturn (curry $ pred *** succ) key mempty tree in
        case lookup key tree of
          Nothing -> property False
          Just Left{} -> property True
          Just (Right v) -> length tree === length tree'
                       .&&. res === pred key
                       .&&. lookup key tree' === Just (Right $ succ v)
  where
  timeFrame = 5000000

--------------------------------------- FrameTree ----------------------------------------

data FakeBlock
instance UIBlock FakeBlock where
  maxSize = error "maxSize FakeBlock"
  minSize = error "minSize FakeBlock"

instance Eq TextBlock where
  TextBlock dx == TextBlock dy = PP.displayS (PP.renderPretty 1.0 80 dx) ""
                              == PP.displayS (PP.renderPretty 1.0 80 dy) ""
deriving instance Eq FakeBlock
deriving instance Show FakeBlock

instance Show (Wrap PrettyConsole) where
  show s = unwords $ T.unpack <$> render (80 :: Int) s
instance {-# OVERLAPPING #-} Show (Interface ()) where
  show = const "Action"

instance Show (Wrap ConsoleRender) where
  show (Wrap (b :: b)) = either id (const "Test block") $ do
    maybe (pure ()) Left $ (\Refl -> show b) <$> (eqT :: Maybe (b :~: TextBlock))
    maybe (pure ()) Left $ (\Refl -> show b) <$> (eqT :: Maybe (b :~: InputBlock))
    maybe (pure ()) Left $ (\Refl -> show b) <$> (eqT :: Maybe (b :~: OptionsBlock))
    maybe (pure ()) Left $ (\Refl -> show b) <$> (eqT :: Maybe (b :~: ProgressBlock))
    maybe (pure ()) Left $ (\Refl -> show b) <$> (eqT :: Maybe (b :~: HiddenBlock))

instance Arbitrary FrameBlock where
  arbitrary = oneof [pure NormalFrame, pure BorderedFrame, pure HiddenFrame]

instance Arbitrary ProgressBlock where
  arbitrary = pure ProgressBlock
  shrink _ = []

instance Arbitrary HiddenBlock where
  arbitrary = HiddenBlock . (`mod` 5) <$> arbitrary

instance Arbitrary TextBlock where
  arbitrary = pure $ TextBlock "Test text"

-- | Only known blocks are enumerated here.
instance Arbitrary (Wrap ConsoleRender) where
  arbitrary =
    oneof [ Wrap <$> (arbitrary :: Gen ProgressBlock)
          , Wrap <$> (arbitrary :: Gen HiddenBlock)
          , Wrap <$> (arbitrary :: Gen TextBlock)
          , pure $ Wrap (InputBlock "Test input: " "data")
          , pure $ Wrap (fromList [ (Wrap ("Option 1" :: String), return ())
                                  , (Wrap ("Option 2" :: String), return ())
                                  , (Wrap ("Option 3" :: String), return ())
                                  , (Wrap ("Option 4" :: String), return ())
                                  , (Wrap ("Option 5" :: String), return ())
                                  ] :: OptionsBlock) ]

instance Eq (Wrap ConsoleRender) where
  Wrap (_ :: bx) == Wrap (_ :: by) = isJust (eqT :: Maybe (bx :~: by))

frameTreeTests :: Spec
frameTreeTests = modifyMaxSuccess (* 100) $ parallel $ do
  prop "FindBlock 1" $ \(TestPair (key, tree)) -> case lookup key tree of
    Nothing -> property False
    Just Right{} -> findBlock (UIPtr key :: UIPtr FakeBlock) tree === Nothing
    Just (Left v) -> let f (x, y, _) = (x, y) in (f <$> findFrame (UIPtr key) tree)
                                             === Just (f v)
  prop "FindBlock 2" $ \tree' (val :: TextBlock) ->
    let (key, tree) = appendReturn ((0, 0), (0, 0), Wrap val) tree' in
      findBlock (UIPtr key :: UIPtr TextBlock) tree === Just ((0, 0), (0, 0), val)
  prop "ResizeTree" $ \tree' -> let tree = resizeTree (0, maxBound) (0, maxBound) tree' in
    counterexample (show tree) (thrd $ testResized (0, 0, property True) tree)
    where
    thrd (_, _, x) = x
    testResized :: (Position, Position, Property)
                -> ConsoleTree
                -> (Position, Position, Property)
    testResized (x, y, res) (Leaf _ ((xPos, _), (yPos, ySize), block)) =
      (x, y + ySize + 1, res .&&. y === yPos .&&. x === xPos
                             .&&. ySize === maxSize maxBound block)
    testResized (x, y, res) (Branch _ (_, _, HiddenFrame) _) = (x, y, res)
    testResized (x, y, res) n@(Branch _ ((xPos, _), (yPos, ySize), frame) ns) =
           ( x
           , if ySize == 0 then y else y + ySize + 1
           , res .&&. y === yPos .&&. x == xPos
        .&&. fst (branchSize maxBound n) === ySize
        .&&. thrd (foldl testResized (property True & if frame == NormalFrame
                                                        then (x, y,)
                                                        else (x + 2, y + 1,)) ns))

--------------------------------------- UI -----------------------------------------------

withStateT2 :: Functor m => (s' -> s) -> (s -> s') -> StateT s m a -> StateT s' m a
withStateT2 f g m = StateT $ fmap (second g) . runStateT m . f

coerceState :: (Coercible s s', Functor m) => StateT s' m a -> StateT s m a
coerceState = withStateT2 coerce coerce

liftCoerceState :: (MonadTrans t, Coercible s s', Monad m)
                => StateT s' m a -> t (StateT s m) a
liftCoerceState = lift . coerceState

newtype TestUI = TUI ConsoleUI

instance Default TestUI where
  def = TUI $ CUI 1000 1000 0 empty mempty (Patch id)

instance Default Config where
  def = Config (RunConfig False False False False False)
               (APIConfig (SafeURL "http://api.com") (AC ""))
               (TokenConfig (TP "test.token"))
               (ProxyConfig (read "proxy.com:443") Nothing)

-- | Add fake timestamp to drop it later.
ts :: T.Text -> T.Text
ts = (T.replicate 18 " " <>)

instance MonadIO m => MonadConsole (ExceptT ExitCode (StateT TestUI m)) where
  clearScreen = do
    liftCoerceState $ modify (\cui -> cui { cBlocks = empty })
    updateLogs (ts "ClearScreen")
  resizeWindow _ _ = updateLogs (ts "ResizeWindow")
  stopRendering e@ExitSuccess = updateLogs (ts "StopRendering 0") >> throwError e
  stopRendering e@(ExitFailure c) = do
    updateLogs (ts $ "StopRendering " <> showT c)
    throwError e
  updateLogs text' = let text = T.drop 18 text' in -- Drop timestamp
    liftCoerceState $ modify (\cui -> cui { cLogs = cLogs cui S.:|> text })
  setProgress new = liftCoerceState $ modify (\cui -> cui { cProgress = new })
  renderUI = return ()
  appendBlockUI = liftCoerceState . appendBlockCUI
  updateBlockUI f = liftCoerceState . updateBlockCUI f
  newFrameUI = liftCoerceState newFrameCUI
  fillFrameUI b = liftCoerceState . fillFrameCUI b
  readInputUI = liftCoerceState . readInputCUI
  readOptionsUI = liftCoerceState . readOptionsCUI

instance IsString UIKey where
  fromString = UIKey

deriving instance Eq Config
deriving instance Eq RunConfig
deriving instance Eq APIConfig
deriving instance Eq TokenConfig
deriving instance Eq ProxyConfig
deriving instance Eq TokenPath
deriving instance Eq APICode
deriving instance Eq BackMessage
deriving instance Show Config
deriving instance Show RunConfig
deriving instance Show APIConfig
deriving instance Show TokenConfig
deriving instance Show ProxyConfig
deriving instance Show BackMessage

defTime :: UTCTime
defTime = read "2020-01-01 12:00:00"

oldToken :: Token
oldToken = Token "saved" defTime (addUTCTime 60 defTime) Loaded

apiToken :: Token
apiToken = let t = addUTCTime nominalDay defTime in Token "saved" t t Received

explToken :: Token
explToken = let t = addUTCTime (nominalDay * 2) defTime in Token "saved" t t Explicit

runUITest :: (?isFallback :: Bool, ?front :: TQueue FrontMessage, ?back :: TQueue BackMessage)
          => Config -> [FrontMessage] -> IO (Double, ConsoleTree, S.Seq T.Text, [BackMessage], ExitCode)
runUITest config input = do
  mapM_ (atomically . writeTQueue ?front) input
  res <- runStateT (runExceptT (runReaderT (gui mainScreen) config)) def
  output <- atomically (flushTQueue ?back)
  return $ case res of
    (Left code, TUI (CUI _ _ prog tree logs _)) -> (prog, tree, logs, output, code)
    (Right{}, TUI (CUI _ _ prog tree logs _)) -> (prog, tree, logs, output, ExitSuccess)

uiTests :: (?front :: TQueue FrontMessage, ?back :: TQueue BackMessage) => Spec
uiTests = do
  describe "Main" mainTests
  describe "Skip" skipTests

mainTests :: (?front :: TQueue FrontMessage, ?back :: TQueue BackMessage) => Spec
mainTests = let ?isFallback = False in do
  it "Ctrl-C" $ do
    res <- runUITest def [Abort (ExitFailure errorInterrupt)]
    res `shouldBe` (0, empty, [ "ClearScreen"
                              , "ClearScreen"
                              , "StopRendering 2"
                              ], [], ExitFailure errorInterrupt)
  it "Fast" pending

skipTests :: (?front :: TQueue FrontMessage, ?back :: TQueue BackMessage) => Spec
skipTests = let ?isFallback = True in do
  it "Ctrl-C" $ do
    res <- runUITest def [Abort (ExitFailure errorInterrupt)]
    res `shouldBe` (0, empty, [ "ClearScreen"
                              , "ClearScreen"
                              , "StopRendering 2"
                              ], [RunToken], ExitFailure errorInterrupt)
  it "Fast" $ do
    res <- runUITest def [
       UserInput KeyEnter
     , FinishToken (Right [oldToken])
     , Progress 0
     , Progress 100
     , FinishProxy (Right ())
     , Progress 0
     , Progress 50
     , FinishProxy (Left (ExitFailure errorNetwork))
     ]
    res `shouldBe` (50, empty, [
       "ClearScreen"
     , "Scan progress: 0.000000000000000"
     , "Scan progress: 0.000000000000000"
     , "Scan progress: 100.000000000000000"
     , "Scan finished"
     , "ClearScreen"
     , "ClearScreen"
     , "Scan progress: 0.000000000000000"
     , "Scan progress: 0.000000000000000"
     , "Scan progress: 50.000000000000000"
     , "ExitFailure 7"
     , "ClearScreen"
     , "StopRendering 7"
     ], [
       RunToken
     , SaveTokens [oldToken]
     , ConfigChange ((proxyConfig.proxyToken ?~ oldToken { tokenSource = Explicit }) def)
     , RunProxy
     , RunProxy
     ], ExitFailure errorNetwork)
  it "API" pending

--------------------------------------- Main ---------------------------------------------

main :: IO ()
main = getZonedTime >>= \time -> do
  (BackChannel back, FrontChannel front) <- newChannels
  let ?back = back
  let ?front = front
  void $ forkIO (handleUserInput (atomically . writeTQueue front . UserInput))
  hspec $ do
    parallel $ do
      describe "Read" $
        prop "LogProcess" $ \(n :: Double) ->
            readMaybe (formatTime defaultTimeLocale "%b %d %X - Scan progress: " time
                    <> show n)
        === Just (LP $ round n)
      describe "JSON" $
        prop "Token" $ \(t :: Token) -> eitherDecode (encode t) === Right t
      describe "ScrollBuffer" scrollBufferTests
      describe "UITree" uiTreeTests
      describe "FrameTree" frameTreeTests
      describe "UI" uiTests
    describe "UserInput" (readInputTest front)
