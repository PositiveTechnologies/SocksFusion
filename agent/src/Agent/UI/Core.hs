{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

------------------------------------------------------------------------------------------
-- |
-- Module: Agent.UI.Core
--
-- User interface core objects: frame tree, its keys, main block class
-- and basic instances, scrollbuffer used for options block.
--
------------------------------------------------------------------------------------------

module Agent.UI.Core (
 -- * UI
 -- ** Keys
   UIKey(..)
 , UIPtr(..)
 , getKey
 , downKey
 , nextLevel
 , leq
 -- ** Blocks
 , Wrap(..)
 , UIBlock(..)
 , FrameBlock(..)
 , ScrollBuffer(..)
 , TextBlock(..)
 , InputBlock(..)
 , ProgressBlock(..)
 , HiddenBlock(..)
 , textBlock
 , frameSize
 -- ** Tree
 , UITree(..)
 , empty
 , member
 , lookup
 , delete
 , append
 , appendReturn
 , appendBranch
 , update
 , updateWithKey
 , updateAndReturn
 , updateWithKeyAndReturn
 , bimapMTree_
 , bimapMTree
 , foldMapWithKey
 , traverseWithKey
 -- * ScrollBuffer
 , (|>)
 , (<|)
 , goForward
 , goBackward
 , setPosition
 ) where

import           Prelude         hiding (lookup, null)
import           Agent.Types

import           GHC.Exts               (Constraint, IsList(..), IsString(..))
import           Data.Typeable

import           Data.Foldable   hiding (toList)
import           Data.Maybe
import           Data.Monoid            (First(..))
import           Data.List              (genericLength)
import           Data.Bifunctor
import           Control.Monad          (void)

import           Data.Text              (Text)
import qualified Data.Text           as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP

#if MIN_VERSION_base(4,11,0)
import           Data.Semigroup         (Max(..), Sum(..))
#else
import           Data.Semigroup  hiding (getOption, First(..))
#endif

-- Because Data.List.deleteBy is a garbage.
deleteBy :: (a -> Bool) -> [a] -> [a]
deleteBy _ [] = []
deleteBy f (x:xs) = if f x then xs else x:deleteBy f xs

--------------------------------------- UI -----------------------------------------------

--------------------------------------- Keys

-- |
-- Keys used for navigating in UITree. Each key is just a printable string with
-- partial order based on prefixes. The length of the word means the deep of the node
-- in the tree.
newtype UIKey = UIKey [Char]                                deriving (Eq, Ord, Monoid, Semigroup, IsList)

-- |
-- Proxy type used in UITree construction protocol.
newtype UIPtr a = UIPtr { uiKey :: UIKey }
type role UIPtr nominal

instance Show UIKey where
  show (UIKey str) = show str

instance Enum UIKey where
  toEnum n = UIKey [toEnum (n + 48)]
  fromEnum (UIKey []) = 0
  fromEnum (UIKey (n:_)) = fromEnum n - 48
  succ (UIKey list) = UIKey $ case reverse list of
    [] -> []
    x:xs -> reverse (succ x:xs)
  pred (UIKey list) = UIKey $ case reverse list of
    [] -> []
    x:xs -> reverse (pred x:xs)

-- | Partial order on UIKeys.
leq :: UIKey -> UIKey -> Bool
UIKey  _ `leq` (UIKey []) = True
UIKey [] `leq` _ = False
UIKey (x:_) `leq` UIKey [y] = x == y
UIKey (x:xs) `leq` UIKey (y:ys) = x == y && UIKey xs `leq` UIKey ys

-- | Create deeper layer.
downKey :: UIKey -> UIKey
downKey = (<> UIKey ['0'])

-- | Test if the first argument is a child of the second one.
nextLevel :: UIKey -> UIKey -> Bool
nextLevel (UIKey [_]) (UIKey []) = True
nextLevel (UIKey []) _ = False
nextLevel _ (UIKey []) = False
nextLevel (UIKey (x:xs)) (UIKey (y:ys)) = x == y && nextLevel (UIKey xs) (UIKey ys)

--------------------------------------- Blocks

-- | Wrapper to incapsulate different items into one heterogeneous collection.
data Wrap (constraint :: * -> Constraint) where
  Wrap :: constraint a => a -> Wrap constraint

-- | Main class for objects passing through UITree construction protocol.
class (Typeable a) => UIBlock a where
  -- | Minimal block height rendered with given width.
  minSize :: Sum Int -> a -> Sum Int
  -- | Preferable height of the block with given width.
  maxSize :: Sum Int -> a -> Sum Int

-- | Different frame types.
data FrameBlock = NormalFrame | BorderedFrame | HiddenFrame deriving (Eq, Ord, Show, Bounded)

-- | Common text block to show plain text.
data TextBlock = TextBlock PP.Doc                           deriving (Show)

-- | Block consists of constant part and variable input.
data InputBlock = InputBlock Text String                    deriving (Eq, Show)

-- | Block indicating some type of progress object. Currently it is just progress bar.
data ProgressBlock = ProgressBlock                          deriving (Eq, Show)

-- | Block without content with given height.
data HiddenBlock = HiddenBlock (Sum Int)                    deriving (Eq, Show)

-- | Used for calculating UITree size.
frameSize :: Num a => FrameBlock -> a
frameSize NormalFrame = 0
frameSize HiddenFrame = 0
frameSize BorderedFrame = 2

-- | Monomorphic method for newBlock/fillFrame functions.
textBlock :: String -> TextBlock
textBlock = fromString

instance PrettyConsole (Wrap PrettyConsole) where
  render n (Wrap t) = render n t

instance IsString (Wrap PrettyConsole) where
  fromString = Wrap . T.pack

instance Semigroup FrameBlock where
  x <> y = getMax $ Max x <> Max y

instance Monoid FrameBlock where
  mappend = (<>)
  mempty = getMax mempty

instance IsString TextBlock where
  fromString = TextBlock . PP.fillSep . map PP.text . words

instance Semigroup TextBlock where
  TextBlock a <> TextBlock b = TextBlock $ a PP.<$> b

instance UIBlock (Wrap UIBlock) where
  minSize width (Wrap x) = minSize width x
  maxSize width (Wrap x) = maxSize width x

instance UIBlock TextBlock where
  minSize width (TextBlock doc) = genericLength $ lines $ PP.displayS (PP.renderPretty 1.0 (getSum width) doc) ""
  maxSize = minSize

instance UIBlock InputBlock where
  minSize _ _ = 1
  maxSize = minSize

instance UIBlock ProgressBlock where
  minSize _ _ = 1
  maxSize _ _ = 3

instance UIBlock HiddenBlock where
  minSize _ _ = 0
  maxSize _ (HiddenBlock n) = n

--------------------------------------- Tree

-- | Abstract prefix tree of UI elements.
data UITree f l where
  Leaf :: UIKey -> l -> UITree f l
  Branch :: UIKey -> f -> [UITree f l] -> UITree f l

deriving instance (Show f, Show l) => Show (UITree f l)
deriving instance (Eq f, Eq l) => Eq (UITree f l)

instance Foldable (UITree f) where
  foldMap f = foldMapWithKey (const f)

  null (Branch _ _ nodes) = and (null <$> nodes)
  null Leaf{} = False

  length Leaf{} = 1
  length (Branch _ _ nodes) = 1 + sum (length <$> nodes)

instance Bifunctor UITree where
  bimap _ g (Leaf k v) = Leaf k (g v)
  bimap f g (Branch k v ns) = Branch k (f v) (bimap f g <$> ns)

-- | Empty tree.
empty :: Monoid f => UITree f l
empty = Branch mempty mempty []

-- | Extract key of the root node.
getKey :: UITree f l -> UIKey
getKey (Leaf k _) = k
getKey (Branch k _ _) = k

-- | /O(log n)/. Check if an element is present in the tree.
member :: UIKey -> UITree f l -> Bool
member k = isJust . lookup k

-- | /O(log n)/. Value of the given key or Nothing if key is not present.
lookup :: UIKey -> UITree f l -> Maybe (Either f l)
lookup kx (Leaf ky vy) = if kx == ky then Just (Right vy) else Nothing
lookup kx (Branch ky vy nodes)
  | kx == ky = Just (Left vy)
  | kx `leq` ky = getFirst $ foldMap (First . lookup kx) nodes
  | otherwise = Nothing

-- | /O(log n)/. Delete a key and its value from the tree. When the key is not a member of the tree, the original tree is returned.
delete :: UIKey -> UITree f l -> UITree f l
delete _ y@Leaf{} = y
delete kx y@(Branch ky vy nodes)
  | kx `nextLevel` ky = Branch ky vy (deleteBy ((== kx) . getKey) nodes)
  | kx `leq` ky = Branch ky vy (delete kx <$> nodes)
  | otherwise = y

-- | /O(log n)/. Append a new value with the key equals to the (max key + 1).
append :: l -> UITree f l -> UITree f l
append v = snd . appendReturn v

-- | /O(log n)/. Append a new value with the key equals to the (max key + 1). Returns the of the new element.
appendReturn :: l -> UITree f l -> (UIKey, UITree f l)
appendReturn _ Leaf{} = error "Impossible appendReturn"
appendReturn v (Branch ky vy nodes) = (res, Branch ky vy newNodes)
  where
  (res, newNodes) = case reverse nodes of
    [] -> let key = downKey ky in (key, [Leaf key v])
    ns@(Leaf k _:_) -> let key = succ k in (key, reverse $ Leaf key v:ns)
    n@Branch{}:ns -> second (reverse . (:ns)) $ appendReturn v n

-- | /O(log n)/. Append a new branch value with the key equals to the (max key + 1). Returns the of the new element.
appendBranch :: f -> UITree f l -> (UIKey, UITree f l)
appendBranch _ Leaf{} = error "Impossible appendBranch"
appendBranch v (Branch ky vy nodes) = (res, Branch ky vy newNodes)
  where
  (res, newNodes) = case reverse nodes of
    [] -> let key = downKey ky in (key, [Branch key v []])
    ns@(Leaf k _:_) -> let key = succ k in (key, reverse $ Branch key v []:ns)
    n@Branch{}:ns -> second (reverse . (:ns)) $ appendBranch v n

-- | /O(log n)/. Update the value by the given with the given function. Do nothing if the value is not present.
update :: (l -> l) -> UIKey -> UITree f l -> UITree f l
update = updateWithKey . const

-- | /O(log n)/. Update the value by the given with the given function. Do nothing if the value is not present.
updateWithKey :: (UIKey -> l -> l) -> UIKey -> UITree f l -> UITree f l
updateWithKey f k = snd . updateWithKeyAndReturn (\k' -> ((),) . f k') k ()

-- | /O(log n)/. Update the value by the given with the given function and return something based on the value. Do nothing if the value is not present and return default output.
updateAndReturn :: (l -> (b, l)) -> UIKey -> b -> UITree f l -> (b, UITree f l)
updateAndReturn f = updateWithKeyAndReturn (const f)

-- | /O(log n)/. Update the value by the given with the given function and return something based on the value. Do nothing if the value is not present and return default output.
updateWithKeyAndReturn :: (UIKey -> l -> (b, l)) -> UIKey -> b -> UITree f l -> (b, UITree f l)
updateWithKeyAndReturn f k initial = first (fromMaybe initial . getFirst) . go
  where
  go (Leaf kx vx) = let (res, bx) = f kx vx in
    (if k == kx then pure res else mempty, Leaf kx bx)
  go n@(Branch kx vx ns)
    | k == kx = (mempty, n)
    | k `leq` kx = second (Branch kx vx) $ foldMap (second pure . go) ns
    | otherwise = (mempty, n)

-- | Bimap a tree with applicators and collect results. Some sort of traverse2.
bimapMTree :: Applicative m => (f -> m g) -> (l -> m n) -> UITree f l -> m (UITree g n)
bimapMTree _ g (Leaf k v) = Leaf k <$> g v
bimapMTree f g (Branch k v ns) = Branch k <$> f v <*> traverse (bimapMTree f g) ns

-- | Bimap a tree with applicators and ignore results. Some sort of traverse2_.
bimapMTree_ :: Applicative m => (f -> m ()) -> (l -> m ()) -> UITree f l -> m ()
bimapMTree_ f g = void . bimapMTree f g

foldMapWithKey :: Monoid m => (UIKey -> l -> m) -> UITree f l -> m
foldMapWithKey f (Leaf k v) = f k v
foldMapWithKey f (Branch _ _ nodes) = fold (foldMapWithKey f <$> nodes)

traverseWithKey :: Applicative t => (UIKey -> l -> t a) -> UITree f l -> t (UITree f a)
traverseWithKey f (Leaf k v) = Leaf k <$> f k v
traverseWithKey f (Branch k v nodes) = Branch k v <$> sequenceA (traverseWithKey f <$> nodes)

--------------------------------------- ScrollBuffer -------------------------------------

-- |
-- Buffer used for scrolling inside options block. Consists of current position and
-- two linked lists represent previous and next elements sorted from closest to furthest.
data ScrollBuffer a = ScrollBuffer { leftScroll :: [a]
                                   , currentScroll :: a
                                   , rightScroll :: [a] }

instance Semigroup (ScrollBuffer a) where
  ScrollBuffer lx cx rx <> ScrollBuffer ly cy ry =
    ScrollBuffer lx cx (rx <> reverse ly <> (cy:ry))

instance Functor ScrollBuffer where
  fmap f (ScrollBuffer lx cx rx) = ScrollBuffer (f <$> lx) (f cx) (f <$> rx)

instance Foldable ScrollBuffer where
  foldMap f x = foldMap f (toList x)

instance IsList (ScrollBuffer a) where
  type Item (ScrollBuffer a) = a
  toList (ScrollBuffer lx cx rx) = reverse lx <> (cx:rx)
  fromList [] = error "ScrollBuffer cannot be empty"
  fromList (x:xs) = ScrollBuffer [] x xs

-- | Append a value to the end.
(|>) :: ScrollBuffer a -> a -> ScrollBuffer a
ScrollBuffer lx cx rx |> a = ScrollBuffer lx cx (rx <> [a])

-- | Prepend a value to the start.
(<|) :: a -> ScrollBuffer a -> ScrollBuffer a
a <| ScrollBuffer lx cx rx = ScrollBuffer (lx <> [a]) cx rx

-- | Navigate to the next element. Do nothing if current element is the last one.
goForward :: ScrollBuffer a -> ScrollBuffer a
goForward x@(ScrollBuffer _ _ []) = x
goForward (ScrollBuffer lx cx (r:rx)) = ScrollBuffer (cx:lx) r rx

-- | Navigate to the previous element. Do nothing if current element is the first one.
goBackward :: ScrollBuffer a -> ScrollBuffer a
goBackward x@(ScrollBuffer [] _ _) = x
goBackward (ScrollBuffer (l:lx) cx rx) = ScrollBuffer lx l (cx:rx)

-- | Navigate to the choosen element started from 1.
setPosition :: Int -> ScrollBuffer a -> ScrollBuffer a
setPosition n x@(ScrollBuffer lx cx rx)
  | n <= ln = case splitAt (ln - n) lx of
    (_, []) -> setPosition 1 x
    (ry, cy:rest) -> ScrollBuffer rest cy (reverse ry <> (cx:rx))
  | n > ln + 1 = case splitAt (n - ln - 2) rx of
    (_, []) -> setPosition (length x) x
    (ly, cy:rest) -> ScrollBuffer (reverse ly <> (cx:lx)) cy rest
  | otherwise = x
  where
  ln = length lx
