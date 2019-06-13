{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module System.Console.Terminal.Size.Types (
   Window(..)
 ) where

import GHC.Generics
import Data.Data

-- | Terminal window width and height.
data Window a = Window { height :: a, widgh :: a } deriving (Functor, Foldable, Traversable, Generic, Generic1, Eq, Show, Read, Data)
