{-# OPTIONS_HADDOCK hide, prune #-}

------------------------------------------------------------------------------------------
-- |
-- Module Agent.Frontend
--
-- Agent frontend.
--
------------------------------------------------------------------------------------------

module Agent.Frontend (
   frontend
 ) where

import           Prelude                (IO)
import           Agent.Types            (Config, FrontChannel, BackChannel)
import           Agent.Frontend.Console (console)

frontend :: Config -> FrontChannel -> BackChannel -> IO ()
frontend = console
