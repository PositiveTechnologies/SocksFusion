{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}

------------------------------------------------------------------------------------------
-- |
-- Module: Agent.Frontend.Console.Internal
--
-- Adapter to termios API.
--
------------------------------------------------------------------------------------------

module Agent.Frontend.Console.Internal (
   ansiSupported
 , withConsole
 , handleUserInput
 , enableQuickEdit
 , disableQuickEdit
 ) where

import           Prelude
import           Agent.Types

import           System.IO
import           Control.Concurrent
import           Foreign.C.Types
import           Foreign.Storable
import           Foreign.Ptr
import           Foreign.Marshal.Alloc

import           System.Posix.Signals
import           System.Console.ANSI
import           System.Console.Terminal.Size
import           Control.Exception

import           Control.Monad
import           Text.Read
import           Data.Bool
import           Data.Maybe

--import           Sound.Honk

#include "termops.h"

foreign import ccall "set_direct" c_set_direct :: CInt -> Ptr Termios -> IO ()
foreign import ccall "reset_terminal" c_reset_terminal :: CInt -> Ptr Termios -> IO ()

newtype Termios = Termios [CInt] deriving Show

instance Storable Termios where
  sizeOf _ = 60
  alignment _ = 60
  peek = error "Peek of Termios shouldn't happen"
  poke = error "Poke of Termios shouldn't happen"

-- | Returns True if stdout supports 'ANSI' control characters.
ansiSupported :: IO Bool
ansiSupported = fromMaybe False <$> hSupportsANSIWithoutEmulation stdout

-- | Set raw console mode, install signal handlers.
withConsole :: IO () -> Bool -> IO a -> IO a
withConsole handler wait = bracket acquire release . const
  where
  tryIO :: IO a -> IO (Either SomeException a)
  tryIO = try
  acquire = do
    h_int <- installHandler sigINT (Catch handler) Nothing
    h_term <- installHandler sigTERM (Catch handler) Nothing
    isAnsi <- ansiSupported
    if isAnsi
      then do
        modes <- malloc
        c_set_direct 0 modes
        hideCursor
        return (h_int, h_term, Right modes)
      else
      return (h_int, h_term, Left ())

  release (h_int, h_term, x) = do
    void $ installHandler sigINT h_int Nothing
    void $ installHandler sigTERM h_term Nothing
    when wait $ putStrLn "Press enter to continue . . ."
    case x of
      Left{} -> return ()
      Right modes -> do
        showCursor
        c_reset_terminal 0 modes
        free modes
    when wait $ void $ tryIO getLine

-- | Return UserInput. May blocks if no events were registered.
handleUserInput :: (UserInput -> IO ()) -> IO ()
handleUserInput action = do
  let handler = Catch $ size
                    >>= action . maybe (ResizeWindow 0 0)
                                       (\(Window cols rows) -> ResizeWindow cols rows)
  void $ installHandler 20 handler Nothing
  void $ installHandler 28 handler Nothing
  forever $ scan >>= go
  where
  scan = do
    new <- hWaitForInput stdin 500
    yield
    if new then getInput [] else scan
  getInput xs = hReady stdin >>= bool (return $ reverse xs) (getChar >>= getInput . (:xs))
  go [] = return ()
  go ('\ESC':'[':x:cdr) = do
    case x of
      'A' -> action KeyUp
      'B' -> action KeyDown
      'C' -> action KeyRight
      'D' -> action KeyLeft
      '3' -> action KeyDelete
      _   -> return ()
    go cdr
  go ('\ESC':cdr) = action KeyEsc >> go cdr
  go ('\n':cdr) = action KeyEnter >> go cdr
  go ('\DEL':cdr) = action KeyBackspace >> go cdr
  go ('\b':cdr) = action KeyBackspace >> go cdr
  go (car:cdr) = action (maybe (KeyChar car) KeyNum $ readMaybe [car]) >> go cdr

enableQuickEdit, disableQuickEdit :: Monad m => m ()
enableQuickEdit = return ()
disableQuickEdit = return ()
