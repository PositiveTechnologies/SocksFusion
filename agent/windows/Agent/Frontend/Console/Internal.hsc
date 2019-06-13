{-
Copyright 2007 Judah Jacobson

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors
may be used to endorse or promote products derived from this software without
specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}

------------------------------------------------------------------------------------------
-- |
-- Module: Agent.Frontend.Console.Internal
--
-- Adapter to WinAPI.
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

import           Foreign         hiding (void)

import           System.Win32    hiding (try)
import           Foreign.C.Types
import           System.Console.ANSI    (showCursor, hideCursor, hSupportsANSIWithoutEmulation)
import           System.Console.Terminal.Size
import           Control.Exception hiding (Handler)
import qualified System.IO as IO

import           Control.Monad
import           Control.Applicative
import           Control.Concurrent
import           Text.Read
import           Data.Maybe

##include "windows_cconv.h"
#include <windows.h>

--------------------------------------- Specific interface -------------------------------

-- | Returns True if stdout supports 'ANSI' control characters.
ansiSupported :: IO Bool
ansiSupported = fromMaybe False <$> hSupportsANSIWithoutEmulation IO.stdout

-- | Initialise console handlers, disable bufferisation, enable raw mode and others, set Ctrl-C/Ctrl-Break handlers.
withConsole :: IO () -> Bool -> IO a -> IO a
withConsole handler wait = bracket acquire release . const
  where
  tryIO :: IO a -> IO (Either SomeException a)
  tryIO = try
  acquire = do
    h_int <- wrapper (\_ -> handler >> return True)
    void $ c_SetConsoleCtrlHandler h_int True
    isAnsi <- ansiSupported
    if isAnsi
      then alloca $ \pIn -> alloca $ \pOut -> do
        (stdin, stdout) <- getHandles
        inMode <- c_GetConsoleMode stdin pIn >> peek pIn
        outMode <- c_GetConsoleMode stdout pOut >> peek pOut
        void $ c_SetConsoleMode stdin $ 0
                                    .|. #{const ENABLE_PROCESSED_INPUT}
                                    .|. #{const ENABLE_EXTENDED_FLAGS}
                                    .|. #{const ENABLE_INSERT_MODE}
                                    .|. #{const ENABLE_WINDOW_INPUT}
        void $ c_SetConsoleMode stdout $ #{const ENABLE_PROCESSED_OUTPUT}
        hideCursor
        return (h_int, Right (inMode, outMode))
      else
        return (h_int, Left ())

  release (h_int, x) = do
    void $ c_SetConsoleCtrlHandler h_int False
    when wait $ putStrLn "Press enter to continue . . ."
    case x of
      Left{} -> return ()
      Right (inMode, outMode) -> do
        showCursor
        (stdin, stdout) <- getHandles
        void $ c_SetConsoleMode stdin inMode
        void $ c_SetConsoleMode stdout outMode
    when wait $ void $ tryIO getLine

-- | Return UserInput. May blocks if not events was registered.
handleUserInput :: (UserInput -> IO ()) -> IO ()
handleUserInput action = forever $ do
  stdin <- c_GetStdHandle #{const STD_INPUT_HANDLE}
  eventReader stdin
  where
  eventReader h = do
    let waitTime = 500 -- milliseconds
    ret <- c_WaitForSingleObject h waitTime
    yield -- otherwise, the above call causes the loop to never respond to the killThread
    if ret /= 0
      then eventReader h
      else readEvents h >>= mapM_ (processEvent >=> maybe (return ()) action)

enableQuickEdit :: IO ()
enableQuickEdit = alloca $ \pIn -> do
  (stdin, _) <- getHandles
  inMode <- c_GetConsoleMode stdin pIn >> peek pIn
  void $ c_SetConsoleMode stdin $ inMode .|. #{const ENABLE_QUICK_EDIT_MODE}

disableQuickEdit :: IO ()
disableQuickEdit = alloca $ \pIn -> do
  (stdin, _) <- getHandles
  inMode <- c_GetConsoleMode stdin pIn >> peek pIn
  void $ c_SetConsoleMode stdin $ inMode .&. (complement #{const ENABLE_QUICK_EDIT_MODE})

--------------------------------------- Signals ------------------------------------------

type Handler = DWORD -> IO BOOL

foreign import WINDOWS_CCONV "SetConsoleCtrlHandler"
  c_SetConsoleCtrlHandler :: FunPtr Handler -> BOOL -> IO BOOL

foreign import WINDOWS_CCONV "wrapper" wrapper :: Handler -> IO (FunPtr Handler)

--------------------------------------- Console Mode -------------------------------------

getHandles :: IO (HANDLE, HANDLE)
getHandles = do
  stdin <- c_GetStdHandle #{const STD_INPUT_HANDLE}
  stdout <- c_GetStdHandle #{const STD_OUTPUT_HANDLE}
  return (stdin, stdout)

foreign import WINDOWS_CCONV "GetStdHandle"
  c_GetStdHandle :: DWORD -> IO HANDLE

foreign import WINDOWS_CCONV "GetConsoleMode"
  c_GetConsoleMode :: HANDLE -> Ptr DWORD -> IO Bool

foreign import WINDOWS_CCONV "SetConsoleMode"
  c_SetConsoleMode :: HANDLE -> DWORD -> IO Bool

--------------------------------------- Read events --------------------------------------

foreign import WINDOWS_CCONV "ReadConsoleInputW"
  c_ReadConsoleInput :: HANDLE -> Ptr InputEvent -> DWORD -> LPDWORD -> IO BOOL

foreign import WINDOWS_CCONV "WaitForSingleObject"
  c_WaitForSingleObject :: HANDLE -> DWORD -> IO DWORD

foreign import WINDOWS_CCONV "GetNumberOfConsoleInputEvents"
  c_GetNumberOfConsoleInputEvents :: HANDLE -> LPDWORD -> IO BOOL

getNumberOfEvents :: HANDLE -> IO DWORD
getNumberOfEvents h = alloca $ \ptr -> c_GetNumberOfConsoleInputEvents h ptr >> peek ptr

processEvent :: InputEvent -> IO (Maybe UserInput)
processEvent OtherEvent = pure $ Nothing
processEvent ResizeEvent{} = fmap (\(Window h w) -> ResizeWindow h w) <$> size
processEvent KeyEvent { keyDown = kd
                      , unicodeChar = c
                      , virtualKeyCode = vc
                      , controlKeyState = cstate }
    | kd || (((cstate .&. #{const LEFT_ALT_PRESSED}) /= 0 || vc == #{const VK_MENU})
          && c /= '\NUL')
      = pure $ keyFromCode vc <|> numberChar <|> simpleChar
    | otherwise = pure Nothing
  where
  simpleChar = guard (c /= '\NUL') >> return (KeyChar c)
  numberChar = KeyNum <$> readMaybe [c]

keyFromCode :: WORD -> Maybe UserInput
keyFromCode #{const VK_BACK} = Just KeyBackspace
keyFromCode #{const VK_LEFT} = Just KeyLeft
keyFromCode #{const VK_RIGHT} = Just KeyRight
keyFromCode #{const VK_UP} = Just KeyUp
keyFromCode #{const VK_DOWN} = Just KeyDown
keyFromCode #{const VK_DELETE} = Just KeyDelete
keyFromCode #{const VK_ESCAPE} = Just KeyEsc
-- The Windows console will return '\r' when return is pressed.
keyFromCode #{const VK_RETURN} = Just KeyEnter
keyFromCode _ = Nothing

data InputEvent = KeyEvent { keyDown :: BOOL
                           , virtualKeyCode :: WORD
                           , unicodeChar :: Char
                           , controlKeyState :: DWORD }
                | ResizeEvent
                | OtherEvent

-- Alignment of KeyEvent:
-- |  01 |  00 | PAD | PAD |
-- |         BOOL          |
-- |   WORD    |    WORD   |
-- |   WORD    |   WCHAR   |
-- |         DWORD         |
-- WindowBufferSize event contains heigth of the buffer, not of the window actually
instance Storable InputEvent where
  sizeOf _ = #size struct _INPUT_RECORD
  alignment _ = #alignment struct _INPUT_RECORD
  peek ptr' = #{peek struct _INPUT_RECORD,EventType} ptr' >>= \case
    #{const KEY_EVENT} -> do
      let ptr = ptr' `plusPtr` #{offset struct _INPUT_RECORD,Event}
      kd <- #{peek struct _KEY_EVENT_RECORD,bKeyDown} ptr
      vkc <- #{peek struct _KEY_EVENT_RECORD,wVirtualKeyCode} ptr
      uc :: CWchar <- #{peek struct _KEY_EVENT_RECORD,uChar} ptr
      cks <- #{peek struct _KEY_EVENT_RECORD,dwControlKeyState} ptr
      return $ KeyEvent kd vkc (toEnum $ fromEnum uc) cks
    #{const WINDOW_BUFFER_SIZE_EVENT} -> return ResizeEvent
    (_ :: WORD) -> return OtherEvent
  poke = error "poke of InputEvent shouldn't happen"

readEvents :: HANDLE -> IO [InputEvent]
readEvents h = do
  n <- getNumberOfEvents h
  alloca $ \numEventsPtr -> allocaBytes (fromIntegral n * (sizeOf (undefined :: InputEvent))) $ \pRecord -> do
    void $ c_ReadConsoleInput h pRecord n numEventsPtr
    peek numEventsPtr >>= (`peekArray` castPtr pRecord) . fromIntegral
