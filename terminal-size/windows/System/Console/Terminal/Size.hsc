{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Get terminal window height and width without ncurses dependency. API is correspond to biegunka/terminal-size, but without process dependency, and under Apache license.

module System.Console.Terminal.Size (
   Window(..)
 , size
 , eitherSize
 , fdSize
 , hSize
 ) where

import           System.Console.Terminal.Size.Types
import           Foreign.Storable
import           Foreign.Ptr
import           Foreign.C.Types
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           System.Win32.Types
import           Control.Monad
import           Control.Exception

##include "windows_cconv.h"
-- #include <ioctl-types.h>
#include <windows.h>

foreign import WINDOWS_CCONV unsafe "GetStdHandle" c_get_std_handle :: DWORD -> IO HANDLE
foreign import WINDOWS_CCONV "GetConsoleScreenBufferInfo" c_get_buffer_info :: HANDLE -> Ptr (Window SHORT) -> IO BOOL

{-
foreign import WINDOWS_CCONV "LoadLibraryW" c_load_library :: LPCWSTR -> IO HMODULE
foreign import WINDOWS_CCONV "GetProcAddress" c_get_proc_address :: HMODULE -> LPCSTR -> IO (FunPtr a)
foreign import WINDOWS_CCONV "FreeLibrary" c_free_library :: HMODULE -> IO Bool

type Ioctl = CInt -> CInt -> Ptr (Window CUShort) -> IO CInt
foreign import WINDOWS_CCONV "dynamic" mkFun :: FunPtr Ioctl -> Ioctl

foreign import WINDOWS_CCONV "ioctl" ioctl :: CInt -> CInt -> Ptr (Window CUShort) -> IO Bool
-}

-- | Peek using WinAPI call
instance Storable (Window SHORT) where
  sizeOf _ = #size struct _CONSOLE_SCREEN_BUFFER_INFO
  alignment _ = #alignment struct _CONSOLE_SCREEN_BUFFER_INFO
  peek ptr' = do
    let ptr = ptr' `plusPtr` #{offset struct _CONSOLE_SCREEN_BUFFER_INFO,srWindow}
    left <- #{peek struct _SMALL_RECT,Left} ptr
    top <- #{peek struct _SMALL_RECT,Top} ptr
    right <- #{peek struct _SMALL_RECT,Right} ptr
    bottom <- #{peek struct _SMALL_RECT,Bottom} ptr
    return $ Window (bottom - top + 1) (right - left + 1)
  poke = error "Poke of Window shouldn't happen"

{-
-- | Peek using Cygwin/MinGW call
instance Storable (Window CUShort) where
  sizeOf _ = #size struct winsize
  alignment _ = #alignment struct winsize
  peek ptr = Window <$> #{peek struct winsize,ws_row} ptr
                    <*> #{peek struct winsize,ws_col} ptr
  poke = error "Poke of Window shouldn't happen"
-}

-- | Get terminal window width and height for stdout.
size :: Integral n => IO (Maybe (Window n))
size = either (const Nothing) Just <$> eitherSize

-- | Like size but returns an error message when syscall fails.
eitherSize :: Integral n => IO (Either String (Window n))
eitherSize = alloca $ \ptr -> do
  stdout <- c_get_std_handle #{const STD_OUTPUT_HANDLE}
  res <- c_get_buffer_info stdout ptr
  if res then Right . fmap (toEnum . fromEnum) <$> peek ptr
    else return $ Left "No native console was found"

{-
    else do
              ptr <- calloc
              void $ ioctl 1 #{const TIOCGWINSZ} ptr
              win <- peek ptr
              free ptr
              errno <- getErrno
              pure $ case () of
                _ | errno == eOK -> Right $ toEnum . fromEnum <$> win
                  | errno == eBADF -> Left "The fildes argument is not a valid open file descriptor."
                  | errno == eINTR -> Left "A signal was caught during the ioctl() operation."
                  | errno == eINVAL -> Left "The  STREAM  or  multiplexer  referenced by fildes is linked (directly or indirectly) downstream from a multiplexer."
                  | otherwise -> Left "Cannot get screen buffer info"
-}

-- | Not available on Windows: Get terminal window width and height for a specified file descriptor. If it's not attached to a terminal then Nothing is returned.
fdSize :: IO ()
fdSize = return ()

-- | Not available on Windows: Same as fdSize, but takes Handle instead of Fd (file descriptor).
hSize :: IO ()
hSize = return ()
