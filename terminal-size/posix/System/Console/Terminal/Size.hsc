{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Get terminal window height and width without ncurses dependency. API is correspond to biegunka/terminal-size, but without process dependency, and under Apache license

module System.Console.Terminal.Size (
   Window(..)
 , size
 , eitherSize
 , fdSize
 , hSize
 ) where

import System.Console.Terminal.Size.Types
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Error
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Control.Monad

#include <sys/ioctl.h>
#include <termios.h>
#include <errno.h>

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr (Window CUShort) -> IO CInt

instance Storable (Window CUShort) where
  sizeOf _ = #size struct winsize
  alignment _ = #alignment struct winsize
  peek ptr = Window <$> #{peek struct winsize,ws_row} ptr
                    <*> #{peek struct winsize,ws_col} ptr
  poke = error "Poke of Window shouldn't happen"

-- | Get terminal window width and height for stdout.
size :: Integral n => IO (Maybe (Window n))
size = either (const Nothing) Just <$> eitherSize

-- | Like size but returns an error message when syscall fails.
eitherSize :: Integral n => IO (Either String (Window n))
eitherSize = alloca $ \ptr -> do
  void $ c_ioctl 1 #{const TIOCGWINSZ} ptr
  errno <- getErrno
  win <- peek ptr
  pure $ case () of
    _ | errno == eOK -> Right $ toEnum . fromEnum <$> win
      | errno == eBADF -> Left "The fildes argument is not a valid open file descriptor."
      | errno == eINTR -> Left "A signal was caught during the ioctl() operation."
      | errno == eINVAL -> Left "The  STREAM  or  multiplexer  referenced by fildes is linked (directly or indirectly) downstream from a multiplexer."
      | otherwise -> Left "Unknown ioctl() error."

-- | Not available on Windows: Get terminal window width and height for a specified file descriptor. If it's not attached to a terminal then Nothing is returned.
fdSize :: IO ()
fdSize = return ()

-- | Not available on Windows: Same as fdSize, but takes Handle instead of Fd (file descriptor).
hSize :: IO ()
hSize = return ()
