{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  System.IO.ExplicitIOModes
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module exports a 'Handle' to a file which is parameterized with the
-- IOMode the handle is in. All operations on handles explicitly specify the
-- needed IOMode. This way it is impossible to read from a write-only handle or
-- write to a read-only handle for example.
--
-- This modules re-exports everything from 'System.IO' so you can just replace:
-- @import System.IO@ with: @import System.IO.ExplicitIOModes@, change some type
-- signatures and expect everything to type-check.
--
-- There's one exception to this last statement: If you are using the standard
-- handles 'stdin', 'stdout' or 'stderr' in a mode which isn't their default
-- mode ('R' for 'stdin' and 'W' for 'stdout' and 'stderr') you have to 'cast'
-- these handles to the expected IOMode.
--
--------------------------------------------------------------------------------

module System.IO.ExplicitIOModes
    ( -- * The IO monad
      SIO.IO
    , SIO.fixIO

      -- * Files and handles
    , SIO.FilePath

    , Handle
    , regularHandle

      -- ** IO Modes

      -- | Types that represent the IOMode a 'Handle' can be in.
    , R, W, A, RW

      -- ** Standard handles

      -- | These standard handles have concrete IOModes by default which work
      -- for the majority of cases. In the rare occasion that you know these
      -- handles have different IOModes you can 'cast' them.
    , stdin
    , stdout
    , stderr

    , cast

      -- * Opening and closing files
      -- ** Opening files
    , withFile
    , openFile
    , IOMode(..)

      -- ** Closing files
    , hClose

      -- ** Special cases
    , SIO.readFile
    , SIO.writeFile
    , SIO.appendFile

      -- * Operations on handles
      -- ** Determining and changing the size of a file
    , hFileSize

#ifdef __GLASGOW_HASKELL__
    , hSetFileSize
#endif

      -- ** Detecting the end of input
    , hIsEOF

      -- ** Buffering operations
    , SIO.BufferMode( SIO.NoBuffering, SIO.LineBuffering, SIO.BlockBuffering )
    , hSetBuffering
    , hGetBuffering
    , hFlush

    -- ** Repositioning handles
    , hGetPosn
    , SIO.hSetPosn
    , SIO.HandlePosn

    , hSeek
    , SIO.SeekMode( SIO.AbsoluteSeek, SIO.RelativeSeek, SIO.SeekFromEnd )
#if !defined(__NHC__)
    , hTell
#endif

    -- ** Handle properties
    , hIsOpen, hIsClosed
    , hIsReadable, hIsWritable
    , hIsSeekable

    -- ** Terminal operations (not portable: GHC/Hugs only)
#if !defined(__NHC__)
    , hIsTerminalDevice
    , hSetEcho
    , hGetEcho
#endif

    -- ** Showing handle state (not portable: GHC only)
#ifdef __GLASGOW_HASKELL__
    , hShow
#endif

    -- * Text input and output
    -- ** Text input

    -- | Note that the following text input operations are polymorphic in the
    -- IOMode of the given handle. However the IOModes are restricted to
    -- @ReadModes@ only which can be either 'R' or 'RW'.
    , hWaitForInput
    , hReady
    , hGetChar
    , hGetLine
    , hLookAhead
    , hGetContents

    -- ** Text ouput

    -- | Note that the following text output operations are polymorphic in the
    -- IOMode of the given handle. However the IOModes are restricted to
    -- @WriteModes@ only which can be either 'W', 'A' or 'RW'.
    , hPutChar
    , hPutStr
    , hPutStrLn
    , hPrint

    -- ** Special cases for standard input and output

    -- | These functions are also exported by the "Prelude".

    , SIO.interact
    , SIO.putChar
    , SIO.putStr
    , SIO.putStrLn
    , SIO.print
    , SIO.getChar
    , SIO.getLine
    , SIO.getContents
    , SIO.readIO
    , SIO.readLn

    -- * Binary input and output
    , withBinaryFile
    , openBinaryFile
    , hSetBinaryMode
    , hPutBuf
    , hGetBuf

#if !defined(__NHC__) && !defined(__HUGS__)
    , hPutBufNonBlocking
    , hGetBufNonBlocking
#endif

    -- * Temporary files
    , openTempFile
    , openBinaryTempFile
    ) where

import Control.Monad       ( liftM, liftM2 )
import Control.Arrow       ( second )
import Foreign.Ptr         ( Ptr )
import Data.Typeable       ( Typeable )
import Data.Tagged         ( Tagged(Tagged), unTagged )

import qualified System.IO as SIO


-- * Files and handles

-- | A handle to a file with an explicit IOMode.
--
-- Wraps: @System.IO.@'SIO.Handle'.
newtype Handle ioMode = Handle
    { -- | Retrieves the regular @System.IO.@'SIO.Handle'.
      regularHandle :: SIO.Handle
    }
    deriving ( Show, Eq, Typeable )

wrap :: (SIO.Handle -> a) -> (Handle ioMode -> a)
wrap f = f . regularHandle


-- ** IO Modes

-- | Read only.
data R

-- | Write only.
data W

-- | Append.
data A

-- | Read and write.
data RW

class ReadModes  ioMode
class WriteModes ioMode

instance ReadModes R
instance ReadModes RW

instance WriteModes W
instance WriteModes A
instance WriteModes RW


-- ** Standard handles

-- | Wraps: @System.IO.@'SIO.stdin'.
stdin :: Handle R
stdin = Handle SIO.stdin

-- | Wraps: @System.IO.@'SIO.stdout'.
stdout :: Handle W
stdout = Handle SIO.stdout

-- | Wraps: @System.IO.@'SIO.stderr'.
stderr :: Handle W
stderr = Handle SIO.stderr

-- | Cast the IOMode of a handle if the handle supports it.
cast :: forall anyIOMode castedIOMode. CheckMode castedIOMode
     => Handle anyIOMode -> IO (Maybe (Handle castedIOMode))
cast (Handle h) = do
  b <- unTagged (checkMode :: Tagged castedIOMode (SIO.Handle -> IO Bool)) h
  return $ if b
           then Just $ Handle h
           else Nothing

class CheckMode ioMode where
    checkMode :: Tagged ioMode (SIO.Handle -> IO Bool)

instance CheckMode R where
    checkMode = Tagged SIO.hIsReadable

instance CheckMode W where
    checkMode = Tagged SIO.hIsWritable

instance CheckMode A where
    checkMode = Tagged SIO.hIsWritable

instance CheckMode RW where
    checkMode = Tagged $ \h -> liftM2 (&&) (SIO.hIsReadable h)
                                           (SIO.hIsWritable h)


-- * Opening and closing files


-- ** Opening files

-- | Wraps: @System.IO.@'SIO.withFile'.
withFile :: FilePath -> IOMode ioMode -> (Handle ioMode -> IO r) -> IO r
withFile fp ioMode f = SIO.withFile fp (convert ioMode) $ f . Handle

-- | Wraps: @System.IO.@'SIO.openFile'.
openFile :: FilePath -> IOMode ioMode -> IO (Handle ioMode)
openFile fp = liftM Handle . SIO.openFile fp . convert

-- | The IOMode GADT which for each constructor specifies the associated IOMode
-- type.
--
-- Also see: @System.IO.@'SIO.IOMode'.
data IOMode ioMode where
    ReadMode      :: IOMode R
    WriteMode     :: IOMode W
    AppendMode    :: IOMode A
    ReadWriteMode :: IOMode RW

convert :: IOMode ioMode -> SIO.IOMode
convert ReadMode      = SIO.ReadMode
convert WriteMode     = SIO.WriteMode
convert AppendMode    = SIO.AppendMode
convert ReadWriteMode = SIO.ReadWriteMode

instance Eq (IOMode ioMode) where
    ReadMode      == ReadMode      = True
    WriteMode     == WriteMode     = True
    AppendMode    == AppendMode    = True
    ReadWriteMode == ReadWriteMode = True
    _             == _             = False

instance Ord (IOMode ioMode) where
    ReadWriteMode <= ReadWriteMode = True
    ReadWriteMode <= _             = False

    AppendMode    <= ReadWriteMode = True
    AppendMode    <= AppendMode    = True
    AppendMode    <= _             = False

    WriteMode     <= ReadWriteMode = True
    WriteMode     <= AppendMode    = True
    WriteMode     <= WriteMode     = True
    WriteMode     <= _             = False

    ReadMode      <= ReadWriteMode = True
    ReadMode      <= AppendMode    = True
    ReadMode      <= WriteMode     = True
    ReadMode      <= ReadMode      = True

instance Show (IOMode ioMode) where
    show ReadMode      = "ReadMode"
    show WriteMode     = "WriteMode"
    show AppendMode    = "AppendMode"
    show ReadWriteMode = "ReadWriteMode"


-- ** Closing files

-- | Wraps: @System.IO.@'SIO.hClose'.
hClose :: Handle ioMode -> IO ()
hClose = wrap SIO.hClose


-- * Operations on handles


-- ** Determining and changing the size of a file

-- | Wraps: @System.IO.@'SIO.hFileSize'.
hFileSize :: Handle ioMode -> IO Integer
hFileSize = wrap SIO.hFileSize

#ifdef __GLASGOW_HASKELL__
-- | Wraps: @System.IO.@'SIO.hSetFileSize'.
hSetFileSize :: Handle ioMode -> Integer -> IO ()
hSetFileSize = wrap SIO.hSetFileSize
#endif

-- ** Detecting the end of input

-- | Wraps: @System.IO.@'SIO.hIsEOF'.
hIsEOF :: ReadModes ioMode => Handle ioMode -> IO Bool
hIsEOF = wrap SIO.hIsEOF


-- ** Buffering operations

-- | Wraps: @System.IO.@'SIO.hSetBuffering'.
hSetBuffering :: Handle ioMode -> SIO.BufferMode -> IO ()
hSetBuffering = wrap SIO.hSetBuffering

-- | Wraps: @System.IO.@'SIO.hGetBuffering'.
hGetBuffering :: Handle ioMode -> IO SIO.BufferMode
hGetBuffering = wrap SIO.hGetBuffering

-- | Wraps: @System.IO.@'SIO.hFlush'.
hFlush :: Handle ioMode -> IO ()
hFlush = wrap SIO.hFlush


-- ** Repositioning handles

-- | Wraps: @System.IO.@'SIO.hGetPosn'.
hGetPosn :: Handle ioMode -> IO SIO.HandlePosn
hGetPosn = wrap SIO.hGetPosn

-- | Wraps: @System.IO.@'SIO.hSeek'.
hSeek :: Handle ioMode -> SIO.SeekMode -> Integer -> IO ()
hSeek = wrap SIO.hSeek

#if !defined(__NHC__)
-- | Wraps: @System.IO.@'SIO.hTell'.
hTell :: Handle ioMode -> IO Integer
hTell = wrap SIO.hTell
#endif

-- ** Handle properties

-- | Wraps: @System.IO.@'SIO.hIsOpen'.
hIsOpen :: Handle ioMode -> IO Bool
hIsOpen = wrap SIO.hIsOpen

-- | Wraps: @System.IO.@'SIO.hIsClosed'.
hIsClosed :: Handle ioMode -> IO Bool
hIsClosed = wrap SIO.hIsClosed

-- | Wraps: @System.IO.@'SIO.hIsReadable'.
hIsReadable :: Handle ioMode -> IO Bool
hIsReadable = wrap SIO.hIsReadable

-- | Wraps: @System.IO.@'SIO.hIsWritable'.
hIsWritable :: Handle ioMode -> IO Bool
hIsWritable = wrap SIO.hIsWritable

-- | Wraps: @System.IO.@'SIO.hIsSeekable'.
hIsSeekable :: Handle ioMode -> IO Bool
hIsSeekable = wrap SIO.hIsSeekable


-- ** Terminal operations (not portable: GHC/Hugs only)

#if !defined(__NHC__)
-- | Wraps: @System.IO.@'SIO.hIsTerminalDevice'.
hIsTerminalDevice :: Handle ioMode -> IO Bool
hIsTerminalDevice = wrap SIO.hIsTerminalDevice

-- | Wraps: @System.IO.@'SIO.hSetEcho'.
hSetEcho :: Handle ioMode -> Bool -> IO ()
hSetEcho = wrap SIO.hSetEcho

-- | Wraps: @System.IO.@'SIO.hGetEcho'.
hGetEcho :: Handle ioMode -> IO Bool
hGetEcho = wrap SIO.hGetEcho
#endif

-- ** Showing handle state (not portable: GHC only)

#ifdef __GLASGOW_HASKELL__
-- | Wraps: @System.IO.@'SIO.hShow'.
hShow :: Handle ioMode -> IO String
hShow = wrap SIO.hShow
#endif

-- * Text input and output

-- ** Text input

-- | Wraps: @System.IO.@'SIO.hWaitForInput'.
hWaitForInput :: ReadModes ioMode => Handle ioMode -> Int -> IO Bool
hWaitForInput = wrap SIO.hWaitForInput

-- | Wraps: @System.IO.@'SIO.hReady'.
hReady :: ReadModes ioMode => Handle ioMode -> IO Bool
hReady = wrap SIO.hReady

-- | Wraps: @System.IO.@'SIO.hGetChar'.
hGetChar :: ReadModes ioMode => Handle ioMode -> IO Char
hGetChar = wrap SIO.hGetChar

-- | Wraps: @System.IO.@'SIO.hGetLine'.
hGetLine :: ReadModes ioMode => Handle ioMode -> IO String
hGetLine = wrap SIO.hGetLine

-- | Wraps: @System.IO.@'SIO.hLookAhead'.
hLookAhead :: ReadModes ioMode => Handle ioMode -> IO Char
hLookAhead = wrap SIO.hLookAhead

-- | Wraps: @System.IO.@'SIO.hGetContents'.
hGetContents :: ReadModes ioMode => Handle ioMode -> IO String
hGetContents = wrap SIO.hGetContents


-- ** Text ouput

-- | Wraps: @System.IO.@'SIO.hPutChar'.
hPutChar :: WriteModes ioMode => Handle ioMode -> Char -> IO ()
hPutChar = wrap SIO.hPutChar

-- | Wraps: @System.IO.@'SIO.hPutStr'.
hPutStr :: WriteModes ioMode => Handle ioMode -> String -> IO ()
hPutStr = wrap SIO.hPutStr

-- | Wraps: @System.IO.@'SIO.hPutStrLn'.
hPutStrLn :: WriteModes ioMode => Handle ioMode -> String -> IO ()
hPutStrLn = wrap SIO.hPutStrLn

-- | Wraps: @System.IO.@'SIO.hPrint'.
hPrint :: (WriteModes ioMode, Show a) => Handle ioMode -> a -> IO ()
hPrint = wrap SIO.hPrint


-- * Binary input and output

-- | Wraps: @System.IO.@'SIO.withBinaryFile'.
withBinaryFile :: FilePath -> IOMode ioMode -> (Handle ioMode -> IO r) -> IO r
withBinaryFile fp ioMode f = SIO.withBinaryFile fp (convert ioMode) $ f . Handle

-- | Wraps: @System.IO.@'SIO.openBinaryFile'.
openBinaryFile :: FilePath -> IOMode ioMode -> IO (Handle ioMode)
openBinaryFile fp = liftM Handle . SIO.openBinaryFile fp . convert

-- | Wraps: @System.IO.@'SIO.hSetBinaryMode'.
hSetBinaryMode :: Handle ioMode -> Bool -> IO ()
hSetBinaryMode = wrap SIO.hSetBinaryMode

-- | Wraps: @System.IO.@'SIO.hPutBuf'.
hPutBuf :: WriteModes ioMode => Handle ioMode -> Ptr a -> Int -> IO ()
hPutBuf = wrap SIO.hPutBuf

-- | Wraps: @System.IO.@'SIO.hGetBuf'.
hGetBuf :: ReadModes ioMode => Handle ioMode -> Ptr a -> Int -> IO Int
hGetBuf = wrap SIO.hGetBuf

#if !defined(__NHC__) && !defined(__HUGS__)
-- | Wraps: @System.IO.@'SIO.hPutBufNonBlocking'.
hPutBufNonBlocking :: WriteModes ioMode => Handle ioMode -> Ptr a -> Int -> IO Int
hPutBufNonBlocking = wrap SIO.hPutBufNonBlocking

-- | Wraps: @System.IO.@'SIO.hGetBufNonBlocking'.
hGetBufNonBlocking :: ReadModes ioMode => Handle ioMode -> Ptr a -> Int -> IO Int
hGetBufNonBlocking = wrap SIO.hGetBufNonBlocking
#endif

-- * Temporary files

-- | Wraps: @System.IO.@'SIO.openTempFile'.
openTempFile :: FilePath -> String -> IO (FilePath, Handle RW)
openTempFile fp template = liftM (second Handle) $ SIO.openTempFile fp template

-- | Wraps: @System.IO.@'SIO.openBinaryTempFile'.
openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle RW)
openBinaryTempFile fp template = liftM (second Handle) $ SIO.openBinaryTempFile fp template


-- The End ---------------------------------------------------------------------
