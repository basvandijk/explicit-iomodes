{-# LANGUAGE CPP
           , NoImplicitPrelude
           , UnicodeSyntax
           , EmptyDataDecls
           , GADTs
           , ScopedTypeVariables
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  System.IO.ExplicitIOModes
-- Copyright   :  (c) 2009-2010 Bas van Dijk
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
-- mode ('ReadMode' for 'stdin' and 'WriteMode' for 'stdout' and 'stderr') you
-- have to 'cast' these handles to the expected IOMode.
--
--------------------------------------------------------------------------------

module System.IO.ExplicitIOModes
    ( -- * The IO monad
      SIO.IO
    , SIO.fixIO

      -- * Files and handles
    , SIO.FilePath

    , Handle

      -- ** IO Modes

      -- | Types that represent the IOMode a 'Handle' can be in.
    , ReadMode
    , WriteMode
    , AppendMode
    , ReadWriteMode

      -- *** Grouping the IOMode types.
    , ReadModes
    , WriteModes

      -- *** A value-level IOMode.
    , IOMode(..)
    , MkIOMode(mkIOMode)
    , regularIOMode

      -- ** Standard handles

      -- | These standard handles have concrete IOModes by default which work
      -- for the majority of cases. In the rare occasion that you know these
      -- handles have different IOModes you can 'cast' them.
    , stdin
    , stdout
    , stderr

    , cast
    , CheckMode

      -- * Opening and closing files
      -- ** Opening files
    , openFile
    , withFile

      -- *** Opening files by inferring the IOMode
    , openFile'
    , withFile'

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
    , SIO.isEOF

      -- ** Buffering operations
    , SIO.BufferMode(..)
    , hSetBuffering
    , hGetBuffering
    , hFlush

    -- ** Repositioning handles
    , hGetPosn
    , SIO.hSetPosn
    , SIO.HandlePosn

    , hSeek
    , SIO.SeekMode(..)
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
    -- 'ReadModes' only which can be either 'ReadMode' or 'ReadWriteMode'.
    , hWaitForInput
    , hReady
    , hGetChar
    , hGetLine
    , hLookAhead
    , hGetContents

    -- ** Text ouput

    -- | Note that the following text output operations are polymorphic in the
    -- IOMode of the given handle. However the IOModes are restricted to
    -- 'WriteModes' only which can be either 'WriteMode', 'AppendMode' or
    -- 'ReadWriteMode'.
    , hPutChar
    , hPutStr
    , hPutStrLn
    , hPrint

    -- ** Special cases for standard input and output

    -- | These functions are also exported by the \"Prelude\".

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
    , openBinaryFile
    , withBinaryFile

      -- ** Opening binary files by inferring the IOMode
    , openBinaryFile'
    , withBinaryFile'

      -- ** Operations on binary handles
    , hSetBinaryMode
    , hPutBuf
    , hGetBuf

#if !defined(__NHC__) && !defined(__HUGS__)
#if MIN_VERSION_base(4,3,0)
    , hGetBufSome
#endif
    , hPutBufNonBlocking
    , hGetBufNonBlocking
#endif

    -- * Temporary files
    , openTempFile
    , openBinaryTempFile

#if MIN_VERSION_base(4,2,0)
    , openTempFileWithDefaultPermissions
    , openBinaryTempFileWithDefaultPermissions
#endif

#if MIN_VERSION_base(4,2,0) && !defined(__NHC__) && !defined(__HUGS__)
    -- * Unicode encoding/decoding
    , hSetEncoding
    , hGetEncoding

    -- ** Unicode encodings
    , SIO.TextEncoding
    , SIO.latin1
    , SIO.utf8, SIO.utf8_bom
    , SIO.utf16, SIO.utf16le, SIO.utf16be
    , SIO.utf32, SIO.utf32le, SIO.utf32be
    , SIO.localeEncoding
    , SIO.mkTextEncoding

    -- * Newline conversion
    , hSetNewlineMode
    , SIO.Newline(..)
    , SIO.nativeNewline
    , SIO.NewlineMode(..)

    , SIO.noNewlineTranslation, SIO.universalNewlineMode, SIO.nativeNewlineMode
#endif
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude             ( Integer )
import Control.Monad       ( return, (>>=), fail, liftM, liftM2 )
import Control.Arrow       ( second )
import Foreign.Ptr         ( Ptr )
import Data.Eq             ( Eq, (==) )
import Data.Function       ( ($) )
import Data.Bool           ( Bool(False, True) )
import Data.Maybe          ( Maybe(Nothing, Just) )
import Data.Int            ( Int )
import Data.Char           ( Char, String )
import Text.Show           ( Show, show )
import System.IO           ( IO, FilePath )

import qualified System.IO as SIO

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )
import Data.Bool.Unicode     ( (∧) )

-- from tagged:
import Data.Tagged ( Tagged(Tagged), unTagged )

-- from ourselves:
import System.IO.ExplicitIOModes.Internal ( Handle(Handle) )
import System.IO.ExplicitIOModes.Unsafe   ( wrap )


--------------------------------------------------------------------------------
-- * Files and handles
--------------------------------------------------------------------------------

-- ** IO Modes

-- | Read only.
data ReadMode

-- | Write only.
data WriteMode

-- | Write only by appending.
data AppendMode

-- | Both read and write.
data ReadWriteMode

-- | Class of readable IO mode types.
--
-- Note the super class @ReadModesPrivate@. This type class is not exported by
-- this module which ensures you can't accidentally make another type (like
-- 'WriteMode' or 'AppendMode') an instance of 'ReadModes'.
class ReadModesPrivate ioMode ⇒ ReadModes ioMode
class ReadModesPrivate ioMode

-- | Class of writable IO mode types.
--
-- Note the super class @WriteModesPrivate@. This type class is not exported by
-- this module which ensures you can't accidentally make another type (like
-- 'ReadMode') an instance of 'WriteModes'.
class WriteModesPrivate ioMode ⇒ WriteModes ioMode
class WriteModesPrivate ioMode

instance ReadModesPrivate ReadMode;       instance ReadModes ReadMode
instance ReadModesPrivate ReadWriteMode;  instance ReadModes ReadWriteMode

instance WriteModesPrivate WriteMode;     instance WriteModes WriteMode
instance WriteModesPrivate AppendMode;    instance WriteModes AppendMode
instance WriteModesPrivate ReadWriteMode; instance WriteModes ReadWriteMode

-- | The IOMode GADT which for each constructor specifies the associated IOMode
-- type.
--
-- Also see: @System.IO.'SIO.IOMode'@.
data IOMode ioMode where
    ReadMode      ∷ IOMode ReadMode
    WriteMode     ∷ IOMode WriteMode
    AppendMode    ∷ IOMode AppendMode
    ReadWriteMode ∷ IOMode ReadWriteMode

class MkIOMode ioMode where
    -- | An overloaded IOMode constructor.
    mkIOMode ∷ IOMode ioMode

instance MkIOMode ReadMode      where mkIOMode = ReadMode
instance MkIOMode WriteMode     where mkIOMode = WriteMode
instance MkIOMode AppendMode    where mkIOMode = AppendMode
instance MkIOMode ReadWriteMode where mkIOMode = ReadWriteMode

-- | Retrieves the regular @System.IO.'SIO.IOMode'@.
regularIOMode ∷ IOMode ioMode → SIO.IOMode
regularIOMode ReadMode      = SIO.ReadMode
regularIOMode WriteMode     = SIO.WriteMode
regularIOMode AppendMode    = SIO.AppendMode
regularIOMode ReadWriteMode = SIO.ReadWriteMode

instance Eq (IOMode ioMode) where
    ReadMode      == ReadMode      = True
    WriteMode     == WriteMode     = True
    AppendMode    == AppendMode    = True
    ReadWriteMode == ReadWriteMode = True
    _             == _             = False

instance Show (IOMode ioMode) where
    show ReadMode      = "ReadMode"
    show WriteMode     = "WriteMode"
    show AppendMode    = "AppendMode"
    show ReadWriteMode = "ReadWriteMode"


-- ** Standard handles

-- | Wraps: @System.IO.'SIO.stdin'@.
stdin ∷ Handle ReadMode
stdin = Handle SIO.stdin

-- | Wraps: @System.IO.'SIO.stdout'@.
stdout ∷ Handle WriteMode
stdout = Handle SIO.stdout

-- | Wraps: @System.IO.'SIO.stderr'@.
stderr ∷ Handle WriteMode
stderr = Handle SIO.stderr

-- | Cast the IOMode of a handle if the handle supports it.
cast ∷ ∀ anyIOMode castedIOMode. CheckMode castedIOMode
     ⇒ Handle anyIOMode → IO (Maybe (Handle castedIOMode))
cast (Handle h) = do
  b ← unTagged (checkMode ∷ Tagged castedIOMode (SIO.Handle → IO Bool)) h
  return $ if b
           then Just $ Handle h
           else Nothing

class CheckMode ioMode where
    checkMode ∷ Tagged ioMode (SIO.Handle → IO Bool)

instance CheckMode ReadMode      where checkMode = Tagged SIO.hIsReadable
instance CheckMode WriteMode     where checkMode = Tagged SIO.hIsWritable
instance CheckMode AppendMode    where checkMode = Tagged SIO.hIsWritable
instance CheckMode ReadWriteMode where checkMode = Tagged $
                                         \h → liftM2 (∧) (SIO.hIsReadable h)
                                                         (SIO.hIsWritable h)


--------------------------------------------------------------------------------
-- * Opening and closing files
--------------------------------------------------------------------------------

-- ** Opening files

-- | Wraps: @System.IO.'SIO.openFile'@.
openFile ∷ FilePath → IOMode ioMode → IO (Handle ioMode)
openFile fp = liftM Handle ∘ SIO.openFile fp ∘ regularIOMode

-- | Wraps: @System.IO.'SIO.withFile'@.
withFile ∷ FilePath → IOMode ioMode → (Handle ioMode → IO α) → IO α
withFile fp ioMode f = SIO.withFile fp (regularIOMode ioMode) $ f ∘ Handle

-- *** Opening files by inferring the IOMode

-- | Open a file without explicitly specifying the IOMode. The IOMode is
-- inferred from the type of the resulting 'Handle'.
--
-- Note that: @openFile' fp = 'openFile' fp 'mkIOMode'@.
openFile' ∷ MkIOMode ioMode ⇒ FilePath → IO (Handle ioMode)
openFile' fp = openFile fp mkIOMode

-- | Note that: @withFile' fp = 'withFile' fp 'mkIOMode'@.
withFile' ∷ MkIOMode ioMode ⇒ FilePath → (Handle ioMode → IO α) → IO α
withFile' fp = withFile fp mkIOMode

-- ** Closing files

-- | Wraps: @System.IO.'SIO.hClose'@.
hClose ∷ Handle ioMode → IO ()
hClose = wrap SIO.hClose


--------------------------------------------------------------------------------
-- * Operations on handles
--------------------------------------------------------------------------------

-- ** Determining and changing the size of a file

-- | Wraps: @System.IO.'SIO.hFileSize'@.
hFileSize ∷ Handle ioMode → IO Integer
hFileSize = wrap SIO.hFileSize

#ifdef __GLASGOW_HASKELL__
-- | Wraps: @System.IO.'SIO.hSetFileSize'@.
hSetFileSize ∷ Handle ioMode → Integer → IO ()
hSetFileSize = wrap SIO.hSetFileSize
#endif

-- ** Detecting the end of input

-- | Wraps: @System.IO.'SIO.hIsEOF'@.
hIsEOF ∷ ReadModes ioMode ⇒ Handle ioMode → IO Bool
hIsEOF = wrap SIO.hIsEOF


-- ** Buffering operations

-- | Wraps: @System.IO.'SIO.hSetBuffering'@.
hSetBuffering ∷ Handle ioMode → SIO.BufferMode → IO ()
hSetBuffering = wrap SIO.hSetBuffering

-- | Wraps: @System.IO.'SIO.hGetBuffering'@.
hGetBuffering ∷ Handle ioMode → IO SIO.BufferMode
hGetBuffering = wrap SIO.hGetBuffering

-- | Wraps: @System.IO.'SIO.hFlush'@.
hFlush ∷ Handle ioMode → IO ()
hFlush = wrap SIO.hFlush


-- ** Repositioning handles

-- | Wraps: @System.IO.'SIO.hGetPosn'@.
hGetPosn ∷ Handle ioMode → IO SIO.HandlePosn
hGetPosn = wrap SIO.hGetPosn

-- | Wraps: @System.IO.'SIO.hSeek'@.
hSeek ∷ Handle ioMode → SIO.SeekMode → Integer → IO ()
hSeek = wrap SIO.hSeek

#if !defined(__NHC__)
-- | Wraps: @System.IO.'SIO.hTell'@.
hTell ∷ Handle ioMode → IO Integer
hTell = wrap SIO.hTell
#endif

-- ** Handle properties

-- | Wraps: @System.IO.'SIO.hIsOpen'@.
hIsOpen ∷ Handle ioMode → IO Bool
hIsOpen = wrap SIO.hIsOpen

-- | Wraps: @System.IO.'SIO.hIsClosed'@.
hIsClosed ∷ Handle ioMode → IO Bool
hIsClosed = wrap SIO.hIsClosed

-- | Wraps: @System.IO.'SIO.hIsReadable'@.
hIsReadable ∷ Handle ioMode → IO Bool
hIsReadable = wrap SIO.hIsReadable

-- | Wraps: @System.IO.'SIO.hIsWritable'@.
hIsWritable ∷ Handle ioMode → IO Bool
hIsWritable = wrap SIO.hIsWritable

-- | Wraps: @System.IO.'SIO.hIsSeekable'@.
hIsSeekable ∷ Handle ioMode → IO Bool
hIsSeekable = wrap SIO.hIsSeekable


-- ** Terminal operations (not portable: GHC/Hugs only)

#if !defined(__NHC__)
-- | Wraps: @System.IO.'SIO.hIsTerminalDevice'@.
hIsTerminalDevice ∷ Handle ioMode → IO Bool
hIsTerminalDevice = wrap SIO.hIsTerminalDevice

-- | Wraps: @System.IO.'SIO.hSetEcho'@.
hSetEcho ∷ Handle ioMode → Bool → IO ()
hSetEcho = wrap SIO.hSetEcho

-- | Wraps: @System.IO.'SIO.hGetEcho'@.
hGetEcho ∷ Handle ioMode → IO Bool
hGetEcho = wrap SIO.hGetEcho
#endif

-- ** Showing handle state (not portable: GHC only)

#ifdef __GLASGOW_HASKELL__
-- | Wraps: @System.IO.'SIO.hShow'@.
hShow ∷ Handle ioMode → IO String
hShow = wrap SIO.hShow
#endif


--------------------------------------------------------------------------------
-- * Text input and output
--------------------------------------------------------------------------------

-- ** Text input

-- | Wraps: @System.IO.'SIO.hWaitForInput'@.
hWaitForInput ∷ ReadModes ioMode ⇒ Handle ioMode → Int → IO Bool
hWaitForInput = wrap SIO.hWaitForInput

-- | Wraps: @System.IO.'SIO.hReady'@.
hReady ∷ ReadModes ioMode ⇒ Handle ioMode → IO Bool
hReady = wrap SIO.hReady

-- | Wraps: @System.IO.'SIO.hGetChar'@.
hGetChar ∷ ReadModes ioMode ⇒ Handle ioMode → IO Char
hGetChar = wrap SIO.hGetChar

-- | Wraps: @System.IO.'SIO.hGetLine'@.
hGetLine ∷ ReadModes ioMode ⇒ Handle ioMode → IO String
hGetLine = wrap SIO.hGetLine

-- | Wraps: @System.IO.'SIO.hLookAhead'@.
hLookAhead ∷ ReadModes ioMode ⇒ Handle ioMode → IO Char
hLookAhead = wrap SIO.hLookAhead

-- | Wraps: @System.IO.'SIO.hGetContents'@.
hGetContents ∷ ReadModes ioMode ⇒ Handle ioMode → IO String
hGetContents = wrap SIO.hGetContents


-- ** Text ouput

-- | Wraps: @System.IO.'SIO.hPutChar'@.
hPutChar ∷ WriteModes ioMode ⇒ Handle ioMode → Char → IO ()
hPutChar = wrap SIO.hPutChar

-- | Wraps: @System.IO.'SIO.hPutStr'@.
hPutStr ∷ WriteModes ioMode ⇒ Handle ioMode → String → IO ()
hPutStr = wrap SIO.hPutStr

-- | Wraps: @System.IO.'SIO.hPutStrLn'@.
hPutStrLn ∷ WriteModes ioMode ⇒ Handle ioMode → String → IO ()
hPutStrLn = wrap SIO.hPutStrLn

-- | Wraps: @System.IO.'SIO.hPrint'@.
hPrint ∷ (WriteModes ioMode, Show α) ⇒ Handle ioMode → α → IO ()
hPrint = wrap SIO.hPrint


--------------------------------------------------------------------------------
-- * Binary input and output
--------------------------------------------------------------------------------

-- | Wraps: @System.IO.'SIO.openBinaryFile'@.
openBinaryFile ∷ FilePath → IOMode ioMode → IO (Handle ioMode)
openBinaryFile fp = liftM Handle ∘ SIO.openBinaryFile fp ∘ regularIOMode

-- | Wraps: @System.IO.'SIO.withBinaryFile'@.
withBinaryFile ∷ FilePath → IOMode ioMode → (Handle ioMode → IO α) → IO α
withBinaryFile fp ioMode f = SIO.withBinaryFile fp (regularIOMode ioMode) $ f ∘ Handle


-- ** Opening binary files by inferring the IOMode

-- | Note that: @openBinaryFile' fp = 'openBinaryFile' fp 'mkIOMode'@.
openBinaryFile' ∷ MkIOMode ioMode ⇒ FilePath → IO (Handle ioMode)
openBinaryFile' fp = openBinaryFile fp mkIOMode

-- | Note that: @withBinaryFile' fp = 'withBinaryFile' fp 'mkIOMode'@.
withBinaryFile' ∷ MkIOMode ioMode ⇒ FilePath → (Handle ioMode → IO α) → IO α
withBinaryFile' fp = withBinaryFile fp mkIOMode


-- ** Operations on binary handles

-- | Wraps: @System.IO.'SIO.hSetBinaryMode'@.
hSetBinaryMode ∷ Handle ioMode → Bool → IO ()
hSetBinaryMode = wrap SIO.hSetBinaryMode

-- | Wraps: @System.IO.'SIO.hPutBuf'@.
hPutBuf ∷ WriteModes ioMode ⇒ Handle ioMode → Ptr α → Int → IO ()
hPutBuf = wrap SIO.hPutBuf

-- | Wraps: @System.IO.'SIO.hGetBuf'@.
hGetBuf ∷ ReadModes ioMode ⇒ Handle ioMode → Ptr α → Int → IO Int
hGetBuf = wrap SIO.hGetBuf

#if !defined(__NHC__) && !defined(__HUGS__)

#if MIN_VERSION_base(4,3,0)
-- | Wraps: @System.IO.'SIO.hGetBufSome'@.
hGetBufSome ∷ ReadModes ioMode ⇒ Handle ioMode → Ptr α → Int → IO Int
hGetBufSome = wrap SIO.hGetBufSome
#endif

-- | Wraps: @System.IO.'SIO.hPutBufNonBlocking'@.
hPutBufNonBlocking ∷ WriteModes ioMode ⇒ Handle ioMode → Ptr α → Int → IO Int
hPutBufNonBlocking = wrap SIO.hPutBufNonBlocking

-- | Wraps: @System.IO.'SIO.hGetBufNonBlocking'@.
hGetBufNonBlocking ∷ ReadModes ioMode ⇒ Handle ioMode → Ptr α → Int → IO Int
hGetBufNonBlocking = wrap SIO.hGetBufNonBlocking
#endif


--------------------------------------------------------------------------------
-- * Temporary files
--------------------------------------------------------------------------------

-- | Wraps: @System.IO.'SIO.openTempFile'@.
openTempFile ∷ FilePath → String → IO (FilePath, Handle ReadWriteMode)
openTempFile fp template =
    liftM (second Handle) $ SIO.openTempFile fp template

-- | Wraps: @System.IO.'SIO.openBinaryTempFile'@.
openBinaryTempFile ∷ FilePath → String → IO (FilePath, Handle ReadWriteMode)
openBinaryTempFile fp template =
    liftM (second Handle) $ SIO.openBinaryTempFile fp template

#if MIN_VERSION_base(4,2,0)
openTempFileWithDefaultPermissions ∷ FilePath → String → IO (FilePath, Handle ReadWriteMode)
openTempFileWithDefaultPermissions fp template =
    liftM (second Handle) $ SIO.openTempFileWithDefaultPermissions fp template

openBinaryTempFileWithDefaultPermissions ∷ FilePath → String → IO (FilePath, Handle ReadWriteMode)
openBinaryTempFileWithDefaultPermissions fp template =
    liftM (second Handle) $ SIO.openBinaryTempFileWithDefaultPermissions fp template
#endif

#if MIN_VERSION_base(4,2,0) && !defined(__NHC__) && !defined(__HUGS__)
-- * Unicode encoding/decoding

hSetEncoding ∷ Handle ioMode → SIO.TextEncoding → IO ()
hSetEncoding = wrap SIO.hSetEncoding

hGetEncoding ∷ Handle ioMode → IO (Maybe SIO.TextEncoding)
hGetEncoding = wrap SIO.hGetEncoding


--------------------------------------------------------------------------------
-- * Newline conversion
--------------------------------------------------------------------------------

hSetNewlineMode ∷ Handle ioMode → SIO.NewlineMode → IO ()
hSetNewlineMode = wrap SIO.hSetNewlineMode
#endif


-- The End ---------------------------------------------------------------------
