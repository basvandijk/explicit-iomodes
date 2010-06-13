{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Data.ByteString.ExplicitIOModes
-- Copyright   :  (c) 2009-2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module lifts the bytestring IO operations to handles with explicit
-- IOModes.
--
-------------------------------------------------------------------------------

module Data.ByteString.Lazy.Char8.ExplicitIOModes
    ( hGetContents
    , hGet
    , hGetNonBlocking

    , hPut
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Int  ( Int )
import System.IO ( IO )

-- from bytestring:
import Data.ByteString.Lazy.Char8                ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as B ( hGetContents
                                                 , hGet
                                                 , hGetNonBlocking

                                                 , hPut
                                                 )

-- from ourselves:
import System.IO.ExplicitIOModes        ( Handle, ReadModes, WriteModes )
import System.IO.ExplicitIOModes.Unsafe ( wrap, wrap2 )


-------------------------------------------------------------------------------
-- ByteString I/O with file handles with explicit IOModes
-------------------------------------------------------------------------------

-- | Wraps @Data.ByteString.@'B.hGetContents'.
hGetContents ∷ ReadModes ioMode ⇒ Handle ioMode → IO ByteString
hGetContents = wrap B.hGetContents

-- | Wraps @Data.ByteString.@'B.hGet'.
hGet ∷ ReadModes ioMode ⇒ Handle ioMode → Int → IO ByteString
hGet = wrap2 B.hGet

-- | Wraps @Data.ByteString.@'B.hGetNonBlocking'.
hGetNonBlocking ∷ ReadModes ioMode ⇒ Handle ioMode → Int → IO ByteString
hGetNonBlocking = wrap2 B.hGetNonBlocking


-- | Wraps @Data.ByteString.@'B.hPut'.
hPut ∷ WriteModes ioMode ⇒ Handle ioMode → ByteString → IO ()
hPut = wrap2 B.hPut


-- The End ---------------------------------------------------------------------
