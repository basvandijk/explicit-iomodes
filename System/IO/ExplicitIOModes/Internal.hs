{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, DeriveDataTypeable #-}

module System.IO.ExplicitIOModes.Internal where

-- from base:
import Text.Show           ( Show )
import Data.Eq             ( Eq )
import Data.Typeable       ( Typeable )
import qualified System.IO ( Handle )

-- | A handle to a file with an explicit IOMode.
--
-- Wraps: @System.IO.'System.IO.Handle'@.
newtype Handle ioMode = Handle
    { -- | Retrieves the regular @System.IO.@'System.IO.Handle'.
      regularHandle ∷ System.IO.Handle
    }
    deriving ( Show, Eq, Typeable )
