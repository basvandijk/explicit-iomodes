{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, DeriveDataTypeable #-}

module System.IO.ExplicitIOModes.Internal where

-- from base:
import Text.Show           ( Show )
import Data.Eq             ( Eq )
import Data.Typeable       ( Typeable )
import qualified System.IO ( Handle )

-- | A handle to a file with an explicit IOMode.
--
-- Wraps: @System.IO.@'SIO.Handle'.
newtype Handle ioMode = Handle
    { -- | Retrieves the regular @System.IO.@'System.IO.Handle'.
      regularHandle ∷ System.IO.Handle
    }
    deriving ( Show, Eq, Typeable )

wrap ∷ (System.IO.Handle → α) → (Handle ioMode → α)
wrap f = \h → f (regularHandle h)

wrap2 ∷ (System.IO.Handle → β → α) → (Handle ioMode → β → α)
wrap2 f = \h y → f (regularHandle h) y
