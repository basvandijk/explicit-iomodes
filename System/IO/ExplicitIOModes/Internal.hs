{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, DeriveDataTypeable #-}

module System.IO.ExplicitIOModes.Internal where

-- from base:
import Text.Show                  ( Show )
import Data.Eq                    ( Eq )
import Data.Typeable              ( Typeable )
import qualified System.IO as SIO ( Handle )

-- | A handle to a file with an explicit IOMode.
--
-- Wraps: @System.IO.@'SIO.Handle'.
newtype Handle ioMode = Handle
    { -- | Retrieves the regular @System.IO.@'SIO.Handle'.
      regularHandle ∷ SIO.Handle
    }
    deriving ( Show, Eq, Typeable )

wrap ∷ (SIO.Handle → α) → (Handle ioMode → α)
wrap f = \h → f (regularHandle h)

wrap2 ∷ (SIO.Handle → β → α) → (Handle ioMode → β → α)
wrap2 f = \h y → f (regularHandle h) y
