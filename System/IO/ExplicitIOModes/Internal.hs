{-# LANGUAGE CPP, NoImplicitPrelude, UnicodeSyntax, DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

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
