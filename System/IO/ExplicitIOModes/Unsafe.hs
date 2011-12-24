{-# LANGUAGE CPP, NoImplicitPrelude, UnicodeSyntax #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

module System.IO.ExplicitIOModes.Unsafe ( regularHandle
                                        , wrap, wrap2
                                        ) where

-- from base:
import qualified System.IO ( Handle )

-- from ourselves:
import System.IO.ExplicitIOModes.Internal ( Handle, regularHandle )

wrap ∷ (System.IO.Handle → α) → (Handle ioMode → α)
wrap f = \h → f (regularHandle h)

wrap2 ∷ (System.IO.Handle → β → α) → (Handle ioMode → β → α)
wrap2 f = \h y → f (regularHandle h) y
