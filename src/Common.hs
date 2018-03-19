{-# LANGUAGE CPP #-}

module Common
    ( (<>)
    ) where
#if MIN_VERSION_base(4, 8, 0)
import Data.Monoid ((<>))
#endif
