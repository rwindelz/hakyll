--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
module Hakyll.Core.Compile
    ( Compile (..)
    , compile
    , create
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                 ((>>>))
import           Data.Typeable                 (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Item
import           Hakyll.Core.Resource


--------------------------------------------------------------------------------
data Compile i = forall a. Typeable a => Compile (Compiler i () a)


--------------------------------------------------------------------------------
create :: Typeable a => Item a -> Compiler i () a -> Compile i
create _ = Compile


--------------------------------------------------------------------------------
compile :: Typeable a => Item a -> Compiler i Resource a -> Compile i
compile _ compiler = Compile (getResource >>> compiler)
