--------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
module Hakyll.Core.Compile
    ( Compile (..)
    , compile
    , create
    ) where


--------------------------------------------------------------------------------
import           Data.Typeable                 (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Item
import           Hakyll.Core.Resource


--------------------------------------------------------------------------------
data Compile i where
    Compile :: Typeable a => Item a -> Compiler i Resource a -> Compile i
    Create  :: Typeable a => Item a -> Compiler i () a       -> Compile i


--------------------------------------------------------------------------------
compile :: Typeable a => Item a -> Compiler i Resource a -> Compile i
compile = Compile


--------------------------------------------------------------------------------
create :: Typeable a => Item a -> Compiler i () a -> Compile i
create = Create
