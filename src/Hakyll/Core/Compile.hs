--------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
module Hakyll.Core.Compile
    ( Compile (..)
    , compile
    , create
    ) where


--------------------------------------------------------------------------------
import           Data.Typeable        (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Core.Resource


--------------------------------------------------------------------------------
data Compile where
    Compile :: Typeable a => Item a -> Compiler Resource a -> Compile
    Create  :: Typeable a => Item a -> Compiler () a       -> Compile


--------------------------------------------------------------------------------
compile :: Typeable a => Item a -> Compiler Resource a -> Compile
compile = Compile


--------------------------------------------------------------------------------
create :: Typeable a => Item a -> Compiler () a -> Compile
create = Create
