--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
module Hakyll.Core.Compile
    ( Box (..)
    , box

    , Compile (..)
    , compile
    , create
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                 (arr, (>>>))
import           Data.Binary                   (Binary)
import           Data.Typeable                 (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Item
import           Hakyll.Core.Resource


--------------------------------------------------------------------------------
data Box = forall a. (Binary a, Typeable a) => Box a


--------------------------------------------------------------------------------
box :: (Binary a, Typeable a) => Compiler i a Box
box = arr Box


--------------------------------------------------------------------------------
newtype Compile i = Compile (Compiler i () Box)


--------------------------------------------------------------------------------
create :: (Binary a, Typeable a)
       => Item a -> Compiler i () a -> Compile i
create _ compiler = Compile (compiler >>> box)


--------------------------------------------------------------------------------
compile :: (Binary a, Typeable a)
        => Item a -> Compiler i Resource a -> Compile i
compile _ compiler = Compile (getResource >>> compiler >>> box)
