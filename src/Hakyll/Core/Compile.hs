--------------------------------------------------------------------------------
module Hakyll.Core.Compile
    ( Compile (..)
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
newtype Compile i = Compile (Compiler i () Box)


--------------------------------------------------------------------------------
create :: (Binary a, Typeable a)
       => Item a -> Compiler i () a -> Compile i
create _ compiler = Compile (compiler >>> box)


--------------------------------------------------------------------------------
compile :: (Binary a, Typeable a)
        => Item a -> Compiler i Resource a -> Compile i
compile _ compiler = Compile (getResource >>> compiler >>> box)
