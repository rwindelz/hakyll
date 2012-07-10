--------------------------------------------------------------------------------
-- | Internal structure of the DirectedGraph type. Not exported outside of the
-- library.
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Hakyll.Core.DirectedGraph.Internal
    ( Node (..)
    , DirectedGraph (..)
    ) where


--------------------------------------------------------------------------------
import Prelude hiding (reverse, filter)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (Monoid, mempty, mappend)
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S


--------------------------------------------------------------------------------
import Data.Binary (Binary, put, get)
import Data.Typeable (Typeable)


