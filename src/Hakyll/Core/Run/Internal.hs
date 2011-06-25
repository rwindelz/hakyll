-- | Core types and monad stack for the run module
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Run.Internal
    ( RuntimeEnvironment (..)
    , RuntimeState (..)
    , Runtime (..)
    ) where

import Prelude hiding (reverse)
import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Error (ErrorT)
import Data.Map (Map)

import Hakyll.Core.Routes
import Hakyll.Core.Identifier
import Hakyll.Core.Compiler
import Hakyll.Core.Resource.Provider
import Hakyll.Core.Rules.Internal
import Hakyll.Core.DependencyAnalyzer
import Hakyll.Core.Store
import Hakyll.Core.Configuration
import Hakyll.Core.Logger

data RuntimeEnvironment = RuntimeEnvironment
    { hakyllLogger           :: Logger
    , hakyllConfiguration    :: HakyllConfiguration
    , hakyllRoutes           :: Routes
    , hakyllResourceProvider :: ResourceProvider
    , hakyllStore            :: Store
    , hakyllFirstRun         :: Bool
    }

data RuntimeState = RuntimeState
    { hakyllAnalyzer  :: DependencyAnalyzer (Identifier ())
    , hakyllCompilers :: Map (Identifier ()) (Compiler () CompileRule)
    }

newtype Runtime a = Runtime
    { unRuntime :: ErrorT String
        (ReaderT RuntimeEnvironment (StateT RuntimeState IO)) a
    } deriving (Functor, Applicative, Monad)
