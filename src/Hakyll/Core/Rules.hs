--------------------------------------------------------------------------------
-- | This module provides a declarative DSL in which the user can specify the
-- different rules used to run the compilers.
--
-- The convention is to just list all items in the 'RulesM' monad, routes and
-- compilation rules.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Rules
    ( RulesM
    , Rules
    , runRules

    , file
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           (Applicative)
import           Control.Monad                 (forM_)
import           Control.Monad.Reader          (ReaderT, ask, runReaderT)
import           Control.Monad.Trans           (MonadIO, liftIO)


--------------------------------------------------------------------------------
import           Hakyll.Core.Pattern
import           Hakyll.Core.Resource
import           Hakyll.Core.Resource.Provider


--------------------------------------------------------------------------------
newtype RulesM a = RulesM
    { unRulesM :: ReaderT ResourceProvider IO a
    } deriving (Applicative, Functor, Monad, MonadIO)


--------------------------------------------------------------------------------
type Rules = RulesM ()


--------------------------------------------------------------------------------
runRules :: Rules -> ResourceProvider -> IO ()
runRules rules = runReaderT (unRulesM rules)


--------------------------------------------------------------------------------
file :: Pattern -> ([String] -> IO ()) -> Rules
file pattern f = RulesM $ do
    provider <- ask
    forM_ (resourceList provider) $ \rs ->
        case capture pattern (unResource rs) of
            Nothing -> return ()
            Just cs -> liftIO $ f cs
