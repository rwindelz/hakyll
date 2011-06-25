-- | This is the module which binds it all together
--
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Hakyll.Core.Run
    ( run
    ) where

import Prelude hiding (reverse)
import Control.Monad (filterM, forM_)
import Control.Monad.Trans (liftIO)
import Control.Monad.Error (runErrorT, throwError)
import Control.Applicative (Applicative, (<$>))
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State.Strict (StateT, runStateT, get, put, modify)
import Data.Map (Map)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Monoid (mempty, mappend)
import System.FilePath ((</>))
import qualified Data.Set as S

import Hakyll.Core.Routes
import Hakyll.Core.Identifier
import Hakyll.Core.Util.File
import Hakyll.Core.Compiler
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Resource
import Hakyll.Core.Resource.Provider
import Hakyll.Core.Resource.Provider.File
import Hakyll.Core.Rules.Internal
import Hakyll.Core.DependencyAnalyzer
import Hakyll.Core.Writable
import Hakyll.Core.Store
import Hakyll.Core.Configuration
import Hakyll.Core.Logger
import Hakyll.Core.Run.Internal
import Hakyll.Core.Run.Workers
import qualified Hakyll.Core.DirectedGraph as DG

-- | Run all rules needed, return the rule set used
--
run :: HakyllConfiguration -> RulesM a -> IO RuleSet
run configuration rules = do
    logger <- makeLogger putStrLn

    message logger "Creating store"
    store <- makeStore $ storeDirectory configuration
    message logger "Creating provider"
    provider <- fileResourceProvider configuration
    message logger "Creating workers"
    workers <- makeWorkers

    -- Fetch the old graph from the store. If we don't find it, we consider this
    -- to be the first run
    graph <- storeGet store "Hakyll.Core.Run.run" "dependencies"
    let (firstRun, oldGraph) = case graph of Found g -> (False, g)
                                             _       -> (True, mempty)

    let ruleSet = runRules rules provider
        compilers = rulesCompilers ruleSet

        -- Extract the reader/state
        reader = runErrorT $ unRuntime $
            addNewCompilers compilers >> run'
        stateT = runReaderT reader $ RuntimeEnvironment
                    { hakyllLogger           = logger
                    , hakyllConfiguration    = configuration
                    , hakyllRoutes           = rulesRoutes ruleSet
                    , hakyllResourceProvider = provider
                    , hakyllStore            = store
                    , hakyllFirstRun         = firstRun
                    , hakyllWorkers          = workers
                    }

    -- Run the program and fetch the resulting state
    (x, state') <- runStateT stateT $ RuntimeState
        { hakyllAnalyzer  = makeDependencyAnalyzer mempty (const False) oldGraph
        , hakyllCompilers = M.empty
        , hakyllRunning   = S.empty
        }

    -- Print possible errors
    case x of Left e -> message logger e; Right _ -> return ()

    -- We want to save the final dependency graph for the next run
    storeSet store "Hakyll.Core.Run.run" "dependencies" $
        analyzerGraph $ hakyllAnalyzer state'

    -- Flush and return
    flushLogger logger
    return ruleSet

-- | Add a number of compilers and continue using these compilers
--
addNewCompilers :: [(Identifier (), Compiler () CompileRule)]
                -- ^ Compilers to add
                -> Runtime ()
addNewCompilers newCompilers = Runtime $ do
    -- Get some information
    logger <- hakyllLogger <$> ask
    message logger "Adding new compilers"
    provider <- hakyllResourceProvider <$> ask
    store <- hakyllStore <$> ask
    firstRun <- hakyllFirstRun <$> ask

    -- Old state information
    oldCompilers <- hakyllCompilers <$> get
    oldAnalyzer <- hakyllAnalyzer <$> get
    oldRunning <- hakyllRunning <$> get

    let -- All known compilers
        universe = M.keys oldCompilers ++ map fst newCompilers

        -- Create a new partial dependency graph
        dependencies = flip map newCompilers $ \(id', compiler) ->
            let deps = runCompilerDependencies compiler id' universe
            in (id', deps)

        -- Create the dependency graph
        newGraph = DG.fromList dependencies

    -- Check which items have been modified
    modified <- fmap S.fromList $ flip filterM (map fst newCompilers) $
        liftIO . resourceModified provider store . fromIdentifier
    let checkModified = if firstRun then const True else (`S.member` modified)

    -- Create a new analyzer and append it to the currect one
        newAnalyzer = makeDependencyAnalyzer newGraph checkModified $
            analyzerPreviousGraph oldAnalyzer
        analyzer = mappend oldAnalyzer newAnalyzer

    -- Check for cycles
    case findCycle analyzer of
        Nothing -> return ()
        Just c  -> throwError $ "Dependency cycle: " ++
            intercalate " -> " (map show c)

    -- Update the state
    put $ RuntimeState
        { hakyllAnalyzer  = analyzer
        , hakyllCompilers = M.union oldCompilers (M.fromList newCompilers)
        , hakyllRunning   = oldRunning
        }

run' :: Runtime ()
run' = Runtime $ do
    -- Step the analyzer
    state <- get
    case takeReady (hakyllAnalyzer state) of
        -- No compilers are ready. Check if we have compilers running.
        Nothing -> if (S.null $ hakyllRunning state)
            -- No compilers running. We are done.
            then return ()
            -- Compilers are still running
            else unRuntime $ endCompiler >> run'
        Just (x, a) -> do
            put $ state {hakyllAnalyzer = a}
            unRuntime $ startCompiler x >> run'

endCompiler :: Runtime ()
endCompiler = Runtime $ do
    workers <- hakyllWorkers <$> ask
    -- Wait for a compiler
    (id', rule) <- liftIO $ readJob workers
    -- Remove it from the running set and put it as done
    modify $ \s -> s
        { hakyllRunning  = S.delete id' (hakyllRunning s)
        , hakyllAnalyzer = putDone id' (hakyllAnalyzer s)
        }
    -- If needed, add new compilers
    case rule of Right (MetaCompileRule n) -> unRuntime $ addNewCompilers n
                 Left err                  -> throwError err
                 _                         -> return ()

startCompiler :: Identifier () -> Runtime ()
startCompiler id' = Runtime $ do
    logger <- hakyllLogger <$> ask
    routes <- hakyllRoutes <$> ask
    provider <- hakyllResourceProvider <$> ask
    store <- hakyllStore <$> ask
    destination <- destinationDirectory . hakyllConfiguration <$> ask
    workers <- hakyllWorkers <$> ask
    compilers <- hakyllCompilers <$> get

    -- Set as running
    modify $ \s -> s {hakyllRunning = S.insert id' (hakyllRunning s)}

    liftIO $ writeJob workers $ do
        -- Fetch the right compiler from the map
        let compiler = compilers M.! id'

        -- Check if the resource was modified
        isModified <- liftIO $ resourceModified provider store $ fromIdentifier id'

        -- Run the compiler
        result <- liftIO $ runCompiler compiler id' provider (M.keys compilers)
            routes store isModified logger

        case result of
            -- Compile rule for one item, easy stuff
            Right (CompileRule compiled) -> do
                case runRoutes routes id' of
                    Nothing  -> return ()
                    Just url -> do
                        messageAbout logger id' $ "Routing to " ++ url
                        let path = destination </> url
                        liftIO $ makeDirectories path
                        liftIO $ write path compiled

            -- Metacompile rule or error. We don't need to deal with it here
            _ -> return ()

        return (id', result)
