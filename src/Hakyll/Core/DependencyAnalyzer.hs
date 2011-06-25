module Hakyll.Core.DependencyAnalyzer where
    {-
    ( DependencyAnalyzer (..)
    , Signal (..)
    , makeDependencyAnalyzer
    , step
    , stepAll
    ) where
    -}

import Prelude hiding (reverse)
import qualified Prelude as P (reverse)
import Control.Arrow (first)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Monoid (Monoid, mappend, mempty)

import Hakyll.Core.DirectedGraph

-- | This data structure represents the state of the dependency analyzer. It
-- holds a complete graph in 'analyzerGraph', which always contains all items,
-- whether they are to be compiled or not.
--
-- We have three sets, 'analyzerRemains', 'analyzerReady' and 'analyzerDone'.
-- Together, they hold all known items.
--
-- We also hold the dependency graph from the previous run because we need it
-- when we want to determine when an item is out-of-date. An item is out-of-date
-- when:
--
-- * the resource from which it compiles is out-of-date, or;
--
-- * any of it's dependencies is out-of-date, or;
--
-- * it's set of dependencies has changed since the previous run.
--
data DependencyAnalyzer a = DependencyAnalyzer
    { -- | The complete dependency graph
      analyzerGraph         :: DirectedGraph a
    , -- | Out-of-date items
      analyzerOutOfDate     :: Set a
    , -- | A set of items yet to be compiled
      analyzerRemains       :: Set a
    , -- | Set of items ready to be compiled
      analyzerReady         :: Set a
    , -- | A set of items already compiled
      analyzerDone          :: Set a
    , -- | The dependency graph from the previous run
      analyzerPreviousGraph :: DirectedGraph a
    } deriving (Show)

instance (Ord a, Show a) => Monoid (DependencyAnalyzer a) where
    mempty = DependencyAnalyzer mempty mempty mempty mempty mempty mempty
    mappend x y = prepare $ DependencyAnalyzer
        (analyzerGraph x `mappend` analyzerGraph y)
        (analyzerOutOfDate x `mappend` analyzerOutOfDate y)
        (analyzerRemains x `mappend` analyzerRemains y)
        (analyzerReady x `mappend` analyzerReady y)
        (analyzerDone x `mappend` analyzerDone y)
        (analyzerPreviousGraph x `mappend` analyzerPreviousGraph y)

-- | Smart constructor
--
makeDependencyAnalyzer :: (Ord a, Show a)
                       => DirectedGraph a       -- ^ The dependency graph
                       -> (a -> Bool)           -- ^ Is an item out-of-date?
                       -> DirectedGraph a       -- ^ The old dependency graph
                       -> DependencyAnalyzer a  -- ^ Resulting analyzer
makeDependencyAnalyzer graph isOutOfDate prev = prepare $
    DependencyAnalyzer graph outOfDate remains S.empty S.empty prev
  where
    -- Construct the remains set by filtering using the given predicate
    remains = S.fromList $ map fst $ toList graph
    outOfDate = S.filter isOutOfDate remains

-- | Prepare a dependency analyzer for use
--
prepare :: Ord a => DependencyAnalyzer a -> DependencyAnalyzer a
prepare = expandAll . findOutOfDate

expandAll :: Ord a => DependencyAnalyzer a -> DependencyAnalyzer a
expandAll analyzer
    | eq analyzer analyzer' = analyzer'
    | otherwise             = expandAll analyzer'
  where
    analyzer' = expandDone $ expandReady analyzer
    eq a1 a2 = analyzerReady a1 == analyzerReady a2 &&
        analyzerDone a1 == analyzerDone a2

findOutOfDate :: Ord a => DependencyAnalyzer a -> DependencyAnalyzer a
findOutOfDate (DependencyAnalyzer graph outOfDate remains ready done prev) =
    DependencyAnalyzer graph outOfDate' remains ready done prev
  where
    -- Select the nodes which are reachable from the remaining nodes in the
    -- reversed dependency graph: these are the indirectly out-of-date items
    outOfDate' = reachableNodes (outOfDate `S.union` changedDeps) $
        reverse graph

    -- For all nodes in the graph, check which items have a different dependency
    -- set compared to the previous run
    changedDeps = S.fromList $ map fst $
        filter (uncurry (/=) . first (`neighbours` prev)) $ toList graph

expandReady :: Ord a => DependencyAnalyzer a -> DependencyAnalyzer a
expandReady (DependencyAnalyzer graph outOfDate remains ready done prev) =
    DependencyAnalyzer graph outOfDate remains' ready' done prev
  where
    ready' = ready `S.union` S.filter isReady remains
    remains' = S.difference remains ready'
    isReady x = all (`S.member` done) $ S.toList $ neighbours x graph

expandDone :: Ord a => DependencyAnalyzer a -> DependencyAnalyzer a
expandDone (DependencyAnalyzer graph outOfDate remains ready done prev) =
    DependencyAnalyzer graph outOfDate remains ready' done' prev
  where
    done' = done `S.union` S.difference ready outOfDate 
    ready' = S.difference ready done'

takeReady :: Ord a => DependencyAnalyzer a -> Maybe (a, DependencyAnalyzer a)
takeReady analyzer
    | S.null ready = Nothing
    | otherwise    = Just (S.findMin ready, analyzer')
  where
    ready = analyzerReady analyzer
    analyzer' = analyzer {analyzerReady = S.deleteMin ready}

putDone :: Ord a => a -> DependencyAnalyzer a -> DependencyAnalyzer a
putDone x analyzer = expandDone $ expandReady analyzer'
  where
    done = analyzerDone analyzer
    analyzer' = analyzer {analyzerDone = S.insert x done}

{-
-- | Construct a dependency analyzer
--
makeDependencyAnalyzer :: (Ord a, Show a)
                       => DirectedGraph a       -- ^ The dependency graph
                       -> (a -> Bool)           -- ^ Is an item out-of-date?
                       -> DirectedGraph a       -- ^ The old dependency graph
                       -> DependencyAnalyzer a  -- ^ Resulting analyzer
makeDependencyAnalyzer graph isOutOfDate prev =
    growRemains $ DependencyAnalyzer graph remains S.empty done prev
  where
    -- Construct the remains set by filtering using the given predicate
    (remains, done) = S.partition isOutOfDate $
        S.fromList $ map fst $ toList graph

-- | The 'analyzerRemains' field of a 'DependencyAnalyzer' is supposed to
-- contain all out-of-date items, including the items with out-of-date
-- dependencies. However, it is easier to just set up the directly out-of-date
-- items initially -- and then grow the remains fields.
--
-- This function assumes the 'analyzerRemains' fields in incomplete, and tries
-- to correct it. Running it when the field is complete has no effect -- but it
-- is a pretty expensive function, and it should be used with care.
--
growRemains :: (Ord a, Show a) => DependencyAnalyzer a -> DependencyAnalyzer a
growRemains (DependencyAnalyzer graph remains ready done prev) =
    (DependencyAnalyzer graph remains' done' prev)
  where
    -- Grow the remains set using the indirect and changedDeps values, then
    -- filter out the items already done
    remains' = S.filter (`S.notMember` done) indirect
    done' = done `S.difference` remains'

    -- Select the nodes which are reachable from the remaining nodes in the
    -- reversed dependency graph: these are the indirectly out-of-date items
    indirect = reachableNodes (remains `S.union` changedDeps) $ reverse graph

    -- For all nodes in the graph, check which items have a different dependency
    -- set compared to the previous run
    changedDeps = S.fromList $ map fst $
        filter (uncurry (/=) . first (`neighbours` prev)) $ toList graph

-- | Step a dependency analyzer
--
step :: (Ord a, Show a) => DependencyAnalyzer a -> (Signal a, DependencyAnalyzer a)
step analyzer@(DependencyAnalyzer graph remains done prev)
    -- No remaining items
    | S.null remains = (Done, analyzer)
    -- An item remains, let's find a ready item
    | otherwise =
        let item = S.findMin remains
        in case findReady analyzer item of
            Done        -> (Done, analyzer)
            Cycle c     -> (Cycle c, analyzer)
            -- A ready item was found, signal a build
            Build build ->
                let remains' = S.delete build remains
                    done'    = S.insert build done
                in (Build build, DependencyAnalyzer graph remains' done' prev)

-- | Step until done, creating a set of items we need to build -- mostly used
-- for debugging purposes
--
stepAll :: (Ord a, Show a) => DependencyAnalyzer a -> Maybe (Set a)
stepAll = stepAll' S.empty
  where
    stepAll' xs analyzer = case step analyzer of
        (Build x, analyzer') -> stepAll' (S.insert x xs) analyzer'
        (Done, _)            -> Just xs
        (Cycle _, _)         -> Nothing

-- | Find an item ready to be compiled
--
findReady :: (Ord a, Show a) => DependencyAnalyzer a -> a -> Signal a
findReady analyzer = findReady' [] S.empty
  where
    -- The dependency graph
    graph = analyzerGraph analyzer

    -- Items to do
    todo = analyzerRemains analyzer `S.difference` analyzerDone analyzer

    -- Worker
    findReady' stack visited item
        -- We already visited this item, the cycle is the reversed stack
        | item `S.member` visited = Cycle $ P.reverse stack'
        -- Look at the neighbours we to do
        | otherwise = case filter (`S.member` todo) neighbours' of
            -- No neighbours available to be done: it's ready!
            []      -> Build item
            -- At least one neighbour is available, search for that one
            (x : _) -> findReady' stack' visited' x
      where
        -- Our neighbours
        neighbours' = S.toList $ neighbours item graph

        -- The new visited stack/set
        stack' = item : stack
        visited' = S.insert item visited
-}
