module Hakyll.Core.DependencyAnalyzer
    ( DependencyAnalyzer (..)
    , makeDependencyAnalyzer
    , takeReady
    , putDone
    ) where

import Prelude hiding (reverse)
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
prepare = findReady . findOutOfDate

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

-- | Find items which are ready to be compiled
--
findReady :: Ord a => DependencyAnalyzer a -> DependencyAnalyzer a
findReady analyzer
    | eq analyzer analyzer' = analyzer'
    | otherwise             = findReady analyzer'
  where
    analyzer' = expandDone $ expandReady analyzer
    eq a1 a2 = analyzerReady a1 == analyzerReady a2 &&
        analyzerDone a1 == analyzerDone a2

-- | Expand the ready set by checking for items which are ready to be compiled
--
expandReady :: Ord a => DependencyAnalyzer a -> DependencyAnalyzer a
expandReady (DependencyAnalyzer graph outOfDate remains ready done prev) =
    DependencyAnalyzer graph outOfDate remains' ready' done prev
  where
    ready' = ready `S.union` S.filter isReady remains
    remains' = S.difference remains ready'
    isReady x = all (`S.member` done) $ S.toList $ neighbours x graph

-- | Shrink the ready set by removing items which have been compiled already
--
expandDone :: Ord a => DependencyAnalyzer a -> DependencyAnalyzer a
expandDone (DependencyAnalyzer graph outOfDate remains ready done prev) =
    DependencyAnalyzer graph outOfDate remains ready' done' prev
  where
    done' = done `S.union` S.difference ready outOfDate 
    ready' = S.difference ready done'

-- | Pick an item which is ready to be compiled
--
takeReady :: Ord a => DependencyAnalyzer a
          -> Maybe (a, DependencyAnalyzer a)
takeReady = takeReady' . findReady

-- | Pick an item which is ready to be compiled
--
takeReady' :: Ord a => DependencyAnalyzer a
           -> Maybe (a, DependencyAnalyzer a)
takeReady' analyzer
    | S.null ready = Nothing
    | otherwise = Just (S.findMin ready, analyzer')
  where
    ready = analyzerReady analyzer
    analyzer' = analyzer {analyzerReady = S.deleteMin ready}

-- | Signal an item as done
--
putDone :: Ord a => a -> DependencyAnalyzer a -> DependencyAnalyzer a
putDone x analyzer = analyzer'
  where
    done = analyzerDone analyzer
    analyzer' = analyzer {analyzerDone = S.insert x done}
