module Hakyll.Core.DependencyAnalyzer.Tests where

import Control.Arrow (second)
import qualified Data.Set as S
import Data.Monoid (mempty)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Hakyll.Core.DirectedGraph
import Hakyll.Core.DependencyAnalyzer

tests :: [Test]
tests =
    [ testCase "step [1]" takeAll01
    -- , testCase "step [2]" step2
    ]

-- Debugging method
takeAll :: Ord a => DependencyAnalyzer a -> [a]
takeAll analyzer = case takeReady analyzer of
    Nothing     -> []
    Just (x, a) -> x : takeAll (putDone x a)

-- | General order testing
--
takeAll01 :: Assertion
takeAll01 = [3, 4, 2, 6, 7, 1, 5, 8, 9] @=?
    takeAll (makeDependencyAnalyzer graph (const True) mempty)
  where
    node = curry $ second S.fromList

    graph = fromList
        [ node (8 :: Int) [2, 4, 6]
        , node 2 [4, 3]
        , node 4 [3]
        , node 6 [4]
        , node 3 []
        , node 9 [5]
        , node 5 [7]
        , node 1 [7]
        , node 7 []
        ]

-- | A few out of date items
--
takeAll02 :: Assertion
takeAll02 = [2, 3, 4] @=?
    takeAll (makeDependencyAnalyzer graph isOutOfDate graph)
  where
    node = curry $ second S.fromList

    isOutOfDate = (== 2)

    graph = fromList
        [ node (1 :: Int) []
        , node 2 [1]
        , node 3 [1, 2]
        , node 4 [1, 2]
        ]

ana = (makeDependencyAnalyzer graph isOutOfDate graph)
  where
    node = curry $ second S.fromList

    isOutOfDate = (== 2)

    graph = fromList
        [ node (1 :: Int) []
        , node 2 [1]
        , node 3 [1, 2]
        , node 4 [1, 2]
        ]
{-
step2 :: Assertion
step2 = Nothing @?= takeAll (makeDependencyAnalyzer graph isOutOfDate mempty)
  where
    node = curry $ second S.fromList

    -- Cycle: 4 -> 7 -> 5 -> 9 -> 4
    graph = fromList
        [ node (1 :: Int) [6]
        , node 2 [3]
        , node 3 []
        , node 4 [1, 7, 8]
        , node 5 [9]
        , node 6 [3]
        , node 7 [5]
        , node 8 [2]
        , node 9 [4]
        ]

    isOutOfDate = const True
-}
