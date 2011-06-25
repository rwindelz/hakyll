module Hakyll.Core.DependencyAnalyzer.Tests where

import Control.Arrow (second)
import Data.Monoid (mempty)
import qualified Data.Set as S

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Hakyll.Core.DirectedGraph
import Hakyll.Core.DependencyAnalyzer

tests :: [Test]
tests =
    [ testCase "takeAll [1]" takeAll01
    , testCase "takeAll [2]" takeAll02
    , testCase "takeAll [3]" takeAll03
    ]

-- | Debugging method
--
takeAll :: (Show a, Ord a) => DependencyAnalyzer a -> [a]
takeAll a = case takeReady a of
    Nothing      -> []
    Just (x, a') -> x : takeAll (putDone x a')

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

-- | Nothing is out-of-date, but a few dependencies have changed since the last
-- run.
--
takeAll03 :: Assertion
takeAll03 = [1, 2, 5] @=?
    takeAll (makeDependencyAnalyzer graph (const False) prev)
  where
    node = curry $ second S.fromList

    graph = fromList
        [ node (1 :: Int) [3, 4]
        , node 2 [4]
        , node 3 []
        , node 4 []
        , node 5 [1]
        ]

    prev = fromList
        [ node (1 :: Int) [3]
        , node 2 [3, 4]
        , node 3 []
        , node 4 []
        , node 5 [1]
        ]
