module Hakyll.Core.DirectedGraph.Tests where

import qualified Data.Set as S

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Hakyll.Core.DirectedGraph

tests :: [Test]
tests =
    [ testCase "findCycle [1]" findCycle01
    , testCase "findCycle [2]" findCycle02
    ]

findCycle01 :: Assertion
findCycle01 = Just [1, 3, 4, 1] @=? findCycle graph
  where
    graph :: DirectedGraph Int
    graph = fromList
        [ (0, S.fromList [1, 2])
        , (1, S.fromList [3, 2])
        , (3, S.fromList [4, 5])
        , (4, S.fromList [1])
        , (5, S.fromList [])
        ]

findCycle02 :: Assertion
findCycle02 = Just [0, 0] @=? findCycle graph
  where
    graph :: DirectedGraph Int
    graph = fromList
        [ (0, S.fromList [0])
        ]
