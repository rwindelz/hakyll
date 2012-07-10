--------------------------------------------------------------------------------
module Hakyll.Core.DependencyAnalyzer.Tests where


--------------------------------------------------------------------------------
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)


--------------------------------------------------------------------------------
import           Hakyll.Core.DependencyAnalyzer
import           Hakyll.Core.DirectedGraph


--------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testCase "findCycle [1]" findCycle1
    , testCase "analyze [1]"   analyze1
    ]


--------------------------------------------------------------------------------
findCycle1 :: Assertion
findCycle1 = Just [4 :: Int, 7, 5, 9, 4] @=?
    findCycle graph
  where
    -- Cycle: 4 -> 7 -> 5 -> 9 -> 4
    graph = fromList
        [ (1, [6])
        , (2, [3])
        , (3, [])
        , (4, [1, 7, 8])
        , (5, [9])
        , (6, [3])
        , (7, [5])
        , (8, [2])
        , (9, [4])
        ]


--------------------------------------------------------------------------------
analyze1 :: Assertion
analyze1 = [7 :: Int, 1, 2, 5, 6, 8, 9] @=?
    analyze prev graph ood
  where
    graph = fromList
        [ (8, [2, 4, 6])
        , (2, [4, 3])
        , (4, [3])
        , (6, [4])
        , (3, [])
        , (9, [5])
        , (5, [7])
        , (1, [7])
        , (7, [])
        ]

    prev = fromList
        [ (8, [2, 4, 6])
        , (2, [4, 3])
        , (4, [3])
        , (6, [4])
        , (3, [])
        , (9, [5])
        , (5, [7])
        , (1, [7])
        , (7, [8])
        ]

    ood = (`elem` [5, 2, 6])
