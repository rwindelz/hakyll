--------------------------------------------------------------------------------
module Hakyll.Core.DependencyAnalyzer.Tests where


--------------------------------------------------------------------------------
import           Control.Arrow                  (second)
import           Data.Monoid                    (mempty)
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)


--------------------------------------------------------------------------------
import           Hakyll.Core.DependencyAnalyzer
import           Hakyll.Core.DirectedGraph


--------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testCase "analyze [1]" analyze1
    , testCase "analyze [2]" analyze2
    ]


--------------------------------------------------------------------------------
analyze1 :: Assertion
analyze1 = [7, 1, 2, 5, 6, 8, 9] @=?
    analyze prev graph ood
  where
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

    prev = fromList
        [ node 8 [2, 4, 6]
        , node 2 [4, 3]
        , node 4 [3]
        , node 6 [4]
        , node 3 []
        , node 9 [5]
        , node 5 [7]
        , node 1 [7]
        , node 7 [8]
        ]

    ood = (`elem` [5, 2, 6])


--------------------------------------------------------------------------------
analyze2 :: Assertion
analyze2 = [] @=?
    analyze mempty graph ood
  where
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

    ood = const True


--------------------------------------------------------------------------------
-- | Utility: create a node
node :: Ord a => a -> [a] -> (a, Set a)
node = curry $ second S.fromList
