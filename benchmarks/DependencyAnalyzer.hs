--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import           Control.Monad                  (forM, replicateM)
import           Criterion                      (bench, nf)
import           Criterion.Main                 (defaultMain)
import           Data.Monoid                    (mappend, mempty)
import           System.Random                  (mkStdGen)
import           Test.QuickCheck                (choose)
import           Test.QuickCheck.Gen            (Gen, unGen)


--------------------------------------------------------------------------------
import           Hakyll.Core.DependencyAnalyzer
import           Hakyll.Core.DirectedGraph


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ bench "findCycle small" $ nf findCycle small
    , bench "findCycle large" $ nf findCycle large

    , bench "analyze small" $ nf analyze' small
    , bench "analyze large" $ nf analyze' large
    ]
  where
    analyze' x = analyze mempty x (const True)


--------------------------------------------------------------------------------
small :: DirectedGraph Int
small = sample $ genDirectedGraph 10 100 10 0
{-# NOINLINE small #-}


--------------------------------------------------------------------------------
large :: DirectedGraph Int
large = sample $ genDirectedGraph 30 1000 3 0
{-# NOINLINE large #-}


--------------------------------------------------------------------------------
sample :: Gen a -> a
sample gen = unGen gen (mkStdGen 0) 0


--------------------------------------------------------------------------------
genDirectedGraph :: Int                      -- ^ Number of levels
                 -> Int                      -- ^ Level size
                 -> Int                      -- ^ Max deps per node
                 -> Int                      -- ^ Current level
                 -> Gen (DirectedGraph Int)  -- ^ Resulting dgraph
genDirectedGraph levels size maxDeps level
    | level + 1 >= levels = return $ fromList [(n, []) | n <- levelNodes]
    | otherwise           = do
        nextLevel <- genDirectedGraph levels size maxDeps (level + 1)
        list      <- forM levelNodes $ \n -> do
            numdeps <- choose (0, maxDeps)
            deps    <- replicateM numdeps $ choose (ldep, udep)
            return (n, deps)

        return $ fromList list `mappend` nextLevel
  where
    levelNodes   = [level * size + x | x <- [0 .. size - 1]]
    (ldep, udep) = ((level + 1) * size, levels * size - 1)
