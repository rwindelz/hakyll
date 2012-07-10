--------------------------------------------------------------------------------
module Hakyll.Core.DependencyAnalyzer where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Monad             (filterM, forM_, when)
import           Control.Monad.Reader      (ask)
import           Control.Monad.RWS         (RWS, runRWS)
import           Control.Monad.State       (evalState, get, modify)
import           Control.Monad.Writer      (tell)
import qualified Data.Map                  as M
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           Prelude                   hiding (reverse)
import qualified Prelude                   as P (reverse)


--------------------------------------------------------------------------------
import           Hakyll.Core.DirectedGraph


--------------------------------------------------------------------------------
analyze :: Ord a
        => DirectedGraph a  -- ^ Old graph
        -> DirectedGraph a  -- ^ New graph
        -> (a -> Bool)      -- ^ Out of date?
        -> [a]              -- ^ Resulting list
analyze old new ood = ls
  where
    -- Make an extension of ood: an item is ood when it is actually ood OR if
    -- the list of its dependencies has changed. Based on that, create a set of
    -- dirty items.
    ood' x = ood x || neighbours x old /= neighbours x new
    dirty' = dirty ood' new

    -- Run all walks in our own little monad...
    (_, _, ls) = runRWS walks new dirty'


--------------------------------------------------------------------------------
type Analyzer i a = RWS (DirectedGraph i) [i] (Set i) a


--------------------------------------------------------------------------------
isDirty :: Ord a => a -> Analyzer a Bool
isDirty x = (x `S.member`) <$> get


--------------------------------------------------------------------------------
walks :: Ord a
      => Analyzer a ()
walks = do
    dirty' <- get
    if S.null dirty'
        then return ()
        else do
            walk $ S.findMin dirty'
            walks


--------------------------------------------------------------------------------
-- | Invariant: given node to walk /must/ be dirty
walk :: Ord a
     => a
     -> Analyzer a ()
walk x = do
    -- Determine dirty neighbours and walk them
    dg <- ask
    forM_ (S.toList $ neighbours x dg) $ \n -> do
        d <- isDirty n
        when d $ walk n

    -- Once all dirty neighbours are done, we're safe to go
    tell [x]
    modify $ S.delete x


--------------------------------------------------------------------------------
-- | This auxiliary function checks which nodes are dirty: a node is dirty if
-- it's out-of-date or if one of its dependencies is dirty.
dirty :: Ord a
      => (a -> Bool)      -- ^ Out of date?
      -> DirectedGraph a  -- ^ Graph
      -> Set a            -- ^ All dirty items
dirty ood dg = S.fromList $ flip evalState M.empty $
    filterM go $ S.toList $ nodes dg
  where
    go x = do
        m <- get
        case M.lookup x m of
            Just d  -> return d
            Nothing -> do
                nd <- mapM go $ S.toList $ neighbours x dg
                let d = ood x || or nd
                modify $ M.insert x d
                return d
