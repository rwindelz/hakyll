{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
        FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
module Hakyll.Web.Paginate
    ( paginate
    ) where

import Prelude hiding (id, (.))

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (arr, second, (>>>), (&&&))
import Control.Category (id, (.))
import Data.Data (Data)
import Data.IntMap (IntMap)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Monoid (mappend)
import Data.Ord (comparing)
import Data.Typeable (Typeable)
import qualified Data.IntMap as IM
import qualified Data.Map as M

import Data.Binary (Binary (..))

import Hakyll.Core.Compiler
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Resource
import Hakyll.Core.Rules
import Hakyll.Core.Writable

data Paginated a = Paginated
    { paginatedRoute :: FilePath
    , paginatedData  :: a
    } deriving (Data, Show, Typeable)

instance Binary a => Binary (Paginated a) where
    get                 = Paginated <$> get <*> get
    put (Paginated r d) = put r >> put d

instance Writable (Maybe (Paginated a)) where
    write _ _ = return ()

data Pagination a = Pagination
    { paginationIndices :: Map (Identifier ()) Int
    , paginationPages   :: IntMap (Paginated a)
    } deriving (Data, Show, Typeable)

instance Binary a => Binary (Pagination a) where
    get                  = Pagination <$> get <*> get
    put (Pagination i p) = put i >> put p

instance Writable (Pagination a) where
    write _ _ = return ()

fromPaginated :: forall a k. Ord k
              => (a -> k)
              -> [(Identifier (), Paginated a)]
              -> Pagination a
fromPaginated f pages = Pagination
    { paginationIndices = M.fromList sorted
    , paginationPages   = IM.fromList $ map (second (snd . snd)) indexed
    }
  where
    sorted :: [(Identifier (), Int)]
    sorted = map ((fst . snd) &&& fst) $
        sortBy (comparing $ fst . snd . snd) indexed

    indexed :: [(Int, (Identifier (), (k, Paginated a)))]
    indexed = zip [0 ..] $ map (second ((f . paginatedData) &&& id)) pages

paginate :: (Binary a, Binary k, Ord k, Writable a, Typeable a, Typeable k)
         => Pattern a            -- ^ Pattern selecting items to be paginated
         -> Compiler Resource a  -- ^ Processing items for information
         -> (a -> k)             -- ^ Determine sorting order
         -> RulesM (Identifier (Pagination a))
paginate pattern getData getKey = do
    -- Create group, Exclude the group from the pattern
    group' <- toFilePath <$> freshIdentifier "Hakyll.Web.Paginate.paginate"
    let pattern' = mappend pattern $ predicate $
            not . matches (inGroup (Just group'))

    -- Compile a 'Paginated' value for each item
    paginated <- group group' $ match pattern' $ compile $
        getRoute &&& getData >>> arr (uncurry maybePaginated)

    -- Create a paginated map
    identifier <- freshIdentifier "Hakyll.Web.Paginate.paginate"
    create identifier $
        requireAll_ paginated              >>>
        arr catMaybes                      >>>
        mapCompiler (getIdentifier &&& id) >>>
        arr (fromPaginated getKey)
  where
    maybePaginated Nothing  _ = Nothing
    maybePaginated (Just r) d = Just (Paginated r d)
