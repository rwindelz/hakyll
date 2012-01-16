{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
        FlexibleInstances, OverloadedStrings #-}
module Hakyll.Web.Paginate
    ( paginate
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (arr, (>>>), (&&&))
import Data.Data (Data)
import Data.Map (Map)
import Data.Typeable (Typeable)
import Data.Monoid (mappend)
import Data.Maybe (catMaybes)
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

newtype PaginatedMap k a = PaginatedMap (Map k (Paginated a))
    deriving (Binary, Data, Show, Typeable)

instance Writable (PaginatedMap k a) where
    write _ _ = return ()

fromPaginated :: Ord k
              => (a -> k)
              -> [Paginated a]
              -> PaginatedMap k a
fromPaginated f = PaginatedMap . M.fromList . map ((f . paginatedData) &&& id)

paginate :: (Binary a, Binary k, Ord k, Writable a, Typeable a, Typeable k)
         => Pattern a            -- ^ Pattern selecting items to be paginated
         -> Compiler Resource a  -- ^ Processing items for information
         -> (a -> k)             -- ^ Determine sorting order
         -> RulesM (Identifier (PaginatedMap k a))
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
        requireAll_ paginated      >>>
        arr catMaybes              >>>
        arr (fromPaginated getKey)
  where
    maybePaginated Nothing  _ = Nothing
    maybePaginated (Just r) d = Just (Paginated r d)
