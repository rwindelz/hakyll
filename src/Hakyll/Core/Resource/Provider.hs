--------------------------------------------------------------------------------
-- | This module provides an wrapper API around the file system which does some
-- caching.
module Hakyll.Core.Resource.Provider
    ( ResourceProvider
    , newResourceProvider

    , resourceList
    , resourceExists

    , resourceString

    , resourceModified

    , resourceMetadata
    , resourceBody
    ) where


--------------------------------------------------------------------------------
import           Hakyll.Core.Resource.Provider.Internal
import           Hakyll.Core.Resource.Provider.MetadataCache
import           Hakyll.Core.Resource.Provider.Modified
