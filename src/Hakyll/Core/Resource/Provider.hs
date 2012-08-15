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
import           Hakyll.Core.Resource
import           Hakyll.Core.Resource.Provider.Internal
import qualified Hakyll.Core.Resource.Provider.MetadataCache as Internal
import           Hakyll.Core.Resource.Provider.Modified


--------------------------------------------------------------------------------
-- | Wrapper to ensure metadata cache is invalidated if necessary
resourceMetadata :: ResourceProvider -> Resource -> IO Metadata
resourceMetadata rp r = do
    _ <- resourceModified rp r
    Internal.resourceMetadata rp r


--------------------------------------------------------------------------------
-- | Wrapper to ensure metadata cache is invalidated if necessary
resourceBody :: ResourceProvider -> Resource -> IO String
resourceBody rp r = do
    _ <- resourceModified rp r
    Internal.resourceBody rp r
