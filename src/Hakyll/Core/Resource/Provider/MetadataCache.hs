--------------------------------------------------------------------------------
module Hakyll.Core.Resource.Provider.MetadataCache
    ( resourceMetadata
    , resourceBody
    , resourceInvalidateMetadataCache
    ) where


--------------------------------------------------------------------------------
import           Hakyll.Core.Resource
import           Hakyll.Core.Resource.Provider.Internal
import           Hakyll.Core.Resource.Provider.Metadata
import           Hakyll.Core.Store                      (Store)
import qualified Hakyll.Core.Store                      as Store
-- import           Hakyll.Core.Resource.Provider


--------------------------------------------------------------------------------
resourceMetadata :: ResourceProvider -> Store -> Resource -> IO Metadata
resourceMetadata rp store r = do
    load rp store r
    Just md <- Store.get store [name, unResource r, "metadata"]
    return md


--------------------------------------------------------------------------------
resourceBody :: ResourceProvider -> Store -> Resource -> IO String
resourceBody rp store r = do
    load rp store r
    Just bd <- Store.get store [name, unResource r, "body"]
    maybe (resourceString r) return bd


--------------------------------------------------------------------------------
resourceInvalidateMetadataCache :: Store -> Resource -> IO ()
resourceInvalidateMetadataCache store r = do
    Store.delete store [name, unResource r, "metadata"]
    Store.delete store [name, unResource r, "body"]


--------------------------------------------------------------------------------
load :: ResourceProvider -> Store -> Resource -> IO ()
load rp store r = do
    mmd <- Store.get store mdk :: IO (Maybe Metadata)
    case mmd of
        -- Already loaded
        Just _  -> return ()
        -- Not yet loaded
        Nothing -> do
            (metadata, body) <- loadMetadata rp r
            Store.set store mdk metadata
            Store.set store bk  body
  where
    mdk = [name, unResource r, "metadata"]
    bk  = [name, unResource r, "body"]


--------------------------------------------------------------------------------
name :: String
name = "Hakyll.Core.Resource.Provider.MetadataCache"
