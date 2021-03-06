--------------------------------------------------------------------------------
module Hakyll.Core.Provider.MetadataCache
    ( resourceMetadata
    , resourceBody
    , resourceInvalidateMetadataCache
    ) where


--------------------------------------------------------------------------------
import qualified Data.Map                      as M


--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider.Internal
import           Hakyll.Core.Provider.Metadata
import qualified Hakyll.Core.Store             as Store


--------------------------------------------------------------------------------
resourceMetadata :: Provider -> Identifier -> IO Metadata
resourceMetadata p r
    | not (resourceExists p r) = return M.empty
    | otherwise                = do
        load p r
        Store.Found md <- Store.get (providerStore p)
            [name, toFilePath r, "metadata"]
        return md


--------------------------------------------------------------------------------
resourceBody :: Provider -> Identifier -> IO String
resourceBody p r = do
    load p r
    Store.Found bd <- Store.get (providerStore p)
        [name, toFilePath r, "body"]
    maybe (resourceString p r) return bd


--------------------------------------------------------------------------------
resourceInvalidateMetadataCache :: Provider -> Identifier -> IO ()
resourceInvalidateMetadataCache p r = do
    Store.delete (providerStore p) [name, toFilePath r, "metadata"]
    Store.delete (providerStore p) [name, toFilePath r, "body"]


--------------------------------------------------------------------------------
load :: Provider -> Identifier -> IO ()
load p r = do
    mmd <- Store.get store mdk :: IO (Store.Result Metadata)
    case mmd of
        -- Already loaded
        Store.Found _  -> return ()
        -- Not yet loaded
        _ -> do
            (metadata, body) <- loadMetadata p r
            Store.set store mdk metadata
            Store.set store bk  body
  where
    store = providerStore p
    mdk   = [name, toFilePath r, "metadata"]
    bk    = [name, toFilePath r, "body"]


--------------------------------------------------------------------------------
name :: String
name = "Hakyll.Core.Resource.Provider.MetadataCache"
