--------------------------------------------------------------------------------
module Hakyll.Core.Resource.Metadata.Cache
    ( resourceMetadata
    , resourceBody
    , invalidate
    ) where


--------------------------------------------------------------------------------
import           Hakyll.Core.Resource
import           Hakyll.Core.Resource.Metadata
import           Hakyll.Core.Resource.Provider
import           Hakyll.Core.Store             (Store)
import qualified Hakyll.Core.Store             as Store


--------------------------------------------------------------------------------
resourceMetadata :: ResourceProvider -> Store -> Resource -> IO Metadata
resourceMetadata provider store rs = do
    load provider store rs
    Just md <- Store.get store [name, unResource rs, "metadata"]
    return md


--------------------------------------------------------------------------------
resourceBody :: ResourceProvider -> Store -> Resource -> IO String
resourceBody provider store rs = do
    load provider store rs
    Just bd <- Store.get store [name, fp, "body"]
    maybe (readFile fp) return bd
  where
    fp = unResource rs


--------------------------------------------------------------------------------
invalidate :: Store -> Resource -> IO ()
invalidate store rs = do
    Store.delete store [name, fp, "metadata"]
    Store.delete store [name, fp, "body"]
  where
    fp = unResource rs


--------------------------------------------------------------------------------
load :: ResourceProvider -> Store -> Resource -> IO ()
load provider store rs = do
    mmd <- Store.get store mdi :: IO (Maybe Metadata)
    case mmd of
        -- Already loaded
        Just _  -> return ()
        -- Not yet loaded
        Nothing -> do
            (metadata, body) <- loadMetadata fp $
                if fileExists provider mfp then  Just mfp else Nothing

            Store.set store mdi metadata
            Store.set store bi  body
  where
    fp   = unResource rs
    mfp  = metadataFilePath fp

    mdi  = [name, fp, "metadata"]
    bi   = [name, fp, "body"]


--------------------------------------------------------------------------------
name :: String
name = "Hakyll.Core.Resource.Metadata.Cache"
