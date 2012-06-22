--------------------------------------------------------------------------------
-- | This module provides an wrapper API around the file system which does some
-- caching.
--
-- A resource is represented by the 'Resource' type. This is basically just a
-- newtype wrapper around 'FilePath'.
module Hakyll.Core.Resource.Provider
    ( ResourceProvider
    , new
    , fileExists
    , resourceModified
    , resourceList
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           ((<$>))
import qualified Crypto.Hash.MD5               as MD5
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import           Data.IORef
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Set                      (Set)
import qualified Data.Set                      as S


--------------------------------------------------------------------------------
import           Hakyll.Core.Resource
import           Hakyll.Core.Resource.Metadata
import           Hakyll.Core.Store             (Store)
import qualified Hakyll.Core.Store             as Store
import           Hakyll.Core.Util.File


--------------------------------------------------------------------------------
-- | Responsible for retrieving and listing resources
data ResourceProvider = ResourceProvider
    { -- | A list of all files found
      resourceSet           :: Set Resource
    , -- | Cache keeping track of modified files
      resourceModifiedCache :: IORef (Map Resource Bool)
    }


--------------------------------------------------------------------------------
-- | Create a resource provider
new :: (FilePath -> Bool)   -- ^ Should we ignore this file?
    -> FilePath             -- ^ Search directory
    -> IO ResourceProvider  -- ^ Resulting provider
new ignore directory = do
    list  <- map resource . filter (not . ignore) <$>
        getRecursiveContents False directory
    cache <- newIORef M.empty
    return $ ResourceProvider (S.fromList list) cache


--------------------------------------------------------------------------------
-- | A resource is modified if it or its metadata has changed
resourceModified :: ResourceProvider -> Store -> Resource -> IO Bool
resourceModified provider store rs = do
    cache <- readIORef cacheRef
    case M.lookup rs cache of
        -- Already in the cache
        Just m  -> return m
        -- Not yet in the cache, check digests (if it exists)
        Nothing -> do
            m <- check
            modifyIORef cacheRef (M.insert rs m)
            -- TODO: Invalidate metadata cache
            return m
  where
    cacheRef = resourceModifiedCache provider
    check    = fmap or $ mapM (fileDigestModified store) $
        if fileExists provider mfp then [mfp, fp] else [fp]

    fp  = unResource rs
    mfp = metadataFilePath fp


--------------------------------------------------------------------------------
-- | Check if a given identifier has a resource
fileExists :: ResourceProvider -> FilePath -> Bool
fileExists provider = (`S.member` resourceSet provider) . resource


--------------------------------------------------------------------------------
-- | Check if a the digest of a file was modified
fileDigestModified :: Store -> FilePath -> IO Bool
fileDigestModified store fp = do
    -- Get the latest seen digest from the store, and calculate the current
    -- digest for the
    lastDigest <- Store.get store key
    newDigest  <- fileDigest fp
    if Just newDigest == lastDigest
        -- All is fine, not modified
        then return False
        -- Resource modified; store new digest
        else do
            Store.set store key newDigest
            return True
  where
    key = ["Hakyll.Core.Resource.Provider.fileModified", fp]


--------------------------------------------------------------------------------
-- | Retrieve a digest for a given file
fileDigest :: FilePath -> IO B.ByteString
fileDigest = fmap MD5.hashlazy . BL.readFile


--------------------------------------------------------------------------------
resourceList :: ResourceProvider -> [Resource]
resourceList = S.toList . resourceSet
