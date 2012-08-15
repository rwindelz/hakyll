--------------------------------------------------------------------------------
module Hakyll.Core.Resource.Provider.Modified
    ( resourceModified
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative                         ((<$>), (<*>))
import           Control.Monad                               (when)
import qualified Crypto.Hash.MD5                             as MD5
import qualified Data.ByteString                             as B
import qualified Data.ByteString.Lazy                        as BL
import           Data.IORef
import qualified Data.Map                                    as M


--------------------------------------------------------------------------------
import           Hakyll.Core.Resource
import           Hakyll.Core.Resource.Provider.Internal
import           Hakyll.Core.Resource.Provider.MetadataCache
import           Hakyll.Core.Store                           (Store)
import qualified Hakyll.Core.Store                           as Store


--------------------------------------------------------------------------------
-- | A resource is modified if it or its metadata has changed
resourceModified :: ResourceProvider -> Store -> Resource -> IO Bool
resourceModified rp store r
    | not exists = return False
    | otherwise  = do
        cache <- readIORef cacheRef
        case M.lookup r cache of
            Just m  -> return m
            Nothing -> do
                -- Check if the actual file was modified, and do a recursive
                -- call to check if the metadata file was modified
                m <- (||)
                    <$> fileDigestModified store (unResource r)
                    <*> resourceModified rp store (resourceMetadataResource r)
                modifyIORef cacheRef (M.insert r m)

                -- Important!
                when m $ resourceInvalidateMetadataCache store r

                return m
  where
    exists   = resourceExists rp r
    cacheRef = resourceModifiedCache rp


--------------------------------------------------------------------------------
-- | Utility: Check if a the digest of a file was modified
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
-- | Utility: Retrieve a digest for a given file
fileDigest :: FilePath -> IO B.ByteString
fileDigest = fmap MD5.hashlazy . BL.readFile
