--------------------------------------------------------------------------------
-- | A store for storing and retreiving items
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Hakyll.Core.Store
    ( Store
    , new
    , set
    , get
    , delete
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Exception   (IOException, handle)
import qualified Crypto.Hash.MD5     as MD5
import           Data.Binary         (Binary, decodeFile, encodeFile)
import qualified Data.ByteString     as B
import           Data.IORef          (IORef, modifyIORef, newIORef, readIORef)
import           Data.List           (intercalate)
import           Data.Map            (Map)
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           System.Directory    (createDirectoryIfMissing)
import           System.Directory    (doesFileExist, removeFile)
import           System.FilePath     ((</>))
import           Text.Printf         (printf)
import           Unsafe.Coerce       (unsafeCoerce)


--------------------------------------------------------------------------------
-- | Simple wrapper type
data Box = forall a. Box a


--------------------------------------------------------------------------------
data Store = Store
    { -- | All items are stored on the filesystem
      storeDirectory :: FilePath
    , -- | Optionally, items are also kept in-memory
      storeMap       :: Maybe (IORef (Map String Box))
    }


--------------------------------------------------------------------------------
-- | Initialize the store
new :: Bool      -- ^ Use in-memory caching
    -> FilePath  -- ^ Directory to use for hard disk storage
    -> IO Store  -- ^ Store
new inMemory directory = do
    createDirectoryIfMissing True directory
    ref <- if inMemory then Just <$> newIORef M.empty else return Nothing
    return Store
        { storeDirectory = directory
        , storeMap       = ref
        }


--------------------------------------------------------------------------------
-- | Auxiliary: add an item to the in-memory cache
cacheInsert :: Store -> String -> a -> IO ()
cacheInsert (Store _ Nothing)    _   _     = return ()
cacheInsert (Store _ (Just ref)) key x =
    modifyIORef ref $ M.insert key (Box x)


--------------------------------------------------------------------------------
-- | Auxiliary: get an item from the in-memory cache
cacheLookup :: Store -> String -> IO (Maybe a)
cacheLookup (Store _ Nothing)    _   = return Nothing
cacheLookup (Store _ (Just ref)) key = do
    map' <- readIORef ref
    case M.lookup key map' of
        Nothing      -> return Nothing
        Just (Box x) -> return $ Just $ unsafeCoerce x


--------------------------------------------------------------------------------
-- | Auxiliary: delete an item from the in-memory cache
cacheDelete :: Store -> String -> IO ()
cacheDelete (Store _ Nothing)    _   = return ()
cacheDelete (Store _ (Just ref)) key = modifyIORef ref (M.delete key)


--------------------------------------------------------------------------------
-- | Store an item
set :: Binary a => Store -> [String] -> a -> IO ()
set store identifier value = do
    encodeFile (storeDirectory store </> key) value
    cacheInsert store key value
  where
    key = hash identifier


--------------------------------------------------------------------------------
-- | Load an item
get :: Binary a => Store -> [String] -> IO (Maybe a)
get store identifier = do
    -- First check the in-memory map
    ref <- cacheLookup store key
    case ref of
        -- Not found in the map, try the filesystem
        Nothing -> do
            exists <- doesFileExist path
            if not exists
                -- Not found in the filesystem either
                then return Nothing
                -- Found in the filesystem
                else do
                    v <- decodeFile path
                    cacheInsert store key v
                    return $ Just v
        -- Found in the in-memory map, just return
        Just s -> return $ Just s
  where
    key  = hash identifier
    path = storeDirectory store </> key


--------------------------------------------------------------------------------
-- | Delete an item
delete :: Store -> [String] -> IO ()
delete store identifier = do
    cacheDelete store key
    deleteFile $ storeDirectory store </> key
  where
    key  = hash identifier


--------------------------------------------------------------------------------
hash :: [String] -> String
hash = concatMap (printf "%02x") . B.unpack .
    MD5.hash . T.encodeUtf8 . T.pack . intercalate "/"


--------------------------------------------------------------------------------
-- | Delete a file unless it doesn't exist...
deleteFile :: FilePath -> IO ()
deleteFile = handle (\(_ :: IOException) -> return ()) . removeFile
