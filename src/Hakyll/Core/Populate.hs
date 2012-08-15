--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Populate
    ( Population
    , populationItems
    , populationUserdatas

    , PopulateM
    , runPopulate

    , match
    , yield
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           (Applicative, (<$>))
import           Control.Monad                 (forM)
import           Control.Monad.Reader          (ReaderT, ask, local, runReaderT)
import           Control.Monad.Trans           (MonadIO)
import           Control.Monad.Writer          (WriterT, execWriterT, tell)
import qualified Data.Map                      as M
import           Data.Maybe                    (catMaybes)
import           Data.Typeable                 (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Item
import           Hakyll.Core.Resource
import           Hakyll.Core.Resource.Pattern
import           Hakyll.Core.Resource.Provider


--------------------------------------------------------------------------------
type Population i = [(String, (SomeItem, i))]


--------------------------------------------------------------------------------
populationItems :: Population i -> [SomeItem]
populationItems = map (fst . snd)


--------------------------------------------------------------------------------
populationUserdatas :: Population i -> [i]
populationUserdatas = map (snd . snd)


--------------------------------------------------------------------------------
data PopulateEnv = PopulateEnv
    { populateProvider :: ResourceProvider
    , populateResource :: Maybe Resource
    }


--------------------------------------------------------------------------------
newtype PopulateM i a = PopulateM
    { unPopulateM :: ReaderT PopulateEnv (WriterT (Population i) IO) a
    } deriving (Applicative, Functor, Monad, MonadIO)


--------------------------------------------------------------------------------
runPopulate :: PopulateM i a -> ResourceProvider -> IO (Population i)
runPopulate populate provider =
    fmap (M.toList . M.fromList) $  -- Ensure uniqueness
    execWriterT $ runReaderT (unPopulateM populate) $
    PopulateEnv provider Nothing


--------------------------------------------------------------------------------
withResource :: Resource -> PopulateM i a -> PopulateM i a
withResource rs =
    PopulateM . local (\env -> env {populateResource = Just rs}) . unPopulateM


--------------------------------------------------------------------------------
match :: Pattern -> (Resource -> [String] -> PopulateM i a) -> PopulateM i [a]
match pattern f = PopulateM $ do
    provider <- populateProvider <$> ask
    fmap catMaybes $ forM (resourceList provider) $ \rs ->
        case capture pattern rs of
            Nothing -> return Nothing
            Just cs -> Just <$> unPopulateM (withResource rs (f rs cs))


--------------------------------------------------------------------------------
yield :: (Show i, Typeable a) => (Item a -> i) -> PopulateM i i
yield f = PopulateM $ do
    rs <- populateResource <$> ask
    let item     = makeItem itemid rs
        userdata = f item
        itemid   = show userdata

    tell [(itemid, (SomeItem item, userdata))]
    return userdata
