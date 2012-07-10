--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Populate
    ( PopulateM
    , Populate
    , runPopulate

    , match
    , yield
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           (Applicative)
import           Control.Monad                 (forM_)
import           Control.Monad.Reader          (ReaderT, ask, runReaderT)
import           Control.Monad.Trans           (MonadIO)
import           Control.Monad.Writer          (WriterT, execWriterT, tell)
import qualified Data.Map                      as M
import           Data.Typeable                 (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Item
import           Hakyll.Core.Pattern
import           Hakyll.Core.Resource
import           Hakyll.Core.Resource.Provider


--------------------------------------------------------------------------------
type Population i = [(String, (SomeItem, i))]


--------------------------------------------------------------------------------
makePopulation :: (Show i, Typeable a)
               => Maybe Resource -> (Item a -> i) -> Population i
makePopulation rs f =
    let item     = makeItem itemid rs
        userdata = f item
        itemid   = show userdata
    in [(itemid, (SomeItem item, userdata))]


--------------------------------------------------------------------------------
newtype PopulateM i a = PopulateM
    { unPopulateM :: ReaderT ResourceProvider (WriterT (Population i) IO) a
    } deriving (Applicative, Functor, Monad, MonadIO)


--------------------------------------------------------------------------------
type Populate i = PopulateM i ()


--------------------------------------------------------------------------------
runPopulate :: Populate i -> ResourceProvider -> IO (Population i)
runPopulate populate provider =
    fmap (M.toList . M.fromList) $  -- Ensure uniqueness
    execWriterT $ runReaderT (unPopulateM populate) provider


--------------------------------------------------------------------------------
match :: (Show i, Typeable a)
      => Pattern -> ([String] -> Item a -> i) -> Populate i
match pattern f = PopulateM $ do
    provider <- ask
    forM_ (resourceList provider) $ \rs ->
        case capture pattern (unResource rs) of
            Nothing -> return ()
            Just cs -> tell $ makePopulation (Just rs) (f cs)


--------------------------------------------------------------------------------
yield :: (Show i, Typeable a) => (Item a -> i) -> Populate i
yield f = PopulateM $ tell $ makePopulation Nothing f
