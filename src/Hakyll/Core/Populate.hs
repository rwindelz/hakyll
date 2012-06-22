--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Populate
    ( PopulateM
    , Populate
    , runPopulate

    , match
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           (Applicative)
import           Control.Monad                 (forM_)
import           Control.Monad.Reader          (ReaderT, ask, runReaderT)
import           Control.Monad.Trans           (MonadIO, liftIO)
import           Control.Monad.Writer          (WriterT, execWriterT, tell)


--------------------------------------------------------------------------------
import           Hakyll.Core.Item
import           Hakyll.Core.Pattern
import           Hakyll.Core.Resource
import           Hakyll.Core.Resource.Provider


--------------------------------------------------------------------------------
newtype PopulateM i a = PopulateM
    { unPopulateM :: ReaderT ResourceProvider (WriterT [(String, i)] IO) a
    } deriving (Applicative, Functor, Monad, MonadIO)


--------------------------------------------------------------------------------
type Populate i = PopulateM i ()


--------------------------------------------------------------------------------
runPopulate :: Populate i -> ResourceProvider -> IO [(String, i)]
runPopulate populate provider =
    execWriterT $ runReaderT (unPopulateM populate) provider


--------------------------------------------------------------------------------
match :: Show i => Pattern -> ([String] -> Item a -> i) -> Populate i
match pattern f = PopulateM $ do
    provider <- ask
    forM_ (resourceList provider) $ \rs ->
        case capture pattern (unResource rs) of
            Nothing -> return ()
            Just cs -> do
                let item     = Item (Just rs)
                    userdata = f cs item
                tell [(show userdata, userdata)]
