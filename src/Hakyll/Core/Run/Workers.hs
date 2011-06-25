-- | Asynchronous workers
--
module Hakyll.Core.Run.Workers
    ( Workers
    , makeWorkers
    , writeJob
    , readJob
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, readChan, writeChan, newChan)
import Control.Monad (forever, forM_, replicateM_)

import GHC.Conc.Sync (numCapabilities)

data Workers a = Workers
    { workersInChan  :: Chan (IO a)
    , workersOutChan :: Chan a
    }

makeWorkers :: IO (Workers a)
makeWorkers = do
    w <- Workers <$> newChan <*> newChan
    spawnWorkers w
    return w

writeJob :: Workers a -> IO a -> IO ()
writeJob workers job = writeChan (workersInChan workers) job

readJob :: Workers a -> IO a
readJob workers = readChan (workersOutChan workers)

spawnWorkers :: Workers a -> IO ()
spawnWorkers workers = do
    forM_ [1 .. numCapabilities] $ \n -> do
        _ <- forkIO $ worker workers n
        return ()

-- | A worker thread
--
worker :: Workers a -> Int -> IO ()
worker workers n = forever $ do
    x <- readChan (workersInChan workers)
    writeChan (workersOutChan workers) =<< x
