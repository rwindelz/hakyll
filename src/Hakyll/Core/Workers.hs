-- | Asynchronous workers
--
module Hakyll.Core.Workers
    ( spawnWorkers
    , worker
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Monad (forever, replicateM_)

import GHC.Conc.Sync (numCapabilities)

spawnWorkers :: Chan (IO a) -> Chan a -> IO ()
spawnWorkers inChan outChan = do
    -- replicateM_ numCapabilities $ do
    replicateM_ 8 $ do
        _ <- forkIO $ worker inChan outChan
        return ()

worker :: Chan (IO a) -> Chan a -> IO ()
worker inChan outChan = forever $ do
    x <- readChan inChan
    writeChan outChan =<< x
