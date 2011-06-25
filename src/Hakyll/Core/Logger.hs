-- | Produce pretty, thread-safe logs
--
{-# LANGUAGE BangPatterns #-}
module Hakyll.Core.Logger
    ( Logger
    , makeLogger
    , flushLogger
    , message
    , messageAbout
    ) where

import Control.Monad (forever)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)

import Hakyll.Core.Identifier

-- | Logger structure. Very complicated.
--
data Logger = Logger
    { loggerChan :: Chan (Maybe String)  -- ^ Nothing marks the end
    , loggerSync :: MVar ()              -- ^ Used for sync on quit
    , loggerSink :: String -> IO ()      -- ^ Out sink
    }

-- | Create a new logger
--
makeLogger :: (String -> IO ()) -> IO Logger
makeLogger sink = do
    logger <- Logger <$> newChan <*> newEmptyMVar <*> pure sink
    _ <- forkIO $ loggerThread logger
    return logger
  where
    loggerThread logger = forever $ do
        msg <- readChan $ loggerChan logger
        case msg of
            -- Stop: sync
            Nothing -> putMVar (loggerSync logger) ()
            -- Print and continue
            Just m  -> loggerSink logger m

-- | Flush the logger (blocks until flushed)
--
flushLogger :: Logger -> IO ()
flushLogger logger = do
    writeChan (loggerChan logger) Nothing
    () <- takeMVar $ loggerSync logger
    return ()

-- | Send a raw message to the logger
--
message :: MonadIO m => Logger -> String -> m ()
message logger = liftIO . writeChan (loggerChan logger) . Just

-- | Send a raw message to the logger
--
messageAbout :: MonadIO m => Logger -> Identifier a -> String -> m ()
messageAbout logger identifier msg = message logger $
    "[" ++ show identifier ++ "] " ++ msg
