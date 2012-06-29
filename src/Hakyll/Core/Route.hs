--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
module Hakyll.Core.Route
    ( Route
    , runRoute

    , route
    , noRoute

    , module Hakyll.Core.Route.Writable
    ) where


--------------------------------------------------------------------------------
import           Hakyll.Core.Item
import           Hakyll.Core.Route.Writable


--------------------------------------------------------------------------------
-- TODO: Make this a reader so we can get the compiled contents?
newtype Route = Route {unRoute :: IO ()}


--------------------------------------------------------------------------------
runRoute :: Route -> IO ()
runRoute = unRoute


--------------------------------------------------------------------------------
route :: Writable a => Item a -> FilePath -> Route
route = undefined


--------------------------------------------------------------------------------
noRoute :: Route
noRoute = Route $ return ()
