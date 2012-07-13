--------------------------------------------------------------------------------
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hakyll.Core.Route
    ( Route
    , runRoute

    , route
    , noRoute

    , module Hakyll.Core.Route.Writable
    ) where


--------------------------------------------------------------------------------
import           Data.Typeable              (Typeable, cast)


--------------------------------------------------------------------------------
import           Hakyll.Core.Item
import           Hakyll.Core.Route.Writable


--------------------------------------------------------------------------------
data Route = Route (forall a. Typeable a => a -> IO (Maybe FilePath))


--------------------------------------------------------------------------------
runRoute :: Typeable a => Route -> a -> IO (Maybe FilePath)
runRoute (Route r) x = r x


--------------------------------------------------------------------------------
route :: forall a. (Writable a, Typeable a) => Item a -> FilePath -> Route
route _ fp = Route $ \x -> case cast x :: Maybe a of
    Nothing -> error "Herp"  -- TODO
    Just x' -> do
        write fp x'
        return $ Just fp


--------------------------------------------------------------------------------
noRoute :: Route
noRoute = Route $ \_ -> return Nothing
