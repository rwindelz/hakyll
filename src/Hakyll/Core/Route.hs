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
import           System.FilePath            ((</>))


--------------------------------------------------------------------------------
import           Hakyll.Core.Item
import           Hakyll.Core.Route.Writable
import           Hakyll.Core.Util.File


--------------------------------------------------------------------------------
data Route = Route
    (forall a. Typeable a => FilePath -> a -> IO (Maybe FilePath))


--------------------------------------------------------------------------------
runRoute :: Typeable a => Route -> FilePath -> a -> IO (Maybe FilePath)
runRoute (Route r) = r


--------------------------------------------------------------------------------
route :: forall a. (Writable a, Typeable a) => Item a -> FilePath -> Route
route _ fp = Route $ \dir x -> case cast x :: Maybe a of
    Nothing -> error "Herp"  -- TODO
    Just x' -> do
        let fp' = dir </> fp
        makeDirectories fp'
        write fp' x'
        return $ Just fp


--------------------------------------------------------------------------------
noRoute :: Route
noRoute = Route $ \_ _ -> return Nothing
