--------------------------------------------------------------------------------
-- | Small internal module for storing the result of a compiler in binary
-- format, and reading it back in as dependency.
{-# LANGUAGE ExistentialQuantification #-}
module Hakyll.Core.Compiler.Store
    ( Box (..)
    , setBox
    , getBox
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Error           (throwError)
import           Control.Monad.Trans           (liftIO)
import           Data.Binary                   (Binary)
import           Data.Typeable                 (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Item
import           Hakyll.Core.Store             (Store)
import qualified Hakyll.Core.Store             as Store


--------------------------------------------------------------------------------
data Box = forall a. (Binary a, Typeable a) => Box a


--------------------------------------------------------------------------------
key :: ItemIdentifier -> [String]
key i = ["Hakyll.Core.Compiler.Store", i]


--------------------------------------------------------------------------------
setBox :: Store -> ItemIdentifier -> Box -> IO ()
setBox s i (Box x) = Store.set s (key i) x


--------------------------------------------------------------------------------
getBox :: (Binary a, Typeable a) => Store -> ItemIdentifier -> CompilerM i a
getBox s i = CompilerM $ do
    result <- liftIO $ Store.get s (key i)
    case result of
        Just x  -> return x
        Nothing -> throwError notFound
  where
    notFound =
        "Hakyll.Core.Compiler.getDependency: " ++ show i ++ " was " ++
        "not found in the cache, the cache might be corrupted or " ++
        "the item you are referring to might not exist"
