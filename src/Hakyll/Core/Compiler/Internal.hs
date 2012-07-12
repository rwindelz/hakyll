--------------------------------------------------------------------------------
-- | Internally used compiler module
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Core.Compiler.Internal
    ( DependencyEnvironment (..)
    , Dependencies

    , CompilerEnvironment (..)
    , CompilerM (..)

    , Compiler
    , runCompilerDependencies
    , runCompilerJob
    , fromDependencies
    , fromJob
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           (Applicative(..), (<$>))
import           Control.Arrow                 (Arrow(..), ArrowChoice(..))
import           Control.Category              (Category, id, (.))
import           Control.Monad.Error           (ErrorT, runErrorT)
import           Control.Monad.Reader
import           Data.Monoid                   (mappend, mempty)
import           Prelude                       hiding (id, (.))


--------------------------------------------------------------------------------
import           Hakyll.Core.Item
import           Hakyll.Core.Logger
import           Hakyll.Core.Resource.Provider
import           Hakyll.Core.Store


--------------------------------------------------------------------------------
data DependencyEnvironment i = DependencyEnvironment
    { dependencyUserdata :: [i]
    , dependencyItems    :: [SomeItem]
    } deriving (Show)


--------------------------------------------------------------------------------
type Dependencies i = DependencyEnvironment i -> [String]


--------------------------------------------------------------------------------
-- | Environment in which a compiler runs
data CompilerEnvironment = CompilerEnvironment
    { -- | Our own item
      compilerItem             :: SomeItem
    , -- | Resource provider
      compilerResourceProvider :: ResourceProvider
    , -- | Compiler routes
      compilerRoutes           :: String -> Maybe FilePath
    , -- | Compiler store
      compilerStore            :: Store
    , -- | Flag indicating if the underlying resource was modified
      compilerResourceModified :: Bool
    , -- | Logger
      compilerLogger           :: Logger
    }


--------------------------------------------------------------------------------
-- | The compiler monad
newtype CompilerM a = CompilerM
    { unCompilerM :: ErrorT String (ReaderT CompilerEnvironment IO) a
    } deriving (Monad, Functor, Applicative)


--------------------------------------------------------------------------------
-- | The compiler arrow
data Compiler i a b = Compiler
    { compilerDependencies :: Dependencies i
    , compilerJob          :: a -> CompilerM b
    }


--------------------------------------------------------------------------------
instance Functor (Compiler i a) where
    fmap f ~(Compiler d j) = Compiler d $ fmap f . j


--------------------------------------------------------------------------------
instance Applicative (Compiler i a) where
    pure                                  = Compiler mempty . const . return
    ~(Compiler d1 f) <*> ~(Compiler d2 j) =
        Compiler (liftM2 (++) d1 d2) $ \x -> f x <*> j x


--------------------------------------------------------------------------------
instance Category (Compiler i) where
    id                                    = Compiler mempty return
    ~(Compiler d1 j1) . ~(Compiler d2 j2) = Compiler (mappend d1 d2) (j1 <=< j2)


--------------------------------------------------------------------------------
instance Arrow (Compiler i) where
    arr f                 = Compiler mempty (return . f)
    first ~(Compiler d j) = Compiler d $ \(x, y) -> do
        x' <- j x
        return (x', y)


--------------------------------------------------------------------------------
instance ArrowChoice (Compiler i) where
    left ~(Compiler d j) = Compiler d $ \e -> case e of
        Left l  -> Left  <$> j l
        Right r -> Right <$> return r


--------------------------------------------------------------------------------
-- | Calculate the dependencies of a compiler
runCompilerDependencies :: Compiler i () a
                        -> [i]
                        -> [SomeItem]
                        -> [String]
runCompilerDependencies compiler userdata items =
    compilerDependencies compiler env
  where
    env = DependencyEnvironment
            { dependencyUserdata = userdata
            , dependencyItems    = items
            }


--------------------------------------------------------------------------------
-- | Run a compiler, yielding the resulting target
runCompilerJob :: Compiler i () a
               -> SomeItem
               -> ResourceProvider
               -> (String -> Maybe FilePath)
               -> Store
               -> Bool
               -> Logger
               -> IO (Either String a)
runCompilerJob compiler item provider routes store modified logger =
    runReaderT (runErrorT $ unCompilerM $ compilerJob compiler ()) env
  where
    env = CompilerEnvironment item provider routes store modified logger


--------------------------------------------------------------------------------
fromDependencies :: (DependencyEnvironment i -> [String])
                 -> Compiler i b b
fromDependencies = flip Compiler return


--------------------------------------------------------------------------------
fromJob :: (a -> CompilerM b)
        -> Compiler i a b
fromJob = Compiler mempty
