--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
module Hakyll.Core.Compiler.Select
    ( selectResource
    , selectItems
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative           ((<$>))
import           Control.Arrow                 ((>>>))
import           Control.Monad.Error           (throwError)
import           Control.Monad.Reader          (ask)
import           Data.Binary                   (Binary)
import           Data.Typeable                 (Typeable)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Compiler.Store
import           Hakyll.Core.Item
import           Hakyll.Core.Resource


--------------------------------------------------------------------------------
selectResource :: (Binary a, Typeable a) => Resource -> Compiler i b a
selectResource rs = fromDependencies ids >>> fromJob job
  where
    ids pop =
        [ itemIdentifier item
        | (_, (SomeItem item, _)) <- pop
        , itemResource item == Just rs
        ]

    job = \_ -> CompilerM $ do
        ids' <- ids . compilerPopulation <$> ask
        case ids' of
            [id']   -> unCompilerM $ fromStore id'
            []      -> throwError' $ "No item matched " ++ unResource rs
            (_ : _) -> throwError' $ "Multiple items matched " ++ unResource rs

    throwError' e = throwError $
        "Hakyll.Core.Compiler.Selector.selectResource: " ++ e


--------------------------------------------------------------------------------
selectItems :: (Binary a, Typeable a) => ([i] -> [Item a]) -> Compiler i b [a]
selectItems selector = fromDependencies ids >>> fromJob job
  where
    ids = map itemIdentifier . selector . map (snd . snd)
    job   = \_ -> CompilerM $ do
        ids' <- ids . compilerPopulation <$> ask
        mapM (unCompilerM . fromStore) ids'


--------------------------------------------------------------------------------
-- | Auxiliary: get a dependency
fromStore :: (Binary a, Typeable a) => ItemIdentifier -> CompilerM i a
fromStore id' = CompilerM $ do
    store <- compilerStore <$> ask
    unCompilerM $ getBox store id'
