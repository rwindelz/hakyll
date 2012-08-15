--------------------------------------------------------------------------------
module Hakyll.Core.Compiler.Select
    ( selectResource
    , selectItem
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
import           Hakyll.Core.Populate
import           Hakyll.Core.Resource


--------------------------------------------------------------------------------
selectResource :: (Binary a, Typeable a) => Resource -> Compiler i b a
selectResource rs = selectSingle
    "Hakyll.Core.Compiler.Selector.selectResource" (unResource rs) $ \pop ->
        [id' | (id', (SomeItem item, _)) <- pop , itemResource item == Just rs]


--------------------------------------------------------------------------------
selectItem :: (Binary a, Eq i, Show i, Typeable a)
           => (Item a -> i) -> Compiler i b a
selectItem selector = selectSingle
    "Hakyll.Core.Compiler.Selector.selectItem" (show selector') $ \pop ->
        [id' | (id', (_, userdata)) <- pop , userdata == selector']
  where
    selector' = selector $ error
        "Hakyll.Core.Compiler.Selector.selectItem: Internal error"


--------------------------------------------------------------------------------
selectItems :: (Binary a, Typeable a) => ([i] -> [Item a]) -> Compiler i b [a]
selectItems selector = fromDependencies ids >>> fromJob job
  where
    ids = map itemIdentifier . selector . map (snd . snd)
    job   = \_ -> CompilerM $ do
        ids' <- ids . compilerPopulation <$> ask
        mapM (unCompilerM . fromStore) ids'


--------------------------------------------------------------------------------
-- | Auxiliary: select a single item
selectSingle :: (Binary a, Typeable a)
             => String                              -- ^ Higher function name
             -> String                              -- ^ Readable item name
             -> (Population i -> [ItemIdentifier])  -- ^ Select ID's
             -> Compiler i b a                      -- ^ Resulting compiler
selectSingle fname iname ids = fromDependencies ids >>> fromJob job
  where
    job = \_ -> CompilerM $ do
        ids' <- ids . compilerPopulation <$> ask
        case ids' of
            [id']   -> unCompilerM $ fromStore id'
            []      -> throwError' $ "No item matched: " ++ iname
            (_ : _) -> throwError' $ "Multiple items matched: " ++ iname

    throwError' e = throwError $ fname ++ ": " ++ e


--------------------------------------------------------------------------------
-- | Auxiliary: get a dependency
fromStore :: (Binary a, Typeable a) => ItemIdentifier -> CompilerM i a
fromStore id' = CompilerM $ do
    store <- compilerStore <$> ask
    unCompilerM $ getBox store id'
