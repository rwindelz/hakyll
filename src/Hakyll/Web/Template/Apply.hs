--------------------------------------------------------------------------------
-- | Applying templates
{-# LANGUAGE Arrows #-}
module Hakyll.Web.Template.Apply
    ( applyTemplate
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     (Monoid, mappend, mempty)
import           GHC.Exts                        (IsString, fromString)


--------------------------------------------------------------------------------
import           Hakyll.Core.Util.Arrow
import           Hakyll.Web.Template.Internal
import           Hakyll.Web.Template.Read.Hakyll


{-
--------------------------------------------------------------------------------
applyTemplate :: (Arrow a, IsString s, Monoid s)
              => Template s -> a (p, String) s -> a p s
applyTemplate (Template elems) get =
    foldr (applyElement get) (constA mempty) elems


--------------------------------------------------------------------------------
applyElement :: (Arrow a, IsString s, Monoid s)
             => a (p, String) s
             -> TemplateElement s
             -> a p s
             -> a p s
applyElement _   (Chunk chunk) a = a >>> arr (mappend chunk)
applyElement _   Escaped       a = a >>> arr (mappend $ fromString "$")
applyElement get (Key str)     a = proc p -> do
    s  <- a   -< p
    s' <- get -< (p, str)
    returnA -< mappend s' s
-}


--------------------------------------------------------------------------------
applyTemplate :: (Arrow a, ArrowChoice a, IsString s, Monoid s)
              => a (p, String) s -> a (Template s, p) s
applyTemplate get = first (arr unTemplate) >>> apply get


--------------------------------------------------------------------------------
apply :: (Arrow a, ArrowChoice a, IsString s, Monoid s)
      => a (p, String) s
      -> a ([TemplateElement s], p) s
apply get = proc (tes, p) -> case tes of
    []       -> returnA -< mempty
    (t : es) -> do
        s <- apply get -< (es, p)
        case t of
            (Chunk c) -> returnA -< mappend c s
            Escaped   -> returnA -< mappend (fromString "$") s
            (Key k)   -> do
                s' <- get -< (p, k)
                returnA -< mappend s' s


--------------------------------------------------------------------------------
-- TODO: Remove or move to tests/

template :: Template String
template = readTemplate "$name$ is $price$$$"

type Context = [(String, String)]

context :: Context
context = [("name", "soba"), ("price", "10")]

simpleGet :: (Context, String) -> String
simpleGet (ctx, str) = fromMaybe "???" $ lookup str ctx

test :: String
test = applyTemplate simpleGet (template, context)
