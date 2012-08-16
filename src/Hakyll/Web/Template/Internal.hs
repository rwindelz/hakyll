--------------------------------------------------------------------------------
-- | Module containing the template data structure
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hakyll.Web.Template.Internal
    ( Template (..)
    , TemplateElement (..)
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Data.Binary         (Binary, get, getWord8, put, putWord8)
import           Data.Typeable       (Typeable)


--------------------------------------------------------------------------------
-- | Datatype used for template substitutions.
newtype Template a = Template
    { unTemplate :: [TemplateElement a]
    } deriving (Binary, Eq, Show, Typeable)


--------------------------------------------------------------------------------
-- | Elements of a template.
data TemplateElement a
    = Chunk a
    | Key String
    | Escaped
    deriving (Eq, Show, Typeable)


--------------------------------------------------------------------------------
instance Binary a => Binary (TemplateElement a) where
    put (Chunk chunk) = putWord8 0 >> put chunk
    put (Key key)     = putWord8 1 >> put key
    put (Escaped)     = putWord8 2

    get = getWord8 >>= \tag -> case tag of
            0 -> Chunk <$> get
            1 -> Key   <$> get
            2 -> return Escaped
            _ -> error $  "Hakyll.Web.Template.Internal: "
                       ++ "Error reading cached template"
