--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
module Hakyll.Core.Item
    ( Item (..)
    , makeItem

    , SomeItem (..)
    , castItem
    , someItemIdentifier
    ) where


--------------------------------------------------------------------------------
import           Data.Typeable        (Typeable, cast)


--------------------------------------------------------------------------------
import           Hakyll.Core.Resource


--------------------------------------------------------------------------------
data Item a = Item
    { itemIdentifier :: String
    , itemResource   :: Maybe Resource
    , itemRoute      :: Maybe FilePath
    } deriving (Typeable)


--------------------------------------------------------------------------------
instance Show (Item a) where
    show _ = "<Item>"


--------------------------------------------------------------------------------
makeItem :: String -> Maybe Resource -> Item a
makeItem i r = Item i r Nothing


--------------------------------------------------------------------------------
data SomeItem = forall a. Typeable a => SomeItem (Item a)


--------------------------------------------------------------------------------
instance Show SomeItem where
    show _ = "<Item>"


--------------------------------------------------------------------------------
castItem :: Typeable a => SomeItem -> Maybe (Item a)
castItem (SomeItem x) = cast x


--------------------------------------------------------------------------------
someItemIdentifier :: SomeItem -> String
someItemIdentifier (SomeItem item) = itemIdentifier item
