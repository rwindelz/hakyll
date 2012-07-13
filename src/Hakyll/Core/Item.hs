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
    } deriving (Typeable)


--------------------------------------------------------------------------------
instance Show (Item a) where
    show _ = "<Item>"


--------------------------------------------------------------------------------
makeItem :: String -> Maybe Resource -> Item a
makeItem = Item


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
