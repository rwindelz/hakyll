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
-- | All items are equal to each other. This is perhaps a bit of a hack but at
-- least it has proper transitivity and reflexitivity, which can't be said about
-- PHP.
--
-- This allows us to compare two userdata values without a filled-in item.
instance Eq (Item a) where
    _ == _ = True


--------------------------------------------------------------------------------
-- | Ord instance should follow the Eq instance.
instance Ord (Item a) where
    compare _ _ = EQ


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
