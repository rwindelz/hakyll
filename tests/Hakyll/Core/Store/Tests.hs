--------------------------------------------------------------------------------
module Hakyll.Core.Store.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


--------------------------------------------------------------------------------
import qualified Hakyll.Core.Store                    as Store
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: [Test]
tests =
    [ testProperty "simple get . set" simpleSetGet
    , testProperty "persistent get . set" persistentSetGet
    ]


--------------------------------------------------------------------------------
simpleSetGet :: Property
simpleSetGet = monadicIO $ do
    store <- run $ makeStoreTest
    key   <- pick arbitrary
    value <- pick arbitrary

    run $ Store.set store key (value :: String)
    value' <- run $ Store.get store key
    assert $ Just value == value'


--------------------------------------------------------------------------------
persistentSetGet :: Property
persistentSetGet = monadicIO $ do
    store1 <- run $ makeStoreTest
    key    <- pick arbitrary
    value  <- pick arbitrary
    run $ Store.set store1 key (value :: String)

    -- Now Create another store from the same dir to test persistence
    store2 <- run $ makeStoreTest
    value' <- run $ Store.get store2 key
    assert $ Just value == value'
