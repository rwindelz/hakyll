--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Rules.Tests
    ( tests
    ) where


<<<<<<< HEAD
import Test.Framework
import Test.HUnit hiding (Test)

import Hakyll.Core.Rules
import Hakyll.Core.Rules.Internal
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Routes
import Hakyll.Core.Compiler
import Hakyll.Core.Resource.Provider
import Hakyll.Core.Resource.Provider.Dummy
import Hakyll.Core.Writable.CopyFile
import Hakyll.Web.Page
import TestSuite.Util

tests :: [Test]
tests = fromAssertions "runRules" [case01]

-- | Dummy resource provider
--
provider :: IO ResourceProvider
provider = dummyResourceProvider $ M.fromList $ map (flip (,) "No content")
    [ "posts/a-post.markdown"
    , "posts/some-other-post.markdown"
    ]

-- | Main test
--
case01 :: Assertion
case01 = do
    p <- provider
    let ruleSet     = runRules rules p
        identifiers = map fst $ rulesCompilers ruleSet
        routes      = rulesRoutes ruleSet
    expected @=? S.fromList identifiers
    Just "posts/a-post.markdown" @=?
        runRoutes routes (Identifier (Just "nav") "posts/a-post.markdown")
  where
    expected = S.fromList
        [ Identifier Nothing "posts/a-post.markdown"
        , Identifier Nothing "posts/some-other-post.markdown"
        , Identifier (Just "raw") "posts/a-post.markdown"
        , Identifier (Just "raw") "posts/some-other-post.markdown"
        , Identifier (Just "nav") "posts/a-post.markdown"
        ]

-- | Example rules
--
rules :: Rules
rules = do
=======
--------------------------------------------------------------------------------
import           Data.IORef                     (IORef, newIORef, readIORef,
                                                 writeIORef)
import qualified Data.Set                       as S
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert, (@=?))


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.File
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Routes
import           Hakyll.Core.Rules
import           Hakyll.Core.Rules.Internal
import           Hakyll.Web.Pandoc
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Hakyll.Core.Rules.Tests"
    [ testCase "runRules" rulesTest
    ]


--------------------------------------------------------------------------------
rulesTest :: Assertion
rulesTest = do
    ioref    <- newIORef False
    store    <- newTestStore
    provider <- newTestProvider store
    ruleSet  <- runRules (rules ioref) provider
    let identifiers = S.fromList $ map fst $ rulesCompilers ruleSet
        routes      = rulesRoutes ruleSet

    -- Test that we have some identifiers and that the routes work out
    assert $ all (`S.member` identifiers) expected
    Just "example.html" @=? runRoutes routes "example.md"
    Just "example.md"   @=? runRoutes routes (sv "raw" "example.md")
    Just "example.md"   @=? runRoutes routes (sv "nav" "example.md")
    Just "example.mv1"  @=? runRoutes routes (sv "mv1" "example.md")
    Just "example.mv2"  @=? runRoutes routes (sv "mv2" "example.md")
    readIORef ioref >>= assert
  where
    sv g     = setVersion (Just g)
    expected =
        [ "example.md"
        , "russian.md"
        , sv "raw" "example.md"
        , sv "raw" "russian.md"
        , sv "nav" "example.md"
        ]


--------------------------------------------------------------------------------
rules :: IORef Bool -> Rules ()
rules ioref = do
>>>>>>> upstream/master
    -- Compile some posts
    match "*.md" $ do
        route $ setExtension "html"
        compile pandocCompiler

    -- Yeah. I don't know how else to test this stuff?
    preprocess $ writeIORef ioref True

    -- Compile them, raw
    match "*.md" $ version "raw" $ do
        route idRoute
        compile getResourceString

    -- Regression test
    version "nav" $ match (fromList ["example.md"]) $ do
        route idRoute
        compile copyFileCompiler

<<<<<<< HEAD
    -- Regression test
    group "nav" $ do
        match (list ["posts/a-post.markdown"]) $ do
            route idRoute
            compile copyFileCompiler

    return ()
=======
    -- Another edge case: different versions in one match
    match "*.md" $ do
        version "mv1" $ do
            route $ setExtension "mv1"
            compile getResourceString
        version "mv2" $ do
            route $ setExtension "mv2"
            compile getResourceString
>>>>>>> upstream/master
