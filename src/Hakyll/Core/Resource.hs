--------------------------------------------------------------------------------
-- | Module exporting the simple 'Resource' type
module Hakyll.Core.Resource
    ( Resource
    , resource
    , unResource
    ) where


--------------------------------------------------------------------------------
import           Data.List       (intercalate)
import           GHC.Exts        (IsString (..))
import           System.FilePath (dropTrailingPathSeparator, splitPath)


--------------------------------------------------------------------------------
-- | A resource
newtype Resource = Resource {unResource :: FilePath}
    deriving (Eq, Show, Ord)


--------------------------------------------------------------------------------
instance IsString Resource where
    fromString = resource


--------------------------------------------------------------------------------
-- | Smart constructor to ensure we have @/@ as path separator
resource :: FilePath -> Resource
resource = Resource .  intercalate "/" .
    filter (not . null) . map dropTrailingPathSeparator . splitPath
