module AllCabalHashes where

import Control.Lens hiding ((<.>))
import Data.Aeson as A
import Data.ByteString as BS
import Data.ByteString.Char8 as B8
import Data.Map as M
import Data.Tagged (Tagged(..))
import Data.Text as T
import Distribution.Nixpkgs.Hashes as NH
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Text ( display )
import Git
import Git.Libgit2 as Libgit2
import OpenSSL.Digest as SSL ( digest, digestByName )
import System.FilePath


type SHA1Hash = T.Text
type SHA256Hash = String

data Meta = Meta
  { _mHashes    :: !(Map String String)
  , _mLocations :: ![String]
  , _mPkgsize   :: !Int
  } deriving (Show)

makeLenses ''Meta

instance FromJSON Meta where
  parseJSON (Object v) = Meta
    <$> v .: "package-hashes"
    <*> v .: "package-locations"
    <*> v .: "package-size"
  parseJSON o          = fail ("invalid Cabal metadata: " ++ show o)

readPackageByHash :: FilePath -> SHA1Hash -> IO (GenericPackageDescription, SHA256Hash)
readPackageByHash repoDir sha1Hash = do
  let repoOpts = defaultRepositoryOptions { repoPath = repoDir }
  repo <- openLgRepository repoOpts
  buf <- runLgRepository repo $ do
    oid <- parseOid sha1Hash
    catBlob (Tagged oid)
  parseCabal ("Git SHA1 " ++ show sha1Hash) buf

readPackageByName :: FilePath -> PackageIdentifier -> IO (GenericPackageDescription, SHA256Hash)
readPackageByName repoDir (PackageIdentifier name version) = do
  let cabalFile = unPackageName name </> display version </> unPackageName name <.> "cabal"
  buf <- readFileFromRepo repoDir cabalFile
  parseCabal cabalFile buf

readFileFromRepo :: FilePath -> FilePath -> IO BS.ByteString
readFileFromRepo repoDir relativeFile = do
  let repoOpts = defaultRepositoryOptions { repoPath = repoDir }
  repo <- openLgRepository repoOpts
  runLgRepository repo $ do
    headCommit <- resolveReference "refs/heads/hackage" >>= \case
      Nothing -> fail "Failed to parse 'hackage' ref"
      Just oid -> lookupCommit $ Tagged oid
    blobOid <- commitTreeEntry headCommit (B8.pack relativeFile) >>= \case
      Nothing -> fail (relativeFile ++ ": no such file in branch 'hackage'")
      Just (BlobEntry oid PlainBlob) -> pure oid
      Just (BlobEntry _ _) -> fail (relativeFile ++ " is not a plain blob")
      _ -> fail (relativeFile ++ " points to a non-blob!")
    catBlob blobOid

parseCabal :: String -> BS.ByteString -> IO (GenericPackageDescription, SHA256Hash)
parseCabal what blob = do
  cabal <- let (_, res) = runParseResult (parseGenericPackageDescription blob)
           in case res of Right a       -> return a
                          Left (_, err) -> fail (what ++ ": " ++ show err)
  let
    hash = NH.printSHA256 (SSL.digest (SSL.digestByName "sha256") blob)
    pkg  = setCabalFileHash hash cabal
  return (pkg, hash)

setCabalFileHash :: SHA256Hash -> GenericPackageDescription -> GenericPackageDescription
setCabalFileHash hash cabal = cabal
  { packageDescription = (packageDescription cabal)
    { customFieldsPD = ("X-Cabal-File-Hash", hash) : customFieldsPD (packageDescription cabal)
    }
  }

readPackageMeta :: FilePath -> PackageIdentifier -> IO Meta
readPackageMeta repoDir (PackageIdentifier name version) = do
  let metaFile = unPackageName name </> display version </> unPackageName name <.> "json"
  buf <- readFileFromRepo repoDir metaFile
  case eitherDecodeStrict buf of
    Left msg -> fail (metaFile ++ ": " ++ msg)
    Right x  -> return $ over (mHashes . ix "SHA256") (printSHA256 . packHex) x
