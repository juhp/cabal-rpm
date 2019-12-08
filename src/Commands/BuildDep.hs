module Commands.BuildDep (builddep) where

import Dependencies (pkgInstallMissing)
import Types

import SimpleCabal (PackageIdentifier)

builddep :: Flags -> Stream -> Maybe PackageIdentifier -> IO ()
builddep flags stream mpkgid = do
  missing <- pkgInstallMissing flags stream mpkgid
  mapM_ (builddep flags stream . Just . unversionedPkgId) missing
