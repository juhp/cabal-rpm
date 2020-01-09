module Commands.BuildDep (builddep) where

import Dependencies (pkgInstallMissing)
import Types

import SimpleCabal (PackageIdentifier)

builddep :: Flags -> Maybe Stream -> Maybe PackageIdentifier -> IO ()
builddep flags mstream mpkgid = do
  missing <- pkgInstallMissing flags mstream mpkgid
  mapM_ (builddep flags mstream . Just . unversionedPkgId) missing
