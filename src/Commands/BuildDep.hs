module Commands.BuildDep (builddep) where

import Dependencies (pkgInstallMissing)
import Types

builddep :: Flags -> Maybe PackageVersionSpecifier -> IO ()
builddep flags mpvs = do
  missing <- pkgInstallMissing flags mpvs
  let mstream = pvsStream =<< mpvs
  mapM_ (builddep flags . streamPkgToPVS mstream . Just . unversionedPkgId) missing
