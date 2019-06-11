module Commands.BuildDep (builddep) where

import Dependencies (pkgInstallMissing)
import Types

builddep :: Flags -> Stream -> Maybe Package -> IO ()
builddep flags stream mpkg = do
  missing <- pkgInstallMissing flags stream mpkg
  mapM_ (builddep flags stream) $ map Just missing
