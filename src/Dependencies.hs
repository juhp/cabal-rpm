{-# LANGUAGE CPP #-}

-- |
-- Module      :  Dependencies
-- Copyright   :  (C) 2012-2016  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Dependency info

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

module Dependencies (
  dependencies,
  missingPackages,
  notInstalled,
  packageDependencies,
  showDep,
  subPackages,
  testsuiteDependencies
  ) where

import PackageUtils (packageName, repoquery)
import SysCmd (cmd, cmdBool, (+-+))

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,2))
#else
import Control.Applicative ((<$>))
#endif
import Control.Monad (filterM, when)

import Data.List (delete, isSuffixOf, nub)
import Data.Maybe (catMaybes, isNothing)

import Distribution.Package  (Dependency (..),
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
                              unPackageName,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
                              unPkgconfigName
#endif
#else
                              PackageName (..)
#endif
                                       )

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.LegacyExeDependency (LegacyExeDependency (..))
import Distribution.Types.PkgconfigDependency (PkgconfigDependency (..))
#else
#endif

import Distribution.PackageDescription (PackageDescription (..),
                                        allBuildInfo,
                                        BuildInfo (..),
                                        TestSuite (..),
                                        hasExes, 
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,24,0)
                                        setupDepends
#endif
                                        )
import System.Directory (doesDirectoryExist, doesFileExist)
import System.IO (hPutStrLn, stderr)

excludedPkgs :: String -> Bool
excludedPkgs = flip notElem ["Cabal", "base", "ghc-prim", "integer-gmp"]

-- returns list of deps and whether package is self-dependent
buildDependencies :: PackageDescription -> String -> ([String], Bool)
buildDependencies pkgDesc self =
  let deps = nub $ map depName (buildDepends pkgDesc)
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,24,0)
                   ++ (maybe [] (map depName . setupDepends) (setupBuildInfo pkgDesc))
#endif
  in
    (filter excludedPkgs (delete self deps), self `elem` deps && hasExes pkgDesc)

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
#else
unPackageName :: PackageName -> String
unPackageName (PackageName n) = n
#endif

depName :: Dependency -> String
depName (Dependency  n _) = unPackageName n

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
exeDepName :: LegacyExeDependency -> String
exeDepName (LegacyExeDependency n _) = n

pkgcfgDepName :: PkgconfigDependency -> String
pkgcfgDepName (PkgconfigDependency n _) = unPkgconfigName n
#else
exeDepName :: Dependency -> String
exeDepName = depName

pkgcfgDepName :: Dependency -> String
pkgcfgDepName = depName
#endif

showDep :: String -> String
showDep p = "ghc-" ++ p ++ "-devel"

dependencies :: PackageDescription  -- ^pkg description
                -> IO ([String], [String], [String], [String], Bool)
                -- ^depends, tools, c-libs, pkgcfg, selfdep
dependencies pkgDesc = do
    let self = packageName $ package pkgDesc
        (deps, selfdep) = buildDependencies pkgDesc self
        buildinfo = allBuildInfo pkgDesc
        tools =  nub $ map exeDepName (concatMap buildTools buildinfo)
        pkgcfgs = nub $ map pkgcfgDepName $ concatMap pkgconfigDepends buildinfo
        clibs = concatMap extraLibs buildinfo
    return (deps, tools, nub clibs, pkgcfgs, selfdep)

data QueryBackend = Rpm | Repoquery deriving Eq

resolveLib :: String -> IO (Maybe String)
resolveLib lib = do
  lib64 <- doesDirectoryExist "/usr/lib64"
  let libsuffix = if lib64 then "64" else ""
  let lib_path = "/usr/lib" ++ libsuffix ++ "/lib" ++ lib ++ ".so"
  libInst <- doesFileExist lib_path
  if libInst
    then rpmqueryFile Rpm lib_path
    else do
    putStrLn $ "Running repoquery on" +-+ "lib" ++ lib
    rpmqueryFile Repoquery lib_path

-- use repoquery or rpm -q to query which package provides file
rpmqueryFile :: QueryBackend -> FilePath -> IO (Maybe String)
rpmqueryFile backend file = do
  -- FIXME dnf repoquery does not support -f !
  let args =  ["-q", "--qf=%{name}", "-f"]
  out <- if backend == Rpm
         then cmd "rpm" (args ++ [file])
         else repoquery args file
  let pkgs = nub $ words out
      -- EL5 repoquery can return "No package provides <file>"
  case pkgs of
    [pkg] -> return $ Just pkg
    [] -> do
      warning $ "Could not resolve package that provides" +-+ file
      return Nothing
    _ -> do
      warning $ "More than one package seems to provide" +-+ file ++ ": " +-+ unwords pkgs
      return Nothing

warning :: String -> IO ()
warning s = hPutStrLn stderr $ "Warning:" +-+ s

packageDependencies :: Bool   -- ^strict mode: True means abort on unknown dependencies
                -> PackageDescription  -- ^pkg description
                -> IO ([String], [String], [String], [String], Bool)
                -- ^depends, tools, c-libs, pkgcfg, selfdep
packageDependencies strict pkgDesc = do
    (deps, tools', clibs', pkgcfgs, selfdep) <- dependencies pkgDesc
    let excludedTools n = n `notElem` ["ghc", "hsc2hs", "perl"]
        mapTools "gtk2hsC2hs" = "gtk2hs-buildtools"
        mapTools "gtk2hsHookGenerator" = "gtk2hs-buildtools"
        mapTools "gtk2hsTypeGen" = "gtk2hs-buildtools"
        mapTools tool = tool
        chrpath = ["chrpath" | selfdep]
        tools = filter excludedTools $ nub $ map mapTools tools' ++ chrpath
    clibsWithErrors <- mapM resolveLib clibs'
    when (any isNothing clibsWithErrors) $
      if strict
      then fail "cannot resolve all clib dependencies"
      else putStrLn "Warning: could not resolve all clib dependencies"
    let clibs = catMaybes clibsWithErrors
    let showPkgCfg p = "pkgconfig(" ++ p ++ ")"
    return (map showDep deps, tools, nub clibs, map showPkgCfg pkgcfgs, selfdep)

testsuiteDependencies :: PackageDescription  -- ^pkg description
                -> String           -- ^self
                -> [String]         -- ^depends
testsuiteDependencies pkgDesc self =
  map showDep . delete self . filter excludedPkgs . nub . map depName $ concatMap (targetBuildDepends . testBuildInfo) (testSuites pkgDesc)

missingPackages :: PackageDescription -> IO [String]
missingPackages pkgDesc = do
  (deps, tools, clibs, pkgcfgs, _) <- packageDependencies False pkgDesc
  pcpkgs <- mapM derefPkg pkgcfgs
  filterM notInstalled $ deps ++ ["ghc-Cabal-devel", "ghc-rpm-macros"] ++ tools ++ clibs ++ pcpkgs

notInstalled :: String -> IO Bool
notInstalled dep =
  not <$> cmdBool "rpm" ["-q", "--whatprovides", shellQuote dep]
  where
    shellQuote :: String -> String
    shellQuote (c:cs) = (if c `elem` "()" then (['\\', c] ++) else (c:)) (shellQuote cs)
    shellQuote "" = ""

derefPkg :: String -> IO String
derefPkg req = do
  res <- singleLine <$> repoquery ["--qf", "%{name}", "--whatprovides"] req
  if null res
    then error $ req +-+ "provider not found by repoquery"
    else return res
  where
    singleLine :: String -> String
    singleLine "" = ""
    singleLine s = (head . lines) s

subPackages :: Maybe FilePath -> PackageDescription -> IO [String]
subPackages mspec pkgDesc = do
  develSubpkgs <- filter ("-devel" `isSuffixOf`) . lines <$> maybe (return "") (\ f -> cmd "rpmspec" ["-q", "--qf", "%{name}\n", f]) mspec
  let self = packageName $ package pkgDesc
      subpkgs = delete (showDep self) develSubpkgs
  missing <- missingPackages pkgDesc
  return $ nub (subpkgs ++ missing)
