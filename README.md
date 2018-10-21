[![Build Status](https://travis-ci.org/juhp/cabal-rpm.png)](https://travis-ci.org/juhp/cabal-rpm)
[![Hackage](http://img.shields.io/hackage/v/cabal-rpm.png)](http://hackage.haskell.org/package/cabal-rpm)
[![Stackage LTS](http://stackage.org/package/cabal-rpm/badge/lts)](http://stackage.org/lts/package/cabal-rpm)
[![Stackage Nightly](http://stackage.org/package/cabal-rpm/badge/nightly)](http://stackage.org/nightly/package/cabal-rpm)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/cabal-rpm/Lobby?utm_source=badge&utm_medium=badge&utm_content=badge)
[![license](https://img.shields.io/badge/license-GPLv3+-brightgreen.svg)](https://www.gnu.org/licenses/gpl.html)

# cabal-rpm

cabal-rpm is a tool for RPM packaging of Haskell Cabal-based packages.
Firstly it creates RPM spec files from the .cabal files of Haskell packages.
It can also install build dependencies using dnf/yum, it prefers package
versions from Stackage, and can build and install packages and their dependencies
recursively as rpm packages. It can also update packages and refresh spec files.

Cabal-rpm was originally created by Bryan O'Sullivan (see README.orig)
to create spec files, and was later updated by Jens Petersen to work with current
Cabal and Fedora Packaging Guidelines replacing cabal2spec, and extended with
many new features.  It is used by Fedora and earlier by OpenSuSE. It is licensed
under the terms of the GPL version 3 (see the COPYING file).

## Requirements
cabal-rpm assumes you are using ghc-rpm-macros for Haskell RPM packaging.
It currently needs Cabal 1.10 or later to build (ie ghc 7 or later).

## Installation
The package is on Hackage. If you have cabal-install (part of Haskell Platform)
you can install simply with

    $ cabal install cabal-rpm

You can also install it with `stack install cabal-rpm`.

You can also build and install it from source as normal by running
`cabal install`.

## Usage
To create a `.spec` file for a package:

    $ cabal-rpm spec somepkg

By default it will use the package version in Stackage LTS or else the latest
version from Hackage.

You can also specify a version if you wish

    $ cabal-rpm spec somepkg-0.1

which will unpack 'somepkg-0.1' from Hackage (unless the dir already exists),
and create a spec file for it.

cabal-rpm also works in an rpm package source directory or inside the source of
a Haskell package:

    $ cabal-rpm spec

cabal-rpm creates `.spec` files in the current dir
and if a `.spec` file already exists it will append `.cblrpm`
to the generated filename to avoid overwriting an existing file.

    $ cabal-rpm install [pkg][-ver]

will yum/dnf install any available missing dependencies and
build an rpm package run "cabal install" to build the package.
Additionally it will recursively package and install missing Haskell
dependencies.

    $ cabal-rpm diff

diffs the current spec file with a freshly generated spec file.

    $ cabal-rpm update

updates the package to the latest Hackage version

    $ cabal-rpm refresh

updates the spec file to the current cabal-rpm packaging.

There are more commands: prep, builddep, depends, requires, missingdeps.
See the manpage or help output for more details.

## Development
The latest source code is available from: https://github.com/juhp/cabal-rpm

## Plans
More features are planned and patches welcome.
See the [TODO](TODO) file for more details.
