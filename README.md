[![Build Status](https://travis-ci.org/juhp/cabal-rpm.png)](https://travis-ci.org/juhp/cabal-rpm)
[![Hackage](http://img.shields.io/hackage/v/cabal-rpm.png)](http://hackage.haskell.org/package/cabal-rpm)

# cabal-rpm

cabal-rpm is a tool for RPM packaging of Haskell Cabal-based packages.
It interacts with yum to install build dependencies and can also act as
a cabal-install wrapper installing dependencies packaged in Fedora before
running "cabal install".

Cabal-rpm was originally created by Bryan O'Sullivan (see README.orig)
but has since been updated to work with current Cabal and Fedora Packaging
Guidelines replacing cabal2spec.  It is used by Fedora and (patched) OpenSuSE. 
It is licensed under the terms of the GPL version 3 (see the COPYING file).

You can build from source as normal by running `cabal install`
or via Hackage with `cabal install cabal-rpm`.

## Requirements
cabal-rpm assumes you are using ghc-rpm-macros for Haskell RPM packaging.
It currently needs Cabal 1.10 or later to build (ie ghc7),
but it should not be hard to patch it to build at least for ghc-6.12.

## Installation
The package is on Hackage. If you have cabal-install (part of Haskell Platform)
you can install simply with

    $ cabal install cabal-rpm

## Usage
To create a `.spec` file for a Haskell src package in the current dir:

    $ cblrpm spec

or directly on a `.cabal` file:

    $ cblrpm spec path/to/mypkg.cabal

or tarball:

    $ cblrpm spec path/to/mypkg-version.tar.gz

or on a package source dir:

    $ cblrpm spec mypkg-0.1

You can also package directly from hackage:

    $ cblrpm srpm somepkg

or

    $ cblrpm rpm somepkg-0.1

will unpack the (latest) 'somepkg' package from hackage
(if the dir does not exist, otherwise it uses the existing dir),
create a spec file for it, and build it.

cblrpm creates `.spec` files in the current dir
and if a `.spec` file already exists it will append `.cblrpm`
to the generated filename to avoid overwriting an existing file.

    $ cblrpm install [pkg][-ver]

will yum install available missing dependencies and
run "cabal install" to build the package.

## Development
The latest source code is available from: https://github.com/juhp/cabal-rpm

## Plans
More features are planned and patches welcome.
See the TODO file for more details.
