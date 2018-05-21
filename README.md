[![Build Status](https://travis-ci.org/juhp/cabal-rpm.png)](https://travis-ci.org/juhp/cabal-rpm)
[![Hackage](http://img.shields.io/hackage/v/cabal-rpm.png)](http://hackage.haskell.org/package/cabal-rpm)
[![Stackage LTS](http://stackage.org/package/cabal-rpm/badge/lts)](http://stackage.org/lts/package/cabal-rpm)
[![Stackage Nightly](http://stackage.org/package/cabal-rpm/badge/nightly)](http://stackage.org/nightly/package/cabal-rpm)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/juhp/cabal-rpm?utm_source=badge&utm_medium=badge&utm_content=badge)

# cabal-rpm

cabal-rpm is a tool for RPM packaging of Haskell Cabal-based packages.
It interacts with yum/dnf to install build dependencies and can also act as
a cabal-install wrapper installing dependencies packaged in Fedora before
running "cabal install".

Cabal-rpm was originally created by Bryan O'Sullivan (see README.orig)
but has since been updated to work with current Cabal and Fedora Packaging
Guidelines replacing cabal2spec.  It is used by Fedora and OpenSuSE.
It is licensed under the terms of the GPL version 3 (see the COPYING file).

You can build from source as normal by running `cabal install`
or via Hackage with `cabal install cabal-rpm`.

## Requirements
cabal-rpm assumes you are using ghc-rpm-macros for Haskell RPM packaging.
It currently needs Cabal 1.10 or later to build (ie ghc 7 or later).

## Installation
The package is on Hackage. If you have cabal-install (part of Haskell Platform)
you can install simply with

    $ cabal install cabal-rpm

## Usage
To create a `.spec` file for a Haskell src package in the current dir:

    $ cabal-rpm spec

or directly on a `.cabal` file:

    $ cabal-rpm spec path/to/mypkg.cabal

or on a package source dir:

    $ cabal-rpm spec mypkg-0.1

You can also package directly from hackage:

    $ cabal-rpm srpm somepkg

or

    $ cabal-rpm local somepkg-0.1

will unpack 'somepkg-0.1' from hackage
(if the dir does not exist, otherwise it uses the existing dir),
create a spec file for it, and build it.

cabal-rpm creates `.spec` files in the current dir
and if a `.spec` file already exists it will append `.cblrpm`
to the generated filename to avoid overwriting an existing file.

    $ cabal-rpm install [pkg][-ver]

will yum/dnf install available missing dependencies and
run "cabal install" to build the package.

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
