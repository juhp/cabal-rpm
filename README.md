[![Build Status](https://travis-ci.org/juhp/cabal-rpm.png)](https://travis-ci.org/juhp/cabal-rpm)

# cabal-rpm

cabal-rpm creates RPM spec files for packaging Haskell Cabal-based packages.

This version updates the older cabal-rpm-0.5.1 package by Bryan O'Sullivan
(see README.orig) to work with current Cabal and also updated to current RPM
packaging conventions defined until now by cabal2spec which is used by Fedora
and also OpenSuSE.  It is licensed under the terms of the GPL version 3
(see the COPYING file).  It is designed to replace the simple cabal2spec tool
shell script made by the maintainer.

You can build from source as normal by running `cabal install`
or via Hackage with `cabal install cabal-rpm`.

## Requirements
cabal-rpm assumes you are using ghc-rpm-macros for Haskell RPM packaging.
It currently needs Cabal 1.10 or later to build (ie ghc7),
but it should not be hard to patch it to build at least for ghc-6.12.

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

    $ cblrpm build somepkg

or

    $ cblrpm build somepkg-0.1

will unpack the (latest) 'somepkg' package from hackage
(if the dir does not exist, otherwise it uses the existing dir),
create a spec file for it, and build it.

cblrpm always creates `.spec` files in the current dir
and if a `.spec` file already exists it will append `.cblrpm`
to the generated filename to avoid overwriting an existing file.

## Development
The latest source code is available from: https://github.com/juhp/cabal-rpm

## Plans
More features are planned and patches welcome, including recursive packaging.
See the TODO file for more details.
