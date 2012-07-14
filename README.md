cabal-rpm helps you to package Haskell Cabal-based packages into rpm packages.

The code here updates the original cabal-rpm package by Bryan O'Sullivan
(see README.orig), and is licensed under the terms of the GPL version 3
(see the COPYING file).

The source code can be installed normally by running "cabal install"
or "cabal install cabal-rpm" via Hackage.

Requirements
------------
cabal-rpm assumes you are using ghc-rpm-macros for Haskell RPM packaging.
It should build with the Cabal library in ghc-7.x and
probably still ghc-6.12 at least.


Usage
-----
To use cabal-rpm, go into a Haskell package directory or
one you just "cabal unpack"ed, and run cabal-rpm there.
This will create a .spec file for you, which you can then rpmbuild and install.


Download
--------
The latest source code is available from github:
https://github.com/juhp/cabal-rpm
