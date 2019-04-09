% CABAL-RPM(1)
% This manpage was written by Jens Petersen
% 2013-01-21

# NAME
cabal-rpm - a RPM packaging tool for Haskell Cabal packages

# SYNOPSIS
cabal-rpm [*options*] spec [*path-or-pkg*]
cabal-rpm [*options*] local [*path-or-pkg*]
cabal-rpm [*options*] srpm [*path-or-pkg*]
cabal-rpm [*options*] prep [*path-or-pkg*]
cabal-rpm [*options*] builddep [*path-or-pkg*]
cabal-rpm [*options*] install [*path-or-pkg*]
cabal-rpm [*options*] depends [*path-or-pkg*]
cabal-rpm [*options*] requires [*path-or-pkg*]
cabal-rpm [*options*] missingdeps [*path-or-pkg*]
cabal-rpm [*options*] diff [*path-or-pkg*]
cabal-rpm [*options*] update [*path-or-pkg*]
cabal-rpm [*options*] refresh [*path-or-pkg*]

# DESCRIPTION
cabal-rpm generates RPM packages and .spec files from Haskell Cabal package.

If no *path-or-pkg* is specified, cabal-rpm looks for a .spec or .cabal file
in the current directory.  Otherwise, it will look for *path-or-pkg*. If
the argument is a directory then it will look there for a .spec or .cabal file.
If the argument is a path to a .cabal file then it will use it.
Otherwise if there is no '/' in the argument and it does not exist
then cabal-rpm will try to unpack the package and use its .cabal file.
cabal-rpm uses a temporary directory for unpackaging tarballs or packages.
cabal-rpm then parses the .cabal file and uses it to generate a .spec file
that can be built.

If a <PKG>.spec already exists, cabal-rpm outputs to <PKG>.spec.cblrpm instead.

# OPTIONS
-h, --help
:   Show the help text.

-b, --binary
:   Force the base package name to be the Hackage package name.

-f *FLAGS*, --flags=*FLAGS*
: Override one or more Cabal build configuration flags.

--force
: overwrite existing spec file

--missing
: comment out unavailable BuildRequires deps

--release=*RELEASE*
: Override the release number in the .spec file.

--standalone
: create a standalone package, built using cabal-install

-s *STREAM* , --stream=*STREAM*
: Stackage stream (eg lts-13) or 'hackage' used to get package version.

-v *N*, --verbose=*N*
: Set verbosity to *N*.

--version=*VERSION*
: Override the version number in the .spec file.

# EXAMPLES
Below CMD can be one of:
    spec, srpm, prep, local, install, diff, builddep, depends, requires,
    missingdeps, update, refresh

Do CMD for the package in current directory:

    cabal-rpm CMD

Do CMD for package (directory or package name):
 
    cabal-rpm CMD [package]

Do CMD for package-version (directory or package name):

    cabal-rpm CMD [package-version]

Do CMD on a .cabal file:

    cabal-rpm CMD path/to/some.cabal

# HISTORY
Cabal-rpm was originally written by Bryan O'Sullivan in 2007-2008
and resurrected by Jens Petersen in 2012 to replace cabal2spec.

# SEE ALSO
<http://github.com/juhp/cabal-rpm/>
