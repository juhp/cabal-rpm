% CABAL-RPM(1)
% This manpage was written by Jens Petersen
% 2012-11-01

# NAME
cabal-rpm - generates RPM .spec files from Cabal packages

# SYNOPSIS
cabal-rpm [*options*] [*path-or-pkg*]

# DESCRIPTION
Cabal-rpm generates RPM .spec files from Haskell Cabal package.

If no *path-or-pkg* is specified, cabal-rpm looks for a .cabal file
in the current directory.  Otherwise, it will look for *path-or-pkg*.
If the argument is a directory then it will look there for a .cabal file.
If the argument is a path to a .cabal or .tar.gz file then it will use it.
Otherwise if there is no '/' in the argument and it does not exist
then cabal-rpm will try to unpack the package and use its .cabal file.
Cabal-rpm uses a temporary directory for unpackaging tarballs or packages.

Cabal-rpm then parses the above specified .cabal file and
uses it to generate a .spec file that can be built.

If a <PKG>.spec already exists, cabal-rpm output to <PKG>.spec.cabal-rpm instead.

The cabal-rpm-diff command can be used in the same way to output a diff
of <PKG>.spec and <PKG>.spec.cabal-rpm directly.

# OPTIONS
-h, --help
:   Show the help text.

-l, --library
:   Force a BinLib package with executable(s) to be considered a Lib package.

-f *FLAGS*, --flags=*FLAGS*
: Override one or more Cabal build configuration flags.

--release=*RELEASE*
: Override the release number in the .spec file.

-v *N*, --verbose=*N*
: Set verbosity to *N*.

--version=*VERSION*
: Override the version number in the .spec file.

# EXAMPLES
Create a .spec file for the Cabal src package in current directory:

    cabal-rpm

Create a .spec file for package (directory or package name):
 
    cabal-rpm [package]

Create a .spec file for package-version (directory or package name):

    cabal-rpm [package-version]

Create a .spec file for a .cabal file:

    cabal-rpm path/to/some.cabal

Create a .spec file from a tarball:

    cabal-rpm path/to/pkg-ver.tar.gz

# HISTORY
Cabal-rpm was originally written by Bryan O'Sullivan in 2007-2008
and resurrected by Jens Petersen in 2012 to replace cabal2spec.

# SEE ALSO
<http://github.com/juhp/cabal-rpm/>
