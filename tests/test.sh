#!/bin/sh

# FIXME check branch
SPECS=${1:-$(ls ~/fedora/haskell/hackage/*/*.spec)}
CBLRPM=/usr/bin/cabal-rpm
#CBLRPM=cabal-rpm

mkdir -p pkgs
cd pkgs/

for p in $SPECS; do
    spec=$(basename $p)
    if [ -f "$spec" ]; then
        NV=$(rpmspec -q --qf "%{name}-%{version}" $spec | sed -e "s/^ghc-//")
        if [ -d "$NV" ]; then
            rm -rf $NV
        fi
        rm $spec
    fi
    pkg=$(basename -s .spec $p)
    case $pkg in
        ghc-*) $CBLRPM spec $(echo $pkg | sed -e "s/^ghc-//") ;;
        *) $CBLRPM spec -b $pkg ;;
    esac
done
