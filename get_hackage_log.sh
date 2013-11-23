#!/bin/sh

: ${DESTDIR:=.}

fetch -q -o - http://hackage.haskell.org/packages/index.tar.gz | tar -tf - | fgrep ".cabal" | cut -d '/' -f1-2 > $DESTDIR/hackage.log

