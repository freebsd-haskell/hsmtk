#!/bin/sh

portstree="ports"
ghc="lang/ghc"
hackagedb="$ghc/bsd.hackage.mk"
checkout="svn co"
update="svn up"
repository="svn+ssh://svn.freebsd.org/ports/head"

rm -rf $portstree
$checkout --depth=immediates $repository $portstree
$update $portstree/$ghc

hs_ports=`cat "$portstree/$hackagedb" | grep _port= | tr -d "\t " | sed 's/\([^=]*\)=\([^#]*\)\(.*\)/\2/' | sort`

cwd=`pwd`
cd $portstree
$update $hs_ports
cd $cwd
