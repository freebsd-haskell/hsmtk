#!/bin/sh

hackagedb="lang/ghc/bsd.hackage.mk"
hs_ports=$(cat $hackagedb | grep _port= | tr -d "\t " | sed 's/\([^=]*\)=\([^#]*\)\(.*\)/\2/' | sort)

for hs_port in $hs_ports; do
  pkgversion=$(make -C $hs_port -V PKGVERSION)
  echo $hs_port $pkgversion
done