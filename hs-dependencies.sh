#!/bin/sh

#
# This script shall be run in your $PORTSDIR (or in a cloned freebsd-haskell
# repository).
#
# Note that if will count the default dependencies only.
#

hackagedb="lang/ghc/bsd.hackage.mk"
hs_ports=`cat $hackagedb | grep _port= | tr -d "\t " | sed 's/\([^=]*\)=\([^#]*\)\(.*\)/\2/' | sort`

for hs_port in $hs_ports; do
  deps=`make -C $hs_port -V USE_CABAL | sed 's/[<=>][<=>]*[0-9\.]*//g'`
  portname=`make -C $hs_port -V PORTNAME`
  echo $portname : $deps
done
