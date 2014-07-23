#!/bin/sh

# It will count the default dependencies only.

: ${PORTSDIR:=/usr/ports}

hackagedb="${PORTSDIR}/lang/ghc/bsd.hackage.mk"
hs_ports=$(cat $hackagedb | grep _port= | tr -d "\t " | sed 's/\([^=]*\)=\([^#]*\)\(.*\)/\2/' | sort)

for hs_port in $hs_ports; do
  deps=$(make -C ${PORTSDIR}/$hs_port -V USE_CABAL | sed 's/[<=>][<=>]*[0-9\.]*//g')
  portname=$(make -C ${PORTSDIR}/$hs_port -V PORTNAME)
  echo $portname : $deps
done
