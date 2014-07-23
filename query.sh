#!/bin/sh

: ${PORTSDIR:=/usr/ports}

if [ "$1" = "" ]; then
  echo USAGE: $0 variablename
  exit 127
else
  VAR=$1
fi

hackagedb="${PORTSDIR}/lang/ghc/bsd.hackage.mk"
hs_ports=$(cat $hackagedb | grep _port= | tr -d "\t " | sed 's/\([^=]*\)=\([^#]*\)\(.*\)/\2/' | sort)

for hs_port in $hs_ports; do
  if [ -d ${PORTSDIR}/$hs_port ]; then
    var=$(make -C ${PORTSDIR}/$hs_port -V ${VAR})
    echo $hs_port $var
  else
    echo Warning: $hs_port is not found. > /dev/stderr
  fi
done
