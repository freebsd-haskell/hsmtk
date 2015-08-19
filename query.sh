#!/bin/sh

: ${PORTSDIR:=/usr/ports}

if [ "$1" = "" ]; then
  echo USAGE: $0 variablename
  exit 127
else
  VAR=$1
fi

while read hs_port; do
  if [ -d ${PORTSDIR}/$hs_port ]; then
    var=$(make -C ${PORTSDIR}/$hs_port -V ${VAR})
    echo $hs_port $var
  else
    echo Warning: $hs_port is not found. > /dev/stderr
  fi
done
