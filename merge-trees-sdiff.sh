#!/bin/sh

PORTSDIR=/usr/ports

for dir in $(find . -type d -depth 1 ! -name .git); do
	for f in $(find $dir -type f); do
		echo -n $f...
		if [ -f $PORTSDIR/$f ]; then
			echo "ok (updating)"
			cp $f $f.old
			sdiff $PORTSDIR/$f $f.old -a -s -o $f
			rm -rf $f.old
		else
			echo "(not found, don't care)"
		fi
	done
done

