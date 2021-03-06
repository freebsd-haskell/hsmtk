#!/bin/sh

: ${PORTSDIR:=/usr/ports}

merge_loop() {
	SUBJECT=$1
	MERGE_AGAIN=yes
	while [ "${MERGE_AGAIN}" = "yes" ]; do
		cp -p "${SUBJECT}" "${SUBJECT}.merged"
		cat "${PORTSDIR}/${SUBJECT}" | sed -e 's/$FreeBSD:.*$/$FreeBSD$/g' > "${SUBJECT}.theirs"
		sdiff -o "${SUBJECT}.merged" --text --suppress-common-lines \
		    --width=${SCREEN_WIDTH:-80} "${SUBJECT}.theirs" "${SUBJECT}"
		SDIFF=$?
		if [ "${SDIFF}" = "0" ]; then
			unset MERGE_AGAIN
			rm -f "${SUBJECT}.merged" "${SUBJECT}.theirs"
		else
			INSTALL_MERGED=V
		fi
		while [ "${INSTALL_MERGED}" = "v" -o "${INSTALL_MERGED}" = "V" ]; do
			echo ''
			echo "  Use 'i' to accept the merged file"
			echo "  Use 'r' to re-do the merge"
			echo "  Use 'v' to view the merged file"
			echo "  Use 'q' to quit"
			echo "  Default is to leave the temporary file to deal with by hand"
			echo ''
			echo -n "    *** How should I deal with the merged file? [Leave it for later] "
			read INSTALL_MERGED

			case "${INSTALL_MERGED}" in
			[iI])
				mv "${SUBJECT}.merged" "${SUBJECT}"
				rm -rf "${SUBJECT}.theirs"
				unset MERGE_AGAIN
				;;
			[rR])
				rm -f "${SUBJECT}.merged" "${SUBJECT}.theirs"
				;;
			[vV])
				${PAGER} "${SUBJECT}.merged"
				;;
			[qQ])
				rm -f "${SUBJECT}.merged" "${SUBJECT}.theirs"
				QUIT_MERGING=yes
				unset MERGE_AGAIN
				;;
			'')
				echo "    *** ${SUBJECT} will remain for your consideration"
				unset MERGE_AGAIN
				;;
			*)
				echo "Invalid choice: ${INSTALL_MERGED}"
				INSTALL_MERGED=V
				;;
			esac
		done
	done
}

for dir in $(find . -type d -depth 1 ! -name .git); do
	for f in $(find $dir -type f); do
		echo -n $f...
		if [ -f $PORTSDIR/$f ]; then
			echo "ok (updating)"
			merge_loop $f
		else
			echo "(not found, don't care)"
		fi
		if [ "${QUIT_MERGING}" = "yes" ]; then
			exit 127
		fi
	done
done
