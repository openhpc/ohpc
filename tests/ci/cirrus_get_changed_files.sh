#!/bin/bash

set -x

if [ "$CIRRUS_PR" == "" ]; then
	echo "This script only support pull requests"
	exit 0
fi

if [ -z  "$CIRRUS_REPO_OWNER" ]; then
	echo "\$CIRRUS_REPO_OWNER not set. Exiting."
	exit 0
fi
if [ -z  "$CIRRUS_REPO_NAME" ]; then
	echo "\$CIRRUS_REPO_NAME not set. Exiting."
	exit 0
fi

RESULT=""
COMMITS=$( curl -s https://api.github.com/repos/"$CIRRUS_REPO_OWNER"/"$CIRRUS_REPO_NAME"/pulls/"$CIRRUS_PR"/commits | jq -r .[].sha )

for c in $COMMITS; do
	FILES=$( git diff-tree --no-commit-id --name-only -r "$c" )
	for f in $FILES; do
		if [ ! -e "$f" ]; then
			# If this file was deleted we ignore it
			continue
		fi
		RESULT="$RESULT $f"
	done
done

echo "$RESULT"
