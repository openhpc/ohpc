#!/bin/bash

set -x

if [ "$DRONE_BUILD_EVENT" != "pull_request" ]; then
	echo "This script only support pull requests"
	exit 0
fi

if [ -z  "$DRONE_REPO_OWNER" ]; then
	echo "\$DRONE_REPO_OWNER not set. Exiting."
	exit 0
fi
if [ -z  "$DRONE_REPO_NAME" ]; then
	echo "\$DRONE_REPO_NAME not set. Exiting."
	exit 0
fi
if [ -z  "$DRONE_PULL_REQUEST" ]; then
	echo "\$DRONE_PULL_REQUEST not set. Exiting."
	exit 0
fi

RESULT=""
COMMITS=$( curl -s https://api.github.com/repos/"$DRONE_REPO_OWNER"/"$DRONE_REPO_NAME"/pulls/"$DRONE_PULL_REQUEST"/commits | jq -r .[].sha )

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
