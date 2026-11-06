#!/bin/bash
# Check that chronyd on this host is synchronised with its NTP source.

# `command -v` is a bash builtin. The compute images do not install the
# `which` package, so a `which` call fails there on hosts whose time is fine.
command -v chronyc >/dev/null || exit 1

# chronyd needs a few seconds after boot to select a source, and the test
# suite can run before that on a freshly provisioned node. waitsync polls
# until chronyd reports a selected source; 12 tries at 5 second intervals
# bound the wait at one minute.
if ! chronyc waitsync 12 0 0 5; then
	echo "chronyd is not synchronised:"
	chronyc tracking
	chronyc -n sources
	exit 1
fi
