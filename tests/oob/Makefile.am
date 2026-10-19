TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

TESTS  = init.sh
TESTS += ipmitool
if CONMAN_ENABLED
TESTS += conman
endif
TESTS += warewulf-ipmi

CLEANFILES=$(wildcard *.xml)
