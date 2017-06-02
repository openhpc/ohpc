TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

TESTS  = init.sh

if IB_FABRIC_ENABLED
TESTS += ibv_devinfo
endif

if OPA_FABRIC_ENABLED
TESTS += opainfo
endif


CLEANFILES=$(wildcard *.xml)
