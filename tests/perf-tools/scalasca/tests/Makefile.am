TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1
AUTOMAKE_OPTIONS  = no-dependencies

SUBDIRS           = mpi
SUBDIRS          += serial

TESTS             = test_module
TESTS            += rm_execution

distclean-local:
	rm -f *.input.c *.input.prep.c *.input.prep.c.opari.inc \
	      *.input.prep.opari.c *.input.prep.opari.i
