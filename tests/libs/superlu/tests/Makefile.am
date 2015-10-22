TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

TESTS                   = superlu
check_PROGRAMS          = superlu
superlu_LDFLAGS          = -lsuperlu
superlu_SOURCES          = superlu.c sp_ienv.c

TESTS                  += test_module
TESTS                  += rm_execution

