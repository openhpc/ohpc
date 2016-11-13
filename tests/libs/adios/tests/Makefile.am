TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

TESTS                   = arrays_write
check_PROGRAMS          = arrays_write
arrays_write_SOURCES    = arrays_write.c

TESTS                  += arrays_read
check_PROGRAMS         += arrays_read
arrays_read_SOURCES     = arrays_read.c

TESTS                  += scalars_write
check_PROGRAMS         += scalars_write
scalars_write_SOURCES  = scalars_write.F90
scalars_write_LDADD    = -ladiosf

TESTS                  += scalars_read
check_PROGRAMS         += scalars_read
scalars_read_SOURCES    = scalars_read.F90
scalars_read_LDADD      = -ladiosf

TESTS                  += test_module
TESTS                  += rm_execution

