TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1
SUBDIRS                 = ohpc_module

TESTS                   = datablock
check_PROGRAMS          = datablock
datablock_LDFLAGS       = -locr_x86
datablock_SOURCES       = dbNoAcquire0.c

TESTS                  += dist
check_PROGRAMS         += dist
dist_LDFLAGS            = -locr_x86
dist_SOURCES            = testDistAffinity0.c

TESTS                  += edt
check_PROGRAMS         += edt
edt_LDFLAGS             = -locr_x86
edt_SOURCES             = edtReturnEvent.c

#TESTS                  += rm_execution
