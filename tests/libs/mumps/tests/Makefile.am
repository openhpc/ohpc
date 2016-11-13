TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

TESTS                    = C_double
check_PROGRAMS           = C_double
C_double_SOURCES         = c_example.c
C_double_LDADD           = -ldmumps 
C_double_LDFLAGS         = @SCALAPACK@

TESTS                   += F_single
check_PROGRAMS          += F_single
F_single_SOURCES         = ssimpletest.F
F_single_LDADD           = -lsmumps
F_single_LDFLAGS         = @SCALAPACK@

TESTS                   += F_double
check_PROGRAMS          += F_double
F_double_SOURCES         = dsimpletest.F
F_double_LDADD           = -ldmumps
F_double_LDFLAGS         = @SCALAPACK@

TESTS                   += F_complex
check_PROGRAMS          += F_complex
F_complex_SOURCES        = csimpletest.F
F_complex_LDADD          = -lcmumps
F_complex_LDFLAGS        = @SCALAPACK@

TESTS                   += F_doublecomplex
check_PROGRAMS          += F_doublecomplex
F_doublecomplex_SOURCES  = zsimpletest.F
F_doublecomplex_LDADD    = -lzmumps
F_doublecomplex_LDFLAGS  = @SCALAPACK@

TESTS                   += test_module
TESTS                   += rm_execution

