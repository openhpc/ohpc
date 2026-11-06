# The tests below run MPI programs on the head node, where MPICH can pick a
# container bridge interface (podman0, ...) as its default NIC and fail. The
# loopback interface exists on every host and is enough for a single process.
# run_mpi_binary removes this value before it submits a job, so the
# rm_execution jobs on the compute nodes are not pinned to it.
AM_TESTS_ENVIRONMENT = FI_TCP_IFACE=lo; export FI_TCP_IFACE;


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

