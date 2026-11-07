# The tests below run MPI programs on the head node, where MPICH can pick a
# container bridge interface (podman0, ...) as its default NIC and fail. The
# loopback interface exists on every host and is enough for a single process.
# The provider is pinned too, because MPICH can select the sockets or
# udp;ofi_rxd provider, and FI_TCP_IFACE means nothing to them.
# run_mpi_binary removes exactly this pair before it submits a job, so the
# rm_execution jobs on the compute nodes keep their native provider.
AM_TESTS_ENVIRONMENT = FI_PROVIDER=tcp; export FI_PROVIDER; FI_TCP_IFACE=lo; export FI_TCP_IFACE;


LDADD                   = ${LIBS}

TESTS                   = C_test
check_PROGRAMS          = C_test
C_test_SOURCES          = C_test.c

TESTS                  += C_mpi_test
check_PROGRAMS         += C_mpi_test
C_mpi_test_SOURCES      = C_mpi_test.c

TESTS                  += F90_test
check_PROGRAMS         += F90_test
F90_test_SOURCES        = F90_test.F90

TESTS                  += test_module
TESTS                  += rm_execution
