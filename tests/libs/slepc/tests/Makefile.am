# The tests below run MPI programs on the head node, where MPICH can pick a
# container bridge interface (podman0, ...) as its default NIC and fail. The
# loopback interface exists on every host and is enough for a single process.
# The provider is pinned too, because MPICH can select the sockets or
# udp;ofi_rxd provider, and FI_TCP_IFACE means nothing to them.
# run_mpi_binary removes exactly this pair before it submits a job, so the
# rm_execution jobs on the compute nodes keep their native provider.
AM_TESTS_ENVIRONMENT = FI_PROVIDER=tcp; export FI_PROVIDER; FI_TCP_IFACE=lo; export FI_TCP_IFACE;


LDADD                   = -lslepc -lpetsc

TESTS                   = test4f
check_PROGRAMS          = test4f
test4f_SOURCES          = test4f.F

TESTS                  += ex15
check_PROGRAMS         += ex15
ex15_SOURCES            = ex15.c

TESTS                  += ex16f90
check_PROGRAMS         += ex16f90
ex16f90_SOURCES         = ex16f90.F90

TESTS                  += ex29
check_PROGRAMS         += ex29
ex29_SOURCES            = ex29.c

TESTS                  += test_module
TESTS                  += rm_execution

