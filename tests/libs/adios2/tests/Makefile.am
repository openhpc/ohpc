# The tests below run MPI programs on the head node, where MPICH can pick a
# container bridge interface (podman0, ...) as its default NIC and fail. The
# loopback interface exists on every host and is enough for a single process.
# The provider is pinned too, because MPICH can select the sockets or
# udp;ofi_rxd provider, and FI_TCP_IFACE means nothing to them.
# run_mpi_binary removes exactly this pair before it submits a job, so the
# rm_execution jobs on the compute nodes keep their native provider.
AM_TESTS_ENVIRONMENT = FI_PROVIDER=tcp; export FI_PROVIDER; FI_TCP_IFACE=lo; export FI_TCP_IFACE;


TESTS                   = arrays_write
check_PROGRAMS          = arrays_write
arrays_write_SOURCES    = arrays_write.cpp
arrays_write_LDADD      = -ladios2_cxx -ladios2_cxx_mpi

TESTS                  += arrays_read
check_PROGRAMS         += arrays_read
arrays_read_SOURCES     = arrays_read.cpp
arrays_read_LDADD       = -ladios2_cxx -ladios2_cxx_mpi

TESTS                  += scalars_write
check_PROGRAMS         += scalars_write
scalars_write_SOURCES  = scalars_write.F90
scalars_write_LDADD    = -ladios2_fortran -ladios2_fortran_mpi

TESTS                  += scalars_read
check_PROGRAMS         += scalars_read
scalars_read_SOURCES    = scalars_read.F90
scalars_read_LDADD      = -ladios2_fortran -ladios2_fortran_mpi

TESTS                  += test_module
TESTS                  += rm_execution

clean-local:
	-rm -rf *.bp

