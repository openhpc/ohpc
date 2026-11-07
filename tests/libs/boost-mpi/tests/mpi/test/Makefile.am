# The tests below run MPI programs on the head node, where MPICH can pick a
# container bridge interface (podman0, ...) as its default NIC and fail. The
# loopback interface exists on every host and is enough for a single process.
# The provider is pinned too, because MPICH can select the sockets or
# udp;ofi_rxd provider, and FI_TCP_IFACE means nothing to them.
# run_mpi_binary removes exactly this pair before it submits a job, so the
# rm_execution jobs on the compute nodes keep their native provider.
AM_TESTS_ENVIRONMENT = FI_PROVIDER=tcp; export FI_PROVIDER; FI_TCP_IFACE=lo; export FI_TCP_IFACE;

AM_CXXFLAGS                = -I$(BOOST_INC)
AM_LDFLAGS                 = -L$(BOOST_LIB)

LDADD                      = -lboost_mpi -lboost_serialization

# Test compilations/execution

TESTS                     = all_gather_test
check_PROGRAMS            = all_gather_test
all_gather_test_SOURCES   = all_gather_test.cpp

TESTS                    += all_reduce_test
check_PROGRAMS           += all_reduce_test
all_reduce_test_SOURCES   = all_reduce_test.cpp

TESTS                    += all_to_all_test
check_PROGRAMS           += all_to_all_test
all_to_all_test_SOURCES   = all_to_all_test.cpp

TESTS                    += groups_test
check_PROGRAMS           += groups_test
groups_test_SOURCES       = groups_test.cpp

check_PROGRAMS           += broadcast_test
broadcast_test_SOURCES    = broadcast_test.cpp

check_PROGRAMS           += ring_test
ring_test_SOURCES         = ring_test.cpp

check_PROGRAMS           += pointer_test
pointer_test_SOURCES      = pointer_test.cpp

# Test execution through resource manager

TESTS                    += rm_execution
