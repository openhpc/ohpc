TESTS_ENVIRONMENT          = BATS_NO_SUMMARY=1
AM_CXXFLAGS                = -I$(BOOST_INC)
AM_LDFLAGS                 = -L$(BOOST_LIB) -lboost_mpi -lboost_serialization

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
