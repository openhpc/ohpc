TESTS_ENVIRONMENT          = BATS_NO_SUMMARY=1
AM_CXXFLAGS                = -I$(BOOST_INC)

# Test compilations/execution

TESTS                      = access
check_PROGRAMS             = access
access_SOURCES             = access.cpp

TESTS                     += iterators
check_PROGRAMS            += iterators
iterators_SOURCES          = iterators.cpp

TESTS                     += resize
check_PROGRAMS            += resize
resize_SOURCES             = resize.cpp

TESTS                     += idxgen1
check_PROGRAMS            += idxgen1
idxgen1_SOURCES           = idxgen1.cpp

# Test execution through resource manager

TESTS                     += rm_execution
