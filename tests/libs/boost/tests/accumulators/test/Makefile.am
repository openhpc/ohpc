TESTS_ENVIRONMENT          = BATS_NO_SUMMARY=1
AM_CXXFLAGS                = -I$(BOOST_INC)
AM_LDFLAGS                 = -L$(BOOST_LIB)  -lboost_unit_test_framework

# Test compilations/execution

TESTS                      = min
check_PROGRAMS             = min
min_SOURCES                = min.cpp

TESTS                     += max
check_PROGRAMS            += max
max_SOURCES                = max.cpp

TESTS                     += skewness
check_PROGRAMS            += skewness
skewness_SOURCES           = skewness.cpp

TESTS                     += variance
check_PROGRAMS            += variance
variance_SOURCES           = variance.cpp

# Test execution through resource manager

TESTS                     += rm_execution
