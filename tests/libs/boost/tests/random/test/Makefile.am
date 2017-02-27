TESTS_ENVIRONMENT          = BATS_NO_SUMMARY=1
AM_CXXFLAGS                = -I$(BOOST_INC) -L$(BOOST_LIB) -lboost_random -lboost_unit_test_framework

# Test compilations/execution

TESTS                                = test_piecewise_linear
check_PROGRAMS                       = test_piecewise_linear
test_piecewise_linear_SOURCES        = test_piecewise_linear.cpp

TESTS                               += test_discrete
check_PROGRAMS                      += test_discrete
test_discrete_SOURCES               = test_discrete.cpp

TESTS                               += test_random_device
check_PROGRAMS                      += test_random_device
test_random_device_SOURCES           = test_random_device.cpp

TESTS                               += test_random_number_generator
check_PROGRAMS                      += test_random_number_generator
test_random_number_generator_SOURCES = test_random_number_generator.cpp

# Test execution through resource manager

TESTS                     += rm_execution
