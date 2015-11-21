TESTS_ENVIRONMENT          = BATS_NO_SUMMARY=1
SUBDIRS                    = regress
AM_CXXFLAGS                = -I$(BOOST_INC)
AM_LDFLAGS                 = -L$(BOOST_LIB)  -lboost_regex 

# Test compilations/execution

TESTS                              = named_subexpressions_test
check_PROGRAMS                     = named_subexpressions_test
named_subexpressions_test_SOURCES  = named_subexpressions_test.cpp

TESTS                             += bad_expression_test
check_PROGRAMS                    += bad_expression_test
bad_expression_test_SOURCES        = bad_expression_test.cpp

TESTS                             += recursion_test
check_PROGRAMS                    += recursion_test
recursion_test_SOURCES             = recursion_test.cpp

# Test execution through resource manager

TESTS                             += rm_execution
