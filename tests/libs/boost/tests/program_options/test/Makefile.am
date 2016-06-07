TESTS_ENVIRONMENT          = BATS_NO_SUMMARY=1
AM_CXXFLAGS                = -I$(BOOST_INC)
AM_LDFLAGS                 = -L$(BOOST_LIB) -lboost_program_options

# Test compilations/execution

TESTS                            = cmdline_test
check_PROGRAMS                   = cmdline_test
cmdline_test_SOURCES             = cmdline_test.cpp

TESTS                           += exception_test
check_PROGRAMS                  += exception_test
exception_test_SOURCES           = exception_test.cpp

TESTS                           += options_description_test
check_PROGRAMS                  += options_description_test
options_description_test_SOURCES = options_description_test.cpp

check_PROGRAMS                  += parsers_test
parsers_test_SOURCES             = parsers_test.cpp

TESTS                           += positional_options_test
check_PROGRAMS                  += positional_options_test
positional_options_test_SOURCES  = positional_options_test.cpp

check_PROGRAMS                  += required_test
required_test_SOURCES            = required_test.cpp

TESTS                           += unicode_test
check_PROGRAMS                  += unicode_test
unicode_test_SOURCES             = unicode_test.cpp

TESTS                           += unrecognized_test
check_PROGRAMS                  += unrecognized_test
unrecognized_test_SOURCES        = unrecognized_test.cpp

TESTS                           += variable_map_test
check_PROGRAMS                  += variable_map_test
variable_map_test_SOURCES        = variable_map_test.cpp

# Test execution through resource manager

TESTS                           += rm_execution
