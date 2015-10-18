TESTS_ENVIRONMENT          = BATS_NO_SUMMARY=1
AM_CXXFLAGS                = -I$(BOOST_INC)
AM_LDFLAGS                 = -L$(BOOST_LIB)  -lboost_regex -lboost_locale

TESTS                      = regress
check_PROGRAMS             = regress
regress_SOURCES            = basic_tests.cpp test_locale.cpp test_mfc.cpp \
			     test_grep.cpp test_deprecated.cpp test_icu.cpp main.cpp 

TESTS                      += rm_execution
