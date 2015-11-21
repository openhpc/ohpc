TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1
CXX=icc

check_PROGRAMS          = CXX_tbb_test
CXX_tbb_test_CXXFLAGS    = -I${TBB_INC} $(CXXFLAGS)
CXX_tbb_test_LDFLAGS    = -ltbb -ltbbmalloc -lrt -L${TBB_LIB} $(LDFLAGS)
CXX_tbb_test_SOURCES    = main.cpp OptimizedParallelSumTree.cpp SerialSumTree.cpp SimpleParallelSumTree.cpp
TESTS                   = run_CXX_tbb_test.sh
