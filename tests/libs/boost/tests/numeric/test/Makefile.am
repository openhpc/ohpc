TESTS_ENVIRONMENT          = BATS_NO_SUMMARY=1
AM_CXXFLAGS                = -I$(BOOST_ROOT) -I$(BOOST_INC) -I$(BOOST_INC)/boost/intel-mkl 

# Test compilations/execution


TESTS                      = bench1_ublas
check_PROGRAMS             = bench1_ublas
bench1_ublas_SOURCES       = bench1.cpp bench11.cpp bench12.cpp bench13.cpp

if GCC
bench1_ublas_CPPFLAGS      = -fopenmp -DNDEBUG 
bench1_ublas_LDFLAGS       = -lgomp -lpthread -lm
endif

if ICC
bench1_ublas_CPPFLAGS      = -qopenmp -DMKL_ILP64 -DMKL -DNDEBUG -I${MKLROOT}/include
bench1_ublas_LDFLAGS       = -L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -liomp5 -lpthread 
endif

TESTS                      += rm_execution

