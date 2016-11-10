TESTS_ENVIRONMENT          = BATS_NO_SUMMARY=1
AM_CXXFLAGS                = -I$(BOOST_ROOT) -I$(BOOST_INC) -I$(BOOST_INC)/boost/intel-mkl 

# Test compilations/execution

if GCC
TESTS                      = bench1_ublas
check_PROGRAMS             = bench1_ublas
else 
TESTS                      = bench1_mkl bench1_ublas
check_PROGRAMS             = bench1_mkl bench1_ublas
endif
bench1_mkl_SOURCES        = bench1.cpp bench11.cpp bench12.cpp bench13.cpp
bench1_ublas_SOURCES      = bench1.cpp bench11.cpp bench12.cpp bench13.cpp

if GCC
$(info ************  GNU VERSION with MKL ************)
bench1_mkl_CPPFLAGS        = -fopenmp -DMKL_ILP64 -DMKL -DNDEBUG -I${MKLROOT}/include
bench1_mkl_LDFLAGS         = -Wl,--no-as-needed -L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_core -lmkl_gnu_thread -lgomp -ldl -lpthread -lm
else 
$(info ************  INTEL VERSION with MKL ************)
bench1_mkl_CPPFLAGS        = -openmp -DMKL_ILP64 -DMKL -DNDEBUG -I${MKLROOT}/include
bench1_mkl_LDFLAGS         = -L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -liomp5 -lpthread -lm
endif

if GCC
$(info ************  GNU VERSION with ublas ************)
bench1_ublas_CPPFLAGS        = -fopenmp -DNDEBUG 
bench1_ublas_LDFLAGS         = -lgomp -lpthread -lm
else 
$(info ************  INTEL VERSION with ublas ************)
bench1_ublas_CPPFLAGS        = -openmp -DNDEBUG 
bench1_ublas_LDFLAGS         = -liomp5 -lpthread -lm
endif

# Test execution through resource manager

TESTS                     += rm_execution

