TESTS_ENVIRONMENT          = BATS_NO_SUMMARY=1
AM_CXXFLAGS                = -I$(BOOST_ROOT) -I$(BOOST_INC) -I$(BOOST_INC)/boost/intel-mkl 

# Test compilations/execution

TESTS                      = sylvester_mkl sylvester_ublas
check_PROGRAMS             = sylvester_mkl sylvester_ublas
sylvester_mkl_SOURCES         = sylvester.cpp
sylvester_ublas_SOURCES         = sylvester.cpp

if GCC
$(info ************  GNU VERSION ************)
sylvester_mkl_CPPFLAGS        = -fopenmp -m64 -DMKL_ILP64 -DMKL -DNDEBUG -I${MKLROOT}/include
sylvester_mkl_LDFLAGS         = -Wl,--no-as-needed -L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_core -lmkl_gnu_thread -ldl -lpthread -lm
sylvester_mkl_LDADD           =/opt/ohpc/pub/compiler/gcc/4.9.2/lib64/libgomp.so
else 
$(info ************  INTEL VERSION ************)
sylvester_mkl_CPPFLAGS        = -openmp -DMKL_ILP64 -DMKL -DNDEBUG -I${MKLROOT}/include
sylvester_mkl_LDFLAGS         = -L${MKLROOT}/lib/intel64 -lmkl_intel_ilp64 -lmkl_core -lmkl_intel_thread -lpthread -lm
sylvester_mkl_LDADD           =/opt/ohpc/pub/compiler/intel/composer_xe_2015.2.164/compiler/lib/intel64/libiomp5.so
endif

if GCC
$(info ************  GNU VERSION ************)
sylvester_ublas_CPPFLAGS        = -fopenmp -m64 -DNDEBUG 
sylvester_ublas_LDADD           =/opt/ohpc/pub/compiler/gcc/4.9.2/lib64/libgomp.so
else 
$(info ************  INTEL VERSION ************)
sylvester_ublas_CPPFLAGS        = -openmp -DNDEBUG 
sylvester_ublas_LDADD           =/opt/ohpc/pub/compiler/intel/composer_xe_2015.2.164/compiler/lib/intel64/libiomp5.so
endif

# Test execution through resource manager

TESTS                     += rm_execution


##
#icc -Wl,--no-as-needed -DMKL_ILP64 -DMKL -I$BOOST_ROOT -I$BOOST_INC -I$BOOST_ROOT/include/boost/intel-mkl -I$MKLROOT/include -L$MKLROOT/lib/intel64 -I/opt/ohpc/pub/compiler/intel/composer_xe_2015.2.164/compiler/lib/intel64/ sylvester.cpp -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm -ldl -o i.out

#gcc -Wl,--no-as-needed -DMKL_ILP64 -DMKL -I$BOOST_ROOT -I$BOOST_INC -I$BOOST_ROOT/include/boost/intel-mkl -I$MKLROOT/include -L$MKLROOT/lib/intel64 -I/opt/ohpc/pub/compiler/intel/composer_xe_2015.2.164/compiler/lib/intel64/ sylvester.cpp -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lstdc++ -lpthread -lm -ldl -o g.out
