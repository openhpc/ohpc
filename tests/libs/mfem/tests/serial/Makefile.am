TESTS_ENVIRONMENT         = BATS_NO_SUMMARY=1
AM_LDFLAGS                = -lmfem

TESTS                     = s_laplace
check_PROGRAMS            = s_laplace
s_laplace_SOURCES         = ex1.cpp

TESTS                    += s_laplace_perf
check_PROGRAMS           += s_laplace_perf
s_laplace_perf_SOURCES    = ex1_perf.cpp

TESTS                    += s_cantilever
check_PROGRAMS           += s_cantilever
s_cantilever_SOURCES      = ex2.cpp

TESTS                    += s_diffusion
check_PROGRAMS           += s_diffusion
s_diffusion_SOURCES       = ex3.cpp
