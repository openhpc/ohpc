TESTS_ENVIRONMENT        = BATS_NO_SUMMARY=1
AM_LDFLAGS               = -lmfem

TESTS                    = p_laplace
check_PROGRAMS           = p_laplace
p_laplace_SOURCES        = ex1p.cpp

TESTS                   += p_laplace_perf
check_PROGRAMS          += p_laplace_perf
p_laplace_perf_SOURCES   = ex1p_perf.cpp

TESTS                   += p_cantilever
check_PROGRAMS          += p_cantilever
p_cantilever_SOURCES     = ex2p.cpp

TESTS                   += p_diffusion
check_PROGRAMS          += p_diffusion
p_diffusion_SOURCES      = ex3p.cpp


TESTS                   += rm_execution
