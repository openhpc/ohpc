
# Autoconf's PACKAGE_STRING define embeds a literal space ("tau 0.10.0"),
# and TAU's tau_cc.sh/tau_compiler.sh wrappers apply the same
# space-escaping twice on their way to a final eval, which corrupts the
# escape and splits the define into two words, breaking the compiler
# invocation. None of these test sources reference the autoconf
# boilerplate defines, so drop them from the compile line entirely.
DEFS =

check_PROGRAMS          = CXX_mpi_test
CXX_mpi_test_LDFLAGS    = $(LDFLAGS)
CXX_mpi_test_SOURCES    = CXX_mpi_test.cpp

check_PROGRAMS         += F_test
F_test_LDFLAGS          = $(LDFLAGS)
F_test_SOURCES          = F_test.f

check_PROGRAMS         += C_mpi_test
C_mpi_test_LDFLAGS      = $(LDFLAGS)
C_mpi_test_SOURCES      = C_mpi_test.c

check_PROGRAMS         += C_hybrid_test
C_hybrid_test_LDFLAGS   = $(LDFLAGS)
C_hybrid_test_SOURCES   = C_hybrid.c

TESTS                   = test_module
TESTS                  += rm_execution
