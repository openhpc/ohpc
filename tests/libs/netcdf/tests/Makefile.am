TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

check_PROGRAMS    = C_test
C_test_SOURCES    = nc_test.c error.c test_get.c test_put.c test_read.c test_write.c util.c error.h tests.h
C_test_LDFLAGS    = $(C_LDFLAGS)

check_PROGRAMS   += C_write
C_write_SOURCES   = C_simple_wr.c
C_write_LDFLAGS   = $(C_LDFLAGS)

check_PROGRAMS   += C_read
C_read_SOURCES   = C_simple_rd.c
C_read_LDFLAGS   = $(C_LDFLAGS)

check_PROGRAMS   += F90_write
F90_write_SOURCES = simple_xy_nc4_wr.f90
F90_write_LDFLAGS = $(FC_LDFLAGS)

check_PROGRAMS   += F90_read
F90_read_SOURCES  = simple_xy_nc4_rd.f90
F90_read_LDFLAGS  = $(FC_LDFLAGS)

check_PROGRAMS   += CXX_write
CXX_write_SOURCES = simple_xy_wr.cpp
CXX_write_LDFLAGS = $(CXX_LDFLAGS)

check_PROGRAMS   += CXX_read
CXX_read_SOURCES  = simple_xy_rd.cpp
CXX_read_LDFLAGS  = $(CXX_LDFLAGS)

TESTS             = test_C_module
TESTS            += test_CXX_module
TESTS            += test_Fortran_module
TESTS            += C_test
TESTS            += test_netcdf
