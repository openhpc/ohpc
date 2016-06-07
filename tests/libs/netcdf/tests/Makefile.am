TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

check_PROGRAMS    = nctest
nctest_SOURCES    = add.c add.h atttests.c cdftests.c dimtests.c driver.c \
		    emalloc.c emalloc.h error.c error.h misctest.c rec.c slabs.c testcdf.h \
		    tests.h val.c val.h vardef.c varget.c vargetg.c varput.c varputg.c \
		    vartests.c vputget.c vputgetg.c
nctest_LDFLAGS    = $(C_LDFLAGS)

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
TESTS            += nctest
TESTS            += compare_test_files.sh
TESTS            += test_netcdf
