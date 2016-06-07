TESTS_ENVIRONMENT          = BATS_NO_SUMMARY=1
AM_CXXFLAGS                = -I$(NETCDF_INC) -I. -I../include -g -O2
AM_LDFLAGS                 = -L$(NETCDF_LIB) -lm  -lnetcdf

# Test compilations/execution

TESTS                      = nctest
check_PROGRAMS             = nctest
nctest_SOURCES             = add.c atttests.c cdftests.c dimtests.c driver.c emalloc.c error.c misctest.c rec.c slabs.c val.c vardef.c varget.c vargetg.c varput.c varputg.c vartests.c vputget.c vputgetg.c 
CLEANFILES          	   = nctest *.nc

# Test execution through resource manager

#TESTS                     += rm_execution

#icc -g -O2 -I. -I../include -I$NETCDF_INC -L$NETCDF_LIB  -o nctest.exe add.c atttests.c cdftests.c dimtests.c driver.c emalloc.c error.c misctest.c rec.c slabs.c val.c vardef.c varget.c vargetg.c varput.c varputg.c vartests.c vputget.c vputgetg.c  -lm -lnetcdf

