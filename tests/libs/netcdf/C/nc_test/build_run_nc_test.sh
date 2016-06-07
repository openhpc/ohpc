#!/bin/bash

CC=icc

echo "Building nc_test.exe with $CC ... "
$CC -g -O2 -I. -I../include -I$NETCDF_INC -L$NETCDF_LIB -o nc_test.exe nc_test.c error.c test_get.c test_put.c test_read.c test_write.c util.c  -lnetcdf
echo "Build complete "

echo "Running nc_test.exe ... "
./nc_test.exe
echo "nc_test done"

rm nc_test.exe *.nc 
