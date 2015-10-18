#!/bin/bash

CC=icc

echo "Building nctest.exe with $CC ... "
$CC -g -O2 -I. -I../include -I$NETCDF_INC -L$NETCDF_LIB  -o nctest.exe add.c atttests.c cdftests.c dimtests.c driver.c emalloc.c error.c misctest.c rec.c slabs.c val.c vardef.c varget.c vargetg.c varput.c varputg.c vartests.c vputget.c vputgetg.c  -lm -lnetcdf
echo "Build complete "

echo "Running nctest.exe ... "
./nctest.exe
echo "nctest done"

rm nctest.exe *.nc 
