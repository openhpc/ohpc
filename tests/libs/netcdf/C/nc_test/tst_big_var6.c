/*
  Copyright 2008, UCAR/Unidata
  See COPYRIGHT file for copying and redistribution conditions.

  This program tests the fix for a large file bug in versions previous
  to netCDF-4.1.2 for 32-bit platforms, writing to a variable with
  more than 1 dimension and more than 2**32 values, where the write
  starts after the first 2**32 elements.

  $Id: tst_big_var6.c,v 1.1 2010/05/22 19:56:53 russ Exp $
*/

#include <nc_tests.h>
#include <netcdf.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define FILE_NAME "tst_big_var6.nc"

/* Test with both classic and 64-bit offset files. If netcdf-4 is
 * included, test with both netCDF-4 format variants also. */
#ifdef USE_NETCDF4
#define NUM_FORMATS (4)
#else
#define NUM_FORMATS (2)
#endif

#define NUMDIMS 4	      /* rank of variable in tests */
#define DIM0 1
#define DIM1 2
#define DIM2 5000
#define DIM3 1000000		/* DIM2*DIM3 > 2**32 */
#define FIRST_VAL   65
#define SECOND_VAL  90
/* 
 * This program tests the fix for a large file bug in versions
 * previous to netCDF-4.1.2 for 32-bit platforms, writing to a
 * variable with more than 1 dimension and more than 2**32 values,
 * where the write starts after the first 2**32 elements.  The bug
 * applies to record variables with more than 2**32 values per record
 * as well, but that's not tested here.
 */
static int
test_big_var(const char *testfile) 
{
    int ncid, varid, dimids[NUMDIMS];
    size_t start[NUMDIMS] = {0, 0, 0, 0};
    size_t count[NUMDIMS] = {1, 1, 1, DIM3};
    short data[DIM3];
    int i, j, k;
    int nerrs = 0;
    
    /* Create a file with one big 4D variable. */
    if (nc_create(testfile, NC_CLOBBER, &ncid)) ERR;
    if (nc_set_fill(ncid, NC_NOFILL, NULL)) ERR;
    if (nc_def_dim(ncid, "dim0", DIM0, &dimids[0])) ERR;
    if (nc_def_dim(ncid, "dim1", DIM1, &dimids[1])) ERR;
    if (nc_def_dim(ncid, "dim2", DIM2, &dimids[2])) ERR;
    if (nc_def_dim(ncid, "dim3", DIM3, &dimids[3])) ERR;
    if (nc_def_var(ncid, "var", NC_SHORT, NUMDIMS, dimids, &varid)) ERR;
    if (nc_enddef(ncid)) ERR;

    /* write var(0,0,4294,*) as all FIRST_VAL */
    start[0] = 0;
    start[1] = 0;
    start[2] = 4294;
    for (j = 0; j < DIM3; j++)
	data[j] = FIRST_VAL;
    if (nc_put_vara_short(ncid, varid, start, count, &data[0])) ERR;

    /* write var(0,1,0,*) as all 8588 */
    start[0] = 0;
    start[1] = 1;
    start[2] = 0;
    for (j = 0; j < DIM3; j++)
	data[j] = SECOND_VAL;
    if (nc_put_vara_short(ncid, varid, start, count, &data[0])) ERR;

    /* Read and check var(0,0,4294,*) */
    start[0] = 0;
    start[1] = 0;
    start[2] = 4294;
    if (nc_get_vara_short(ncid, varid, start, count, &data[0])) ERR;
    for (j = 0; j < DIM3; j++) {
	if (data[j] != FIRST_VAL ) {
	    printf("error on start[0..2]: %d,%d,%d  j: %d, expected %d got %d\n", 
		   start[0], start[1], start[2], j, FIRST_VAL, data[j]);
	    ERR;
	    if(nerrs++ > 1)
	      return nerrs;
	}
    }
    if (nc_close(ncid)) ERR;
    return NC_NOERR;
}

int
main(int argc, char **argv) {
    int i;
    char testfile[NC_MAX_NAME + 1];

    printf("\n*** Testing multidimensional variable with more than 2**32 values\n");
    sprintf(testfile, "%s/%s", TEMP_LARGE, FILE_NAME);
    for (i = NC_FORMAT_CLASSIC; i <= NUM_FORMATS; i++)
    {
       printf("*** testing format %d file with short variable with > 2**32 values...", i);
       nc_set_default_format(i, NULL);
       if (test_big_var(testfile)) ERR_RET;
       (void) remove(testfile);
       SUMMARIZE_ERR;
   }
    
    FINAL_RESULTS;
}
