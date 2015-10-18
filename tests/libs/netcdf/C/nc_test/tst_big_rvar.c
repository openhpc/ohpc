/*
  Copyright 2008, UCAR/Unidata
  See COPYRIGHT file for copying and redistribution conditions.

  This program tests a large file bug on 32-bit platforms in versions
  previous to netCDF-4.1.2, writing to a record variable with more
  than 2 dimensions and more than 2**32 values per record, where the
  write starts after the first 2**32 values in a record for that
  variable.

  $Id: tst_big_rvar.c,v 1.2 2010/05/21 15:20:26 russ Exp $
*/

#include <nc_tests.h>
#include <netcdf.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define FILE_NAME "tst_big_rvar.nc"

/* Test with both classic and 64-bit offset files. If netcdf-4 is
 * included, test with both netCDF-4 format variants also. */
/* #ifdef USE_NETCDF4 */
/* #define NUM_FORMATS (4) */
/* #else */
/* #define NUM_FORMATS (2) */
/* #endif */

#define NUM_FORMATS (2)

#define NUMDIMS 4		/* rank of variable in tests */
#define DIMR NC_UNLIMITED
#define DIM1 2149		/* just big enough to demonstrate bug */
#define DIM2 1000
#define DIM3 2000		/* DIM1*DIM2*DIM3 > 2**32 */


static int
test_big_var(const char *testfile) 
{
    int ncid, varid, dimids[NUMDIMS];
    size_t index[NUMDIMS];
    int nval = 99;
    int nval_in;
    size_t start[NUMDIMS] = {0, 0, 0, 0};
    size_t count[NUMDIMS] = {1, 1, DIM2, DIM3};
    signed char data[DIM2][DIM3];
    int i, j;
    int nerrs = 0;
    
    /* Create a file with one big record variable. */
    if (nc_create(testfile, NC_CLOBBER, &ncid)) ERR;
    if (nc_set_fill(ncid, NC_NOFILL, NULL)) ERR;
    if (nc_def_dim(ncid, "rec", DIMR, &dimids[0])) ERR;
    if (nc_def_dim(ncid, "dim1", DIM1, &dimids[1])) ERR;
    if (nc_def_dim(ncid, "dim2", DIM2, &dimids[2])) ERR;
    /* if (nc_def_dim(ncid, "dim3", DIM3 - 1, &dimids[1])) ERR; */
    if (nc_def_dim(ncid, "dim3", DIM3, &dimids[3])) ERR;
    if (nc_def_var(ncid, "var", NC_BYTE, NUMDIMS, dimids, &varid)) ERR;
    if (nc_enddef(ncid)) ERR;

    /* Initialize slab of data. */
    for (i = 0; i < DIM2; i++)
	for (j = 0; j < DIM3; j++) {
	    data[i][j] = (i + j) % 16;
	}
    /* Just write the first and last slabs */
    start[1] = 0;
    if (nc_put_vara_schar(ncid, varid, start, count, &data[0][0])) ERR;
    start[1] = DIM1 - 1;
    if (nc_put_vara_schar(ncid, varid, start, count, &data[0][0])) ERR;
    if (nc_close(ncid)) ERR;

    /* Open the file and check it. */
    if (nc_open(testfile, NC_NOWRITE, &ncid)) ERR;
    if (nc_inq_varid(ncid, "var", &varid)) ERR;
    /* Read and check data in the first and last slabs */
    start[1] = 0;
    if (nc_get_vara_schar(ncid, varid, start, count, &data[0][0])) ERR;
    for (i = 0; i < DIM2; i++)
	for (j = 0; j < DIM3; j++)
	{
	    if (data[i][j] != (signed char)((i + j) % 16)) 
	    {
		printf("error on start[0]: %d i: %d j: %d expected %d got %d\n", 
		       start[0], i, j, (i + j) % 16, data[i][j]);
		ERR;
		if(nerrs++ > 2)
		  return nerrs;
	    }
	}
    start[1] = DIM1 - 1;
    if (nc_get_vara_schar(ncid, varid, start, count, &data[0][0])) ERR;
    for (i = 0; i < DIM2; i++)
	for (j = 0; j < DIM3; j++)
	{
	    if (data[i][j] != (signed char)((i + j) % 16)) 
	    {
		printf("error on start[0]: %d i: %d j: %d expected %d got %d\n", 
		       start[0], i, j, (i + j) % 16, data[i][j]);
		ERR;
		if(nerrs++ > 2)
		  return nerrs;
	    }
	}
    if (nc_close(ncid)) ERR;
    return 0;
}

int
main(int argc, char **argv) {
    int i;
    char testfile[NC_MAX_NAME + 1];

    printf("\n*** Testing files with multidimensional variable with more than 2**32 values\n");
    sprintf(testfile, "%s/%s", TEMP_LARGE, FILE_NAME);
    for (i = NC_FORMAT_CLASSIC; i <= NUM_FORMATS; i++)
    {
       printf("*** testing format %d file with record variable with > 2**32 values...", i);
       nc_set_default_format(i, NULL);
       test_big_var(testfile);
       (void) remove(testfile);
       SUMMARIZE_ERR;
   }
    
    FINAL_RESULTS;
}
