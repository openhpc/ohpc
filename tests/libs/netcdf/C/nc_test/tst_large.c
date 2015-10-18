/*
  Copyright 2008, UCAR/Unidata
  See COPYRIGHT file for copying and redistribution conditions.

  This program tests the large file bug in netCDF 3.6.2,
  creating byte and short variables larger than 4 GiB.

  $Id: tst_large.c,v 1.16 2010/05/18 20:20:01 russ Exp $
*/
#include <config.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <nc_tests.h>
#include <netcdf.h>

/* Test with both classic and 64-bit offset files. If netcdf-4 is
 * included, test with both netCDF-4 format variants also. */
#ifdef USE_NETCDF4
#define NUM_FORMATS (4)
#else
#define NUM_FORMATS (2)
#endif

#define NUMDIMS 2		/* rank of each variable in tests */
#define DIM1 2048
#define DIM2 2097153		/* DIM1*DIM2*sizeof(char)   > 2**32 */
#define DIM3 1024
#define DIM4 2097153		/* DIM3*DIM4*sizeof(short)  > 2**32  */

/* 
 * In netCDF-3.6.2, a divide by zero occurs on 32-bit platforms when
 * creating a variable for which the product of dimensions is exactly
 * 2**32.  Check that this bug has been fixed. 
 */
static int
test_big_var(const char *testfile) {
    int ncid, varid, dimids[NUMDIMS];
    int cflag = NC_CLOBBER;
    nc_type type = NC_BYTE;
    size_t index[NUMDIMS];
    signed char nval = 99;
    int nval_in;

    /* Define the file with one large variable. */
    if (nc_create(testfile, cflag, &ncid)) ERR;
    if (nc_set_fill(ncid, NC_NOFILL, NULL)) ERR;
    if (nc_def_dim(ncid, "dim1", DIM1, &dimids[0])) ERR;
    if (nc_def_dim(ncid, "dim2", DIM2 - 1, &dimids[1])) ERR;
    if (nc_def_var(ncid, "var", type, NUMDIMS, dimids, &varid)) ERR;
    if (nc_enddef(ncid)) ERR;

    /* Write one datum, near the end of the variable. */
    index[0] = DIM1 - 1;
    index[1] = DIM2 - 2;
    if (nc_put_var1_schar(ncid, varid, index, &nval)) ERR;
    if (nc_close(ncid)) ERR;

    /* Reopen the file and check that datum. */
    if (nc_open(testfile, NC_NOWRITE, &ncid)) ERR;
    if (nc_inq_varid(ncid, "var", &varid)) ERR;
    if (nc_get_var1_int(ncid, varid, index, &nval_in)) ERR;
    if (nval != nval_in)
	ERR;
    if (nc_close(ncid)) ERR;
    return 0;
}

static int
test_large_byte_var(const char *testfile) {
    int ncid, varid, dimids[NUMDIMS];
    size_t index[NUMDIMS] = {0, 0};
    signed char vals[DIM2];
    signed char char_val_in;
    size_t start[NUMDIMS], count[NUMDIMS];
    int j;

    if (nc_create(testfile, NC_CLOBBER, &ncid)) ERR;
    if (nc_set_fill(ncid, NC_NOFILL, NULL)) ERR;
    if (nc_def_dim(ncid, "dim1", DIM1, &dimids[0])) ERR;
    if (nc_def_dim(ncid, "dim2", DIM2, &dimids[1])) ERR;
    if (nc_def_var(ncid, "var", NC_BYTE, NUMDIMS, dimids, &varid)) ERR;
    if (nc_enddef(ncid)) ERR;

    for (j = 0; j < DIM2; j++) {
       vals[j] = 9 * (j + 11); /* note vals[j] is 99 when j==0 */
    }
    start[1] = 0;
    count[0] = 1;
    count[1] = DIM2;
    for (start[0] = 0; start[0] < DIM1; start[0]++) {
	if (nc_put_vara_schar(ncid, varid, start, count, vals)) 
	{
	    ERR;
	    break;
	}
    }

    if (nc_close(ncid)) ERR;
    if (nc_open(testfile, NC_NOWRITE, &ncid)) ERR;
    if (nc_inq_varid(ncid, "var", &varid)) ERR;
    if (nc_get_var1_schar(ncid, varid, index, &char_val_in)) ERR;
    if (char_val_in != 99)	/* see above, the value written when start[0]==0, j==0 */
	ERR;
    if (nc_close(ncid)) ERR;
    return 0;
}

static int
test_large_short_var(const char *testfile) {
    int ncid, varid, dimids[NUMDIMS];
    int int_val_in, int_val_out = 99;
    size_t index[2];
    int cflag = NC_CLOBBER;

    if (nc_create(testfile, cflag, &ncid)) ERR;
    if (nc_def_dim(ncid, "dim3", DIM3, &dimids[0])) ERR;
    if (nc_def_dim(ncid, "dim4", DIM4, &dimids[1])) ERR;
    if (nc_def_var(ncid, "var", NC_SHORT, NUMDIMS, dimids, &varid)) ERR;
    if (nc_enddef(ncid)) ERR;
    index[0] = 0;
    index[1] = 1;
    if (nc_put_var1_int(ncid, varid, index, &int_val_out)) ERR;
    if (nc_close(ncid)) ERR;
    if (nc_open(testfile, NC_NOWRITE, &ncid)) ERR;
    if (nc_inq_varid(ncid, "var", &varid)) ERR;
    if (nc_get_var1_int(ncid, varid, index, &int_val_in)) ERR;
    if (int_val_in != int_val_out)
	ERR;
#ifndef NOFILL
    index[0] = 1;
    index[1] = 2;
    if (nc_get_var1_int(ncid, varid, index, &int_val_in)) ERR;
    if (int_val_in != NC_FILL_SHORT)
	ERR;
#endif
    if (nc_close(ncid)) ERR;
    return 0;
}

#define FILE_NAME "tst_large.nc"

int
main(int argc, char **argv) {
    int i;
    char testfile[NC_MAX_NAME + 1];

    sprintf(testfile, "%s/%s", TEMP_LARGE, FILE_NAME);

    printf("\n*** Testing fix for 3.6.2 large file bug in %s.\n",
	   testfile);
   /* Go thru formats and run all tests for each of two (for netCDF-3
    * only builds), or 4 (for netCDF-4 builds) different formats. */
    for (i = NC_FORMAT_CLASSIC; i <= NUM_FORMATS; i++) 
    {
       nc_set_default_format(i, NULL);
       
       printf("*** testing format %d with a variable with 2**32 values...", i);
       test_big_var(testfile);
       (void) remove(testfile);
       SUMMARIZE_ERR;
       
       printf("*** testing format %d with a byte variable with > 2**32 values...", i);
       test_large_byte_var(testfile);
       (void) remove(testfile);
       SUMMARIZE_ERR;

       printf("*** testing format %d with a short variable with > 2**32 values...", i);
       test_large_short_var(testfile);
       (void) remove(testfile);
       SUMMARIZE_ERR;
    }

    FINAL_RESULTS;
}
