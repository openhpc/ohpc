/*
  Copyright 2008, UCAR/Unidata
  See COPYRIGHT file for copying and redistribution conditions.

  This program tests the large file bug in netCDF 3.6.2,
  creating byte and short variables larger than 4 GiB.

  $Id: tst_big_var.c,v 1.9 2010/05/15 00:50:10 russ Exp $
*/

#include <nc_tests.h>
#include <netcdf.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define FILE_NAME "tst_big_var.nc"

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

/* 
 * In netCDF-3.6.2, an assertion failure occurs on 32-bit platforms
 * when creating a byte variable for which the product of dimensions
 * is greater than 2**32.  Check that this bug has been fixed.
 */
static int
test_big_var(const char *testfile) 
{
    int ncid, varid, dimids[NUMDIMS];
    size_t index[NUMDIMS];
    int nval = 99;
    int nval_in;

    /* Create a file with one big variable. */
    if (nc_create(testfile, NC_CLOBBER, &ncid)) ERR;
    if (nc_set_fill(ncid, NC_NOFILL, NULL)) ERR;
    if (nc_def_dim(ncid, "dim1", DIM1, &dimids[0])) ERR;
    if (nc_def_dim(ncid, "dim2", DIM2 - 1, &dimids[1])) ERR;
    if (nc_def_var(ncid, "var", NC_BYTE, NUMDIMS, dimids, &varid)) ERR;
    if (nc_enddef(ncid)) ERR;
    index[0] = DIM1 - 1;
    index[1] = DIM2 - 2;
    if (nc_put_var1_int(ncid, varid, index, &nval)) ERR;
    if (nc_close(ncid)) ERR;

    /* Open the file and check it. */
    if (nc_open(testfile, NC_NOWRITE, &ncid)) ERR;
    if (nc_inq_varid(ncid, "var", &varid)) ERR;
    if (nc_get_var1_int(ncid, varid, index, &nval_in)) ERR;
    if (nval != nval_in) ERR;
    if (nc_close(ncid)) ERR;
    return 0;
}

int
main(int argc, char **argv) {
    int i;
    char testfile[NC_MAX_NAME + 1];

    printf("\n*** Testing files with one very big variable.\n");
    sprintf(testfile, "%s/%s", TEMP_LARGE, FILE_NAME);
    for (i = NC_FORMAT_CLASSIC; i <= NUM_FORMATS; i++)
    {
       printf("*** testing format %d file with byte variable with > 2**32 values...", i);
       nc_set_default_format(i, NULL);
       test_big_var(testfile);
       (void) remove(testfile);
       SUMMARIZE_ERR;
   }
    
    FINAL_RESULTS;
}
