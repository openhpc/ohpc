/* This is part of the netCDF package. Copyright 2005 University
   Corporation for Atmospheric Research/Unidata See COPYRIGHT file for
   conditions of use. See www.unidata.ucar.edu for more info.

   Test small files.

   $Id: tst_small.c,v 1.15 2008/10/20 01:48:08 ed Exp $
*/

#include <nc_tests.h>
#include <netcdf.h>

/* Derived from tst_small.c */

/* Test everything for classic using in-memory files */

#define NCFILENAME "tst_diskless3.nc"

#define ATT_NAME "Atom"
#define MAX_LEN 7   

#define VAR_NAME2 "var2"
#define NUM_VARS 2

#define ONE_DIM 1
#define MAX_RECS 10

#define DIM1_NAME "Time"
#define DIM2_NAME "DataStrLen"
#define VAR_NAME "Times"
#define STR_LEN 19
#define NUM_VALS 2
#define NDIMS 2
#define TITLE " OUTPUT FROM WRF V2.0.3.1 MODEL"
#define ATT_NAME2 "TITLE"

#undef ERR
#define ERR report(status,__LINE__)

static int status = NC_NOERR;

/* Control flags  */
static int persist, usenetcdf4, mmap, diskless;

static int diskmode;

void report(int stat, int lno)
{
    fprintf(stderr,"line: %d ; %s\n",stat,nc_strerror(stat));
    fflush(stderr);
    exit(1);
}

/* Test a diskless file with two record vars, which grow, and has
 * attributes added. */
static int
test_two_growing_with_att(const char *testfile)
{
   int ncid, dimid, varid[NUM_VARS];
   char data[MAX_RECS], data_in;
   char att_name[NC_MAX_NAME + 1];
   size_t start[ONE_DIM], count[ONE_DIM], index[ONE_DIM], len_in;
   int v, r;

   /* Create a file with one ulimited dimensions, and one var. */
   if((status=nc_create(testfile, NC_CLOBBER, &ncid))) ERR;
   if((status=nc_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, &dimid))) ERR;
   if((status=nc_def_var(ncid, VAR_NAME, NC_CHAR, 1, &dimid, &varid[0]))) ERR;
   if((status=nc_def_var(ncid, VAR_NAME2, NC_CHAR, 1, &dimid, &varid[1]))) ERR;
   if((status=nc_close(ncid))) ERR;

   /* Create some phoney data. */
   for (data[0] = 'a', r = 1; r < MAX_RECS; r++)
      data[r] = data[r - 1] + 1;

   /* Normally one would not close and reopen the file for each
    * record, nor add an attribute each time I add a record, but I am
    * giving the library a little work-out here... */
   for (r = 0; r < MAX_RECS; r++)
   {
      /* Write one record of var data, a single character. */
      if((status=nc_open(testfile, NC_WRITE, &ncid))) ERR;
      count[0] = 1;
      start[0] = r;
      sprintf(att_name, "a_%d", data[r]);
      for (v = 0; v < NUM_VARS; v++)
      {
	 if((status=nc_put_vara_text(ncid, varid[v], start, count, &data[r]))) ERR;
	 if((status=nc_redef(ncid))) ERR;
	 if((status=nc_put_att_text(ncid, varid[v], att_name, 1, &data[r]))) ERR;
	 if((status=nc_enddef(ncid))) ERR;
      }
      if((status=nc_close(ncid))) ERR;
      
      /* Reopen the file and check it. */
      if((status=nc_open(testfile, diskmode|NC_WRITE, &ncid))) ERR;
      if((status=nc_inq_dimlen(ncid, 0, &len_in))) ERR;
      if (len_in != r + 1) ERR;
      index[0] = r;
      for (v = 0; v < NUM_VARS; v++)
      {
	 if((status=nc_get_var1_text(ncid, varid[v], index, &data_in))) ERR;
	 if (data_in != data[r]) ERR;
      }
      if((status=nc_close(ncid))) ERR; 
   } /* Next record. */
   return 0;
}

#if 0
/* Test a diskless file with one var and one att. */
static int
test_one_with_att(const char *testfile)
{
   int ncid, dimid, varid;
   char data = 'h', data_in;
   int ndims, nvars, natts, unlimdimid;
   size_t start[NDIMS], count[NDIMS];

   /* Create a file with one ulimited dimensions, and one var. */
   if((status=nc_create(testfile, NC_CLOBBER, &ncid))) ERR;
   if((status=nc_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, &dimid))) ERR;
   if((status=nc_def_var(ncid, VAR_NAME, NC_CHAR, 1, &dimid, &varid))) ERR;
   if((status=nc_put_att_text(ncid, NC_GLOBAL, ATT_NAME, 1, &data))) ERR;
   if((status=nc_enddef(ncid))) ERR;

   /* Write one record of var data, a single character. */
   count[0] = 1;
   start[0] = 0;
   if((status=nc_put_vara_text(ncid, varid, start, count, &data))) ERR;

   /* We're done! */
   if((status=nc_close(ncid))) ERR;
   
   /* Reopen the file and check it. */
   if((status=nc_open(testfile, diskmode|NC_WRITE, &ncid))) ERR;
   if((status=nc_inq(ncid, &ndims, &nvars, &natts, &unlimdimid))) ERR;
   if (ndims != 1 && nvars != 1 && natts != 0 && unlimdimid != 0) ERR;
   if((status=nc_get_var_text(ncid, varid, &data_in))) ERR;
   if (data_in != data) ERR;
   if((status=nc_get_att_text(ncid, NC_GLOBAL, ATT_NAME, &data_in))) ERR;
   if (data_in != data) ERR;
   if((status=nc_close(ncid))) ERR; 
   return 0;
}
#endif

int
main(int argc, char **argv)
{
    int i;

    /* Set defaults */
    persist = 0;
    usenetcdf4 = 0;
    mmap = 0;
    diskless = 0;
    diskmode = 0;

    for(i=1;i<argc;i++) {
	if(strcmp(argv[i],"diskless")==0) diskless=1;
	else if(strcmp(argv[i],"mmap")==0) mmap=1;
	/* ignore anything not recognized */
    }

    if(diskless)
        diskmode |= NC_DISKLESS;
    if(diskless && mmap)
        diskmode |= NC_MMAP;

    printf("\n*** Testing diskless file: create/modify %s",
	    diskless?"in-memory":"in-file");
    if(diskless && mmap)
        printf("+mmap");
    printf(" %s\n",NCFILENAME);

    /* case NC_FORMAT_CLASSIC: only test this format */
    nc_set_default_format(NC_FORMAT_CLASSIC, NULL);

    printf("*** testing diskless file with two growing record "
	     "variables, with attributes added...");
    test_two_growing_with_att(NCFILENAME);
    SUMMARIZE_ERR;

   FINAL_RESULTS;
}

