/*
  Copyright 2011, UCAR/Unidata
  See COPYRIGHT file for copying and redistribution conditions.

  This is part of netCDF.
   
  This program tests for a bug discovered with nofill mode that failed
  only on file systems with block size in a particular range.  It fails
  when invoked with the blksize argument between 2091953 and 2150032,
  inclusive, and succeeds for other blksizes.
*/

#include <config.h>
#include <nc_tests.h>
#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <errno.h>
#include <netcdf.h>

#define FILE_NAME "tst_nofill2.nc"
#define LON_LEN 240
#define LAT_LEN 121
#define LVL_LEN 31
#define TIME_LEN 1
#define NDIMS1 1
#define NDIMS4 4

int
create_file(char *file_name, int fill_mode, size_t* sizehintp) 
{
   int ncid, time_id, zonal_wnd_id;
   int dimids[NDIMS4];
   size_t start[NDIMS4] = {0, 0, 0, 0};
   size_t count[NDIMS4] = {TIME_LEN, 1, LAT_LEN, LON_LEN};
   float zonal_wnd[LON_LEN * LAT_LEN * TIME_LEN];
   size_t default_initialsize = 0;
   double time[TIME_LEN] = {1.};
   int i;

   /* Init data. */
   for(i = 0; i < TIME_LEN * LAT_LEN * LON_LEN; i++) 
      zonal_wnd[i] = 100 + i;

   /* To test bug on filesystem without large block size, we can get
    * the same effect by providing the desired value as sizehint to
    * nc__create() instead of calling nc_create() and getting the
    * block size reported by fstat */
   if (nc__create(file_name, NC_CLOBBER, default_initialsize, sizehintp, &ncid)) ERR;
   if (nc_set_fill(ncid, fill_mode, NULL)) ERR;

   /* define dimensions */
   if (nc_def_dim(ncid, "lon", LON_LEN, &dimids[3])) ERR;
   if (nc_def_dim(ncid, "lat", LAT_LEN, &dimids[2])) ERR;
   if (nc_def_dim(ncid, "lvl", LVL_LEN, &dimids[1])) ERR;
   if (nc_def_dim(ncid, "time", TIME_LEN, &dimids[0])) ERR;

   /* define variables */
   if (nc_def_var(ncid, "time", NC_DOUBLE, NDIMS1, &dimids[0], &time_id)) ERR;
   if (nc_def_var(ncid, "zonal_wnd", NC_FLOAT, NDIMS4, dimids, &zonal_wnd_id)) ERR;

   if (nc_enddef (ncid)) ERR;
   if (nc_put_var_double(ncid, time_id, time)) ERR;

   /* Bug exposed when written in reverse order. */
   for(i = LVL_LEN - 1; i>=0; i--)
   {
      start[1] = i;
      if (nc_put_vara_float(ncid, zonal_wnd_id, start, count, zonal_wnd)) ERR;
   }
   if (nc_close(ncid)) ERR;
   return 0;
}

int
main(int argc, char **argv)
{
   size_t sizehint = (1750000);	/* default if not set on command line,
				 * exposes bug.  It turns out any
				 * value between 2091953 and 2150032
				 * triggers bug, whereas all other
				 * values work fine. */

   printf("\n*** Testing nofill mode.\n");
   printf("*** Create file in nofill mode, writing all values...");
   {
#define NUM_TRIES 1
      int ncid;
      size_t idx[1] = {0};
      double data;
      int i;

      for (i = 0; i < NUM_TRIES; i++)
      {
	printf(", trying sizehint of %lu ...", (unsigned long)sizehint);
	 if (create_file(FILE_NAME, NC_NOFILL, &sizehint)) ERR;
	 if (nc_open(FILE_NAME, 0, &ncid)) ERR;
	 if (nc_get_var1(ncid, 0, idx, &data)) ERR;
	 if (!data) ERR;
	 if (nc_close(ncid)) ERR;
	 sizehint += 10000;
      }
   }
   SUMMARIZE_ERR;
   FINAL_RESULTS;
}
