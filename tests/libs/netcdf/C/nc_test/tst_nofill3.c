/*
  Copyright 2011, UCAR/Unidata
  See COPYRIGHT file for copying and redistribution conditions.

  This is part of netCDF.
   
  This program tests for a bug discovered with nofill mode that failed
  only on file systems with block size in a particular range.  This version
  of the test showed failure using a normal nc_create() call rather tha
  a special nc__create() call to change the blksize.
*/

#include <config.h>
#include <nc_tests.h>
#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <errno.h>
#include <netcdf.h>

#define FILE_NAME "tst_nofill3.nc"
#define TIME_LEN 1
/* Change any of these values and the bug doesn't happen or requires a
 * larger disk block size (set with the "chunksize" parameter to
 * nc__create()) to see. Similarly, adding attributes or additional
 * variables will either make the bug go away or require a larger disk
 * block size to observe.  But not just any larger disk block size; it
 * will have to be in a relatively narrow range. */
#define LON_LEN 11
#define LAT_LEN 11
#define LVL_LEN 34

int
create_file(char *file_name, int fill_mode) 
{
   int ncid;
   int lon_dim, lat_dim, lvl_dim, time_dim;
   int time_id, zonal_wnd_id;
   int i;

   /* rank (number of dimensions) for each variable */
#  define RANK_time 1
#  define RANK_zonal_wnd 3

   /* variable shapes */
   int zonal_wnd_dims[RANK_zonal_wnd];
   size_t zonal_wnd_start[RANK_zonal_wnd];
   size_t zonal_wnd_count[RANK_zonal_wnd];
   float zonal_wnd[LON_LEN*LAT_LEN];
   int time[TIME_LEN];

   for(i = 0; i < TIME_LEN; i++) {
       time[i] = 1;
   }
   if (nc_create(file_name, NC_CLOBBER, &ncid)) ERR;
   if (nc_set_fill(ncid, fill_mode, NULL)) ERR;

   /* define dimensions */
   if (nc_def_dim(ncid, "lon", LON_LEN, &lon_dim)) ERR;
   if (nc_def_dim(ncid, "lat", LAT_LEN, &lat_dim)) ERR;
   if (nc_def_dim(ncid, "lvl", LVL_LEN, &lvl_dim)) ERR;
   if (nc_def_dim(ncid, "time", TIME_LEN, &time_dim)) ERR;

   /* define variables */
   if (nc_def_var(ncid, "time", NC_INT, RANK_time, &time_dim, &time_id)) ERR;

   zonal_wnd_dims[0] = lvl_dim;
   zonal_wnd_dims[1] = lat_dim;
   zonal_wnd_dims[2] = lon_dim;
   if (nc_def_var(ncid, "zonal_wnd", NC_FLOAT, RANK_zonal_wnd, zonal_wnd_dims, &zonal_wnd_id)) ERR;

   if (nc_enddef (ncid)) ERR;
   if (nc_put_var_int(ncid, time_id, time)) ERR;
   /* Bug exposed when last value written. */
   {
      int izw;

      i = LVL_LEN - 1;
      for(izw = 0; izw < LAT_LEN * LON_LEN; izw++) {
	 zonal_wnd[izw] = 100 + i;
      }
      zonal_wnd_start[0] = i;
      zonal_wnd_start[1] = 0;
      zonal_wnd_start[2] = 0;
      zonal_wnd_count[0] = 1;
      zonal_wnd_count[1] = LAT_LEN;
      zonal_wnd_count[2] = LON_LEN;
      if (nc_put_vara_float(ncid, zonal_wnd_id, zonal_wnd_start, zonal_wnd_count, zonal_wnd)) ERR;
   }
   if (nc_close(ncid)) ERR;
   return 0;
}

int
main(int argc, char **argv)
{
   printf("\n*** Testing nofill mode.\n");
   printf("*** Create file in nofill mode using nc_create()...");
   {
       int ncid, varid;
      int data[TIME_LEN];

      if (create_file(FILE_NAME, NC_NOFILL)) ERR;
      if (nc_open(FILE_NAME, 0, &ncid)) ERR;
      if (nc_inq_varid(ncid, "time", &varid)) ERR;
      if (nc_get_var(ncid, varid, data)) ERR;
      if (data[0] != 1) ERR; /* value overwritten by zero due to NOFILL bug */
      if (nc_close(ncid)) ERR;
   }
   SUMMARIZE_ERR;
   FINAL_RESULTS;
}
