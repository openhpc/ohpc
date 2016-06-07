/*
  Copyright 2007, UCAR/Unidata
  See COPYRIGHT file for copying and redistribution conditions.

  This is part of netCDF.
   
  This program tests for a bug discovered with nofill mode that failed
  only on file systems with block size in a particular range.  It fails
  when invoked with the blksize argument between 2091953 and 2150032,
  inclusive, and succeeds for other blksizes.

  $Id$
*/

#include <config.h>
#include <nc_tests.h>
#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <errno.h>
#include <netcdf.h>

#define FILE_NAME1 "tst_fill1.nc"
#define FILE_NAME2 "tst_fill2.nc"

void
check_err(const int stat, const int line, const char *file) {
    if (stat != NC_NOERR) {
	(void) fprintf(stderr, "line %d of %s: %s\n", line, file, nc_strerror(stat));
        fflush(stderr);
	exit(1);
    }
}

#define LON_LEN 240
#define LAT_LEN 121
#define LVL_LEN 31
#define TIME_LEN 1

int
create_file(char *file_name, int fill_mode, size_t* sizehintp) 
{
   int i;
   int  stat;			/* return status */
   int  ncid;			/* netCDF id */

   /* dimension ids */
   int lon_dim;
   int lat_dim;
   int lvl_dim;
   int time_dim;

   /* dimension lengths */
   size_t lon_len = LON_LEN;
   size_t lat_len = LAT_LEN;
   size_t lvl_len = LVL_LEN;
   size_t time_len = TIME_LEN;

   /* variable ids */
   int time_id;
   int lat_id;
   int lon_id;
   int lvl_id;
   int sfc_pres_id;
   int temp_scrn_id;
   int qsair_scrn_id;
   int topog_id;
   int mslp_id;
   int sfc_temp_id;
   int zonal_wnd_id;

   /* rank (number of dimensions) for each variable */
#  define RANK_time 1
#  define RANK_lat 1
#  define RANK_lon 1
#  define RANK_lvl 1
#  define RANK_sfc_pres 3
#  define RANK_temp_scrn 3
#  define RANK_qsair_scrn 3
#  define RANK_topog 3
#  define RANK_mslp 3
#  define RANK_sfc_temp 3
#  define RANK_zonal_wnd 4

   /* variable shapes */
   int time_dims[RANK_time];
   int lat_dims[RANK_lat];
   int lon_dims[RANK_lon];
   int lvl_dims[RANK_lvl];
   int sfc_pres_dims[RANK_sfc_pres];
   int temp_scrn_dims[RANK_temp_scrn];
   int qsair_scrn_dims[RANK_qsair_scrn];
   int topog_dims[RANK_topog];
   int mslp_dims[RANK_mslp];
   int sfc_temp_dims[RANK_sfc_temp];
   int zonal_wnd_dims[RANK_zonal_wnd];

   size_t zonal_wnd_start[RANK_zonal_wnd];
   size_t zonal_wnd_count[RANK_zonal_wnd];
   float zonal_wnd[LON_LEN*LAT_LEN*TIME_LEN];
   int ii;

   int old_fill_mode;
   size_t default_initialsize = 0;

   /* To test bug on filesystem without large block size, we can get
    * the same effect by providing the desired value as sizehint to
    * nc__create() instead of calling nc_create() and getting the
    * block size reported by fstat */
   stat = nc__create(file_name, NC_CLOBBER, default_initialsize, sizehintp, &ncid);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_set_fill(ncid, fill_mode, &old_fill_mode);
   check_err(stat,__LINE__,__FILE__);

   /* define dimensions */
   stat = nc_def_dim(ncid, "lon", lon_len, &lon_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "lat", lat_len, &lat_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "lvl", lvl_len, &lvl_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "time", time_len, &time_dim);
   check_err(stat,__LINE__,__FILE__);

   /* define variables */
   time_dims[0] = time_dim;
   stat = nc_def_var(ncid, "time", NC_DOUBLE, RANK_time, time_dims, &time_id);
   check_err(stat,__LINE__,__FILE__);

   lat_dims[0] = lat_dim;
   stat = nc_def_var(ncid, "lat", NC_FLOAT, RANK_lat, lat_dims, &lat_id);
   check_err(stat,__LINE__,__FILE__);

   lon_dims[0] = lon_dim;
   stat = nc_def_var(ncid, "lon", NC_FLOAT, RANK_lon, lon_dims, &lon_id);
   check_err(stat,__LINE__,__FILE__);

   lvl_dims[0] = lvl_dim;
   stat = nc_def_var(ncid, "lvl", NC_FLOAT, RANK_lvl, lvl_dims, &lvl_id);
   check_err(stat,__LINE__,__FILE__);

   sfc_pres_dims[0] = time_dim;
   sfc_pres_dims[1] = lat_dim;
   sfc_pres_dims[2] = lon_dim;
   stat = nc_def_var(ncid, "sfc_pres", NC_FLOAT, RANK_sfc_pres, sfc_pres_dims, &sfc_pres_id);
   check_err(stat,__LINE__,__FILE__);

   temp_scrn_dims[0] = time_dim;
   temp_scrn_dims[1] = lat_dim;
   temp_scrn_dims[2] = lon_dim;
   stat = nc_def_var(ncid, "temp_scrn", NC_FLOAT, RANK_temp_scrn, temp_scrn_dims, &temp_scrn_id);
   check_err(stat,__LINE__,__FILE__);

   qsair_scrn_dims[0] = time_dim;
   qsair_scrn_dims[1] = lat_dim;
   qsair_scrn_dims[2] = lon_dim;
   stat = nc_def_var(ncid, "qsair_scrn", NC_FLOAT, RANK_qsair_scrn, qsair_scrn_dims, &qsair_scrn_id);
   check_err(stat,__LINE__,__FILE__);

   topog_dims[0] = time_dim;
   topog_dims[1] = lat_dim;
   topog_dims[2] = lon_dim;
   stat = nc_def_var(ncid, "topog", NC_FLOAT, RANK_topog, topog_dims, &topog_id);
   check_err(stat,__LINE__,__FILE__);

   mslp_dims[0] = time_dim;
   mslp_dims[1] = lat_dim;
   mslp_dims[2] = lon_dim;
   stat = nc_def_var(ncid, "mslp", NC_FLOAT, RANK_mslp, mslp_dims, &mslp_id);
   check_err(stat,__LINE__,__FILE__);

   sfc_temp_dims[0] = time_dim;
   sfc_temp_dims[1] = lat_dim;
   sfc_temp_dims[2] = lon_dim;
   stat = nc_def_var(ncid, "sfc_temp", NC_FLOAT, RANK_sfc_temp, sfc_temp_dims, &sfc_temp_id); 
   check_err(stat,__LINE__,__FILE__);

   zonal_wnd_dims[0] = time_dim;
   zonal_wnd_dims[1] = lvl_dim;
   zonal_wnd_dims[2] = lat_dim;
   zonal_wnd_dims[3] = lon_dim;
   stat = nc_def_var(ncid, "zonal_wnd", NC_FLOAT, RANK_zonal_wnd, zonal_wnd_dims, &zonal_wnd_id);
   check_err(stat,__LINE__,__FILE__);

   /* leave define mode */
   stat = nc_enddef (ncid);
   check_err(stat,__LINE__,__FILE__);

   {				/* store time */
       size_t time_start[RANK_time];
       size_t time_count[RANK_time];
       double time[TIME_LEN] = {1.};
       time_len = 1;
       time_start[0] = 0;
       time_count[0] = time_len;
       stat = nc_put_vara_double(ncid, time_id, time_start, time_count, time);
       check_err(stat,__LINE__,__FILE__);
   }
   
   {				/* store lat */
       float lat[] = {90, 88.5, 87, 85.5, 84, 82.5, 81, 79.5, 78, 76.5, 75, 73.5, 72, 70.5, 69, 67.5, 66, 64.5, 63, 61.5, 60, 58.5, 57, 55.5, 54, 52.5, 51, 49.5, 48, 46.5, 45, 43.5, 42, 40.5, 39, 37.5, 36, 34.5, 33, 31.5, 30, 28.5, 27, 25.5, 24, 22.5, 21, 19.5, 18, 16.5, 15, 13.5, 12, 10.5, 9, 7.5, 6, 4.5, 3, 1.5, 0, -1.5, -3, -4.5, -6, -7.5, -9, -10.5, -12, -13.5, -15, -16.5, -18, -19.5, -21, -22.5, -24, -25.5, -27, -28.5, -30, -31.5, -33, -34.5, -36, -37.5, -39, -40.5, -42, -43.5, -45, -46.5, -48, -49.5, -51, -52.5, -54, -55.5, -57, -58.5, -60, -61.5, -63, -64.5, -66, -67.5, -69, -70.5, -72, -73.5, -75, -76.5, -78, -79.5, -81, -82.5, -84, -85.5, -87, -88.5, -90};
       stat = nc_put_var_float(ncid, lat_id, lat);
       check_err(stat,__LINE__,__FILE__);
   }
   
   {				/* store lon */
       float lon[] = {0, 1.5, 3, 4.5, 6, 7.5, 9, 10.5, 12, 13.5, 15, 16.5, 18, 19.5, 21, 22.5, 24, 25.5, 27, 28.5, 30, 31.5, 33, 34.5, 36, 37.5, 39, 40.5, 42, 43.5, 45, 46.5, 48, 49.5, 51, 52.5, 54, 55.5, 57, 58.5, 60, 61.5, 63, 64.5, 66, 67.5, 69, 70.5, 72, 73.5, 75, 76.5, 78, 79.5, 81, 82.5, 84, 85.5, 87, 88.5, 90, 91.5, 93, 94.5, 96, 97.5, 99, 100.5, 102, 103.5, 105, 106.5, 108, 109.5, 111, 112.5, 114, 115.5, 117, 118.5, 120, 121.5, 123, 124.5, 126, 127.5, 129, 130.5, 132, 133.5, 135, 136.5, 138, 139.5, 141, 142.5, 144, 145.5, 147, 148.5, 150, 151.5, 153, 154.5, 156, 157.5, 159, 160.5, 162, 163.5, 165, 166.5, 168, 169.5, 171, 172.5, 174, 175.5, 177, 178.5, 180, 181.5, 183, 184.5, 186, 187.5, 189, 190.5, 192, 193.5, 195, 196.5, 198, 199.5, 201, 202.5, 204, 205.5, 207, 208.5, 210, 211.5, 213, 214.5, 216, 217.5, 219, 220.5, 222, 223.5, 225, 226.5, 228, 229.5, 231, 232.5, 234, 235.5, 237, 238.5, 240, 241.5, 243, 244.5, 246, 247.5, 249, 250.5, 252, 253.5, 255, 256.5, 258, 259.5, 261, 262.5, 264, 265.5, 267, 268.5, 270, 271.5, 273, 274.5, 276, 277.5, 279, 280.5, 282, 283.5, 285, 286.5, 288, 289.5, 291, 292.5, 294, 295.5, 297, 298.5, 300, 301.5, 303, 304.5, 306, 307.5, 309, 310.5, 312, 313.5, 315, 316.5, 318, 319.5, 321, 322.5, 324, 325.5, 327, 328.5, 330, 331.5, 333, 334.5, 336, 337.5, 339, 340.5, 342, 343.5, 345, 346.5, 348, 349.5, 351, 352.5, 354, 355.5, 357, 358.5};
       stat = nc_put_var_float(ncid, lon_id, lon);
       check_err(stat,__LINE__,__FILE__);
   }

   {				/* store lvl */
       float lvl[] = {1000, 995, 990, 985, 975, 950, 925, 900, 875, 850, 800, 750, 700, 600, 500, 450, 400, 350, 300, 275, 250, 225, 200, 175, 150, 100, 70, 50, 30, 20, 10};
       stat = nc_put_var_float(ncid, lvl_id, lvl);
       check_err(stat,__LINE__,__FILE__);
   }
   
   {				/* store sfc_pres */
       size_t sfc_pres_start[RANK_sfc_pres];
       size_t sfc_pres_count[RANK_sfc_pres];
       float sfc_pres[LON_LEN*LAT_LEN];
       
       for(ii = 0; ii < LAT_LEN * LON_LEN; ii++) {
	   sfc_pres[ii] = 6;
       }
       sfc_pres_start[0] = 0;
       sfc_pres_start[1] = 0;
       sfc_pres_start[2] = 0;
       sfc_pres_count[0] = time_len;
       sfc_pres_count[1] = lat_len;
       sfc_pres_count[2] = lon_len;
       stat = nc_put_vara_float(ncid, sfc_pres_id, sfc_pres_start, sfc_pres_count, sfc_pres);
       check_err(stat,__LINE__,__FILE__);
   }

   {				/* store temp_scrn */
       size_t temp_scrn_start[RANK_temp_scrn];
       size_t temp_scrn_count[RANK_temp_scrn];
       float temp_scrn[LON_LEN*LAT_LEN];
       
       for(ii = 0; ii < LAT_LEN * LON_LEN; ii++) {
	   temp_scrn[ii] = 11;
       }
       temp_scrn_start[0] = 0;
       temp_scrn_start[1] = 0;
       temp_scrn_start[2] = 0;
       temp_scrn_count[0] = time_len;
       temp_scrn_count[1] = lat_len;
       temp_scrn_count[2] = lon_len;
       stat = nc_put_vara_float(ncid, temp_scrn_id, temp_scrn_start, temp_scrn_count, temp_scrn);
       check_err(stat,__LINE__,__FILE__);
   }
   
   {				/* store qsair_scrn */
       size_t qsair_scrn_start[RANK_qsair_scrn];
       size_t qsair_scrn_count[RANK_qsair_scrn];
       float qsair_scrn[LON_LEN*LAT_LEN];
       
       for(ii = 0; ii < LAT_LEN * LON_LEN; ii++) {
	   qsair_scrn[ii] = 22;
       }
       qsair_scrn_start[0] = 0;
       qsair_scrn_start[1] = 0;
       qsair_scrn_start[2] = 0;
       qsair_scrn_count[0] = time_len;
       qsair_scrn_count[1] = lat_len;
       qsair_scrn_count[2] = lon_len;
       stat = nc_put_vara_float(ncid, qsair_scrn_id, qsair_scrn_start, qsair_scrn_count, qsair_scrn);
       check_err(stat,__LINE__,__FILE__);
   }
   
   {				/* store topog */
       size_t topog_start[RANK_topog];
       size_t topog_count[RANK_topog];
       float topog[LON_LEN*LAT_LEN];
       
       for(ii = 0; ii < LAT_LEN * LON_LEN; ii++) {
	   topog[ii] = 33;
       }
       topog_start[0] = 0;
       topog_start[1] = 0;
       topog_start[2] = 0;
       topog_count[0] = time_len;
       topog_count[1] = lat_len;
       topog_count[2] = lon_len;
       stat = nc_put_vara_float(ncid, topog_id, topog_start, topog_count, topog);
       check_err(stat,__LINE__,__FILE__);
   }
   
   {				/* store mslp */
       size_t mslp_start[RANK_mslp];
       size_t mslp_count[RANK_mslp];
       float mslp[LON_LEN*LAT_LEN];
       
       for(ii = 0; ii < LAT_LEN * LON_LEN; ii++) {
	   mslp[ii] = 44;
       }
       mslp_start[0] = 0;
       mslp_start[1] = 0;
       mslp_start[2] = 0;
       mslp_count[0] = time_len;
       mslp_count[1] = lat_len;
       mslp_count[2] = lon_len;
       stat = nc_put_vara_float(ncid, mslp_id, mslp_start, mslp_count, mslp);
       check_err(stat,__LINE__,__FILE__);
   }
   
   {				/* store sfc_temp */
       size_t sfc_temp_start[RANK_sfc_temp];
       size_t sfc_temp_count[RANK_sfc_temp];
       float sfc_temp[LON_LEN*LAT_LEN];
       
       for(ii = 0; ii < LAT_LEN * LON_LEN; ii++) {
	   sfc_temp[ii] = 55;
       }
       sfc_temp_start[0] = 0;
       sfc_temp_start[1] = 0;
       sfc_temp_start[2] = 0;
       sfc_temp_count[0] = time_len;
       sfc_temp_count[1] = lat_len;
       sfc_temp_count[2] = lon_len;
       stat = nc_put_vara_float(ncid, sfc_temp_id, sfc_temp_start, sfc_temp_count, sfc_temp);
       check_err(stat,__LINE__,__FILE__);
   }
   
   {		      /* store zonal_wnd */
       /* Bug exposed when written in reverse order. */
       for(i = LVL_LEN - 1; i>=0; i--)
       /* for(i = 0; i < LVL_LEN; i++) */
       {
	   int izw;
	   for(izw = 0; izw < TIME_LEN * LAT_LEN * LON_LEN; izw++) {
	       zonal_wnd[izw] = 100 + i;
	   }
	   zonal_wnd_start[0] = 0;
	   zonal_wnd_start[1] = i;
	   zonal_wnd_start[2] = 0;
	   zonal_wnd_start[3] = 0;
	   zonal_wnd_count[0] = time_len;
	   zonal_wnd_count[1] = 1;
	   zonal_wnd_count[2] = lat_len;
	   zonal_wnd_count[3] = lon_len;
	   stat = nc_put_vara_float(ncid, zonal_wnd_id, zonal_wnd_start, zonal_wnd_count, zonal_wnd);
	   check_err(stat,__LINE__,__FILE__);
       }
   }
   stat = nc_close(ncid);
   check_err(stat,__LINE__,__FILE__);
   return 0;
}

int
main(int argc, char **argv)
{
    size_t sizehint = 2100000;	/* default if not set on command line,
				 * exposes bug.  It turns out any
				 * value between 2091953 and 2150032
				 * triggers bug, whereas all other
				 * values work fine. */

    if (argc > 1) {
	char *endptr, *str = argv[1];
	errno = 0;
	sizehint = strtol(str, &endptr, 0);
	/* check for various possible errors */
	if ((errno == ERANGE && (sizehint == LONG_MAX || sizehint == LONG_MIN))
                   || (errno != 0 && sizehint == 0)) {
               perror("strtol");
               exit(EXIT_FAILURE);
	}
	if (endptr == str) {
	    fprintf(stderr, "No digits were found\n");
	    exit(EXIT_FAILURE);
	}
    }
   printf("\n*** Testing nofill mode.\n");
   {
       printf("*** Create file in nofill mode, writing all values...");
       if (create_file(FILE_NAME1, NC_NOFILL, &sizehint)) ERR;
       SUMMARIZE_ERR;
   }
   {
       printf("*** Create file with same data in fill mode, writing all values...");
       if (create_file(FILE_NAME2, NC_FILL, &sizehint)) ERR;
       SUMMARIZE_ERR;
   }
   {
       int ncid1, ncid2;
       int nvars1, nvars2;
       int varid;
       int badvars;

       printf("*** Compare values in nofill mode and fill mode files...");
       /* compare data in two files created with nofill mode and fill
	* mode, which should be identical if all the data were written */
       if (nc_open(FILE_NAME1, NC_NOWRITE, &ncid1)) ERR;
       if (nc_open(FILE_NAME2, NC_NOWRITE, &ncid2)) ERR;
       if (nc_inq_nvars(ncid1, &nvars1)) ERR;
       if (nc_inq_nvars(ncid2, &nvars2)) ERR;
       if (nvars1 != nvars2) ERR;
       badvars = 0;
       for(varid = 0; varid < nvars1; varid++) {
	   size_t nvals, nn;
	   int ndims, *dimids, dim;
	   nc_type vtype;
	   char varname1[NC_MAX_NAME];		   
	   char varname2[NC_MAX_NAME];
	   /* How many values in this variable to compare? */
	   if (nc_inq_varndims(ncid1, varid, &ndims)) ERR;
	   dimids = malloc((ndims + 1) * sizeof(int));
	   if (!dimids) ERR;
	   if (nc_inq_vardimid (ncid1, varid, dimids)) ERR;
	   nvals = 1;
	   for(dim = 0; dim < ndims; dim++) {
	       size_t len;
	       if (nc_inq_dimlen(ncid1, dimids[dim], &len)) ERR;
	       nvals *= len;
	   }
	   if (nc_inq_vartype(ncid1, varid, &vtype)) ERR;
	   if (nc_inq_varname(ncid1, varid, varname1)) ERR;
	   if (nc_inq_varname(ncid1, varid, varname2)) ERR;
	   
	   if (vtype != NC_CHAR) {  /* numeric data, just read in as doubles */
	       double *data1, *data2;
	       /* Allocate space to hold values in both files */
	       data1 = malloc((nvals + 1) * sizeof(double));
	       if (!data1) ERR;
	       data2 = malloc((nvals + 1) * sizeof(double));
	       if (!data2) ERR;
	       /* Read in values */
	       if (nc_get_var_double(ncid1, varid, data1)) ERR;
	       if (nc_get_var_double(ncid2, varid, data2)) ERR;
	       /* Compare values */
	       for(nn = 0; nn < nvals; nn++) {
		   if (data1[nn] != data2[nn]) {
		       badvars++;
		       fprintf(stderr, 
			       "\tFrom nofill file, %s[%lu] = %.15g\tFrom fill file, %s[%lu] = %.15g\n", 
			       varname1, (unsigned long)nn, data1[nn], varname2, (unsigned long)nn, data2[nn]);
		       break;
		   };
	       }
	       free(data1);
	       free(data2);
	   } else {		/* character data */
	       char *data1, *data2;
	       /* Allocate space to hold values in both files */
	       data1 = malloc((nvals + 1) * sizeof(char));
	       if (!data1) ERR;
	       data2 = malloc((nvals + 1) * sizeof(char));
	       if (!data2) ERR;
	       /* Read in values */
	       if (nc_get_var_text(ncid1, varid, data1)) ERR;
	       if (nc_get_var_text(ncid2, varid, data2)) ERR;
	       /* Compare values */
	       for(nn = 0; nn < nvals; nn++) {
		   if (data1[nn] != data2[nn]) {
		       badvars++;
		       fprintf(stderr, 
			       "\tFrom nofill file, %s[%lu] = %d\tFrom fill file, %s[%lu] = %d\n", 
			       varname1, (unsigned long)nn, data1[nn], varname2, (unsigned long)nn, data2[nn]);
		       break;
		   };
	       }
	       free(data1);
	       free(data2);
	   }
	   free(dimids);
       }
       if(badvars > 0) ERR;
       if (nc_close(ncid1)) ERR;
       if (nc_close(ncid2)) ERR;
       SUMMARIZE_ERR;
   }
   FINAL_RESULTS;
}
