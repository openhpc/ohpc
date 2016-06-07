/*
  Copyright 2004-2006, UCAR/Unidata
  See COPYRIGHT file for copying and redistribution conditions.

  This is part of netCDF.
   
  This program also takes a long time to run - it writes some data in
  a very large file, and then reads it all back to be sure it's
  correct.

  This program is an add-on test to check very large 64-bit offset
  files (8 GB, so make sure you have the disk space!).

  $Id: large_files.c,v 1.4 2008/03/10 17:02:41 ed Exp $
*/

#include <config.h>
#include <nc_tests.h>
#include <stdio.h>
#include <stdlib.h>
#include <netcdf.h>

#define FILE_NAME "large_files.nc"

static void
check_err(const int stat, const int line, const char *file) {
    if (stat != NC_NOERR) {
	   (void) fprintf(stderr, "line %d of %s: %s\n", line, file, nc_strerror(stat));
        exit(1);
    }
}

int
main(int argc, char **argv) {

   int  stat;			/* return status */
   char file_name[NC_MAX_NAME + 1];
   int  ncid;			/* netCDF id */
   int rec, i, j, k;
   int x[] = {42, 21};

   /* dimension ids */
   int rec_dim;
   int i_dim;
   int j_dim;
   int k_dim;
   int n_dim;

#define NUMRECS 2
#define I_LEN 5
//#define J_LEN 214700000
#define J_LEN   500000000
#define K_LEN 1023
#define N_LEN 2

   /* dimension lengths */
   size_t rec_len = NC_UNLIMITED;
   size_t i_len = I_LEN;
   size_t j_len = J_LEN;
   size_t k_len = K_LEN;
   size_t n_len = N_LEN;

   /* variable ids */
   int var1_id;
   int x_id;

   /* rank (number of dimensions) for each variable */
#  define RANK_var1 3
#  define RANK_x 2

   /* variable shapes */
   int var1_dims[RANK_var1];
   int x_dims[RANK_x];

    printf("\n*** Testing large files, slowly.\n");

    sprintf(file_name, "%s/%s", TEMP_LARGE, FILE_NAME);
    printf("*** Creating large file %s...", file_name);

   /* enter define mode */
   stat = nc_create(file_name, NC_CLOBBER|NC_64BIT_OFFSET, &ncid);
   check_err(stat,__LINE__,__FILE__);

   /* define dimensions */
   stat = nc_def_dim(ncid, "rec", rec_len, &rec_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "i", i_len, &i_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "j", j_len, &j_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "k", k_len, &k_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "n", n_len, &n_dim);
   check_err(stat,__LINE__,__FILE__);

   /* define variables */

   x_dims[0] = rec_dim;
   x_dims[1] = n_dim;
   stat = nc_def_var(ncid, "x", NC_INT, RANK_x, x_dims, &x_id);
   check_err(stat,__LINE__,__FILE__);

   var1_dims[0] = rec_dim;
   var1_dims[1] = i_dim;
   var1_dims[2] = j_dim;
   //var1_dims[3] = k_dim;
   stat = nc_def_var(ncid, "var1", NC_FLOAT, RANK_var1, var1_dims, &var1_id);
   check_err(stat,__LINE__,__FILE__);

   /* don't initialize variables with fill values */
   stat = nc_set_fill(ncid, NC_NOFILL, 0);
   check_err(stat,__LINE__,__FILE__);

   /* leave define mode */
   stat = nc_enddef (ncid);
   check_err(stat,__LINE__,__FILE__);

   {			/* store var1 */
     //static float var1[J_LEN];
     static float *var1 = NULL;
     var1 = malloc(sizeof(float)*J_LEN);
     static size_t var1_start[RANK_var1] = {0, 0, 0};
     static size_t var1_count[RANK_var1] = {1, 1, J_LEN};
     static size_t x_start[RANK_x] = {0, 0};
     static size_t x_count[RANK_x] = {1, N_LEN};
     for(rec=0; rec<NUMRECS; rec++) {
       var1_start[0] = rec;
       x_start[0] = rec;
       for(i=0; i<I_LEN; i++) {
         for(j=0; j<J_LEN; j++) {
           var1[j] = (float)(j + (rec+1) * i);
         }
         var1_start[1] = i;
         stat = nc_put_vara_float(ncid, var1_id, var1_start, var1_count, var1);
         check_err(stat,__LINE__,__FILE__);
       }
       x[0] += rec; x[1] += rec;
       stat = nc_put_vara_int(ncid, x_id, x_start, x_count, x);
       check_err(stat,__LINE__,__FILE__);
     }
     free(var1);
   }

   stat = nc_close(ncid);
   check_err(stat,__LINE__,__FILE__);

   printf("ok\n");
   printf("*** Reading large file %s...", file_name);

   stat = nc_open(file_name, NC_NOWRITE, &ncid);
   check_err(stat,__LINE__,__FILE__);

   {			/* read var1 */
     
     //static float avar1[J_LEN];
     static float *avar1 = NULL;
     avar1 = (float*)malloc(sizeof(float)*J_LEN);
     static size_t avar1_start[RANK_var1] = {0, 0, 0};
     static size_t avar1_count[RANK_var1] = {1, 1, J_LEN};
     static size_t ax_start[RANK_x] = {0, 0};
     static size_t ax_count[RANK_x] = {1, N_LEN};
     for(rec=0; rec<NUMRECS; rec++) {
       avar1_start[0] = rec;
       ax_start[0] = rec;
       for(i=0; i<I_LEN; i++) {
         avar1_start[1] = i;
         stat = nc_get_vara_float(ncid, var1_id, avar1_start, avar1_count, avar1);
         check_err(stat,__LINE__,__FILE__);
         for(j=0; j<J_LEN; j++) 
         {
           if (avar1[j] != (float)(j + (rec + 1) * i)) {
             printf("Error on read, var1[%d, %d, %d] = %g wrong, "
                    "should be %g !\n", rec, i, j, avar1[j], (float) (j + (rec + 1)* i)); 
             return 1;
           }
         }
       }
       nc_get_vara_int(ncid, x_id, ax_start, ax_count, x);
       if(x[0] != (42 + rec) || x[1] != (21+rec)) {
         printf("Error on read, x[] = %d, %d\n", x[0], x[1]);
         return 1;
       }
     }
     free(avar1);
   }
   stat = nc_close(ncid);
   check_err(stat,__LINE__,__FILE__);

   printf("ok\n");
   printf("*** Tests successful!\n");

   /* Delete the file. */
   (void) remove(file_name);

   return 0;
}
