/* This is part of the netCDF package.
   Copyright 2010 University Corporation for Atmospheric Research/Unidata
   See COPYRIGHT file for conditions of use.

   Test dim rename that is causing problems with v2 API.
*/

#include <nc_tests.h>


#define FILE_NAME "tst_rename.nc"

void
check_err(const int stat, const int line, const char *file) {
    if (stat != NC_NOERR) {
        (void)fprintf(stderr,"line %d of %s: %s\n", line, file, nc_strerror(stat));
        fflush(stderr);
        exit(1);
    }
}

int
create_file()
{
   int  stat;  /* return status */
   int  ncid;  /* netCDF id */

   /* dimension ids */
   int ii_dim;
   int jj_dim;
   int kk_dim;
   int i1_dim;
   int i2_dim;
   int i3_dim;
   int rec_dim;
   int ll_dim;
   int mm_dim;
   int nn_dim;

   /* dimension lengths */
   size_t ii_len = 4;
   size_t jj_len = 3;
   size_t kk_len = 3;
   size_t i1_len = 5;
   size_t i2_len = 3;
   size_t i3_len = 7;
   size_t rec_len = NC_UNLIMITED;
   size_t ll_len = 3;
   size_t mm_len = 1;
   size_t nn_len = 1;

   /* variable ids */
   int aa_id;
   int bb_id;
   int cc_id;
   int cd_id;
   int ce_id;
   int dd_id;

   /* rank (number of dimensions) for each variable */
#   define RANK_aa 1
#   define RANK_bb 2
#   define RANK_cc 1
#   define RANK_cd 2
#   define RANK_ce 3
#   define RANK_dd 1

   /* variable shapes */
   int aa_dims[RANK_aa];
   int bb_dims[RANK_bb];
   int cc_dims[RANK_cc];
   int cd_dims[RANK_cd];
   int ce_dims[RANK_ce];
   int dd_dims[RANK_dd];

   /* enter define mode */
   stat = nc_create(FILE_NAME, NC_CLOBBER, &ncid);
   check_err(stat,__LINE__,__FILE__);

   /* define dimensions */
   stat = nc_def_dim(ncid, "ii", ii_len, &ii_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "jj", jj_len, &jj_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "kk", kk_len, &kk_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "i1", i1_len, &i1_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "i2", i2_len, &i2_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "i3", i3_len, &i3_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "rec", rec_len, &rec_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "ll", ll_len, &ll_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "mm", mm_len, &mm_dim);
   check_err(stat,__LINE__,__FILE__);
   stat = nc_def_dim(ncid, "nn", nn_len, &nn_dim);
   check_err(stat,__LINE__,__FILE__);

   /* define variables */

   aa_dims[0] = ii_dim;
   stat = nc_def_var(ncid, "aa", NC_INT, RANK_aa, aa_dims, &aa_id);
   check_err(stat,__LINE__,__FILE__);

   bb_dims[0] = kk_dim;
   bb_dims[1] = jj_dim;
   stat = nc_def_var(ncid, "bb", NC_INT, RANK_bb, bb_dims, &bb_id);
   check_err(stat,__LINE__,__FILE__);

   cc_dims[0] = rec_dim;
   stat = nc_def_var(ncid, "cc", NC_INT, RANK_cc, cc_dims, &cc_id);
   check_err(stat,__LINE__,__FILE__);

   cd_dims[0] = rec_dim;
   cd_dims[1] = i2_dim;
   stat = nc_def_var(ncid, "cd", NC_SHORT, RANK_cd, cd_dims, &cd_id);
   check_err(stat,__LINE__,__FILE__);

   ce_dims[0] = rec_dim;
   ce_dims[1] = i2_dim;
   ce_dims[2] = i3_dim;
   stat = nc_def_var(ncid, "ce", NC_FLOAT, RANK_ce, ce_dims, &ce_id);
   check_err(stat,__LINE__,__FILE__);

   dd_dims[0] = ll_dim;
   stat = nc_def_var(ncid, "dd", NC_SHORT, RANK_dd, dd_dims, &dd_id);
   check_err(stat,__LINE__,__FILE__);

   /* assign global attributes */
   { /* title */
      stat = nc_put_att_text(ncid, NC_GLOBAL, "title", 11, "test netcdf");
      check_err(stat,__LINE__,__FILE__);
   }


   /* assign per-variable attributes */
   { /* units */
      stat = nc_put_att_text(ncid, aa_id, "units", 8, "furlongs");
      check_err(stat,__LINE__,__FILE__);
   }
   { /* valid_range */
      static const float bb_valid_range_att[2] = {0, 100} ;
      stat = nc_put_att_float(ncid, bb_id, "valid_range", NC_FLOAT, 2, bb_valid_range_att);
      check_err(stat,__LINE__,__FILE__);
   }
   { /* units */
      stat = nc_put_att_text(ncid, cc_id, "units", 5, "moles");
      check_err(stat,__LINE__,__FILE__);
   }
   { /* units */
      stat = nc_put_att_text(ncid, cd_id, "units", 5, "moles");
      check_err(stat,__LINE__,__FILE__);
   }
   { /* units */
      stat = nc_put_att_text(ncid, ce_id, "units", 5, "moles");
      check_err(stat,__LINE__,__FILE__);
   }
   { /* fill_value */
      static const short dd_fill_value_att[1] = {-999} ;
      stat = nc_put_att_short(ncid, dd_id, "fill_value", NC_SHORT, 1, dd_fill_value_att);
      check_err(stat,__LINE__,__FILE__);
   }


   /* leave define mode */
   stat = nc_enddef (ncid);
   check_err(stat,__LINE__,__FILE__);

   /* assign variable data */
   {
      int aa_data[4] = {-2147483647, -2147483647, -2147483647, -2147483647} ;
      size_t aa_startset[1] = {0} ;
      size_t aa_countset[1] = {4} ;
      stat = nc_put_vara(ncid, aa_id, aa_startset, aa_countset, aa_data);
      check_err(stat,__LINE__,__FILE__);
   }

   {
      int bb_data[9] = {-2147483647, -2147483647, -2147483647, -2147483647, -2147483647, -2147483647, -2147483647, -2147483647, -2147483647} ;
      size_t bb_startset[2] = {0, 0} ;
      size_t bb_countset[2] = {3, 3} ;
      stat = nc_put_vara(ncid, bb_id, bb_startset, bb_countset, bb_data);
      check_err(stat,__LINE__,__FILE__);
   }

   {
      short dd_data[3] = {1, 2, -32767} ;
      size_t dd_startset[1] = {0} ;
      size_t dd_countset[1] = {3} ;
      stat = nc_put_vara(ncid, dd_id, dd_startset, dd_countset, dd_data);
      check_err(stat,__LINE__,__FILE__);
   }


   stat = nc_close(ncid);
   check_err(stat,__LINE__,__FILE__);
   return 0;
}

int
main(int argc, char **argv)
{
   printf("\n*** Testing v3/v4 API versions of some v2 tests.\n");
   printf("*** testing simple dim rename...");
   {
#define PP1 "pp"
#define PP1_SIZE 7
#define P1_NAME "p"

      int ncid, pp_dimid, dimid_in;

      /* Create a file with one dimension. */
      if (nc_create(FILE_NAME, NC_CLOBBER, &ncid)) ERR;
      if (nc_def_dim(ncid, PP1, PP1_SIZE, &pp_dimid)) ERR;

      /* Renaming to shorter name is possible in data mode... */
      if (nc_enddef(ncid)) ERR;
      if (nc_rename_dim(ncid, pp_dimid, P1_NAME)) ERR;
      if (nc_inq_dimid(ncid, P1_NAME, &dimid_in)) ERR;
      if (dimid_in != pp_dimid) ERR;
      if (nc_close(ncid)) ERR;
   }
   SUMMARIZE_ERR;
   printf("*** testing dim rename from nctest...");
   {
#define PP "pp"
#define PP_SIZE 7
#define QQ "qq"
#define QQ_SIZE 10
#define NEW_NAME "new_name"
#define ANOTHER_NAME "another_name"
#define P_NAME "p"

      int ncid, pp_dimid, qq_dimid, dimid_in;
      char name_in[NC_MAX_NAME + 1];

      /* Create the same file as nctest.c does. */
      if (create_file()) ERR;

      /* Open it and test renames of dimensions. */
      if (nc_open(FILE_NAME, NC_WRITE, &ncid)) ERR;
      if (nc_redef(ncid)) ERR;
      if (nc_def_dim(ncid, PP, PP_SIZE, &pp_dimid)) ERR;
      if (nc_def_dim(ncid, QQ, QQ_SIZE, &qq_dimid)) ERR;
      if (nc_rename_dim(ncid, pp_dimid, NEW_NAME)) ERR;
      if (nc_inq_dimname(ncid, pp_dimid, name_in)) ERR;
      if (strcmp(NEW_NAME, name_in)) ERR;
      if (nc_rename_dim(ncid, pp_dimid, QQ) != NC_ENAMEINUSE) ERR;
      if (nc_rename_dim(ncid, -1, ANOTHER_NAME) != NC_EBADDIM) ERR;
      if (nc_rename_dim(ncid, 12, ANOTHER_NAME) != NC_EBADDIM) ERR;
      if (nc_enddef(ncid)) ERR;
      if (nc_rename_dim(ncid, pp_dimid, P_NAME)) ERR;
      if (nc_inq_dimid(ncid, P_NAME, &dimid_in)) ERR;
      if (dimid_in != pp_dimid) ERR;
      if (nc_inq_dimid(ncid, P_NAME, NULL)) ERR;
      if (nc_close(ncid)) ERR;
   }
   SUMMARIZE_ERR;
   FINAL_RESULTS;
}
