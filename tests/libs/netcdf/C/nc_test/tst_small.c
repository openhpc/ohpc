/* This is part of the netCDF package. Copyright 2005 University
   Corporation for Atmospheric Research/Unidata See COPYRIGHT file for
   conditions of use. See www.unidata.ucar.edu for more info.

   Test small files.

   $Id: tst_small.c,v 1.15 2008/10/20 01:48:08 ed Exp $
*/

#include <nc_tests.h>
#include <netcdf.h>

/* Test everything for classic and 64-bit offsetfiles. If netcdf-4 is
 * included, that means another whole round of testing. */
#ifdef USE_NETCDF4
#define NUM_FORMATS (4)
#else
#define NUM_FORMATS (2)
#endif

#define ATT_NAME "Atom"
#define MAX_LEN 7   

static int
test_small_atts(const char *testfile)
{
   int ncid;
   char att[MAX_LEN + 1], att_in[MAX_LEN + 1], source[MAX_LEN + 1] = "0123456";
   int ndims, nvars, natts, unlimdimid;
   size_t len_in;
   int t, f;
   
   /* Run this with and without fill mode. */
   for (f = 0; f < 2; f++)
   {
      /* Create small files with an attribute that grows by one each
       * time. */
      for (t = 1; t < MAX_LEN; t++)
      {
	 /* Create null-terminated text string of correct length. */
	 strncpy(att, source, t);
	 
	 /* Create a file with one attribute. */
	 if (nc_create(testfile, NC_CLOBBER, &ncid)) ERR;
	 if (nc_put_att_text(ncid, NC_GLOBAL, ATT_NAME, t + 1, att)) ERR;
	 if (f && nc_set_fill(ncid, NC_NOFILL, NULL)) ERR;
	 if (nc_close(ncid)) ERR;
	 
	 /* Reopen the file and check it. */
	 if (nc_open(testfile, NC_NOWRITE, &ncid)) ERR;
	 if (nc_inq(ncid, &ndims, &nvars, &natts, &unlimdimid)) ERR;
	 if (ndims != 0 && nvars != 0 && natts != 1 && unlimdimid != -1) ERR;
	 if (nc_inq_attlen(ncid, NC_GLOBAL, ATT_NAME, &len_in)) ERR;
	 if (len_in != t + 1) ERR;
	 if (nc_get_att_text(ncid, NC_GLOBAL, ATT_NAME, att_in)) ERR;
	 if (strncmp(att_in, att, t)) ERR;
	 if (nc_close(ncid)) ERR; 
      }
   }
   return 0;
}

#define DIM1_NAME "Time"
#define DIM2_NAME "DataStrLen"
#define VAR_NAME "Times"
#define STR_LEN 19
#define NUM_VALS 2
#define NDIMS 2
#define TITLE " OUTPUT FROM WRF V2.0.3.1 MODEL"
#define ATT_NAME2 "TITLE"

/* Test a small file with an unlimited dimension. NOTE: Normally I
 * write a NULL terminator for my attributes and text strings, but
 * this reproduces a bug that a fortran user sent us. So string data
 * are written to the file without null terminators. - Ed */
static int
test_small_unlim(const char *testfile)
{
   int ncid, dimids[NDIMS], varid;
   char data[NUM_VALS][STR_LEN + 1], data_in[NUM_VALS][STR_LEN];
   int ndims, nvars, natts, unlimdimid;
   size_t i, start[NDIMS], count[NDIMS];

   /* Create null-terminated text strings of correct length. */
   /*for (i = 0; i < NUM_VALS; i++)
     strcpy(data[i], source);*/
   strcpy(data[0], "2005-04-11_12:00:00");
   strcpy(data[1], "2005-04-11_13:00:00");
   
   /* Create a file with two dimensions, one unlimited, and one
    * var, and a global att. */
   if (nc_create(testfile, NC_CLOBBER, &ncid)) ERR;
   if (nc_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, dimids)) ERR;
   if (nc_def_dim(ncid, DIM2_NAME, STR_LEN, &dimids[1])) ERR;
   if (nc_def_var(ncid, VAR_NAME, NC_CHAR, 2, dimids, &varid)) ERR;
   if (nc_put_att_text(ncid, NC_GLOBAL, ATT_NAME2, strlen(TITLE), TITLE)) ERR;
   if (nc_enddef(ncid)) ERR;

   /* Write some records of var data. */
   count[0] = 1;
   count[1] = STR_LEN;
   start[1] = 0;
   for (start[0] = 0; start[0] < NUM_VALS; start[0]++)
      if (nc_put_vara_text(ncid, varid, start, count, data[start[0]])) ERR;

   /* We're done! */
   if (nc_close(ncid)) ERR;
   
   /* Reopen the file and check it. */
   if (nc_open(testfile, NC_NOWRITE, &ncid)) ERR;
   if (nc_inq(ncid, &ndims, &nvars, &natts, &unlimdimid)) ERR;
   if (ndims != 2 && nvars != 1 && natts != 0 && unlimdimid != 0) ERR;
   if (nc_get_var_text(ncid, varid, (char *)data_in)) ERR;
   for (i = 0; i < NUM_VALS; i++)
      if (strncmp(data[i], data_in[i], STR_LEN)) ERR;
   if (nc_close(ncid)) ERR; 
   return 0;
}

/* Test a small file with a fixed dimension. */
static int
test_small_fixed(const char *testfile)
{
   int ncid, dimids[NDIMS], varid;
   char data[NUM_VALS][STR_LEN + 1], data_in[NUM_VALS][STR_LEN];
   int ndims, nvars, natts, unlimdimid;
   size_t i, start[NDIMS], count[NDIMS];

   /* Create null-terminated text strings of correct length. */
   /*for (i = 0; i < NUM_VALS; i++)
     strcpy(data[i], source);*/
   strcpy(data[0], "2005-04-11_12:00:00");
   strcpy(data[1], "2005-04-11_13:00:00");
   
   /* Create a file with two dimensions, one unlimited, and one
    * var, and a global att. */
   if (nc_create(testfile, NC_CLOBBER, &ncid)) ERR;
   if (nc_def_dim(ncid, DIM1_NAME, NUM_VALS, dimids)) ERR;
   if (nc_def_dim(ncid, DIM2_NAME, STR_LEN, &dimids[1])) ERR;
   if (nc_def_var(ncid, VAR_NAME, NC_CHAR, NDIMS, dimids, &varid)) ERR;
   if (nc_put_att_text(ncid, NC_GLOBAL, ATT_NAME2, strlen(TITLE), TITLE)) ERR;
   if (nc_enddef(ncid)) ERR;

   /* Write some records of var data. */
   count[0] = 1;
   count[1] = STR_LEN;
   start[1] = 0;
   for (start[0] = 0; start[0] < NUM_VALS; start[0]++)
      if (nc_put_vara_text(ncid, varid, start, count, data[start[0]])) ERR;

   /* We're done! */
   if (nc_close(ncid)) ERR;
   
   /* Reopen the file and check it. */
   if (nc_open(testfile, NC_NOWRITE, &ncid)) ERR;
   if (nc_inq(ncid, &ndims, &nvars, &natts, &unlimdimid)) ERR;
   if (ndims != 2 && nvars != 1 && natts != 0 && unlimdimid != -1) ERR;
   if (nc_get_var_text(ncid, varid, (char *)data_in)) ERR;
   for (i = 0; i < NUM_VALS; i++)
      if (strncmp(data[i], data_in[i], STR_LEN)) ERR;
   if (nc_close(ncid)) ERR; 
   return 0;
}

/* Test a small file with one var. */
static int
test_small_one(const char *testfile)
{
   int ncid, dimid, varid;
   char data = 'h', data_in;
   int ndims, nvars, natts, unlimdimid;
   size_t start[NDIMS], count[NDIMS];

   /* Create a file with one ulimited dimensions, and one var. */
   if (nc_create(testfile, NC_CLOBBER, &ncid)) ERR;
   if (nc_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, &dimid)) ERR;
   if (nc_def_var(ncid, VAR_NAME, NC_CHAR, 1, &dimid, &varid)) ERR;
   if (nc_enddef(ncid)) ERR;

   /* Write one record of var data, a single character. */
   count[0] = 1;
   start[0] = 0;
   if (nc_put_vara_text(ncid, varid, start, count, &data)) ERR;

   /* We're done! */
   if (nc_close(ncid)) ERR;
   
   /* Reopen the file and check it. */
   if (nc_open(testfile, NC_NOWRITE, &ncid)) ERR;
   if (nc_inq(ncid, &ndims, &nvars, &natts, &unlimdimid)) ERR;
   if (ndims != 1 && nvars != 1 && natts != 0 && unlimdimid != 0) ERR;
   if (nc_get_var_text(ncid, varid, &data_in)) ERR;
   if (data_in != data) ERR;
   if (nc_close(ncid)) ERR; 
   return 0;
}

#define ONE_DIM 1
#define MAX_RECS 10

/* Test a small file with one record var, which grows. */
static int
test_one_growing(const char *testfile)
{
   int ncid, dimid, varid;
   char data[MAX_RECS], data_in;
   size_t start[ONE_DIM], count[ONE_DIM], index[ONE_DIM], len_in;
   int r, f;

   /* Create some phoney data. */
   for (data[0] = 'a', r = 1; r < MAX_RECS; r++)
      data[r] = data[r - 1] + 1;

   /* Run this with and without fill mode. */
   for (f = 0; f < 2; f++)
   {
      /* Create a file with one ulimited dimensions, and one var. */
      if (nc_create(testfile, NC_CLOBBER, &ncid)) ERR;
      if (nc_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, &dimid)) ERR;
      if (nc_def_var(ncid, VAR_NAME, NC_CHAR, 1, &dimid, &varid)) ERR;
      if (nc_close(ncid)) ERR;

      /* Normally one would not close and reopen the file for each
       * record, but I am giving the library a little work-out here... */
      for (r = 0; r < MAX_RECS; r++)
      {
	 /* Write one record of var data, a single character. */
	 if (nc_open(testfile, NC_WRITE, &ncid)) ERR;
	 if (f && nc_set_fill(ncid, NC_NOFILL, NULL)) ERR;
	 count[0] = 1;
	 start[0] = r;
	 if (nc_put_vara_text(ncid, varid, start, count, &data[r])) ERR;
	 if (nc_close(ncid)) ERR;
      
	 /* Reopen the file and check it. */
	 if (nc_open(testfile, NC_NOWRITE, &ncid)) ERR;
	 if (nc_inq_dimlen(ncid, 0, &len_in)) ERR;
	 if (len_in != r + 1) ERR;
	 index[0] = r;
	 if (nc_get_var1_text(ncid, 0, index, &data_in)) ERR;
	 if (data_in != data[r]) ERR;
	 if (nc_close(ncid)) ERR; 
      } /* Next record. */
   }
   return 0;
}

#define ONE_DIM 1
#define MAX_RECS 10

/* Test a small file with one record var, which grows, and has
 * attributes. */
static int
test_one_growing_with_att(const char *testfile)
{
   int ncid, dimid, varid;
   char data[MAX_RECS], data_in;
   char att_name[NC_MAX_NAME + 1];
   size_t start[ONE_DIM], count[ONE_DIM], index[ONE_DIM], len_in;
   int r;

   /* Create a file with one ulimited dimensions, and one var. */
   if (nc_create(testfile, NC_CLOBBER, &ncid)) ERR;
   if (nc_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, &dimid)) ERR;
   if (nc_def_var(ncid, VAR_NAME, NC_CHAR, 1, &dimid, &varid)) ERR;
   if (nc_close(ncid)) ERR;

   /* Create some phoney data. */
   for (data[0] = 'a', r = 1; r < MAX_RECS; r++)
      data[r] = data[r - 1] + 1;

   /* Normally one would not close and reopen the file for each
    * record, nor add an attribute each time I add a record, but I am
    * giving the library a little work-out here... */
   for (r = 0; r < MAX_RECS; r++)
   {
      /* Write one record of var data, a single character. */
      if (nc_open(testfile, NC_WRITE, &ncid)) ERR;
      count[0] = 1;
      start[0] = r;
      if (nc_put_vara_text(ncid, varid, start, count, &data[r])) ERR;
      sprintf(att_name, "a_%d", data[r]);
      if (nc_redef(ncid)) ERR;
      if (nc_put_att_text(ncid, varid, att_name, 1, &data[r])) ERR;
      if (nc_close(ncid)) ERR;
      
      /* Reopen the file and check it. */
      if (nc_open(testfile, NC_NOWRITE, &ncid)) ERR;
      if (nc_inq_dimlen(ncid, 0, &len_in)) ERR;
      if (len_in != r + 1) ERR;
      index[0] = r;
      if (nc_get_var1_text(ncid, 0, index, &data_in)) ERR;
      if (data_in != data[r]) ERR;
      if (nc_get_att_text(ncid, varid, att_name, &data_in)) ERR;
      if (data_in != data[r]) ERR;
      if (nc_close(ncid)) ERR; 
   } /* Next record. */
   return 0;
}

#define VAR_NAME2 "var2"
#define NUM_VARS 2

/* Test a small file with two record vars, which grow, and has
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
   if (nc_create(testfile, NC_CLOBBER, &ncid)) ERR;
   if (nc_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, &dimid)) ERR;
   if (nc_def_var(ncid, VAR_NAME, NC_CHAR, 1, &dimid, &varid[0])) ERR;
   if (nc_def_var(ncid, VAR_NAME2, NC_CHAR, 1, &dimid, &varid[1])) ERR;
   if (nc_close(ncid)) ERR;

   /* Create some phoney data. */
   for (data[0] = 'a', r = 1; r < MAX_RECS; r++)
      data[r] = data[r - 1] + 1;

   /* Normally one would not close and reopen the file for each
    * record, nor add an attribute each time I add a record, but I am
    * giving the library a little work-out here... */
   for (r = 0; r < MAX_RECS; r++)
   {
      /* Write one record of var data, a single character. */
      if (nc_open(testfile, NC_WRITE, &ncid)) ERR;
      count[0] = 1;
      start[0] = r;
      sprintf(att_name, "a_%d", data[r]);
      for (v = 0; v < NUM_VARS; v++)
      {
	 if (nc_put_vara_text(ncid, varid[v], start, count, &data[r])) ERR;
	 if (nc_redef(ncid)) ERR;
	 if (nc_put_att_text(ncid, varid[v], att_name, 1, &data[r])) ERR;
	 if (nc_enddef(ncid)) ERR;
      }
      if (nc_close(ncid)) ERR;
      
      /* Reopen the file and check it. */
      if (nc_open(testfile, NC_NOWRITE, &ncid)) ERR;
      if (nc_inq_dimlen(ncid, 0, &len_in)) ERR;
      if (len_in != r + 1) ERR;
      index[0] = r;
      for (v = 0; v < NUM_VARS; v++)
      {
	 if (nc_get_var1_text(ncid, varid[v], index, &data_in)) ERR;
	 if (data_in != data[r]) ERR;
      }
      if (nc_close(ncid)) ERR; 
   } /* Next record. */
   return 0;
}

/* Test a small file with one var and one att. */
static int
test_one_with_att(const char *testfile)
{
   int ncid, dimid, varid;
   char data = 'h', data_in;
   int ndims, nvars, natts, unlimdimid;
   size_t start[NDIMS], count[NDIMS];

   /* Create a file with one ulimited dimensions, and one var. */
   if (nc_create(testfile, NC_CLOBBER, &ncid)) ERR;
   if (nc_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, &dimid)) ERR;
   if (nc_def_var(ncid, VAR_NAME, NC_CHAR, 1, &dimid, &varid)) ERR;
   if (nc_put_att_text(ncid, NC_GLOBAL, ATT_NAME, 1, &data)) ERR;
   if (nc_enddef(ncid)) ERR;

   /* Write one record of var data, a single character. */
   count[0] = 1;
   start[0] = 0;
   if (nc_put_vara_text(ncid, varid, start, count, &data)) ERR;

   /* We're done! */
   if (nc_close(ncid)) ERR;
   
   /* Reopen the file and check it. */
   if (nc_open(testfile, NC_NOWRITE, &ncid)) ERR;
   if (nc_inq(ncid, &ndims, &nvars, &natts, &unlimdimid)) ERR;
   if (ndims != 1 && nvars != 1 && natts != 0 && unlimdimid != 0) ERR;
   if (nc_get_var_text(ncid, varid, &data_in)) ERR;
   if (data_in != data) ERR;
   if (nc_get_att_text(ncid, NC_GLOBAL, ATT_NAME, &data_in)) ERR;
   if (data_in != data) ERR;
   if (nc_close(ncid)) ERR; 
   return 0;
}

int
main(int argc, char **argv)
{
   int i;
   char testfile[NC_MAX_NAME + 1];

   printf("\n*** Testing small files.\n");
   /*nc_set_log_level(3);*/

   /* Go thru formats and run all tests for each of two (for netCDF-3
    * only builds), or 4 (for netCDF-4 builds) different formats. */
   for (i = NUM_FORMATS; i >= 1; i--)
   {
      switch (i) 
      {
	 case NC_FORMAT_CLASSIC:
	    nc_set_default_format(NC_FORMAT_CLASSIC, NULL);
	    printf("Switching to netCDF classic format.\n");
	    strcpy(testfile, "tst_small_classic.nc");
	    break;
	 case NC_FORMAT_64BIT:
	    nc_set_default_format(NC_FORMAT_64BIT, NULL);
	    printf("Switching to 64-bit offset format.\n");
	    strcpy(testfile, "tst_small_64bit.nc");
	    break;
#ifdef USE_NETCDF4
	 case NC_FORMAT_NETCDF4_CLASSIC:
	    nc_set_default_format(NC_FORMAT_NETCDF4_CLASSIC, NULL);
	    strcpy(testfile, "tst_small_netcdf4_classic.nc");
	    printf("Switching to netCDF-4 format (with NC_CLASSIC_MODEL).\n");
	    break;
	 case NC_FORMAT_NETCDF4: /* actually it's _CLASSIC. */
	    nc_set_default_format(NC_FORMAT_NETCDF4, NULL);
	    strcpy(testfile, "tst_small_netcdf4.nc");
	    printf("Switching to netCDF-4 format.\n");
	    break;
#endif
	 default:
	    printf("Unexpected format!\n");
	    return 2;
      }

      printf("*** testing simple small file with a global attribute...");
      test_small_atts(testfile);
      SUMMARIZE_ERR;
      
      printf("*** testing simple small file with fixed dimensions...");
      test_small_fixed(testfile);
      SUMMARIZE_ERR;

      printf("*** testing simple small file with an unlimited dimension...");
      test_small_unlim(testfile);
      SUMMARIZE_ERR;
      
      printf("*** testing small file with one variable...");
      test_small_one(testfile);
      SUMMARIZE_ERR;
      
      printf("*** testing small file with one variable and one att...");
      test_one_with_att(testfile);
      SUMMARIZE_ERR;
      
      printf("*** testing small file with one record variable, which grows...");
      test_one_growing(testfile);
      SUMMARIZE_ERR;

      printf("*** testing small file with one growing record "
	     "variable, with attributes added...");
      test_one_growing_with_att(testfile);
      SUMMARIZE_ERR;

      printf("*** testing small file with two growing record "
	     "variables, with attributes added...");
      test_two_growing_with_att(testfile);
      SUMMARIZE_ERR;
   }

   FINAL_RESULTS;
}

