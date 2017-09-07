/*
 *  Copyright (C) 2014, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 */
/* $Id: tst_small.c 2744 2016-12-28 16:25:22Z wkliao $ */

/* This program is based on the test program tst_small.c of the netCDF package */

/* This is part of the netCDF package. Copyright 2005 University
   Corporation for Atmospheric Research/Unidata See COPYRIGHT file for
   conditions of use. See www.unidata.ucar.edu for more info.

   Test small files.
*/

#include <stdio.h>
#include <string.h>
#include <libgen.h> /* basename() */
#include <mpi.h>
#include <pnetcdf.h>

#include <testutils.h>

/* Test everything for classic, 64-bit offseti, and 64-bit data files. */
#define NUM_FORMATS 3

#define ATT_NAME "Atom"
#define MAX_LEN 7   

#define ERR {if (err != NC_NOERR) {printf("Error at %s line %d: %s\n",__func__,__LINE__,ncmpi_strerror(err)); return 1;}}

static int
test_small_atts(const char *testfile, int cmode)
{
   int ncid, err;
   char att[MAX_LEN + 1], att_in[MAX_LEN + 1], source[MAX_LEN + 1] = "0123456";
   int ndims, nvars, natts, unlimdimid;
   MPI_Offset len_in;
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
         att[t] = '\0';
	 
	 /* Create a file with one attribute. */
         err = ncmpi_create(MPI_COMM_WORLD, testfile,cmode, MPI_INFO_NULL, &ncid); ERR
	 err = ncmpi_put_att_text(ncid, NC_GLOBAL, ATT_NAME, t + 1, att); ERR
	 if (f) { err=ncmpi_set_fill(ncid, NC_NOFILL, NULL); ERR}
	 err=ncmpi_close(ncid); ERR;
	 
	 /* Reopen the file and check it. */
         err=ncmpi_open(MPI_COMM_WORLD, testfile, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
	 err=ncmpi_inq(ncid, &ndims, &nvars, &natts, &unlimdimid); ERR
	 if (ndims != 0 && nvars != 0 && natts != 1 && unlimdimid != -1) {printf("Error at line %d\n",__LINE__);return 1;}
	 err=ncmpi_inq_attlen(ncid, NC_GLOBAL, ATT_NAME, &len_in); ERR
	 if (len_in != t + 1) {printf("Error at line %d\n",__LINE__);return 1;}
	 err=ncmpi_get_att_text(ncid, NC_GLOBAL, ATT_NAME, att_in); ERR
	 if (strncmp(att_in, att, t)) {printf("Error at line %d\n",__LINE__);return 1;}
	 err=ncmpi_close(ncid); ERR
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
test_small_unlim(const char *testfile, int cmode)
{
   int i, err, ncid, dimids[NDIMS], varid;
   char data[NUM_VALS][STR_LEN + 1], data_in[NUM_VALS][STR_LEN];
   int ndims, nvars, natts, unlimdimid;
   MPI_Offset start[NDIMS], count[NDIMS];

   /* Create null-terminated text strings of correct length. */
   /*for (i = 0; i < NUM_VALS; i++)
     strcpy(data[i], source);*/
   strcpy(data[0], "2005-04-11_12:00:00");
   strcpy(data[1], "2005-04-11_13:00:00");
   
   /* Create a file with two dimensions, one unlimited, and one
    * var, and a global att. */
   err=ncmpi_create(MPI_COMM_WORLD, testfile,cmode, MPI_INFO_NULL, &ncid); ERR
   err=ncmpi_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, dimids); ERR
   err=ncmpi_def_dim(ncid, DIM2_NAME, STR_LEN, &dimids[1]); ERR
   err=ncmpi_def_var(ncid, VAR_NAME, NC_CHAR, 2, dimids, &varid); ERR
   err=ncmpi_put_att_text(ncid, NC_GLOBAL, ATT_NAME2, strlen(TITLE), TITLE); ERR
   err=ncmpi_enddef(ncid); ERR

   /* Write some records of var data. */
   count[0] = 1;
   count[1] = STR_LEN;
   start[1] = 0;
   for (start[0] = 0; start[0] < NUM_VALS; start[0]++) {
      err=ncmpi_put_vara_text_all(ncid, varid, start, count, data[start[0]]); ERR
   }

   /* We're done! */
   err=ncmpi_close(ncid); ERR
   
   /* Reopen the file and check it. */
   err=ncmpi_open(MPI_COMM_WORLD, testfile, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
   err=ncmpi_inq(ncid, &ndims, &nvars, &natts, &unlimdimid); ERR
   if (ndims != 2 && nvars != 1 && natts != 0 && unlimdimid != 0) {printf("Error at line %d\n",__LINE__);return 1;}
   err=ncmpi_get_var_text_all(ncid, varid, (char *)data_in); ERR
   for (i = 0; i < NUM_VALS; i++)
      /* if (strncmp(data[i], data_in[i], STR_LEN)) {printf("Error at line %d\n",__LINE__);return 1;} */
      if (strncmp(data[i], data_in[i], STR_LEN)) {
printf("i=%d data=%s data_in=%s\n",i,data[i],data_in[i]);
      }
   err=ncmpi_close(ncid); ERR
   return 0;
}

/* Test a small file with a fixed dimension. */
static int
test_small_fixed(const char *testfile, int cmode)
{
   int i, err, ncid, dimids[NDIMS], varid;
   char data[NUM_VALS][STR_LEN + 1], data_in[NUM_VALS][STR_LEN];
   int ndims, nvars, natts, unlimdimid;
   MPI_Offset start[NDIMS], count[NDIMS];

   /* Create null-terminated text strings of correct length. */
   /*for (i = 0; i < NUM_VALS; i++)
     strcpy(data[i], source);*/
   strcpy(data[0], "2005-04-11_12:00:00");
   strcpy(data[1], "2005-04-11_13:00:00");
   
   /* Create a file with two dimensions, one unlimited, and one
    * var, and a global att. */
   err=ncmpi_create(MPI_COMM_WORLD, testfile,cmode, MPI_INFO_NULL, &ncid); ERR
   err=ncmpi_def_dim(ncid, DIM1_NAME, NUM_VALS, dimids); ERR
   err=ncmpi_def_dim(ncid, DIM2_NAME, STR_LEN, &dimids[1]); ERR
   err=ncmpi_def_var(ncid, VAR_NAME, NC_CHAR, NDIMS, dimids, &varid); ERR
   err=ncmpi_put_att_text(ncid, NC_GLOBAL, ATT_NAME2, strlen(TITLE), TITLE); ERR
   err=ncmpi_enddef(ncid); ERR

   /* Write some records of var data. */
   count[0] = 1;
   count[1] = STR_LEN;
   start[1] = 0;
   for (start[0] = 0; start[0] < NUM_VALS; start[0]++) {
      err=ncmpi_put_vara_text_all(ncid, varid, start, count, data[start[0]]); ERR
   }

   /* We're done! */
   err=ncmpi_close(ncid); ERR
   
   /* Reopen the file and check it. */
   err=ncmpi_open(MPI_COMM_WORLD, testfile, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
   err=ncmpi_inq(ncid, &ndims, &nvars, &natts, &unlimdimid); ERR
   if (ndims != 2 && nvars != 1 && natts != 0 && unlimdimid != -1) {printf("Error at line %d\n",__LINE__);return 1;}
   err=ncmpi_get_var_text_all(ncid, varid, (char *)data_in); ERR
   for (i = 0; i < NUM_VALS; i++)
      if (strncmp(data[i], data_in[i], STR_LEN)) {printf("Error at line %d\n",__LINE__);return 1;}
   err=ncmpi_close(ncid); ERR
   return 0;
}

/* Test a small file with one var. */
static int
test_small_one(const char *testfile, int cmode)
{
   int err, ncid, dimid, varid;
   char data = 'h', data_in;
   int ndims, nvars, natts, unlimdimid;
   MPI_Offset start[NDIMS], count[NDIMS];

   /* Create a file with one ulimited dimensions, and one var. */
   err=ncmpi_create(MPI_COMM_WORLD, testfile,cmode, MPI_INFO_NULL, &ncid); ERR
   err=ncmpi_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, &dimid); ERR
   err=ncmpi_def_var(ncid, VAR_NAME, NC_CHAR, 1, &dimid, &varid); ERR
   err=ncmpi_enddef(ncid); ERR

   /* Write one record of var data, a single character. */
   count[0] = 1;
   start[0] = 0;
   err=ncmpi_put_vara_text_all(ncid, varid, start, count, &data); ERR

   /* We're done! */
   err=ncmpi_close(ncid); ERR
   
   /* Reopen the file and check it. */
   err=ncmpi_open(MPI_COMM_WORLD, testfile, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
   err=ncmpi_inq(ncid, &ndims, &nvars, &natts, &unlimdimid); ERR
   if (ndims != 1 && nvars != 1 && natts != 0 && unlimdimid != 0) {printf("Error at line %d\n",__LINE__);return 1;}
   err=ncmpi_get_var_text_all(ncid, varid, &data_in); ERR
   if (data_in != data) {printf("Error at line %d\n",__LINE__);return 1;}
   err=ncmpi_close(ncid); ERR
   return 0;
}

#define ONE_DIM 1
#define MAX_RECS 10

/* Test a small file with one record var, which grows. */
static int
test_one_growing(const char *testfile, int cmode)
{
   int err, ncid, dimid, varid;
   char data[MAX_RECS], data_in;
   MPI_Offset start[ONE_DIM], count[ONE_DIM], index[ONE_DIM], len_in;
   int r, f;

   /* Create some phoney data. */
   for (data[0] = 'a', r = 1; r < MAX_RECS; r++)
      data[r] = data[r - 1] + 1;

   /* Run this with and without fill mode. */
   for (f = 0; f < 2; f++)
   {
      /* Create a file with one ulimited dimensions, and one var. */
      err=ncmpi_create(MPI_COMM_WORLD, testfile,cmode, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, &dimid); ERR
      err=ncmpi_def_var(ncid, VAR_NAME, NC_CHAR, 1, &dimid, &varid); ERR
      err=ncmpi_close(ncid); ERR

      /* Normally one would not close and reopen the file for each
       * record, but I am giving the library a little work-out here... */
      for (r = 0; r < MAX_RECS; r++)
      {
	 /* Write one record of var data, a single character. */
         err=ncmpi_open(MPI_COMM_WORLD, testfile, NC_WRITE, MPI_INFO_NULL, &ncid); ERR
	 /* if (f) { err=ncmpi_set_fill(ncid, NC_NOFILL, NULL); ERR} */
	 count[0] = 1;
	 start[0] = r;
	 err=ncmpi_put_vara_text_all(ncid, varid, start, count, &data[r]); ERR
	 err=ncmpi_close(ncid); ERR
      
	 /* Reopen the file and check it. */
         err=ncmpi_open(MPI_COMM_WORLD, testfile, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
	 err=ncmpi_inq_dimlen(ncid, 0, &len_in); ERR
	 if (len_in != r + 1) {printf("Error at line %d\n",__LINE__);return 1;}
	 index[0] = r;
	 err=ncmpi_begin_indep_data(ncid); ERR
	 err=ncmpi_get_var1_text(ncid, 0, index, &data_in); ERR
	 if (data_in != data[r]) {printf("Error at line %d\n",__LINE__);return 1;}
	 err=ncmpi_close(ncid); ERR
      } /* Next record. */
   }
   return 0;
}

#define ONE_DIM 1
#define MAX_RECS 10

/* Test a small file with one record var, which grows, and has
 * attributes. */
static int
test_one_growing_with_att(const char *testfile, int cmode)
{
   int err, ncid, dimid, varid;
   char data[MAX_RECS], data_in;
   char att_name[NC_MAX_NAME + 1];
   MPI_Offset start[ONE_DIM], count[ONE_DIM], index[ONE_DIM], len_in;
   int r;

   /* Create a file with one ulimited dimensions, and one var. */
   err=ncmpi_create(MPI_COMM_WORLD, testfile,cmode, MPI_INFO_NULL, &ncid); ERR
   err=ncmpi_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, &dimid); ERR
   err=ncmpi_def_var(ncid, VAR_NAME, NC_CHAR, 1, &dimid, &varid); ERR
   err=ncmpi_close(ncid); ERR

   /* Create some phoney data. */
   for (data[0] = 'a', r = 1; r < MAX_RECS; r++)
      data[r] = data[r - 1] + 1;

   /* Normally one would not close and reopen the file for each
    * record, nor add an attribute each time I add a record, but I am
    * giving the library a little work-out here... */
   for (r = 0; r < MAX_RECS; r++)
   {
      /* Write one record of var data, a single character. */
      err=ncmpi_open(MPI_COMM_WORLD, testfile, NC_WRITE, MPI_INFO_NULL, &ncid); ERR
      count[0] = 1;
      start[0] = r;
      err=ncmpi_put_vara_text_all(ncid, varid, start, count, &data[r]); ERR
      sprintf(att_name, "a_%d", data[r]);
      err=ncmpi_redef(ncid); ERR
      err=ncmpi_put_att_text(ncid, varid, att_name, 1, &data[r]); ERR
      err=ncmpi_close(ncid); ERR
      
      /* Reopen the file and check it. */
      err=ncmpi_open(MPI_COMM_WORLD, testfile, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_inq_dimlen(ncid, 0, &len_in); ERR
      if (len_in != r + 1) {printf("Error at line %d\n",__LINE__);return 1;}
      index[0] = r;
      err=ncmpi_begin_indep_data(ncid); ERR
      err=ncmpi_get_var1_text(ncid, 0, index, &data_in); ERR
      if (data_in != data[r]) {printf("Error at line %d\n",__LINE__);return 1;}
      err=ncmpi_get_att_text(ncid, varid, att_name, &data_in); ERR
      if (data_in != data[r]) {printf("Error at line %d\n",__LINE__);return 1;}
      err=ncmpi_close(ncid); ERR
   } /* Next record. */
   return 0;
}

#define VAR_NAME2 "var2"
#define NUM_VARS 2

/* Test a small file with two record vars, which grow, and has
 * attributes added. */
static int
test_two_growing_with_att(const char *testfile, int cmode)
{
   int err, ncid, dimid, varid[NUM_VARS];
   char data[MAX_RECS], data_in;
   char att_name[NC_MAX_NAME + 1];
   MPI_Offset start[ONE_DIM], count[ONE_DIM], index[ONE_DIM], len_in;
   int v, r;

   /* Create a file with one ulimited dimensions, and one var. */
   err=ncmpi_create(MPI_COMM_WORLD, testfile,cmode, MPI_INFO_NULL, &ncid); ERR
   err=ncmpi_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, &dimid); ERR
   err=ncmpi_def_var(ncid, VAR_NAME, NC_CHAR, 1, &dimid, &varid[0]); ERR
   err=ncmpi_def_var(ncid, VAR_NAME2, NC_CHAR, 1, &dimid, &varid[1]); ERR
   err=ncmpi_close(ncid); ERR

   /* Create some phoney data. */
   for (data[0] = 'a', r = 1; r < MAX_RECS; r++)
      data[r] = data[r - 1] + 1;

   /* Normally one would not close and reopen the file for each
    * record, nor add an attribute each time I add a record, but I am
    * giving the library a little work-out here... */
   for (r = 0; r < MAX_RECS; r++)
   {
      /* Write one record of var data, a single character. */
      err=ncmpi_open(MPI_COMM_WORLD, testfile, NC_WRITE, MPI_INFO_NULL, &ncid); ERR
      count[0] = 1;
      start[0] = r;
      sprintf(att_name, "a_%d", data[r]);
      for (v = 0; v < NUM_VARS; v++)
      {
	 err=ncmpi_put_vara_text_all(ncid, varid[v], start, count, &data[r]); ERR
	 err=ncmpi_redef(ncid); ERR
	 err=ncmpi_put_att_text(ncid, varid[v], att_name, 1, &data[r]); ERR
	 err=ncmpi_enddef(ncid); ERR
      }
      err=ncmpi_close(ncid); ERR
      
      /* Reopen the file and check it. */
      err=ncmpi_open(MPI_COMM_WORLD, testfile, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_inq_dimlen(ncid, 0, &len_in); ERR
      if (len_in != r + 1) {printf("Error at line %d\n",__LINE__);return 1;}
      index[0] = r;
      err=ncmpi_begin_indep_data(ncid); ERR
      for (v = 0; v < NUM_VARS; v++)
      {
	 err=ncmpi_get_var1_text(ncid, varid[v], index, &data_in); ERR
	 if (data_in != data[r]) {printf("Error at line %d\n",__LINE__);return 1;}
      }
      err=ncmpi_close(ncid); ERR
   } /* Next record. */
   return 0;
}

/* Test a small file with one var and one att. */
static int
test_one_with_att(const char *testfile, int cmode)
{
   int err, ncid, dimid, varid;
   char data = 'h', data_in;
   int ndims, nvars, natts, unlimdimid;
   MPI_Offset start[NDIMS], count[NDIMS];

   /* Create a file with one ulimited dimensions, and one var. */
   err=ncmpi_create(MPI_COMM_WORLD, testfile,cmode, MPI_INFO_NULL, &ncid); ERR
   err=ncmpi_def_dim(ncid, DIM1_NAME, NC_UNLIMITED, &dimid); ERR
   err=ncmpi_def_var(ncid, VAR_NAME, NC_CHAR, 1, &dimid, &varid); ERR
   err=ncmpi_put_att_text(ncid, NC_GLOBAL, ATT_NAME, 1, &data); ERR
   err=ncmpi_enddef(ncid); ERR

   /* Write one record of var data, a single character. */
   count[0] = 1;
   start[0] = 0;
   err=ncmpi_put_vara_text_all(ncid, varid, start, count, &data); ERR

   /* We're done! */
   err=ncmpi_close(ncid); ERR
   
   /* Reopen the file and check it. */
   err=ncmpi_open(MPI_COMM_WORLD, testfile, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
   err=ncmpi_inq(ncid, &ndims, &nvars, &natts, &unlimdimid); ERR
   if (ndims != 1 && nvars != 1 && natts != 0 && unlimdimid != 0) {printf("Error at line %d\n",__LINE__);return 1;}
   err=ncmpi_get_var_text_all(ncid, varid, &data_in); ERR
   if (data_in != data) {printf("Error at line %d\n",__LINE__);return 1;}
   err=ncmpi_get_att_text(ncid, NC_GLOBAL, ATT_NAME, &data_in); ERR
   if (data_in != data) {printf("Error at line %d\n",__LINE__);return 1;}
   err=ncmpi_close(ncid); ERR
   return 0;
}

int main(int argc, char *argv[])
{
    char filename[256];
    int i, rank, nprocs, err, nerrs=0;
    int cmode[NUM_FORMATS]={0, NC_64BIT_OFFSET, NC_64BIT_DATA};

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (argc > 2) {
        if (!rank) printf("Usage: %s [filename]\n",argv[0]);
        MPI_Finalize();
        return 0;
    }
    if (argc == 2) snprintf(filename, 256, "%s", argv[1]);
    else           strcpy(filename, "testfile.nc");
    MPI_Bcast(filename, 256, MPI_CHAR, 0, MPI_COMM_WORLD);

    char *cmd_str = (char*)malloc(strlen(argv[0]) + 256);
    sprintf(cmd_str, "*** TESTING C   %s for emulating netCDF tst_small ", basename(argv[0]));
    if (rank == 0) printf("%-66s ------ ", cmd_str);
    free(cmd_str);

    for (i=0; i<NUM_FORMATS; i++) {
#ifdef DEBUG
      printf("*** testing simple small file with a global attribute...");
#endif
      nerrs += test_small_atts(filename, cmode[i]|NC_CLOBBER);
      
#ifdef DEBUG
      printf("*** testing simple small file with fixed dimensions...");
#endif
      nerrs += test_small_fixed(filename, cmode[i]|NC_CLOBBER);

#ifdef DEBUG
      printf("*** testing simple small file with an unlimited dimension...");
#endif
      nerrs += test_small_unlim(filename, cmode[i]|NC_CLOBBER);
      
#ifdef DEBUG
      printf("*** testing small file with one variable...");
#endif
      nerrs += test_small_one(filename, cmode[i]|NC_CLOBBER);
      
#ifdef DEBUG
      printf("*** testing small file with one variable and one att...");
#endif
      nerrs += test_one_with_att(filename, cmode[i]|NC_CLOBBER);
      
#ifdef DEBUG
      printf("*** testing small file with one record variable, which grows...");
#endif
      nerrs += test_one_growing(filename, cmode[i]|NC_CLOBBER);

#ifdef DEBUG
      printf("*** testing small file with one growing record "
	     "variable, with attributes added...");
#endif
      nerrs += test_one_growing_with_att(filename, cmode[i]|NC_CLOBBER);

#ifdef DEBUG
      if (verbose) printf("*** testing small file with two growing record "
	     "variables, with attributes added...");
#endif
      nerrs += test_two_growing_with_att(filename, cmode[i]|NC_CLOBBER);
   }

    /* check if PnetCDF freed all internal malloc */
    MPI_Offset malloc_size, sum_size;
    err = ncmpi_inq_malloc_size(&malloc_size);
    if (err == NC_NOERR) {
        MPI_Reduce(&malloc_size, &sum_size, 1, MPI_OFFSET, MPI_SUM, 0, MPI_COMM_WORLD);
        if (rank == 0 && sum_size > 0)
            printf("heap memory allocated by PnetCDF internally has %lld bytes yet to be freed\n",
                   sum_size);
    }

    MPI_Allreduce(MPI_IN_PLACE, &nerrs, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (rank == 0) {
        if (nerrs) printf(FAIL_STR,nerrs);
        else       printf(PASS_STR);
    }

    MPI_Finalize();
    return 0;
}

