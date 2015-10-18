/*********************************************************************
 *   Copyright 1996-2005, UCAR/Unidata
 *   See COPYRIGHT file for copying and redistribution conditions.
 *   $Id: nc_test.c,v 1.44 2008/10/20 01:48:08 ed Exp $
 *********************************************************************/

#include "tests.h"

/*
 * Test driver for netCDF-3 interface.  This program performs tests against
 * the netCDF-3 specification for all user-level functions in an
 * implementation of the netCDF library.
 *
 * Files:
 * The read-only tests read files:
 *     test.nc (see below)
 *     tests.h (used merely as an example of a non-netCDF file)
 * 
 * The write tests 
 *     read test.nc (see below) 
 *     write scratch.nc (deleted after each test)
 * 
 * The file test.nc is created by running nc_test with the -c (create) option.
 * It is described by the following global variables.
 */

/* 
 * global variables (defined by function init_gvars) describing file test.nc
 */
char dim_name[NDIMS][3];
size_t dim_len[NDIMS];
char var_name[NVARS][2+MAX_RANK];
nc_type var_type[NVARS];
size_t var_rank[NVARS];
int var_dimid[NVARS][MAX_RANK];
size_t var_shape[NVARS][MAX_RANK];
size_t var_nels[NVARS];
size_t var_natts[NVARS];
char att_name[NVARS][MAX_NATTS][2];
char gatt_name[NGATTS][3];
nc_type att_type[NVARS][NGATTS];
nc_type gatt_type[NGATTS];
size_t att_len[NVARS][MAX_NATTS];
size_t gatt_len[NGATTS];

/* 
 * command-line options
 */
int  verbose;		/* if 1, print details of tests */
int  max_nmpt;		/* max. number of messages per test */

/* 
 * Misc. global variables
 */
int  nfails;		/* number of failures in specific test */
char testfile[NC_MAX_NAME];
char scratch[] = "scratch.nc";  /* writable scratch file */

#define NC_TEST(func) \
    print( "*** testing " #func " ... ");\
    nfails = 0;\
    test_ ## func();\
    nfailsTotal += nfails;\
    if (verbose) \
	print("\n"); \
    if ( nfails == 0) \
        print( "ok\n");\
    else\
        print( "\n\t### %d FAILURES TESTING %s! ###\n", nfails, #func)


#if 1		/* both CRAY MPP and OSF/1 Alpha systems need this */
#include <signal.h>
#endif /* T90 */

/* Test everything for classic and 64-bit offsetfiles. If netcdf-4 is
 * included, that means another whole round of testing. */
#ifdef USE_NETCDF4
#define NUM_FORMATS (3)
#else
#define NUM_FORMATS (2)
#endif

int
main(int argc, char *argv[])
{
    int i;
    int  nfailsTotal = 0;        /* total number of failures */

    /* Both CRAY MPP and OSF/1 Alpha systems need this.  Some of the
     * extreme test assignments in this program trigger floating point
     * exceptions on CRAY T90
     */
    (void) signal(SIGFPE, SIG_IGN);

    verbose = 0;
    max_nmpt = 8;

    /* Initialize global variables defining test file */
    init_gvars();

    /* If you uncomment the nc_set_log_level line, you will get a lot
     * of debugging info. If you set the number higher, you'll get
     * more. 6 is max, 0 shows only errors. 3 is a good place to
     * start. */
    /*nc_set_log_level(3);*/

    fprintf(stderr, "Testing %d different netCDF formats.\n", NUM_FORMATS);

    /* Go thru formats and run all tests for each of two (for netCDF-3
     * only builds), or 3 (for netCDF-4 builds) different formats. Do
     * the netCDF-4 format last, however, because, as an additional
     * test, the ../nc_test4/tst_nc_test_file program looks at the
     * output of this program. */
    for (i = 1; i <= NUM_FORMATS; i++)
    {
       switch (i) 
       {
	  case NC_FORMAT_CLASSIC:
	     nc_set_default_format(NC_FORMAT_CLASSIC, NULL);
	     fprintf(stderr, "\n\nSwitching to netCDF classic format.\n");
	     strcpy(testfile, "nc_test_classic.nc");
	     break;
	  case NC_FORMAT_64BIT:
	     nc_set_default_format(NC_FORMAT_64BIT, NULL);
	     fprintf(stderr, "\n\nSwitching to 64-bit offset format.\n");
	     strcpy(testfile, "nc_test_64bit.nc");
	     break;
#ifdef USE_NETCDF4
	  case NC_FORMAT_NETCDF4: /* actually it's _CLASSIC. */
	     nc_set_default_format(NC_FORMAT_NETCDF4_CLASSIC, NULL);
	     strcpy(testfile, "nc_test_netcdf4.nc");
	     fprintf(stderr, "\n\nSwitching to netCDF-4 format (with NC_CLASSIC_MODEL).\n");
	     break;
#endif
	  default:
	     fprintf(stderr, "Unexpected format!\n");
	     return 2;
       }

	/* Write the test file, needed for the read-only tests below. */
       write_file(testfile);

	/* delete any existing scratch netCDF file */
       (void) remove(scratch);

	/* Test read-only functions, using pre-generated test-file */
 	NC_TEST(nc_strerror);
	NC_TEST(nc_open);
	NC_TEST(nc_close);
	NC_TEST(nc_inq);
	NC_TEST(nc_inq_dimid);
	NC_TEST(nc_inq_dim);
	NC_TEST(nc_inq_dimlen);
	NC_TEST(nc_inq_dimname);
	NC_TEST(nc_inq_varid);
	NC_TEST(nc_inq_var);
	NC_TEST(nc_inq_natts);
	NC_TEST(nc_inq_ndims);
	NC_TEST(nc_inq_nvars);
	NC_TEST(nc_inq_unlimdim);
	NC_TEST(nc_inq_vardimid);
	NC_TEST(nc_inq_varname);
	NC_TEST(nc_inq_varnatts);
	NC_TEST(nc_inq_varndims);
	NC_TEST(nc_inq_vartype);
	NC_TEST(nc_get_var_text);
	NC_TEST(nc_get_var_uchar);
	NC_TEST(nc_get_var_schar);
	NC_TEST(nc_get_var_short);
	NC_TEST(nc_get_var_int);
	NC_TEST(nc_get_var_long);
	NC_TEST(nc_get_var_float);
	NC_TEST(nc_get_var_double);
	NC_TEST(nc_get_var1_text);
	NC_TEST(nc_get_var1_uchar);
	NC_TEST(nc_get_var1_schar);
	NC_TEST(nc_get_var1_short);
	NC_TEST(nc_get_var1_int);
	NC_TEST(nc_get_var1_long);
	NC_TEST(nc_get_var1_float);
	NC_TEST(nc_get_var1_double);
	NC_TEST(nc_get_var1);
	NC_TEST(nc_get_vara_text);
	NC_TEST(nc_get_vara_uchar);
	NC_TEST(nc_get_vara_schar);
	NC_TEST(nc_get_vara_short);
	NC_TEST(nc_get_vara_int);
	NC_TEST(nc_get_vara_long);
	NC_TEST(nc_get_vara_float);
	NC_TEST(nc_get_vara_double);
	NC_TEST(nc_get_vara);
	NC_TEST(nc_get_vars_text);
	NC_TEST(nc_get_vars_uchar);
	NC_TEST(nc_get_vars_schar);
	NC_TEST(nc_get_vars_short);
	NC_TEST(nc_get_vars_int);
	NC_TEST(nc_get_vars_long);
	NC_TEST(nc_get_vars_float);
	NC_TEST(nc_get_vars_double);
	NC_TEST(nc_get_vars);
	NC_TEST(nc_get_varm_text);
	NC_TEST(nc_get_varm_uchar);
	NC_TEST(nc_get_varm_schar);
	NC_TEST(nc_get_varm_short);
	NC_TEST(nc_get_varm_int);
	NC_TEST(nc_get_varm_long);
	NC_TEST(nc_get_varm_float);
	NC_TEST(nc_get_varm_double);
	NC_TEST(nc_get_varm);
	NC_TEST(nc_get_att_text);
	NC_TEST(nc_get_att_uchar);
	NC_TEST(nc_get_att_schar);
	NC_TEST(nc_get_att_short);
	NC_TEST(nc_get_att_int);
	NC_TEST(nc_get_att_long);
	NC_TEST(nc_get_att_float);
	NC_TEST(nc_get_att_double);
	NC_TEST(nc_get_att);
	NC_TEST(nc_inq_att);
	NC_TEST(nc_inq_attname);
	NC_TEST(nc_inq_attid);
	NC_TEST(nc_inq_attlen);
	NC_TEST(nc_inq_atttype);

	/* Test write functions */
	NC_TEST(nc_create);
	NC_TEST(nc_redef);
	/* NC_TEST(nc_enddef); *//* redundant */
	NC_TEST(nc_sync);
	NC_TEST(nc_abort);
	NC_TEST(nc_def_dim);
	NC_TEST(nc_rename_dim);
	NC_TEST(nc_def_var);
	NC_TEST(nc_put_var_text);
	NC_TEST(nc_put_var_uchar);
	NC_TEST(nc_put_var_schar);
	NC_TEST(nc_put_var_short);
	NC_TEST(nc_put_var_int);
	NC_TEST(nc_put_var_long);
	NC_TEST(nc_put_var_float);
	NC_TEST(nc_put_var_double);
	NC_TEST(nc_put_var1_text);
	NC_TEST(nc_put_var1_uchar);
	NC_TEST(nc_put_var1_schar);
	NC_TEST(nc_put_var1_short);
	NC_TEST(nc_put_var1_int);
	NC_TEST(nc_put_var1_long);
	NC_TEST(nc_put_var1_float);
	NC_TEST(nc_put_var1_double);
	NC_TEST(nc_put_var1);
	NC_TEST(nc_put_vara_text);
	NC_TEST(nc_put_vara_uchar);
	NC_TEST(nc_put_vara_schar);
	NC_TEST(nc_put_vara_short);
	NC_TEST(nc_put_vara_int);
	NC_TEST(nc_put_vara_long);
	NC_TEST(nc_put_vara_float);
	NC_TEST(nc_put_vara_double);
	NC_TEST(nc_put_vara);
	NC_TEST(nc_put_vars_text);
	NC_TEST(nc_put_vars_uchar);
	NC_TEST(nc_put_vars_schar);
	NC_TEST(nc_put_vars_short);
	NC_TEST(nc_put_vars_int);
	NC_TEST(nc_put_vars_long);
	NC_TEST(nc_put_vars_float);
	NC_TEST(nc_put_vars_double);
	NC_TEST(nc_put_vars);
	NC_TEST(nc_put_varm_text);
	NC_TEST(nc_put_varm_uchar);
	NC_TEST(nc_put_varm_schar);
	NC_TEST(nc_put_varm_short);
	NC_TEST(nc_put_varm_int);
	NC_TEST(nc_put_varm_long);
	NC_TEST(nc_put_varm_float);
	NC_TEST(nc_put_varm_double);
	NC_TEST(nc_put_varm);
	NC_TEST(nc_rename_var);
	NC_TEST(nc_put_att_text);
	NC_TEST(nc_put_att_uchar);
	NC_TEST(nc_put_att_schar);
	NC_TEST(nc_put_att_short);
	NC_TEST(nc_put_att_int);
	NC_TEST(nc_put_att_long);
	NC_TEST(nc_put_att_float);
	NC_TEST(nc_put_att_double);
	NC_TEST(nc_put_att);
	NC_TEST(nc_copy_att);
	NC_TEST(nc_rename_att);
	NC_TEST(nc_del_att);
	NC_TEST(nc_set_default_format);
    }

    fprintf(stderr, "\n*** Total number of failures: %d\n", nfailsTotal);

    if (nfailsTotal)
    {
       fprintf(stderr, "*** nc_test FAILURE!!!\n");
       return 2;
    }
    else
       fprintf(stderr, "*** nc_test SUCCESS!!!\n");

    exit(0);
    return 0;
}

