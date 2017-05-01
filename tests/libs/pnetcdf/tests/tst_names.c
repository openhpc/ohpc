/*
 *  Copyright (C) 2014, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 */
/* $Id: tst_names.c 2744 2016-12-28 16:25:22Z wkliao $ */

/* This program is based on the test program tst_names.c of the netCDF package */

/* This is part of the netCDF package.
   Copyright 2006 University Corporation for Atmospheric Research/Unidata.
   See COPYRIGHT file for conditions of use.

   This is a very simple example which tests rejection of bad names for
   netCDF data objects, including names with "/" character, trailing spaces, 
   leading special characters, and invalid UTF-8 strings.
*/
#include <stdlib.h>
#include <stdio.h>
#include <libgen.h> /* basename() */
#include <string.h>
#include <pnetcdf.h>

#include <testutils.h>

#define ERROR {printf("Error at line %d: %s\n",__LINE__,ncmpi_strerror(res)); nerrs++;}
#define ERRORI {printf("Error at line %d (loop=%d): %s\n",__LINE__,i,ncmpi_strerror(res)); nerrs++;}

/* The data file we will create. */
#define NDIMS 1
#define DIMLEN 1

int
main(int argc, char **argv)
{
   char *valid[] = {
       /* pressure in 23 languages */
       "\xd8\xa7\xd9\x84\xd8\xb6\xd8\xba\xd8\xb7",
       "\xd0\xbd\xd0\xb0\xd0\xbb\xd1\x8f\xd0\xb3\xd0\xb0\xd0\xbd\xd0\xb5",
       "\xe5\x8e\x8b\xe5\x8a\x9b",
       "\xe5\xa3\x93\xe5\x8a\x9b",
       "pritisak",
       "tlaku",
       "pres",
       "druk",
       "pressure",
       "paine",
       "pression",
       "Druck",
       "\xcf\x80\xce\xaf\xce\xb5\xcf\x83\xce\xb7",
       "\xe0\xa4\xa6\xe0\xa4\xac\xe0\xa4\xbe\xe0\xa4\xb5",
       "pressione",
       "\xe5\x9c\xa7\xe5\x8a\x9b",
       "\xec\x95\x95\xeb\xa0\xa5",
       "press",
       "ci\xc5\x9bnienie",
       "Press\xc3\xa3o",
       "presiune",
       "\xd0\xb4\xd0\xb0\xd0\xb2\xd0\xbb\xd0\xb5\xd0\xbd\xd0\xb8\xd0\xb5",
       "presi\xc3\xb3n",
       /* special characters in names, numeric characters at the start of names */
       "has blank",
       "has:colon",
       "a",
       "A",
       "0leading_numeric_char",
       "1",
       "0x123"
   };
   char *notvalid[] = {
       "-leading_special_char",
       "trailing_space ",
       "trailing_tab\t",
       "trailing_newline\n",
       "has_control_char_\a_in_name",
       "has ascii_del_\x7f_in name",
       /* Invalid UTF-8 of various sorts, thanks to Markus Kuhn */
       "\xA0\xB0\xC0\xD0",
       "xyz\x80", 		/* unexpected continuation bytes */
       "\x80xyz",
       "xyz\xBF",
       "\xBFxyz",
       "\xC0xyz",		/* lonely start characters */
       "x\xC0yz",
       "xy\xC0z",
       "xyz\xC0",
       "\xDFxyz",
       "x\xDFyz",
       "xy\xDFz",
       "xyz\xDF",
       "\xE0xyz",
       "x\xE0yz",
       "xy\xE0z",
       "xyz\xE0",
       "\xE0\xBFxy",
       "x\xE0\xBFy",
       "xy\xE0\xBF",
       "\xEFxyz",
       "x\xEFyz",
       "xy\xEFz",
       "xyz\xEF",
       "\xEF\x80xy",
       "x\xEF\x80y",
       "xy\xEF\x80",
       "\xF0xyz",
       "x\xF0yz",
       "xy\xF0z",
       "xyz\xF0",
       "\xF7xyz",
       "x\xF7yz",
       "xy\xF7z",
       "xyz\xF7",
       "\xF8xyz",
       "x\xF8yz",
       "xy\xF8z",
       "xyz\xF8",
       "\xFBxyz",
       "x\xFByz",
       "xy\xFBz",
       "xyz\xFB",
       "\xFCxyz",
       "x\xFCyz",
       "xy\xFCz",
       "xyz\xFC",
       "\xFDxyz",
       "x\xFDyz",
       "xy\xFDz",
       "xyz\xFD",
       "\xC0\xC0xy",		/* last continuation byte missing */
       "x\xC0\xC0y",
       "xy\xC0\xC0",
       "\xDF\xDFxy",
       "x\xDF\xDFy",
       "xy\xDF\xDF",
       "\xE0\x80xy",
       "x\xE0\x80y",
       "xy\xE0\x80",
       "\xEF\x80xy",
       "x\xEF\x80y",
       "xy\xEF\x80",
       "\xF0\x80\x80x",
       "x\xF0\x80\x80",
       "\xF7\x80\x80x",
       "x\xF7\x80\x80",
       "\xF8\x80\x80\x80x",
       "x\xF8\x80\x80\x80",
       "\xFB\x80\x80\x80x",
       "x\xFB\x80\x80\x80",
       "\xFC\x80\x80\x80\x80x",
       "x\xFC\x80\x80\x80\x80",
       "\xFD\x80\x80\x80\x80x",
       "x\xFD\x80\x80\x80\x80",
       "\xFExyz",		/* impossible bytes */
       "x\xFEyz",
       "xy\xFEz",
       "xyz\xFE",
       "\xFFxyz",
       "x\xFFyz",
       "xy\xFFz",
       "xyz\xFF",
       "\xC0\xAFxy",		/* overlong sequences */
       "x\xC0\xAFy",
       "xy\xC0\xAF",
       "\xE0\x80\xAFx",
       "x\xE0\x80\xAF",
       "\xF0\x80\x80\xAFx",
       "x\xF0\x80\x80\xAF",
       "\xF8\x80\x80\x80\xAFx",
       "x\xF8\x80\x80\x80\xAF",
       "\xFC\x80\x80\x80\x80\xAFx",
       "x\xFC\x80\x80\x80\x80\xAF",
       "\xC1\xBFxy",
       "x\xC1\xBFy",
       "xy\xC1\xBF",
       "\xE0\x9F\xBFx",
       "x\xE0\x9F\xBF",
       "\xF0\x8F\xBF\xBFx",
       "x\xF0\x8F\xBF\xBF",
       "\xF8\x87\xBF\xBF\xBFx",
       "x\xF8\x87\xBF\xBF\xBF",
       "\xFC\x83\xBF\xBF\xBF\xBFx",
       "x\xFC\x83\xBF\xBF\xBF\xBF",
       "x\xC0\x80",		/* overlong NULs */
       "x\xE0\x80\x80",
       "x\xF0\x80\x80\x80",
       "x\xF8\x80\x80\x80\x80",
       "x\xFC\x80\x80\x80\x80\x80",
       /* single UTF-16 surrogates */
       "x\xED\xA0\x80",
       "x\xED\xAD\xBF",
       "x\xED\xAE\x80",
       "x\xED\xAF\xBF",
       "x\xED\xB0\x80",
       "x\xED\xBE\x80",
       "x\xED\xBF\xBF",
       "x\xED\xA0\x80\xED\xB0\x80", /* paired UTF-16 surrogates */
       "x\xED\xA0\x80\xED\xBF\xBF",
       "x\xED\xAD\xBF\xED\xB0\x80",
       "x\xED\xAD\xBF\xED\xBF\xBF",
       "x\xED\xAE\x80\xED\xB0\x80",
       "x\xED\xAE\x80\xED\xBF\xBF",
       "x\xED\xAF\xBF\xED\xB0\x80",
       "x\xED\xAF\xBF\xED\xBF\xBF",
       "x\xEF\xBF\xBE",		/* other illegal code positions */
       "x\xEF\xBF\xBF"
   };
   int i, j;
#define NUM_BAD (sizeof notvalid / sizeof notvalid[0])
#define NUM_GOOD (sizeof valid / sizeof valid[0])
   int ncid, dimid, varid, res;
   double attvals[] = {-2.0};
   double attvals_in[1];
#define NATTVALS (sizeof attvals / sizeof attvals[0])
   char *attstring = "text";
#define MAX_ATTSTRING_LEN 100
   char attstr_in[MAX_ATTSTRING_LEN];
   int dimids[NUM_GOOD];
   int varids[NUM_GOOD];
   int cmode[2] = {NC_64BIT_OFFSET, NC_64BIT_DATA};
#ifdef DEBUG
   char *format_names[] = { "CDF-2", "CDF-5" };
#endif

    char filename[256];
    int rank, nprocs, err, nerrs=0;

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

    char *cmd_str = (char*)malloc(strlen(argv[0]) + 256);
    sprintf(cmd_str, "*** TESTING C   %s for emulating netCDF tst_names ", basename(argv[0]));
    if (rank == 0) printf("%-66s ------ ", cmd_str);
    free(cmd_str);

#ifdef DEBUG
   printf("\n*** testing names with file %s...\n", filename);
#endif
   for (j = 0; j < 2; j++)
   {
#ifdef DEBUG
       printf("*** switching to netCDF %s format...", format_names[j]);
#endif
       if((res = ncmpi_create(MPI_COMM_WORLD, filename, NC_CLOBBER|cmode[j], MPI_INFO_NULL, &ncid)))
	   ERROR
       
       /* Define dimensions, variables, and attributes with various
	* acceptable names */
       for (i = 0; i < NUM_GOOD; i++) {
	   if ((res = ncmpi_def_dim(ncid, valid[i], DIMLEN, &dimid)))
	       ERRORI

	   dimids[i] = dimid;
	   /* Define variable with same name */
	   if ((res = ncmpi_def_var(ncid, valid[i], NC_FLOAT, NDIMS, &dimids[i], &varid)))
	       ERRORI
	   varids[i] = varid;
	   /* Define variable and global attributes with same name and value */
	   if ((res = ncmpi_put_att_text(ncid, varid, valid[i], strlen(valid[i]), valid[i])))
	       ERRORI
	   if ((res = ncmpi_put_att_double(ncid, NC_GLOBAL, valid[i], NC_DOUBLE, NATTVALS, attvals)))
	       ERRORI
       }
       
       /* Try defining dimensions, variables, and attributes with various
	* bad names and make sure these are rejected */
       for (i = 0; i < NUM_BAD; i++) {
	   if ((res = ncmpi_def_dim(ncid, notvalid[i], DIMLEN, &dimid)) != NC_EBADNAME)
               ERRORI
	   if ((res = ncmpi_def_var(ncid, notvalid[i], NC_FLOAT, NDIMS, dimids, &varid)) != NC_EBADNAME)
               ERRORI
	   if ((res = ncmpi_put_att_text(ncid, varid, notvalid[i], strlen(attstring), attstring)) != NC_EBADNAME)
               ERRORI
	   if ((res = ncmpi_put_att_double(ncid, NC_GLOBAL, notvalid[i], NC_DOUBLE, NATTVALS, attvals)) != NC_EBADNAME)
               ERRORI
       }
       if ((res = ncmpi_enddef(ncid)))
	   ERROR
       if ((res = ncmpi_close(ncid)))
	   ERROR
       
       /* Check it out, make sure all objects with good names were defined OK */
       if ((res = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid)))
	   ERROR
       for (i = 0; i < NUM_GOOD; i++) {
	   MPI_Offset attlen;
	   if ((res = ncmpi_inq_dimid(ncid, valid[i], &dimid)) || dimid != dimids[i])
	       ERRORI
	   if ((res = ncmpi_inq_varid(ncid, valid[i], &varid)) || varid != varids[i])
	       ERRORI
	   res = ncmpi_inq_attlen(ncid, varid, valid[i], &attlen);
	   if ((res = ncmpi_get_att_text(ncid, varid, valid[i], attstr_in))) 
	       ERRORI
	   attstr_in[attlen] = '\0';
	   if (strcmp(valid[i], attstr_in) != 0) 
	       ERRORI
	   if ((res = ncmpi_get_att_double(ncid, NC_GLOBAL, valid[i], attvals_in)) || attvals[0] != attvals_in[0]) 
	       ERRORI
       }
       if ((res = ncmpi_close(ncid)))
	   ERROR
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
