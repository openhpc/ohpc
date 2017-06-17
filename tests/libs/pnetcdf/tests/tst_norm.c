/*
 *  Copyright (C) 2014, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 */
/* $Id: tst_norm.c 2744 2016-12-28 16:25:22Z wkliao $ */

/* This program is based on the test program tst_norm.c of the netCDF package */

/* This is part of the netCDF package.
   Copyright 2006 University Corporation for Atmospheric Research/Unidata.
   See COPYRIGHT file for conditions of use.

   This is a very simple example which tests NFC normalization of 
   Unicode names encoded with UTF-8.
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libgen.h> /* basename() */
#include <pnetcdf.h>

#include <testutils.h>

/* The data file we will create. */
#define UNITS "units"
#define NDIMS 1
#define NX 18

#define ERR {if (err != NC_NOERR) {printf("Error at %s line %d: %s\n",__func__,__LINE__,ncmpi_strerror(err)); return 1;}}

static
int tst_norm(char *filename, int cmode)
{
   int ncid, dimid, varid;
   int dimids[NDIMS];

   /* unnormalized UTF-8 encoding for Unicode 8-character "Hello" in Greek: */
   unsigned char uname_utf8[] = {
       0x41,              	/* LATIN CAPITAL LETTER A */
       0xCC, 0x80,		/* COMBINING GRAVE ACCENT */
       0x41,              	/* LATIN CAPITAL LETTER A */
       0xCC, 0x81,		/* COMBINING ACUTE ACCENT */
       0x41,              	/* LATIN CAPITAL LETTER A */
       0xCC, 0x82,		/* COMBINING CIRCUMFLEX ACCENT */
       0x41,              	/* LATIN CAPITAL LETTER A */
       0xCC, 0x83,		/* COMBINING TILDE */
       0x41,              	/* LATIN CAPITAL LETTER A */
       0xCC, 0x88,		/* COMBINING DIAERESIS */
       0x41,              	/* LATIN CAPITAL LETTER A */
       0xCC, 0x8A,		/* COMBINING RING ABOVE */
       0x43,			/* LATIN CAPITAL LETTER C */
       0xCC, 0xA7,		/* COMBINING CEDILLA */
       0x45,			/* LATIN CAPITAL LETTER E */
       0xCC, 0x80,		/* COMBINING GRAVE ACCENT */
       0x45,			/* LATIN CAPITAL LETTER E */
       0xCC, 0x81,		/* COMBINING ACUTE ACCENT */
       0x45,			/* LATIN CAPITAL LETTER E */
       0xCC, 0x82,		/* COMBINING CIRCUMFLEX ACCENT */
       0x45,			/* LATIN CAPITAL LETTER E */
       0xCC, 0x88,		/* COMBINING DIAERESIS */
       0x49,			/* LATIN CAPITAL LETTER I */
       0xCC, 0x80,		/* COMBINING GRAVE ACCENT */
       0x49,			/* LATIN CAPITAL LETTER I */
       0xCC, 0x81,		/* COMBINING ACUTE ACCENT */
       0x49,			/* LATIN CAPITAL LETTER I */
       0xCC, 0x82,		/* COMBINING CIRCUMFLEX ACCENT */
       0x49,			/* LATIN CAPITAL LETTER I */
       0xCC, 0x88,		/* COMBINING DIAERESIS */
       0x4E,			/* LATIN CAPITAL LETTER N */
       0xCC, 0x83,		/* COMBINING TILDE */
       0x00
   };

   /* NFC normalized UTF-8 encoding for same Unicode string: */
   unsigned char nname_utf8[] = {
       0xC3, 0x80,	        /* LATIN CAPITAL LETTER A WITH GRAVE */
       0xC3, 0x81,	        /* LATIN CAPITAL LETTER A WITH ACUTE */
       0xC3, 0x82,	        /* LATIN CAPITAL LETTER A WITH CIRCUMFLEX */
       0xC3, 0x83,	        /* LATIN CAPITAL LETTER A WITH TILDE */
       0xC3, 0x84,		/* LATIN CAPITAL LETTER A WITH DIAERESIS */
       0xC3, 0x85,		/* LATIN CAPITAL LETTER A WITH RING ABOVE */
       0xC3, 0x87,		/* LATIN CAPITAL LETTER C WITH CEDILLA */
       0xC3, 0x88,		/* LATIN CAPITAL LETTER E WITH GRAVE */
       0xC3, 0x89,		/* LATIN CAPITAL LETTER E WITH ACUTE */
       0xC3, 0x8A,		/* LATIN CAPITAL LETTER E WITH CIRCUMFLEX */
       0xC3, 0x8B,		/* LATIN CAPITAL LETTER E WITH DIAERESIS */
       0xC3, 0x8C,		/* LATIN CAPITAL LETTER I WITH GRAVE */
       0xC3, 0x8D,		/* LATIN CAPITAL LETTER I WITH ACUTE */
       0xC3, 0x8E,		/* LATIN CAPITAL LETTER I WITH CIRCUMFLEX */
       0xC3, 0x8F,		/* LATIN CAPITAL LETTER I WITH DIAERESIS */
       0xC3, 0x91,	        /* LATIN CAPITAL LETTER N WITH TILDE */
       0x00
   };

/* Unnormalized name used for dimension, variable, and attribute value */
#define UNAME ((char *) uname_utf8)
#define UNAMELEN (sizeof uname_utf8)
/* Normalized name */
#define NNAME ((char *) nname_utf8)
#define NNAMELEN (sizeof nname_utf8)

   char name_in[UNAMELEN + 1], strings_in[UNAMELEN + 1];
   nc_type att_type;
   MPI_Offset att_len;
   int err, dimid_in, varid_in, attnum_in;
   int attvals[] = {42};
#define ATTNUM ((sizeof attvals)/(sizeof attvals[0]))

   err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR

   /* Define dimension with unnormalized Unicode UTF-8 encoded name */
   err = ncmpi_def_dim(ncid, UNAME, NX, &dimid); ERR
   dimids[0] = dimid;

   /* Define variable with same name */
   err = ncmpi_def_var(ncid, UNAME, NC_CHAR, NDIMS, dimids, &varid); ERR

   /* Create string attribute with same value */
   err = ncmpi_put_att_text(ncid, varid, UNITS, UNAMELEN, UNAME); ERR

   /* Create int attribute with same name */
   err = ncmpi_put_att_int(ncid, varid, UNAME, NC_INT, ATTNUM, attvals); ERR

   /* Try to create dimension and variable with NFC-normalized
    * version of same name.  These should fail, as unnormalized name
    * should have been normalized in library, so these are attempts to
    * create duplicate netCDF objects. */
   if ((err = ncmpi_def_dim(ncid, NNAME, NX, &dimid)) != NC_ENAMEINUSE) {
       printf("Error at line %d: expecting error code %d but got %d\n",__LINE__,NC_ENAMEINUSE,err);
       return 1;
   }

   if ((err=ncmpi_def_var(ncid, NNAME, NC_CHAR, NDIMS, dimids, &varid)) != NC_ENAMEINUSE) {
       printf("Error at line %d: expecting error code %d but got %d\n",__LINE__,NC_ENAMEINUSE,err);
       return 1;
   }
   err = ncmpi_enddef(ncid); ERR

   /* Write string data, UTF-8 encoded, to the file */
   err = ncmpi_put_var_text_all(ncid, varid, UNAME); ERR
   err = ncmpi_close(ncid); ERR

   /* Check it out. */
   err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
   err = ncmpi_inq_varid(ncid, UNAME, &varid); ERR
   err = ncmpi_inq_varname(ncid, varid, name_in); ERR
   err = strncmp(NNAME, name_in, NNAMELEN); ERR
   err = ncmpi_inq_varid(ncid, NNAME, &varid_in); ERR
   if ((err = ncmpi_inq_dimid(ncid, UNAME, &dimid_in)) || dimid != dimid_in)
       {printf("Error at line %d\n",__LINE__);return 1;}
   if ((err = ncmpi_inq_dimid(ncid, NNAME, &dimid_in)) || dimid != dimid_in)
       {printf("Error at line %d\n",__LINE__);return 1;}
   err = ncmpi_inq_att(ncid, varid, UNITS, &att_type, &att_len); ERR
   if ( att_type != NC_CHAR || att_len != UNAMELEN)
       {printf("Error at line %d\n",__LINE__);return 1;}
   err = ncmpi_get_att_text(ncid, varid, UNITS, strings_in); ERR
   strings_in[UNAMELEN] = '\0';
   err = strncmp(UNAME, strings_in, UNAMELEN); ERR
   if ((err = ncmpi_inq_attid(ncid, varid, UNAME, &attnum_in)) || ATTNUM != attnum_in)
       {printf("Error at line %d\n",__LINE__);return 1;}
   if ((err = ncmpi_inq_attid(ncid, varid, NNAME, &attnum_in)) || ATTNUM != attnum_in)
       {printf("Error at line %d\n",__LINE__);return 1;}
   err = ncmpi_close(ncid); ERR

   return 0;
}

int
main(int argc, char **argv)
{
    char filename[256];
    int rank, nprocs, cmode, err, nerrs=0;

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
    sprintf(cmd_str, "*** TESTING C   %s for emulating netCDF tst_norm ", basename(argv[0]));
    if (rank == 0) printf("%-66s ------ ", cmd_str);
    free(cmd_str);

    /*---- testing UTF-8 normalization ----*/

    /* test CDF-2 format */
    cmode = NC_CLOBBER | NC_64BIT_OFFSET;
    nerrs += tst_norm(filename, cmode);

    /* test CDF-5 format */
    cmode = NC_CLOBBER | NC_64BIT_DATA;
    nerrs += tst_norm(filename, cmode);

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
