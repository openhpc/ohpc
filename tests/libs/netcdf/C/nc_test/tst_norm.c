/* This is part of the netCDF package.
   Copyright 2006 University Corporation for Atmospheric Research/Unidata.
   See COPYRIGHT file for conditions of use.

   This is a very simple example which tests NFC normalization of 
   Unicode names encoded with UTF-8.

   $Id: tst_norm.c,v 1.8 2008/10/20 01:48:08 ed Exp $
*/
#include <config.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <netcdf.h>
#include <nc_tests.h>

/* The data file we will create. */
#define FILE7_NAME "tst_norm.nc"
#define UNITS "units"
#define NDIMS 1
#define NX 18

int
main(int argc, char **argv)
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
   size_t att_len;
   int res;
   int dimid_in, varid_in, attnum_in;
   int attvals[] = {42};
#define ATTNUM ((sizeof attvals)/(sizeof attvals[0]))

   printf("\n*** testing UTF-8 normalization...");
   if((res = nc_create(FILE7_NAME, NC_CLOBBER, &ncid)))
       ERR;

   /* Define dimension with unnormalized Unicode UTF-8 encoded name */
   if ((res = nc_def_dim(ncid, UNAME, NX, &dimid)))
       ERR;
   dimids[0] = dimid;

   /* Define variable with same name */
   if ((res = nc_def_var(ncid, UNAME, NC_CHAR, NDIMS, dimids, &varid)))
       ERR;

   /* Create string attribute with same value */
   if ((res = nc_put_att_text(ncid, varid, UNITS, UNAMELEN, UNAME)))
       ERR;

   /* Create int attribute with same name */
   if ((res = nc_put_att_int(ncid, varid, UNAME, NC_INT, ATTNUM, attvals)))
       ERR;

   /* Try to create dimension and variable with NFC-normalized
    * version of same name.  These should fail, as unnormalized name
    * should have been normalized in library, so these are attempts to
    * create duplicate netCDF objects. */
   if ((res = nc_def_dim(ncid, NNAME, NX, &dimid)) 
       != NC_ENAMEINUSE) ERR;
   if ((res = nc_def_var(ncid, NNAME, NC_CHAR, NDIMS, dimids, &varid)) 
       != NC_ENAMEINUSE) ERR;
   if ((res = nc_enddef(ncid)))
       ERR;

   /* Write string data, UTF-8 encoded, to the file */
   if ((res = nc_put_var_text(ncid, varid, UNAME)))
       ERR;
   if ((res = nc_close(ncid)))
       ERR;

   /* Check it out. */
   if ((res = nc_open(FILE7_NAME, NC_NOWRITE, &ncid)))
       ERR;
   if ((res = nc_inq_varid(ncid, UNAME, &varid)))
       ERR;
   if ((res = nc_inq_varname(ncid, varid, name_in)))
       ERR;
   if ((res = strncmp(NNAME, name_in, NNAMELEN)))
       ERR;
   if ((res = nc_inq_varid(ncid, NNAME, &varid_in)) || varid != varid_in)
       ERR;
   if ((res = nc_inq_dimid(ncid, UNAME, &dimid_in)) || dimid != dimid_in)
       ERR;
   if ((res = nc_inq_dimid(ncid, NNAME, &dimid_in)) || dimid != dimid_in)
       ERR;
   if ((res = nc_inq_att(ncid, varid, UNITS, &att_type, &att_len)))
       ERR;
   if ((res = att_type != NC_CHAR || att_len != UNAMELEN))
       ERR;
   if ((res = nc_get_att_text(ncid, varid, UNITS, strings_in)))
       ERR;
   strings_in[UNAMELEN] = '\0';
   if ((res = strncmp(UNAME, strings_in, UNAMELEN)))
       ERR;
   if ((res = nc_inq_attid(ncid, varid, UNAME, &attnum_in)) || ATTNUM != attnum_in)
       ERR;
   if ((res = nc_inq_attid(ncid, varid, NNAME, &attnum_in)) || ATTNUM != attnum_in)
       ERR;
   if ((res = nc_close(ncid)))
       ERR;

   SUMMARIZE_ERR;
   FINAL_RESULTS;
   return 0;
}
