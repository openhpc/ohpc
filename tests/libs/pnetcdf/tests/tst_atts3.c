/*
 *  Copyright (C) 2014, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 */
/* $Id: tst_atts3.c 2744 2016-12-28 16:25:22Z wkliao $ */

/* This program is based on the test program tst_atts3.c of the netCDF package */

/* This is part of the netCDF package. Copyright 2005-2007 University
   Corporation for Atmospheric Research/Unidata. See COPYRIGHT file
   for conditions of use.

   Test attributes. 
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h> /* basename() */
#include <pnetcdf.h>
#include <signal.h>

#include <testutils.h>

#define ERR {if (err != NC_NOERR) {printf("Error at %s line %d: %s\n",__func__,__LINE__,ncmpi_strerror(err)); nerrs++;}}
#define ERRV {printf("Unexpected result at %s line %d\n",__func__,__LINE__); nerrs++;}
static int verbose;

#define VAR1_NAME "Horace_Rumpole"
#define VAR2_NAME "Claude_Erskine-Brown"
#define VAR3_NAME "Phillida_Erskine-Brown_Q.C."
#define DIM1_NAME "Old_Bailey_case_number"
#define DIM1_LEN 10
#define DIM2_NAME "occupancy_in_chambers"
#define DIM2_LEN 15
#define ATT_INT_NAME "Old_Bailey_Room_Numbers"
#define ATT_DOUBLE_NAME "Equity_Court_Canteen_Charges"
#define ATT_SHORT_NAME "Ecclesiastical_Court_Appearences"
#define ATT_TEXT_NAME "Speech_to_Jury"
#define ATT_TEXT_NAME2 "Speech_to_She_Who_Must_be_Obeyed"
#define ATT_UCHAR_NAME "Number_of_current_briefs"
#define ATT_SCHAR_NAME "Slate_totals_at_Pomeroys_Wine_Bar"
#define ATT_USHORT_NAME "brief_no"
#define ATT_UINT_NAME "Orders_from_SWMBO"
#define ATT_INT64_NAME "judges_golf_score"
#define ATT_UINT64_NAME "Number_of_drinks_in_career_to_date"

/*
#define ATT_USHORT_NAME "Chamber_Gas_Electric_and_Telephone_Bill_Share"
*/
#define ATT_FLOAT_NAME "Average_Nanoseconds_for_Lose_Win_or_Appeal"
#define ATT_LEN 3

char speech[] = "Once more unto the breach, dear friends, once more;\n\
Or close the wall up with our English dead.\n\
In peace there's nothing so becomes a man\n\
As modest stillness and humility:\n\
But when the blast of war blows in our ears,\n\
Then imitate the action of the tiger;\n\
Stiffen the sinews, summon up the blood,\n\
Disguise fair nature with hard-favour'd rage;\n\
Then lend the eye a terrible aspect;\n\
Let pry through the portage of the head\n\
Like the brass cannon; let the brow o'erwhelm it\n\
As fearfully as doth a galled rock\n\
O'erhang and jutty his confounded base,\n\
Swill'd with the wild and wasteful ocean.\n\
Now set the teeth and stretch the nostril wide,\n\
Hold hard the breath and bend up every spirit\n\
To his full height. On, on, you noblest English.\n\
Whose blood is fet from fathers of war-proof!\n\
Fathers that, like so many Alexanders,\n\
Have in these parts from morn till even fought\n\
And sheathed their swords for lack of argument:\n\
Dishonour not your mothers; now attest\n\
That those whom you call'd fathers did beget you.\n\
Be copy now to men of grosser blood,\n\
And teach them how to war. And you, good yeoman,\n\
Whose limbs were made in England, show us here\n\
The mettle of your pasture; let us swear\n\
That you are worth your breeding; which I doubt not;\n\
For there is none of you so mean and base,\n\
That hath not noble lustre in your eyes.\n\
I see you stand like greyhounds in the slips,\n\
Straining upon the start. The game's afoot:\n\
Follow your spirit, and upon this charge\n\
Cry 'God for Harry, England, and Saint George!'";

/* Test the ordering of atts for a cmode. */
#define NUM_ATTS 8
#define ATT_MAX_NAME 25
static int
tst_att_ordering(char *filename, int cmode)
{
   int ncid, err, nerrs=0;
   char name[NUM_ATTS][ATT_MAX_NAME + 1] = {"Gc", "Gb", "Gs", "Gi", "Gf", 
					    "Gd", "Gatt-name-dashes", "Gatt.name.dots"};
   int len[NUM_ATTS] = {0, 2, 3, 3, 3, 3, 1, 1};
   signed char b[2] = {-128, 127};
   short s[3] = {-32768, 0, 32767};
   int i[3] = {42, 0, -42};
   float f[3] = {42.0, -42.0, 42.0};
   double d[3] = {420.0, -420.0, 420.0};
   int att_name_dashes = -1, att_name_dots = -2;
   char name_in[NC_MAX_NAME];
   int j;

   /* Create a file with some global atts. */
   err=ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR
   err=ncmpi_put_att_text(ncid, NC_GLOBAL, name[0], len[0], NULL); ERR
   err=ncmpi_put_att_schar(ncid, NC_GLOBAL, name[1], NC_BYTE, len[1], b); ERR
   err=ncmpi_put_att_short(ncid, NC_GLOBAL, name[2], NC_SHORT, len[2], s); ERR
   err=ncmpi_put_att_int(ncid, NC_GLOBAL, name[3], NC_INT, len[3], i); ERR
   err=ncmpi_put_att_float(ncid, NC_GLOBAL, name[4], NC_FLOAT, len[4], f); ERR
   err=ncmpi_put_att_double(ncid, NC_GLOBAL, name[5], NC_DOUBLE, len[5], d); ERR
   err=ncmpi_put_att_int(ncid, NC_GLOBAL, name[6], NC_INT, len[6], &att_name_dashes); ERR
   err=ncmpi_put_att_int(ncid, NC_GLOBAL, name[7], NC_INT, len[7], &att_name_dots); ERR
   err=ncmpi_close(ncid); ERR
      
   /* Reopen the file and check the order. */
   err=ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
   for (j = 0; j < NUM_ATTS; j++)
   {
      err=ncmpi_inq_attname(ncid, NC_GLOBAL, j, name_in); ERR
      if (strcmp(name_in, name[j])) ERRV
   }

   /* Close up shop. */
   err=ncmpi_close(ncid); ERR
   return nerrs;
}

static int
tst_atts3(char *filename, int cmode)
{
    char filename2[256];
    int err, nerrs=0;
    signed char schar_in[ATT_LEN], schar_out[ATT_LEN] = {NC_MIN_BYTE, 1, NC_MAX_BYTE};
    unsigned char uchar_in[ATT_LEN];
    short short_in[ATT_LEN], short_out[ATT_LEN] = {NC_MIN_SHORT, -128, NC_MAX_SHORT};
    int int_in[ATT_LEN], int_out[ATT_LEN] = {-100000, 127, 100000};
    float float_in[ATT_LEN], float_out[ATT_LEN] = {-0.5, 0.25, 0.125};
    double double_in[ATT_LEN], double_out[ATT_LEN] = {-0.25, .5, 0.125};
    long long longlong_in[ATT_LEN] = {-1LL, -1LL, -1LL};
#ifdef USE_NETCDF4
    long long_in[ATT_LEN];
    unsigned short ushort_in[ATT_LEN], ushort_out[ATT_LEN] = {0, 128, NC_MAX_USHORT};
    unsigned int uint_in[ATT_LEN], uint_out[ATT_LEN] = {0, 128, NC_MAX_UINT};
    long long longlong_out[ATT_LEN] = {-3123456789LL, 128LL, 3123456789LL};
    unsigned long long ulonglong_in[ATT_LEN] = {NC_MAX_UINT64, NC_MAX_UINT64, NC_MAX_UINT64};
    unsigned long long ulonglong_out[ATT_LEN] = {0LL, 128LL, 3123456789LL};
#endif

    (void) signal(SIGFPE, SIG_IGN);

   sprintf(filename2, "%s.2", filename);

   if (verbose) printf("\n*** Testing netcdf-3 attribute functions.\n");
   if (verbose) printf("*** testing really simple global atts...");
#define NUM_SIMPLE_ATTS 9
   {
      int ncid;
      char name[NUM_SIMPLE_ATTS][ATT_MAX_NAME + 1] = {"Gc", "Gb", "Gs", "Gi", "Gf", 
						      "Gd", "G7", "G8", "G9"};
      char name_in[NC_MAX_NAME];
      int j;

      /* Create a file with some global atts. */
      err=ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR
      for (j = 0; j < NUM_SIMPLE_ATTS; j++)
	 err=ncmpi_put_att_int(ncid, NC_GLOBAL, name[j], NC_INT, 0, NULL); ERR
      err=ncmpi_close(ncid); ERR
      
      /* Reopen the file and check the order. */
      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
      for (j = 0; j < NUM_SIMPLE_ATTS; j++)
      {
	 err=ncmpi_inq_attname(ncid, NC_GLOBAL, j, name_in); ERR
	 if (strcmp(name_in, name[j])) ERRV
      }

      /* Close up shop. */
      err=ncmpi_close(ncid); ERR
   }
   if (verbose) printf("ok\n");
   if (verbose) printf("*** testing simple global atts...");
   {
      int ncid;
      nc_type att_type;
      MPI_Offset att_len;
      int i;

      char *speech_in;

      /* This won't work, because classic files can't create these types. */
      err=ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR
      err=ncmpi_put_att_int(ncid, NC_GLOBAL, ATT_INT_NAME, NC_INT, ATT_LEN, int_out); ERR
      /* It is also OK to read classic types converted into
       * supported C types. though the conversion may encounter
       * out-of-range values */
      if ((err=ncmpi_get_att_uchar(ncid, NC_GLOBAL, ATT_INT_NAME, uchar_in)) != NC_ERANGE) ERR
      for (i = 0; i < ATT_LEN; i++) {
	if (i == 0 || i == 2) continue;
	if (uchar_in[i] != (unsigned char) int_out[i]) ERRV
      }

      /* This was bug NCF-171: on 32-bit platforms, bad values returned */
      err=ncmpi_get_att_longlong(ncid, NC_GLOBAL, ATT_INT_NAME, longlong_in); ERR
      for (i = 0; i < ATT_LEN; i++)
      	if (longlong_in[i] != (long long) int_out[i]) ERRV
      err=ncmpi_close(ncid); ERR

      /* Create a file with a global attribute of each type. */
      err=ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR
      err=ncmpi_put_att_text(ncid, NC_GLOBAL, ATT_TEXT_NAME, strlen(speech)+1, speech); ERR
      err=ncmpi_put_att_schar(ncid, NC_GLOBAL, ATT_SCHAR_NAME, NC_BYTE, ATT_LEN, schar_out); ERR
      err=ncmpi_put_att_short(ncid, NC_GLOBAL, ATT_SHORT_NAME, NC_SHORT, ATT_LEN, short_out); ERR
      err=ncmpi_put_att_int(ncid, NC_GLOBAL, ATT_INT_NAME, NC_INT, ATT_LEN, int_out); ERR
      err=ncmpi_put_att_float(ncid, NC_GLOBAL, ATT_FLOAT_NAME, NC_FLOAT, ATT_LEN, float_out); ERR
      err=ncmpi_put_att_double(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, NC_DOUBLE, ATT_LEN, double_out); ERR
      err=ncmpi_close(ncid); ERR

      /* Open the file and check attributes. */
      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
      /* Check text. */
      err=ncmpi_inq_att(ncid, NC_GLOBAL, ATT_TEXT_NAME, &att_type, &att_len); ERR
      if (att_type != NC_CHAR || att_len != strlen(speech) + 1) ERRV
      if (!(speech_in = malloc(att_len + 1))) ERRV
      else {
          err=ncmpi_get_att_text(ncid, NC_GLOBAL, ATT_TEXT_NAME, speech_in); ERR
          if (strcmp(speech, speech_in)) ERRV
          free(speech_in);
      }
      /* Check numeric values. */
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_SCHAR_NAME, schar_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (schar_in[i] != schar_out[i]) ERRV
      err=ncmpi_get_att_short(ncid, NC_GLOBAL, ATT_SHORT_NAME, short_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (short_in[i] != short_out[i]) ERRV
      err=ncmpi_get_att_int(ncid, NC_GLOBAL, ATT_INT_NAME, int_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (int_in[i] != int_out[i]) ERRV
      err=ncmpi_get_att_float(ncid, NC_GLOBAL, ATT_FLOAT_NAME, float_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (float_in[i] != float_out[i]) ERRV
      err=ncmpi_get_att_double(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, double_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (double_in[i] != double_out[i]) ERRV
      err=ncmpi_close(ncid); ERR
   }
   if (verbose) printf("ok\n");
   if (verbose) printf("*** testing attribute data type conversions...");

   {
      int ncid;
      int i;

      /* Reopen the file and try different type conversions. */
      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
      /* No text conversions are allowed, and people who try them should
       * be locked up, away from decent folk! */
      if ((err=ncmpi_get_att_short(ncid, NC_GLOBAL, ATT_TEXT_NAME, short_in)) != NC_ECHAR) ERR
      if ((err=ncmpi_get_att_int(ncid, NC_GLOBAL, ATT_TEXT_NAME, int_in)) != NC_ECHAR) ERR
      if ((err=ncmpi_get_att_float(ncid, NC_GLOBAL, ATT_TEXT_NAME, float_in)) != NC_ECHAR) ERR
      if ((err=ncmpi_get_att_double(ncid, NC_GLOBAL, ATT_TEXT_NAME, double_in)) != NC_ECHAR) ERR

      /* Read all atts (except text) as double. */
      err=ncmpi_get_att_double(ncid, NC_GLOBAL, ATT_SCHAR_NAME, double_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (double_in[i] != schar_out[i]) ERRV
      err=ncmpi_get_att_double(ncid, NC_GLOBAL, ATT_SHORT_NAME, double_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (double_in[i] != short_out[i]) ERRV
      err=ncmpi_get_att_double(ncid, NC_GLOBAL, ATT_INT_NAME, double_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (double_in[i] != int_out[i]) ERRV
      err=ncmpi_get_att_double(ncid, NC_GLOBAL, ATT_FLOAT_NAME, double_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (double_in[i] != float_out[i]) ERRV
      err=ncmpi_get_att_double(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, double_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (double_in[i] != double_out[i]) ERRV

      /* Read all atts (except text) as float. */
      err=ncmpi_get_att_float(ncid, NC_GLOBAL, ATT_SCHAR_NAME, float_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (float_in[i] != schar_out[i]) ERRV
      err=ncmpi_get_att_float(ncid, NC_GLOBAL, ATT_SHORT_NAME, float_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (float_in[i] != short_out[i]) ERRV
      err=ncmpi_get_att_float(ncid, NC_GLOBAL, ATT_INT_NAME, float_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (float_in[i] != int_out[i]) ERRV
      err=ncmpi_get_att_float(ncid, NC_GLOBAL, ATT_FLOAT_NAME, float_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (float_in[i] != float_out[i]) ERRV
      err=ncmpi_get_att_float(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, float_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	  if (float_in[i] != (float) double_out[i]) ERRV

      /* Read all atts (except text) as int. */
      err=ncmpi_get_att_int(ncid, NC_GLOBAL, ATT_SCHAR_NAME, int_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (int_in[i] != schar_out[i]) ERRV
      err=ncmpi_get_att_int(ncid, NC_GLOBAL, ATT_SHORT_NAME, int_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (int_in[i] != short_out[i]) ERRV
      err=ncmpi_get_att_int(ncid, NC_GLOBAL, ATT_INT_NAME, int_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (int_in[i] != int_out[i]) ERRV
      err=ncmpi_get_att_int(ncid, NC_GLOBAL, ATT_FLOAT_NAME, int_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	  if (int_in[i] != (int) float_out[i]) ERRV
      err=ncmpi_get_att_int(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, int_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	  if (int_in[i] != (int) double_out[i]) ERRV

      /* Read all atts (except text) as short. */
      err=ncmpi_get_att_short(ncid, NC_GLOBAL, ATT_SCHAR_NAME, short_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (short_in[i] != schar_out[i]) ERRV
      err=ncmpi_get_att_short(ncid, NC_GLOBAL, ATT_SHORT_NAME, short_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (short_in[i] != short_out[i]) ERRV
      if ((err=ncmpi_get_att_short(ncid, NC_GLOBAL, ATT_INT_NAME, short_in)) != NC_ERANGE) ERR
      for (i = 0; i < ATT_LEN; i++) {
          if (i == 0 || i == 2) continue;
	  if (short_in[i] != (short) int_out[i]) ERRV
      }
      err=ncmpi_get_att_short(ncid, NC_GLOBAL, ATT_FLOAT_NAME, short_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	  if (short_in[i] != (short) float_out[i]) ERRV
      err=ncmpi_get_att_short(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, short_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	  if (short_in[i] != (short) double_out[i]) ERRV

      /* Read all atts (except text) as schar. */
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_SCHAR_NAME, schar_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (schar_in[i] != schar_out[i]) ERRV
      if ((err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_SHORT_NAME, schar_in)) != NC_ERANGE) ERR
      for (i = 0; i < ATT_LEN; i++) {
         if (i == 0 || i == 2) continue;
	 if (schar_in[i] != (signed char) short_out[i]) ERRV
      }
      if ((err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_INT_NAME, schar_in)) != NC_ERANGE) ERR
      for (i = 0; i < ATT_LEN; i++) {
	 if (i == 0 || i == 2) continue; /* int_out[0] and int_out[2] are out of schar range */
	 if (schar_in[i] != (signed char) int_out[i]) ERRV
      }
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_FLOAT_NAME, schar_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (schar_in[i] != (signed char) float_out[i]) ERRV
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, schar_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (schar_in[i] != (signed char) double_out[i]) ERRV

      /* Read all atts (except text) as uchar. */
      if ((err=ncmpi_get_att_uchar(ncid, NC_GLOBAL, ATT_SCHAR_NAME, uchar_in)) != NC_ERANGE) ERR
      for (i = 0; i < ATT_LEN; i++) {
	 if (i == 0) continue; /* skip schar_out[0]=NC_MIN_BYTE as it causes NC_ERANGE */
	 if (uchar_in[i] != (unsigned char) schar_out[i]) ERRV
      }
      if ((err=ncmpi_get_att_uchar(ncid, NC_GLOBAL, ATT_SHORT_NAME, uchar_in)) != NC_ERANGE) ERR
/*
      for (i = 0; i < ATT_LEN; i++)
	 if (uchar_in[i] != (unsigned char) short_out[i]) ERRV
*/
      if ((err=ncmpi_get_att_uchar(ncid, NC_GLOBAL, ATT_INT_NAME, uchar_in)) != NC_ERANGE) ERR
      for (i = 0; i < ATT_LEN; i++) {
	 if (i == 0 || i == 2) continue;
	 if (uchar_in[i] != (unsigned char) int_out[i]) ERRV
      }
      if ((err=ncmpi_get_att_uchar(ncid, NC_GLOBAL, ATT_FLOAT_NAME, uchar_in)) != NC_ERANGE) ERR
      for (i = 0; i < ATT_LEN; i++) {
	 if (i == 0) continue;
	 if (uchar_in[i] != (unsigned char) float_out[i]) ERRV
      }
      if ((err=ncmpi_get_att_uchar(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, uchar_in)) != NC_ERANGE) ERR
      for (i = 0; i < ATT_LEN; i++) {
	 if (i == 0) continue;
	 if (uchar_in[i] != (unsigned char) double_out[i]) ERRV
      }

      /* Read all atts (except text) into long long variable. */
      err=ncmpi_get_att_longlong(ncid, NC_GLOBAL, ATT_SCHAR_NAME, longlong_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (longlong_in[i] != schar_out[i]) ERRV
      err=ncmpi_get_att_longlong(ncid, NC_GLOBAL, ATT_SHORT_NAME, longlong_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (longlong_in[i] != short_out[i]) ERRV
      err=ncmpi_get_att_longlong(ncid, NC_GLOBAL, ATT_INT_NAME, longlong_in); ERR
      /* This was bug NCF-171: on 32-bit platforms, bad values returned */
      err=ncmpi_get_att_longlong(ncid, NC_GLOBAL, ATT_INT_NAME, longlong_in); ERR
      for (i = 0; i < ATT_LEN; i++)
      	if (longlong_in[i] != (long long) int_out[i]) ERRV
      err=ncmpi_get_att_longlong(ncid, NC_GLOBAL, ATT_FLOAT_NAME, longlong_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (longlong_in[i] != (long long)float_out[i]) ERRV
      err=ncmpi_get_att_longlong(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, longlong_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (longlong_in[i] != (long long)double_out[i]) ERRV

      err=ncmpi_close(ncid); ERR
   }
   if (verbose) printf("ok\n");
   if (verbose) printf("*** testing zero-length attributes...");
   {
      int ncid;

      /*int int_in[ATT_LEN], int_out[ATT_LEN] = {NC_MIN_INT, 128, NC_MAX_INT};*/

      /* Create a file with a global attribute of each type of zero length. */
      err=ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR
      err=ncmpi_put_att_text(ncid, NC_GLOBAL, ATT_TEXT_NAME, 0, NULL); ERR
      err=ncmpi_put_att_schar(ncid, NC_GLOBAL, ATT_SCHAR_NAME, NC_BYTE, 0, NULL); ERR
      err=ncmpi_put_att_short(ncid, NC_GLOBAL, ATT_SHORT_NAME, NC_SHORT, 0, NULL); ERR
      err=ncmpi_put_att_int(ncid, NC_GLOBAL, ATT_INT_NAME, NC_INT, 0, NULL); ERR
      err=ncmpi_put_att_float(ncid, NC_GLOBAL, ATT_FLOAT_NAME, NC_FLOAT, 0, NULL); ERR
      err=ncmpi_put_att_double(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, NC_DOUBLE, 0, NULL); ERR
      err=ncmpi_close(ncid); ERR
   }

   /* Make sure we can read all these zero-length atts. */
   {
      int ncid;
      signed char schar_in[ATT_LEN];
      short short_in[ATT_LEN];
      /*int int_in[ATT_LEN], int_out[ATT_LEN] = {NC_MIN_INT, 128, NC_MAX_INT};*/
      int int_in[ATT_LEN];
      float float_in[ATT_LEN];
      double double_in[ATT_LEN];
      MPI_Offset len;
      nc_type xtype;

      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_get_att_text(ncid, NC_GLOBAL, ATT_TEXT_NAME, NULL); ERR
      err=ncmpi_inq_att(ncid, NC_GLOBAL, ATT_TEXT_NAME, &xtype, &len); ERR
      if (len || xtype != NC_CHAR) ERRV
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_SCHAR_NAME, schar_in); ERR
      err=ncmpi_inq_att(ncid, NC_GLOBAL, ATT_SCHAR_NAME, &xtype, &len); ERR
      if (len || xtype != NC_BYTE) ERRV
      err=ncmpi_get_att_short(ncid, NC_GLOBAL, ATT_SHORT_NAME, short_in); ERR
      err=ncmpi_inq_att(ncid, NC_GLOBAL, ATT_SHORT_NAME, &xtype, &len); ERR
      if (len || xtype != NC_SHORT) ERRV
      err=ncmpi_get_att_int(ncid, NC_GLOBAL, ATT_INT_NAME, int_in); ERR
      err=ncmpi_inq_att(ncid, NC_GLOBAL, ATT_INT_NAME, &xtype, &len); ERR
      if (len || xtype != NC_INT) ERRV
      err=ncmpi_get_att_float(ncid, NC_GLOBAL, ATT_FLOAT_NAME, float_in); ERR
      err=ncmpi_inq_att(ncid, NC_GLOBAL, ATT_FLOAT_NAME, &xtype, &len); ERR
      if (len || xtype != NC_FLOAT) ERRV
      err=ncmpi_get_att_double(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, double_in); ERR
      err=ncmpi_inq_att(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, &xtype, &len); ERR
      if (len || xtype != NC_DOUBLE) ERRV
      /* Conversions no longer result in range errors, since there's no data. */
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, schar_in); ERR
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_FLOAT_NAME, schar_in); ERR
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_INT_NAME, schar_in); ERR
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_SHORT_NAME, schar_in); ERR
      err=ncmpi_close(ncid); ERR
   }

   if (verbose) printf("ok\n");
   if (verbose) printf("*** testing zero-length attributes and redef...");
   {
      int ncid;
      signed char schar_in[ATT_LEN];
      short short_in[ATT_LEN];
      int int_in[ATT_LEN];
      float float_in[ATT_LEN];
      double double_in[ATT_LEN];


      /* Create a file with a global attribute of each type of zero length. */
      err=ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR
      err=ncmpi_enddef(ncid); ERR
      err=ncmpi_redef(ncid); ERR
      err=ncmpi_put_att_text(ncid, NC_GLOBAL, ATT_TEXT_NAME, 0, NULL); ERR
      err=ncmpi_put_att_schar(ncid, NC_GLOBAL, ATT_SCHAR_NAME, NC_BYTE, 0, NULL); ERR
      err=ncmpi_put_att_short(ncid, NC_GLOBAL, ATT_SHORT_NAME, NC_SHORT, 0, NULL); ERR
      err=ncmpi_put_att_int(ncid, NC_GLOBAL, ATT_INT_NAME, NC_INT, 0, NULL); ERR
      err=ncmpi_put_att_float(ncid, NC_GLOBAL, ATT_FLOAT_NAME, NC_FLOAT, 0, NULL); ERR
      err=ncmpi_put_att_double(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, NC_DOUBLE, 0, NULL); ERR
      err=ncmpi_close(ncid); ERR

      /* Make sure we can read all these zero-length atts added during a
       * redef. */
      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_get_att_text(ncid, NC_GLOBAL, ATT_TEXT_NAME, NULL); ERR
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_SCHAR_NAME, schar_in); ERR
      err=ncmpi_get_att_short(ncid, NC_GLOBAL, ATT_SHORT_NAME, short_in); ERR
      err=ncmpi_get_att_int(ncid, NC_GLOBAL, ATT_INT_NAME, int_in); ERR
      err=ncmpi_get_att_float(ncid, NC_GLOBAL, ATT_FLOAT_NAME, float_in); ERR
      err=ncmpi_get_att_double(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, double_in); ERR
      /* Conversions no longer result in range errors, since there's no data. */
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, schar_in); ERR
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_FLOAT_NAME, schar_in); ERR
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_INT_NAME, schar_in); ERR
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_SHORT_NAME, schar_in); ERR
      err=ncmpi_close(ncid); ERR
   }

   if (verbose) printf("ok\n");
   if (verbose) printf("*** testing attribute deletes and renames...");
   {
      int ncid, varid, dimids[2];
      nc_type att_type;
      MPI_Offset att_len;
      char *speech_in;
      char name_in[NC_MAX_NAME + 1];
      int attid_in, natts_in;
      int int_out[ATT_LEN] = {-100000, 128, 100000};

      /* Create a file with a global attribute. */
      err=ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR
      err=ncmpi_put_att_text(ncid, NC_GLOBAL, ATT_TEXT_NAME, strlen(speech)+1, speech); ERR
      err=ncmpi_close(ncid); ERR
      
      /* Rename it. */
      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_WRITE, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_inq_attid(ncid, NC_GLOBAL, ATT_TEXT_NAME, &attid_in); ERR
      if (attid_in != 0) ERRV
      err=ncmpi_inq_attname(ncid, NC_GLOBAL, attid_in, name_in); ERR
      if (strcmp(name_in, ATT_TEXT_NAME)) ERRV
      err=ncmpi_redef(ncid); ERR
      err=ncmpi_rename_att(ncid, NC_GLOBAL, ATT_TEXT_NAME, ATT_TEXT_NAME2); ERR
      err=ncmpi_inq_attname(ncid, NC_GLOBAL, attid_in, name_in); ERR
      if (strcmp(name_in, ATT_TEXT_NAME2)) ERRV
      err=ncmpi_close(ncid); ERR

      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_WRITE, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_inq_att(ncid, NC_GLOBAL, ATT_TEXT_NAME2, &att_type, &att_len); ERR
      if (att_type != NC_CHAR || att_len != strlen(speech) + 1) ERRV
      if (!(speech_in = malloc(att_len + 1))) ERRV
      else {
          err=ncmpi_get_att_text(ncid, NC_GLOBAL, ATT_TEXT_NAME2, speech_in); ERR
          if (strcmp(speech, speech_in)) ERRV
          if ((err=ncmpi_get_att_text(ncid, NC_GLOBAL, ATT_TEXT_NAME, speech_in)) != NC_ENOTATT) ERR      
          free(speech_in);
      }
      err=ncmpi_close(ncid); ERR

      /* Now delete the att. */
      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_WRITE, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_redef(ncid); ERR
      err=ncmpi_del_att(ncid, NC_GLOBAL, ATT_TEXT_NAME2); ERR
      err=ncmpi_close(ncid); ERR

      /* Now create a file with a variable, which has an att. */
      err=ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR
      err=ncmpi_put_att_text(ncid, NC_GLOBAL, ATT_TEXT_NAME, strlen(speech)+1, speech); ERR
      err=ncmpi_def_dim(ncid, DIM1_NAME, DIM1_LEN, &dimids[0]); ERR
      err=ncmpi_def_dim(ncid, DIM2_NAME, DIM2_LEN, &dimids[1]); ERR
      err=ncmpi_def_var(ncid, VAR1_NAME, NC_INT, 2, dimids, &varid); ERR
      err=ncmpi_put_att_int(ncid, varid, ATT_INT_NAME, NC_INT, 3, int_out); ERR
      err=ncmpi_close(ncid); ERR
      
      /* Reopen the file and delete it. Make sure it's gone. */
      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_WRITE, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_redef(ncid); ERR
      err=ncmpi_del_att(ncid, 0, ATT_INT_NAME); ERR
      err=ncmpi_close(ncid); ERR

      /* Reopen the file and readd the attribute. Enddef and redef,
       * and delete it, then check to make sure it's gone. */
      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_WRITE, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_redef(ncid); ERR
      err=ncmpi_put_att_int(ncid, varid, ATT_INT_NAME, NC_INT, 3, int_out); ERR
      err=ncmpi_enddef(ncid); ERR
      err=ncmpi_redef(ncid); ERR
      err=ncmpi_del_att(ncid, 0, ATT_INT_NAME); ERR
      err=ncmpi_inq_varnatts(ncid, 0, &natts_in); ERR
      if (natts_in != 0) ERRV
      err=ncmpi_close(ncid); ERR
   }

   if (verbose) printf("ok\n");
   if (verbose) printf("*** testing attribute create order...");

#define ATT0 "Maturin"
#define ATT1 "Aubery"
   {
      int ncid, varid, dimids[2];
      int attid_in;
      const int number = 42;

      /* Create a file with several global attributes. */
      err=ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR
      err=ncmpi_put_att_int(ncid, NC_GLOBAL, ATT0, NC_INT, 1, &number); ERR
      err=ncmpi_put_att_int(ncid, NC_GLOBAL, ATT1, NC_INT, 1, &number); ERR
      err=ncmpi_close(ncid); ERR
      
      /* Open it and check the order. */
      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_WRITE, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_inq_attid(ncid, NC_GLOBAL, ATT0, &attid_in); ERR
      if (attid_in != 0) ERRV
      err=ncmpi_inq_attid(ncid, NC_GLOBAL, ATT1, &attid_in); ERR
      if (attid_in != 1) ERRV
      err=ncmpi_close(ncid); ERR

      /* Now create a file with a variable, which has two atts. */
      err=ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR
      err=ncmpi_def_dim(ncid, DIM1_NAME, DIM1_LEN, &dimids[0]); ERR
      err=ncmpi_def_dim(ncid, DIM2_NAME, DIM2_LEN, &dimids[1]); ERR
      err=ncmpi_def_var(ncid, VAR1_NAME, NC_INT, 2, dimids, &varid); ERR
      err=ncmpi_put_att_int(ncid, varid, ATT0, NC_INT, 1, &number); ERR
      err=ncmpi_put_att_int(ncid, varid, ATT1, NC_INT, 1, &number); ERR
      err=ncmpi_close(ncid); ERR
      
      /* Reopen the file and check the order of the attributes on the var. */
      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_WRITE, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_inq_attid(ncid, 0, ATT0, &attid_in); ERR
      if (attid_in != 0) ERRV
      err=ncmpi_inq_attid(ncid, 0, ATT1, &attid_in); ERR
      if (attid_in != 1) ERRV
      err=ncmpi_close(ncid); ERR
   }

   if (verbose) printf("ok\n");
   if (verbose) printf("*** testing attribute ordering some more...");

#define VAR_NAME "i"
#define A1_NAME "i"      
#define A2_NAME "f"      
#define A3_NAME "d"      
#define A1_LEN 3
#define A2_LEN 4
#define A3_LEN 5
   {
      int ncid;
      int varid, natts, nvars;
      double dvalue[] = {999.99, 999.99, 999.99, 999.99, 999.99};
      char name_in[NC_MAX_NAME + 1];

      /* Create a file with one var, and attach three atts to it. */
      err=ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR
      err=ncmpi_def_var(ncid, VAR_NAME, NC_INT, 0, NULL, &varid); ERR
      err=ncmpi_put_att_double(ncid, varid, A1_NAME, NC_INT, A1_LEN, dvalue); ERR
      err=ncmpi_put_att_double(ncid, varid, A2_NAME, NC_INT, A2_LEN, dvalue); ERR
      err=ncmpi_put_att_double(ncid, varid, A3_NAME, NC_INT, A3_LEN, dvalue); ERR
      err=ncmpi_close(ncid); ERR
      
      /* Reopen the file and check. */
      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_inq_nvars(ncid, &nvars); ERR
      if (nvars != 1) ERRV
      err=ncmpi_inq_varnatts(ncid, 0, &natts); ERR
      if (natts != 3) ERRV
      err=ncmpi_inq_attname(ncid, 0, 0, name_in); ERR
      if (strcmp(name_in, A1_NAME)) ERRV
      err=ncmpi_inq_attname(ncid, 0, 1, name_in); ERR
      if (strcmp(name_in, A2_NAME)) ERRV
      err=ncmpi_inq_attname(ncid, 0, 2, name_in); ERR
      if (strcmp(name_in, A3_NAME)) ERRV

      /* Close up shop. */
      err=ncmpi_close(ncid); ERR
   }

   if (verbose) printf("ok\n");
   if (verbose) printf("*** testing attribute ordering even more...");

   /* Test the ordering of atts for each cmode. */
   if (tst_att_ordering(filename, cmode)) ERRV
   if (tst_att_ordering(filename, cmode)) ERRV

   if (verbose) printf("ok\n");
   if (verbose) printf("*** testing attributes and enddef/redef...");

#define ATT_1 "a"
#define ATT_2 "b"
#define ATT_3 "c"
   {
      int ncid, att = 1;

      err=ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR
      err=ncmpi_enddef(ncid); ERR
      err=ncmpi_redef(ncid); ERR
      err=ncmpi_put_att(ncid, NC_GLOBAL, ATT_1, NC_INT, 1, &att); ERR
      err=ncmpi_put_att(ncid, NC_GLOBAL, ATT_2, NC_INT, 1, &att); ERR
      err=ncmpi_put_att(ncid, NC_GLOBAL, ATT_3, NC_INT, 1, &att); ERR

      err=ncmpi_close(ncid); ERR

      err=ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
      err=ncmpi_close(ncid); ERR
   }

   if (verbose) printf("ok\n");
   if (verbose) printf("*** testing copy of simple global atts...");
   {      
      int ncid, ncid2;
      nc_type att_type;
      MPI_Offset att_len;
      int i;

      char *speech_in;
      signed char schar_in[ATT_LEN], schar_out[ATT_LEN] = {NC_MIN_BYTE, 1, NC_MAX_BYTE};
      short short_in[ATT_LEN], short_out[ATT_LEN] = {NC_MIN_SHORT, -128, NC_MAX_SHORT};
      int int_in[ATT_LEN], int_out[ATT_LEN] = {-100000, 128, 100000};
      float float_in[ATT_LEN], float_out[ATT_LEN] = {.5, 0.25, 0.125};
      double double_in[ATT_LEN], double_out[ATT_LEN] = {0.25, .5, 0.125};

      /* Create a file with a global attribute of each type. */
      err=ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL,&ncid); ERR
      err=ncmpi_put_att_text(ncid, NC_GLOBAL, ATT_TEXT_NAME, strlen(speech)+1, speech); ERR
      err=ncmpi_put_att_schar(ncid, NC_GLOBAL, ATT_SCHAR_NAME, NC_BYTE, ATT_LEN, schar_out); ERR
      err=ncmpi_put_att_short(ncid, NC_GLOBAL, ATT_SHORT_NAME, NC_SHORT, ATT_LEN, short_out); ERR
      err=ncmpi_put_att_int(ncid, NC_GLOBAL, ATT_INT_NAME, NC_INT, ATT_LEN, int_out); ERR
      err=ncmpi_put_att_float(ncid, NC_GLOBAL, ATT_FLOAT_NAME, NC_FLOAT, ATT_LEN, float_out); ERR
      err=ncmpi_put_att_double(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, NC_DOUBLE, ATT_LEN, double_out); ERR

      /* Create another file and copy all the attributes. */
      err=ncmpi_create(MPI_COMM_WORLD, filename2, cmode, MPI_INFO_NULL,&ncid2); ERR
      err=ncmpi_copy_att(ncid, NC_GLOBAL, ATT_TEXT_NAME, ncid2, NC_GLOBAL); ERR
      err=ncmpi_copy_att(ncid, NC_GLOBAL, ATT_SCHAR_NAME, ncid2, NC_GLOBAL); ERR
      err=ncmpi_copy_att(ncid, NC_GLOBAL, ATT_SHORT_NAME, ncid2, NC_GLOBAL); ERR
      err=ncmpi_copy_att(ncid, NC_GLOBAL, ATT_INT_NAME, ncid2, NC_GLOBAL); ERR
      err=ncmpi_copy_att(ncid, NC_GLOBAL, ATT_FLOAT_NAME, ncid2, NC_GLOBAL); ERR
      err=ncmpi_copy_att(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, ncid2, NC_GLOBAL); ERR

      /* Close both files. */
      err=ncmpi_close(ncid); ERR
      err=ncmpi_close(ncid2); ERR

      /* Open the file and check attributes. */
      err=ncmpi_open(MPI_COMM_WORLD, filename2, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
      /* Check text. */
      err=ncmpi_inq_att(ncid, NC_GLOBAL, ATT_TEXT_NAME, &att_type, &att_len); ERR
      if (att_type != NC_CHAR || att_len != strlen(speech) + 1) ERRV
      if (!(speech_in = malloc(att_len + 1))) ERRV
      else {
          err=ncmpi_get_att_text(ncid, NC_GLOBAL, ATT_TEXT_NAME, speech_in); ERR
          if (strcmp(speech, speech_in)) ERRV
          free(speech_in);
      }
      /* Check numeric values. */
      err=ncmpi_get_att_schar(ncid, NC_GLOBAL, ATT_SCHAR_NAME, schar_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (schar_in[i] != schar_out[i]) ERRV
      err=ncmpi_get_att_short(ncid, NC_GLOBAL, ATT_SHORT_NAME, short_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (short_in[i] != short_out[i]) ERRV
      err=ncmpi_get_att_int(ncid, NC_GLOBAL, ATT_INT_NAME, int_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (int_in[i] != int_out[i]) ERRV
      err=ncmpi_get_att_float(ncid, NC_GLOBAL, ATT_FLOAT_NAME, float_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (float_in[i] != float_out[i]) ERRV
      err=ncmpi_get_att_double(ncid, NC_GLOBAL, ATT_DOUBLE_NAME, double_in); ERR
      for (i = 0; i < ATT_LEN; i++)
	 if (double_in[i] != double_out[i]) ERRV
      err=ncmpi_close(ncid); ERR
   }
   if (verbose) printf("ok\n");
   return nerrs;
}

int main(int argc, char *argv[])
{
    char filename[256];
    int cmode, rank, nprocs, err, nerrs=0;

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
    sprintf(cmd_str, "*** TESTING C   %s for emulating netCDF tst_atts3 ", basename(argv[0]));
    if (rank == 0) printf("%-66s ------ ", cmd_str);
    free(cmd_str);

    verbose = 0;

    cmode = NC_CLOBBER;
    nerrs += tst_atts3(filename, cmode);

    cmode = NC_CLOBBER | NC_64BIT_OFFSET;
    nerrs += tst_atts3(filename, cmode);

    cmode = NC_CLOBBER | NC_64BIT_DATA;
    nerrs += tst_atts3(filename, cmode);

    MPI_Allreduce(MPI_IN_PLACE, &nerrs, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    /* check if PnetCDF freed all internal malloc */
    MPI_Offset malloc_size, sum_size;
    err = ncmpi_inq_malloc_size(&malloc_size);
    if (err == NC_NOERR) {
        MPI_Reduce(&malloc_size, &sum_size, 1, MPI_OFFSET, MPI_SUM, 0, MPI_COMM_WORLD);
        if (rank == 0 && sum_size > 0)
            printf("heap memory allocated by PnetCDF internally has %lld bytes yet to be freed\n",
                   sum_size);
    }

    if (rank == 0) {
        if (nerrs) printf(FAIL_STR,nerrs);
        else       printf(PASS_STR);
    }

    MPI_Finalize();
    return 0;
}

