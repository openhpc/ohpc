/*
  Copyright 2007, UCAR/Unidata
  See COPYRIGHT file for copying and redistribution conditions.

  This is part of netCDF.
   
  This program runs some extra tests.

  $Id: tst_misc.c,v 1.6 2010/05/05 22:15:36 dmh Exp $
*/

#include <config.h>
#include <nc_tests.h>
#include <stdio.h>
#include <stdlib.h>
#include <netcdf.h>

#define FILE_NAME "tst_misc.nc"

int
main(int argc, char **argv) 
{
   printf("\n*** Testing some extra stuff.\n");
   printf("*** Trying to open non-netCDF files of tiny length...");
   {
#define DATA_LEN 32    
     int ncid,openstat;
      char dummy_data[DATA_LEN];
      FILE *file;
      int i;

      /* Appease valgrind by initializing our data. */
      for (i = 0; i < DATA_LEN; i++)
	 dummy_data[i] = i;

      for (i = DATA_LEN; i >= 0; i--)
      {
	 /* Create a small file which is not a netCDF file. */
	 if (!(file = fopen(FILE_NAME, "w+"))) ERR;
	 if (fwrite(dummy_data, 1, i, file) != i) ERR;
	 if (fclose(file)) ERR;
	 
	 /* Make sure that netCDF rejects this file politely. */
	 openstat = nc_open(FILE_NAME, 0, &ncid);
	 /* Some platforms (OSX, buddy) return stat = 2 (file not found)
	    for index i == 2.  Not sure why, but this is a work around. */
	 if(openstat != NC_ENOTNC && openstat != 2) ERR;
	
      }
   }

   SUMMARIZE_ERR;
   FINAL_RESULTS;
}
