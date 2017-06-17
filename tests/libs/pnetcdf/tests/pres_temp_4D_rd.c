/*
 *  Copyright (C) 2003, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 */
/* $Id: pres_temp_4D_rd.c 2744 2016-12-28 16:25:22Z wkliao $ */

/*
   This is an example which reads some 4D pressure and
   temperatures. The data file read by this program is produced by the
   companion program pres_temp_4D_wr.c. It is intended to illustrate
   the use of the netCDF C API.

   This program is part of the netCDF tutorial:
   http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-tutorial

   Full documentation of the netCDF C API can be found at:
   http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-c

   $Id: pres_temp_4D_rd.c 2744 2016-12-28 16:25:22Z wkliao $
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h> /* basename() */
#include <pnetcdf.h>
#include <mpi.h>
#include <testutils.h>

/* This is the name of the data file we will read. */
#define FILE_NAME "pres_temp_4D.nc"

/* We are reading 4D data, a 2 x 6 x 12 lvl-lat-lon grid, with 2
   timesteps of data. */
#define NDIMS 4
#define NLAT 6
#define NLON 12
#define LAT_NAME "latitude"
#define LON_NAME "longitude"
#define NREC 2
#define REC_NAME "time"
#define LVL_NAME "level"
#define NLVL 4

/* Names of things. */
#define PRES_NAME "pressure"
#define TEMP_NAME "temperature"
#define UNITS "units"
#define DEGREES_EAST "degrees_east"
#define DEGREES_NORTH "degrees_north"

/* These are used to calculate the values we expect to find. */
#define SAMPLE_PRESSURE 900
#define SAMPLE_TEMP 9.0
#define START_LAT 25.0
#define START_LON -125.0

/* For the units attributes. */
#define UNITS "units"
#define PRES_UNITS "hPa"
#define TEMP_UNITS "celsius"
#define LAT_UNITS "degrees_north"
#define LON_UNITS "degrees_east"
#define MAX_ATT_LEN 80

/* Handle errors by printing an error message and exiting with a
 * non-zero status. */
#define CHECK_ERR { \
    if (err != NC_NOERR) { \
        nerrs++; \
        printf("Error: %s at line %d (%s)\n", __FILE__,__LINE__,ncmpi_strerror(err)); } }


int
main(int argc, char **argv)
{
   int rank, nprocs, ncid, pres_varid, temp_varid;
   int lat_varid, lon_varid;

   /* The start and count arrays will tell the netCDF library where to
      read our data. */
   MPI_Offset start[NDIMS], count[NDIMS];

   /* Program variables to hold the data we will read. We will only
      need enough space to hold one timestep of data; one record. */
   float **pres_in; /* [NLVL/nprocs][NLAT][NLON] */
   float **temp_in; /* [NLVL/nprocs][NLAT][NLON] */

   /* These program variables hold the latitudes and longitudes. */
   float lats[NLAT], lons[NLON];

   /* Loop indexes. */
   int lvl, lat, lon, rec, i = 0;
   
   /* Error handling. */
   int err, nerrs=0;

   char *filename = FILE_NAME;

   MPI_Init(&argc, &argv);
   MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);

   if (argc > 2) {
       if (!rank) printf("Usage: %s [filename]\n",argv[0]);
       MPI_Finalize();
       return 0;
   }
   if (argc == 2) filename = argv[1];

   if (rank == 0) {
       char *cmd_str = (char*)malloc(strlen(argv[0]) + 256);
       sprintf(cmd_str, "*** TESTING C   %s for reading file", basename(argv[0]));
       printf("%-66s ------ ", cmd_str);
       free(cmd_str);
   }

   /* Open the file. */
   err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid);
   CHECK_ERR

   /* Get the varids of the latitude and longitude coordinate
    * variables. */
   err = ncmpi_inq_varid(ncid, LAT_NAME, &lat_varid);
   CHECK_ERR
   err = ncmpi_inq_varid(ncid, LON_NAME, &lon_varid);
   CHECK_ERR

   /* Read the coordinate variable data. */
   err = ncmpi_get_var_float_all(ncid, lat_varid, &lats[0]);
   CHECK_ERR
   err = ncmpi_get_var_float_all(ncid, lon_varid, &lons[0]);
   CHECK_ERR

   /* Check the coordinate variable data. */
   for (lat = 0; lat < NLAT; lat++)
      if (lats[lat] != START_LAT + 5.*lat)
	 return 2;
   for (lon = 0; lon < NLON; lon++)
      if (lons[lon] != START_LON + 5.*lon)
	 return 2;

   /* Get the varids of the pressure and temperature netCDF
    * variables. */
   err = ncmpi_inq_varid(ncid, PRES_NAME, &pres_varid);
   CHECK_ERR
   err = ncmpi_inq_varid(ncid, TEMP_NAME, &temp_varid);
   CHECK_ERR

   /* Read the data. Since we know the contents of the file we know
    * that the data arrays in this program are the correct size to
    * hold one timestep. */
   count[0] = 1;
   count[2] = NLAT;
   count[3] = NLON;
   start[2] = 0;
   start[3] = 0;
 
   /* divide NLVL dimension among processes */
   count[1] = NLVL / nprocs;
   start[1] = count[1] * rank;
   if (rank < NLVL % nprocs) {
       start[1] += rank;
       count[1]++;
   }
   else {
       start[1] += NLVL % nprocs;
   }
   if (count[1] == 0) start[1] = 0;

   /* allocate read buffers */
   pres_in = (float**) malloc(count[1]*2 * sizeof(float*));
   temp_in = pres_in + count[1];
   if (count[1] > 0) {
       pres_in[0] = (float*) malloc(count[1]*2 * NLAT*NLON * sizeof(float));
       temp_in[0] = pres_in[0] + count[1] * NLAT*NLON;
       for (i=1; i<count[1]; i++) {
           pres_in[i] = pres_in[i-1] + NLAT*NLON;
           temp_in[i] = temp_in[i-1] + NLAT*NLON;
       }
   }

   /* Read and check one record at a time. */
   for (rec = 0; rec < NREC; rec++)
   {
      start[0] = rec;
      err = ncmpi_get_vara_float_all(ncid, pres_varid, start, count, &pres_in[0][0]);
      CHECK_ERR
      err = ncmpi_get_vara_float_all(ncid, temp_varid, start, count, &temp_in[0][0]);
      CHECK_ERR

      /* Check the data. */
      i = (int)start[1] * NLAT * NLON;
      for (lvl=0; lvl<count[1]; lvl++)
	 for (lat = 0; lat < NLAT; lat++)
	    for (lon = 0; lon < NLON; lon++)
	    {
	       if (pres_in[lvl][lat*NLON+lon] != SAMPLE_PRESSURE + i) 
		  nerrs++;
	       if (temp_in[lvl][lat*NLON+lon] != SAMPLE_TEMP + i) 
		  nerrs++;
	       i++;
	    }

   } /* next record */

   /* Close the file. */
   err = ncmpi_close(ncid);
   CHECK_ERR

   if (count[1] > 0) free(pres_in[0]);
   free(pres_in);

    /* check if there is any malloc residue */
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
   return (nerrs) ? 2 : 0;
}
