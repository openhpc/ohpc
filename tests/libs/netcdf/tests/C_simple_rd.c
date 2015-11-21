/** \file
Read the simple_xy file, with some of the features of netCDF-4.

This is a very simple example which is based on the simple_xy example,
but whch uses netCDF-4 features, such as compression. Please see the
simple_xy example to learn more about the netCDF-3 API.

Like simple_xy_rd.c, this example reads a small dummy array, which was
written by simple_xy_wr.c, and is compressed. This is intended to
illustrate the use of the netCDF C API.

Full documentation for netCDF can be found at:
http://www.unidata.ucar.edu/netcdf/docs
*/

#include <stdlib.h>
#include <stdio.h>
#include <netcdf.h>

/* This is the name of the data file we will read. */
#define FILE_NAME "simple_xy_nc4.nc"

/* We are reading 2D data, a 6 x 12 grid. */
#define NX 6
#define NY 12

/* Handle errors by printing an error message and exiting with a
 * non-zero status. */
#define ERRCODE 2
#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(ERRCODE);}

int
main()
{
   /* This will be the netCDF ID for the file and data variable. */
   int ncid, varid;

   int data_in[NX][NY];

   /* Loop indexes, and error handling. */
   int x, y, retval;

   /* Open the file. NC_NOWRITE tells netCDF we want read-only access
    * to the file.*/
   if ((retval = nc_open(FILE_NAME, NC_NOWRITE, &ncid)))
      ERR(retval);

   /* Get the varid of the data variable, based on its name. */
   if ((retval = nc_inq_varid(ncid, "data", &varid)))
      ERR(retval);

   /* Read the data. */
   if ((retval = nc_get_var_int(ncid, varid, &data_in[0][0])))
      ERR(retval);

   /* Check the data. */
   for (x = 0; x < NX; x++)
      for (y = 0; y < NY; y++)
	 if (data_in[x][y] != x * NY + y)
	    return ERRCODE;

   /* Close the file, freeing all resources. */
   if ((retval = nc_close(ncid)))
      ERR(retval);

   printf("*** SUCCESS reading example file %s!\n", FILE_NAME);
   return 0;
}
