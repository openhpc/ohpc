/*
 *  Copyright (C) 2003, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 */
/* $Id: pnctest.c 2258 2015-12-22 04:50:40Z wkliao $ */

#include <stdio.h>
#include <mpi.h>
#include <pnetcdf.h>
#include <testutils.h>

/* Test program thanks to From: John Tannahill <tannahill1@llnl.gov> */

#define ERR if (err!=NC_NOERR) {printf("Error at line %d: %s\n", __LINE__,ncmpi_strerror(err)); nerrs++;}

int main (int argc, char *argv[])
{
  int dim_id[3], isperiodic[3] = { 0, 0, 0 };
  int err, lat_id, lev_id, lon_id, ncid, totpes, rank, tt_id;
  int  reorder=0, nerrs=0;
  int numpes[3] = { 0, 1, 1 };  /* number of PEs along axes;
                                   determined by MPI where a
                                   zero is specified */
  MPI_Offset TOTSIZ_3D[3] = { 10, 20, 30 };
  MPI_Comm comm_cart;

  MPI_Init (&argc, &argv);
  MPI_Comm_size (MPI_COMM_WORLD, &totpes);
  MPI_Comm_size (MPI_COMM_WORLD, &rank);

  MPI_Dims_create (totpes, 3, numpes);
  MPI_Cart_create (MPI_COMM_WORLD, 3, numpes, isperiodic, reorder, &comm_cart);

  err = ncmpi_create (comm_cart, "testfile.nc", NC_CLOBBER, MPI_INFO_NULL,
                       &ncid); ERR

  err = ncmpi_def_dim (ncid, "level",     TOTSIZ_3D[0], &lev_id); ERR
  err = ncmpi_def_dim (ncid, "latitude",  TOTSIZ_3D[1], &lat_id); ERR
  err = ncmpi_def_dim (ncid, "longitude", TOTSIZ_3D[2], &lon_id); ERR

  dim_id[0] = lev_id;
  dim_id[1] = lat_id;
  dim_id[2] = lon_id;

  err = ncmpi_def_var (ncid, "tt", NC_FLOAT, 3, dim_id, &tt_id); ERR

  err = ncmpi_enddef (ncid); ERR

  err = ncmpi_close (ncid); ERR

  MPI_Comm_free (&comm_cart);

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

  MPI_Finalize ( );
  return 0;
}
