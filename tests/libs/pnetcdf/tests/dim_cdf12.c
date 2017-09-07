/*********************************************************************
 *
 *  Copyright (C) 2016, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 *
 *********************************************************************/
/* $Id$ */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * This program is to test CDF-1, CDF-2 file formats using the allowable
 * maximal dimension size
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <stdio.h>
#include <stdlib.h>
#include <string.h> /* strcpy() */
#include <libgen.h> /* basename() */
#include <limits.h>
#include <mpi.h>
#include <pnetcdf.h>
#include <testutils.h>

#define FOUR_G 4294967296
#define TWO_G  2147483648
#define ONE_G  1073741824

#define NZ 4
#define NY 10
#define NX TWO_G

#define ERR if (err!=NC_NOERR) {printf("Error at line %d: err=%s (%s)\n", __LINE__,nc_err_code_name(err),ncmpi_strerror(err)); nerrs++;}

#define ERR_EXPECT(exp) if (err!=exp) {printf("Error at line %d: expect error %s but got %s\n", __LINE__,nc_err_code_name(exp),nc_err_code_name(err)); nerrs++;}

int main(int argc, char** argv)
{
    char filename[256];
    int rank, nprocs, err, nerrs=0;
    int ncid, cmode, varid, dimid[3];

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

    /* get command-line arguments */
    if (argc > 2) {
        if (!rank) printf("Usage: %s [filename]\n",argv[0]);
        MPI_Finalize();
        return 0;
    }
    if (argc == 2) snprintf(filename, 256, "%s", argv[1]);
    else           strcpy(filename, "testfile.nc");
    MPI_Bcast(filename, 256, MPI_CHAR, 0, MPI_COMM_WORLD);

    if (rank == 0) {
        char *cmd_str = (char*)malloc(strlen(argv[0]) + 256);
        sprintf(cmd_str, "*** TESTING C   %s for defining dim in CDF-1/2 format ", basename(argv[0]));
        printf("%-66s ------ ", cmd_str); fflush(stdout);
        free(cmd_str);
    }

    /* create a new CDF-1 file ----------------------------------------------*/
    cmode = NC_CLOBBER;

    /* max dimension size for CDF-2 file is 2^31-3 = 2147483647 - 3 */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", INT_MAX, &dimid[0]);
    ERR_EXPECT(NC_EDIMSIZE)
    err = ncmpi_def_dim(ncid, "Y", INT_MAX-3, &dimid[0]); ERR
    err = ncmpi_close(ncid); ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_close(ncid); ERR

    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", INT_MAX-3, &dimid[0]); ERR
    err = ncmpi_def_var(ncid, "var0", NC_CHAR, 1, dimid, &varid); ERR
    err = ncmpi_close(ncid); ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_close(ncid); ERR

    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", INT_MAX-3,  &dimid[0]); ERR
    err = ncmpi_def_dim(ncid, "X", 2,          &dimid[1]); ERR
    err = ncmpi_def_var(ncid, "var0", NC_CHAR, 1, &dimid[0], &varid); ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT,  1, &dimid[1], &varid); ERR
    err = ncmpi_close(ncid);
    ERR_EXPECT(NC_EVARSIZE)

    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", INT_MAX-1024,  &dimid[0]); ERR
    err = ncmpi_def_dim(ncid, "X", 2,             &dimid[1]); ERR
    err = ncmpi_def_var(ncid, "var0", NC_CHAR, 1, &dimid[0], &varid); ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT,  1, &dimid[1], &varid); ERR
    err = ncmpi_close(ncid); ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_close(ncid); ERR

    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", INT_MAX-3, &dimid[0]); ERR
    err = ncmpi_def_dim(ncid, "X", 2,         &dimid[1]); ERR
    err = ncmpi_def_var(ncid, "var0", NC_SHORT, 1, &dimid[0], &varid); ERR
    err = ncmpi_def_var(ncid, "var1", NC_CHAR,  1, &dimid[1], &varid); ERR
    err = ncmpi_close(ncid);
    ERR_EXPECT(NC_EVARSIZE)

    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", INT_MAX-3-512-8, &dimid[0]); ERR
    err = ncmpi_def_dim(ncid, "X", 2,       &dimid[1]); ERR
    err = ncmpi_def_var(ncid, "var0", NC_CHAR, 1, &dimid[0], &varid); ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT,  1, &dimid[1], &varid); ERR
    err = ncmpi_close(ncid); ERR

    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", INT_MAX/2+1, &dimid[0]); ERR
    err = ncmpi_def_dim(ncid, "X", 2,           &dimid[1]); ERR
    err = ncmpi_def_var(ncid, "var0", NC_INT, 1, &dimid[0], &varid); ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT, 1, &dimid[1], &varid); ERR
    err = ncmpi_close(ncid);
    ERR_EXPECT(NC_EVARSIZE)

    /* create a new CDF-2 file ----------------------------------------------*/
    cmode = NC_CLOBBER;
    cmode = NC_CLOBBER | NC_64BIT_OFFSET;

    /* max dimension size for CDF-2 file is 2^32-3 = 4294967295 - 3 */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", UINT_MAX, &dimid[0]);
    ERR_EXPECT(NC_EDIMSIZE)
    err = ncmpi_def_dim(ncid, "Y", UINT_MAX-3, &dimid[0]); ERR
    err = ncmpi_close(ncid); ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_close(ncid); ERR

    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", UINT_MAX-3, &dimid[0]); ERR
    err = ncmpi_def_var(ncid, "var0", NC_CHAR, 1, dimid, &varid); ERR
    err = ncmpi_close(ncid); ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_close(ncid); ERR

    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", UINT_MAX-3, &dimid[0]); ERR
    err = ncmpi_def_dim(ncid, "X", 2,          &dimid[1]); ERR
    err = ncmpi_def_var(ncid, "var0", NC_CHAR, 1, &dimid[0], &varid); ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT,  1, &dimid[1], &varid); ERR
    err = ncmpi_close(ncid); ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_close(ncid); ERR

    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", UINT_MAX-3, &dimid[0]); ERR
    err = ncmpi_def_dim(ncid, "X", 2,          &dimid[1]); ERR
    err = ncmpi_def_var(ncid, "var0", NC_SHORT, 1, &dimid[0], &varid); ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT,   1, &dimid[1], &varid); ERR
    err = ncmpi_close(ncid);
    ERR_EXPECT(NC_EVARSIZE)

    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", INT_MAX, &dimid[0]); ERR
    err = ncmpi_def_dim(ncid, "X", 2,       &dimid[1]); ERR
    err = ncmpi_def_var(ncid, "var0", NC_INT, 1, &dimid[0], &varid); ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT, 1, &dimid[1], &varid); ERR
    err = ncmpi_close(ncid);
    ERR_EXPECT(NC_EVARSIZE)

    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", INT_MAX/2+1, &dimid[0]); ERR
    err = ncmpi_def_dim(ncid, "X", 2,           &dimid[1]); ERR
    err = ncmpi_def_var(ncid, "var0", NC_INT, 1, &dimid[0], &varid); ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT, 1, &dimid[1], &varid); ERR
    err = ncmpi_close(ncid);
    ERR_EXPECT(NC_EVARSIZE)

    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_def_dim(ncid, "Y", INT_MAX/2, &dimid[0]); ERR
    err = ncmpi_def_dim(ncid, "X", 2,         &dimid[1]); ERR
    err = ncmpi_def_var(ncid, "var0", NC_INT, 1, &dimid[0], &varid); ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT, 1, &dimid[1], &varid); ERR
    err = ncmpi_close(ncid); ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, MPI_INFO_NULL, &ncid); ERR
    err = ncmpi_close(ncid); ERR

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
    return (nerrs) ? 2 : 0;
}

