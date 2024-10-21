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
#include <mpi.h>
#include <pnetcdf.h>
#include <testutils.h>

int main(int argc, char** argv)
{
    char filename[256];
    int rank, nprocs, err, nerrs=0;
    int ncid, cmode, varid, dimid[3];
    MPI_Info info=MPI_INFO_NULL;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

    /* Note this test program must use the 512-byte alignment setting */
    MPI_Info_create(&info);
    /* use the 512-byte header align size */
    MPI_Info_set(info, "nc_header_align_size", "512");
    /* use the 512-byte fixed-size variable starting file offset alignment */
    MPI_Info_set(info, "nc_var_align_size", "512");

    /* get command-line arguments */
    if (argc > 2) {
        if (!rank) printf("Usage: %s [filename]\n",argv[0]);
        MPI_Finalize();
        return 1;
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

    /* max dimension size for CDF-1 file is NC_MAX_INT */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", (MPI_Offset)1+NC_MAX_INT, &dimid[0]);
    EXP_ERR(NC_EDIMSIZE)
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT, &dimid[0]); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, info, &ncid); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR

    /* use the max dimension size to define a 1D variable */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT, &dimid[0]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_CHAR, 1, dimid, &varid); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, info, &ncid); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR

    /* use the max dimension size to define a 1D variable, followed by
     * another variable to make the file size > NC_MAX_INT */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT, &dimid[0]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "X", 2,          &dimid[1]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_CHAR, 1, &dimid[0], &varid); CHECK_ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT,  1, &dimid[1], &varid); CHECK_ERR
    /* for cdf-1, adding var1 after var0 will cause NC_EVARSIZE */
    err = ncmpi_close(ncid);
    EXP_ERR(NC_EVARSIZE)

    /* use the max dimension size - 1024 to define a 1D variable, followed
     * by another variable to make the file size < 2147483647 */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT-1024, &dimid[0]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "X", 2,               &dimid[1]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_CHAR, 1, &dimid[0], &varid); CHECK_ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT,  1, &dimid[1], &varid); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, info, &ncid); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR

    /* define the first variable of type short that makes the file size >
     * NC_MAX_INT. error should be reported in ncmpi_enddef() or
     * ncmpi_close() */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT, &dimid[0]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "X", 2,          &dimid[1]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_SHORT, 1, &dimid[0], &varid); CHECK_ERR
    err = ncmpi_def_var(ncid, "var1", NC_CHAR,  1, &dimid[1], &varid); CHECK_ERR
    err = ncmpi_close(ncid);
    EXP_ERR(NC_EVARSIZE)

    /* define two variables to make the file size just < 2147483647 */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT-512-8, &dimid[0]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "X", 2,                &dimid[1]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_CHAR, 1, &dimid[0], &varid); CHECK_ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT,  1, &dimid[1], &varid); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR

    /* define two variables to make the file size just > NC_MAX_INT */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT/2+1, &dimid[0]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "X", 2,              &dimid[1]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_INT, 1, &dimid[0], &varid); CHECK_ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT, 1, &dimid[1], &varid); CHECK_ERR
    err = ncmpi_close(ncid);
    EXP_ERR(NC_EVARSIZE)

    /* create a new CDF-2 file ----------------------------------------------*/
    cmode = NC_CLOBBER | NC_64BIT_OFFSET;

    /* max dimension size for CDF-2 file is NC_MAX_INT */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", (MPI_Offset)1+NC_MAX_INT, &dimid[0]);
    EXP_ERR(NC_EDIMSIZE)
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT, &dimid[0]); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, info, &ncid); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR

    /* use the max dimension size to define a 1D variable */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT, &dimid[0]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_CHAR, 1, dimid, &varid); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, info, &ncid); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR

    /* use the max dimension size to define a 1D variable, followed by
     * another variable to make the file size > 2 * NC_MAX_INT */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT, &dimid[0]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "X", 2,          &dimid[1]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_CHAR, 1, &dimid[0], &varid); CHECK_ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT,  1, &dimid[1], &varid); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, info, &ncid); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR

    /* define the first variable of type short that makes the file size >
     * 4294967295. error should be reported in ncmpi_enddef() or
     * ncmpi_close() */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT, &dimid[0]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "X", 2,       &dimid[1]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_SHORT, 1, &dimid[0], &varid); CHECK_ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT,   1, &dimid[1], &varid); CHECK_ERR
    err = ncmpi_close(ncid);
    EXP_ERR(NC_EVARSIZE)

    /* define 2 1D int variables of dimension size > NC_MAX_INT */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT, &dimid[0]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "X", 2,          &dimid[1]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_INT, 1, &dimid[0], &varid); CHECK_ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT, 1, &dimid[1], &varid); CHECK_ERR
    err = ncmpi_close(ncid);
    EXP_ERR(NC_EVARSIZE)

    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT/2+1, &dimid[0]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "X", 2,              &dimid[1]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_INT, 1, &dimid[0], &varid); CHECK_ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT, 1, &dimid[1], &varid); CHECK_ERR
    err = ncmpi_close(ncid);
    EXP_ERR(NC_EVARSIZE)

    /* No record variable can require more than 2^32 - 4 bytes of storage for
     * each record's worth of data, unless it is the last record variable.
     */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Z", NC_UNLIMITED, &dimid[0]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT/64,   &dimid[1]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "X", 64,              &dimid[2]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_INT, 3, dimid, &varid); CHECK_ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT, 3, dimid, &varid); CHECK_ERR
    err = ncmpi_close(ncid);
    EXP_ERR(NC_EVARSIZE)

    /* test large record variable that is not defined last */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Z", NC_UNLIMITED, &dimid[0]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT/64,   &dimid[1]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "X", 64,              &dimid[2]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_INT, 3, dimid, &varid); CHECK_ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT, 2, dimid, &varid); CHECK_ERR
    err = ncmpi_close(ncid);
    EXP_ERR(NC_EVARSIZE)

    /* Note for developers: keep the last test that produces no error, so the
     * output file can be tested by ncvalidator in wrap_runs.sh
     */
    err = ncmpi_create(MPI_COMM_WORLD, filename, cmode, info, &ncid); CHECK_ERR
    err = ncmpi_def_dim(ncid, "Y", NC_MAX_INT/2, &dimid[0]); CHECK_ERR
    err = ncmpi_def_dim(ncid, "X", 2,            &dimid[1]); CHECK_ERR
    err = ncmpi_def_var(ncid, "var0", NC_INT, 1, &dimid[0], &varid); CHECK_ERR
    err = ncmpi_def_var(ncid, "var1", NC_INT, 1, &dimid[1], &varid); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR
    err = ncmpi_open(MPI_COMM_WORLD, filename, NC_NOWRITE, info, &ncid); CHECK_ERR
    err = ncmpi_close(ncid); CHECK_ERR

    MPI_Info_free(&info);

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
    return (nerrs > 0);
}