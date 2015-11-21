/* This is part of the netCDF package.
   Copyright 2005 University Corporation for Atmospheric Research/Unidata
   See COPYRIGHT file for conditions of use.

   Test fix of bug involving creation of a file with pnetcdf APIs,
   then opening and modifying the file with netcdf.

   Author: Wei-keng Liao.
*/

#include <nc_tests.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>
#include <netcdf.h>
#include <netcdf_par.h>
#include <assert.h>

#define NVARS 6
#define NX    5
#define FILENAME "tst_pnetcdf.nc"

int main(int argc, char* argv[])
{
    int i, j, rank, nprocs, ncid, cmode, varid[NVARS], dimid[2], *buf;
    int err = 0;
    char str[32];
    size_t start[2], count[2];
    MPI_Comm comm=MPI_COMM_SELF;
    MPI_Info info=MPI_INFO_NULL;

    printf("\n*** Testing bug fix with changing pnetcdf variable offsets...");

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (nprocs > 1 && rank == 0)
        printf("This test program is intended to run on ONE process\n");
    if (rank > 0) goto fn_exit;

    /* first, use PnetCDF to create a file with default header/variable alignment */
#ifdef DISABLE_PNETCDF_ALIGNMENT
    MPI_Info_create(&info);
    MPI_Info_set(info, "nc_header_align_size", "1");
    MPI_Info_set(info, "nc_var_align_size",    "1");
#endif

    cmode = NC_PNETCDF | NC_CLOBBER;
    if (nc_create_par(FILENAME, cmode, comm, info, &ncid)) ERR_RET;

    /* define dimension */
    if (nc_def_dim(ncid, "Y", NC_UNLIMITED, &dimid[0])) ERR;
    if (nc_def_dim(ncid, "X", NX,           &dimid[1])) ERR;

    /* Odd numbers are fixed variables, even numbers are record variables */
    for (i=0; i<NVARS; i++) {
        if (i%2) {
            sprintf(str,"fixed_var_%d",i);
            if (nc_def_var(ncid, str, NC_INT, 1, dimid+1, &varid[i])) ERR;
        }
        else {
            sprintf(str,"record_var_%d",i);
            if (nc_def_var(ncid, str, NC_INT, 2, dimid, &varid[i])) ERR;
        }
    }
    if (nc_enddef(ncid)) ERR;

    for (i=0; i<NVARS; i++) {
        /* Note NC_INDEPENDENT is the default */
        if (nc_var_par_access(ncid, varid[i], NC_INDEPENDENT)) ERR;
    }

    /* write all variables */
    buf = (int*) malloc(NX * sizeof(int));
    for (i=0; i<NVARS; i++) {
        for (j=0; j<NX; j++) buf[j] = i*10 + j;
        if (i%2) {
            start[0] = 0; count[0] = NX;
            if (nc_put_vara_int(ncid, varid[i], start, count, buf)) ERR;
        }
        else {
            start[0] = 0; start[1] = 0;
            count[0] = 1; count[1] = NX;
            if (nc_put_vara_int(ncid, varid[i], start, count, buf)) ERR;
        }
    }
    if (nc_close(ncid)) ERR;
    if (info != MPI_INFO_NULL) MPI_Info_free(&info);

    /* re-open the file with netCDF (parallel) and enter define mode */
    if (nc_open_par(FILENAME, NC_WRITE|NC_PNETCDF, comm, info, &ncid)) ERR_RET;
    if (nc_redef(ncid)) ERR;

    /* add attributes to make header grow */
    for (i=0; i<NVARS; i++) {
        sprintf(str, "annotation_for_var_%d",i);
        if (nc_put_att_text(ncid, varid[i], "text_attr", strlen(str), str)) ERR;
    }
    if (nc_enddef(ncid)) ERR;

    /* read variables and check their contents */
    for (i=0; i<NVARS; i++) {
        for (j=0; j<NX; j++) buf[j] = -1;
        if (i%2) {
            start[0] = 0; count[0] = NX;
            if (nc_get_var_int(ncid, varid[i], buf)) ERR;
            for (j=0; j<NX; j++)
                if (buf[j] != i*10 + j)
                    printf("unexpected read value var i=%d buf[j=%d]=%d should be %d\n",i,j,buf[j],i*10+j);
        }
        else {
            start[0] = 0; start[1] = 0;
            count[0] = 1; count[1] = NX;
            if (nc_get_vara_int(ncid, varid[i], start, count, buf)) ERR;
            for (j=0; j<NX; j++)
                if (buf[j] != i*10+j)
                    printf("unexpected read value var i=%d buf[j=%d]=%d should be %d\n",i,j,buf[j],i*10+j);
        }
    }
    if (nc_close(ncid)) ERR;

fn_exit:
    MPI_Finalize();
    SUMMARIZE_ERR;
    FINAL_RESULTS;
    return 0;
}

/*
    Compile:
        mpicc -g -o nc_pnc nc_pnc.c -lnetcdf -lcurl -lhdf5_hl -lhdf5 -lpnetcdf -lz -lm

    Run:
        nc_pnc

    Standard Output:
        At the time of this test is written, I used the following libraries.
            HDF5    version 1.8.10
            netCDF  version 4.2.1.1 and
            PnetCDF version 1.3.1

        If macro DISABLE_PNETCDF_ALIGNMENT is defined (i.e. disable PnetCDF
        alignment) then there is no standard output.

        If macro DISABLE_PNETCDF_ALIGNMENT is NOT defined (i.e. default PnetCDF
        alignment) then this test reports unexpected read values below.

         unexpected read value var i=1 buf[j=0]=0 should be 10
         unexpected read value var i=1 buf[j=1]=0 should be 11
         unexpected read value var i=1 buf[j=2]=0 should be 12
         unexpected read value var i=1 buf[j=3]=0 should be 13
         unexpected read value var i=1 buf[j=4]=0 should be 14
         unexpected read value var i=3 buf[j=0]=0 should be 30
         unexpected read value var i=3 buf[j=1]=0 should be 31
         unexpected read value var i=3 buf[j=2]=0 should be 32
         unexpected read value var i=3 buf[j=3]=0 should be 33
         unexpected read value var i=3 buf[j=4]=0 should be 34
         unexpected read value var i=5 buf[j=0]=0 should be 50
         unexpected read value var i=5 buf[j=1]=0 should be 51
         unexpected read value var i=5 buf[j=2]=0 should be 52
         unexpected read value var i=5 buf[j=3]=0 should be 53
         unexpected read value var i=5 buf[j=4]=0 should be 54
*/
