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
#define FILENAME "tst_formatx.nc"

int main(int argc, char* argv[])
{
    int err = 0;
    int ecode = 0;
    int ncid;
    int cmode, format;   
    int nprocs, rank;
    MPI_Comm comm=MPI_COMM_SELF;
    MPI_Info info=MPI_INFO_NULL;

    printf("\n*** Testing nc_inq_format_extended for pnetcdf...");

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

    if (nc_enddef(ncid)) ERR;

    if(nc_inq_format_extended(ncid,&format,&cmode)) ERR;
    if(cmode != 0x8000) {
	printf("***FAIL: mode was %08x ; expected %08x\n",cmode,0);
	ecode = 1;
	ERR;
    }
    if(format != NC_FORMAT_PNETCDF) {
	printf("***FAIL: format was %d ; expected %d\n",format,NC_FORMAT_PNETCDF);
	ecode = 1;
	ERR;
    }

    if (nc_abort(ncid)) ERR;

fn_exit:
    MPI_Finalize();
    SUMMARIZE_ERR;
    FINAL_RESULTS;
    return ecode;
}
