/* 
Copyright 2009, UCAR/Unidata
See COPYRIGHT file for copying and redistribution conditions.

This program tests netcdf-4 parallel I/O. 

$Id: tst_parallel.c,v 1.7 2009/08/19 15:58:57 ed Exp $
*/

/* Defining USE_MPE causes the MPE trace library to be used (and you
 * must also relink with -llmpe -lmpe). This causes clog2 output to be
 * written, which can be converted to slog2 (by the program
 * clog2TOslog2) and then used in the analysis program jumpshot. */
/*#define USE_MPE 1*/

#include <nc_tests.h>
#include <mpi.h>
#ifdef USE_MPE
#include <mpe.h>
#endif /* USE_MPE */
#define FILE "tst_parallel.nc"
#define NDIMS 3
#define DIMSIZE 24
#define QTR_DATA (DIMSIZE * DIMSIZE / 4)
#define NUM_PROC 4
#define NUM_SLABS 10

int
main(int argc, char **argv)
{
    /* MPI stuff. */
    int mpi_namelen;		
    char mpi_name[MPI_MAX_PROCESSOR_NAME];
    int mpi_size, mpi_rank;
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    /* Netcdf-4 stuff. */
    int ncid, v1id, dimids[NDIMS];
    size_t start[NDIMS], count[NDIMS];

    int data[DIMSIZE * DIMSIZE], i, res;
    int slab_data[DIMSIZE * DIMSIZE / 4]; /* one slab */
    char file_name[NC_MAX_NAME + 1];

#ifdef USE_MPE
    int s_init, e_init, s_define, e_define, s_write, e_write, s_close, e_close;
#endif /* USE_MPE */

    /* Initialize MPI. */
    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Get_processor_name(mpi_name, &mpi_namelen);
    /*printf("mpi_name: %s size: %d rank: %d\n", mpi_name, 
      mpi_size, mpi_rank);*/

#ifdef USE_MPE
    MPE_Init_log();
    s_init = MPE_Log_get_event_number(); 
    e_init = MPE_Log_get_event_number(); 
    s_define = MPE_Log_get_event_number(); 
    e_define = MPE_Log_get_event_number(); 
    s_write = MPE_Log_get_event_number(); 
    e_write = MPE_Log_get_event_number(); 
    s_close = MPE_Log_get_event_number(); 
    e_close = MPE_Log_get_event_number(); 
    MPE_Describe_state(s_init, e_init, "Init", "red");
    MPE_Describe_state(s_define, e_define, "Define", "yellow");
    MPE_Describe_state(s_write, e_write, "Write", "green");
    MPE_Describe_state(s_close, e_close, "Close", "purple");
    MPE_Start_log();
    MPE_Log_event(s_init, 0, "start init");
#endif /* USE_MPE */

    if (mpi_rank == 1)
    {
       printf("\n*** tst_parallel testing very basic parallel access.\n");
       printf("*** tst_parallel testing whether we can create file for parallel access and write to it...");
    }

    /* Create phony data. We're going to write a 24x24 array of ints,
       in 4 sets of 144. */
    /*printf("mpi_rank*QTR_DATA=%d (mpi_rank+1)*QTR_DATA-1=%d\n",
      mpi_rank*QTR_DATA, (mpi_rank+1)*QTR_DATA);*/
    for (i = mpi_rank * QTR_DATA; i < (mpi_rank + 1) * QTR_DATA; i++)
       data[i] = mpi_rank;
    for (i = 0; i < DIMSIZE * DIMSIZE / 4; i++)
       slab_data[i] = mpi_rank;

#ifdef USE_MPE
    MPE_Log_event(e_init, 0, "end init");
    MPE_Log_event(s_define, 0, "start define file");
#endif /* USE_MPE */

    /* Create a parallel netcdf-4 file. */
    /*nc_set_log_level(3);*/
    sprintf(file_name, "%s/%s", TEMP_LARGE, FILE);
    if ((res = nc_create_par(file_name, NC_NETCDF4|NC_MPIIO, comm, 
			     info, &ncid))) ERR;

    /* Create three dimensions. */
    if (nc_def_dim(ncid, "d1", DIMSIZE, dimids)) ERR;
    if (nc_def_dim(ncid, "d2", DIMSIZE, &dimids[1])) ERR;
    if (nc_def_dim(ncid, "d3", NUM_SLABS, &dimids[2])) ERR;

    /* Create one var. */
    if ((res = nc_def_var(ncid, "v1", NC_INT, NDIMS, dimids, &v1id))) ERR;

    /* Write metadata to file. */
    if ((res = nc_enddef(ncid))) ERR;

#ifdef USE_MPE
    MPE_Log_event(e_define, 0, "end define file");
    if (mpi_rank)
       sleep(mpi_rank);
#endif /* USE_MPE */

    /* Set up slab for this process. */
    start[0] = mpi_rank * DIMSIZE/mpi_size;
    start[1] = 0;
    count[0] = DIMSIZE/mpi_size;
    count[1] = DIMSIZE;
    count[2] = 1;
    /*printf("mpi_rank=%d start[0]=%d start[1]=%d count[0]=%d count[1]=%d\n",
      mpi_rank, start[0], start[1], count[0], count[1]);*/

    if (nc_var_par_access(ncid, v1id, NC_COLLECTIVE)) ERR;
/*    if (nc_var_par_access(ncid, v1id, NC_INDEPENDENT)) ERR;*/

    for (start[2] = 0; start[2] < NUM_SLABS; start[2]++)
    {
#ifdef USE_MPE
       MPE_Log_event(s_write, 0, "start write slab");
#endif /* USE_MPE */

       /* Write slabs of phoney data. */
       if (nc_put_vara_int(ncid, v1id, start, count, slab_data)) ERR;
#ifdef USE_MPE
       MPE_Log_event(e_write, 0, "end write file");
#endif /* USE_MPE */
    }

#ifdef USE_MPE
    MPE_Log_event(s_close, 0, "start close file");
#endif /* USE_MPE */

    /* Close the netcdf file. */
    if ((res = nc_close(ncid)))	ERR;
    
#ifdef USE_MPE
    MPE_Log_event(e_close, 0, "end close file");
#endif /* USE_MPE */

    /* Delete this large file. */
#if 0
    remove(file_name); 
#endif

    /* Shut down MPI. */
    MPI_Finalize();

    if (mpi_rank == 1)
    {
       SUMMARIZE_ERR;
       FINAL_RESULTS;
    }
    return 0;
}
