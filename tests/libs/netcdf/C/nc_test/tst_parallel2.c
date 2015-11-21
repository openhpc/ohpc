/* This is a benchmarking program for netCDF-4 parallel I/O. */

/* Defining USE_MPE causes the MPE trace library to be used (and you
 * must also relink with -llmpe -lmpe). This causes clog2 output to be
 * written, which can be converted to slog2 (by the program
 * clog2TOslog2) and then used in the analysis program jumpshot. */
/*#define USE_MPE 1*/

#include <nc_tests.h>
#include <mpi.h>
#include <pnetcdf.h>

#ifdef USE_MPE
#include <mpe.h>
#endif /* USE_MPE */

#define FILE_NAME "tst_parallel2.nc"
#define NDIMS 3
#define DIMSIZE 8
#define NUM_SLABS 8
#define DIM1_NAME "slab"
#define DIM2_NAME "x"
#define DIM3_NAME "y"
#define VAR_NAME "Bond_James_Bond"

int
main(int argc, char **argv)
{
    /* MPI stuff. */
    int mpi_namelen;		
    char mpi_name[MPI_MAX_PROCESSOR_NAME];
    int mpi_size, mpi_rank;
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;
    double start_time = 0, total_time;

    /* Netcdf-4 stuff. */
    int ncid, varid, dimids[NDIMS];
    size_t start[NDIMS] = {0, 0, 0};
    size_t count[NDIMS] = {1, DIMSIZE, DIMSIZE};
    int data[DIMSIZE * DIMSIZE], data_in[DIMSIZE * DIMSIZE];
    int j, i;
    
    char file_name[NC_MAX_NAME + 1];
    int ndims_in, nvars_in, natts_in, unlimdimid_in;

#ifdef USE_MPE
    int s_init, e_init, s_define, e_define, s_write, e_write, s_close, e_close;
#endif /* USE_MPE */

    /* Initialize MPI. */
    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Get_processor_name(mpi_name, &mpi_namelen);
    /*printf("mpi_name: %s size: %d rank: %d\n", mpi_name, mpi_size, mpi_rank);*/

    /* Must be able to evenly divide my slabs between processors. */
    if (NUM_SLABS % mpi_size != 0)
    {
       if (!mpi_rank) printf("NUM_SLABS (%d) is not evenly divisible by mpi_size(%d)\n", 
                             NUM_SLABS, mpi_size);
       ERR;
    }

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
    s_open = MPE_Log_get_event_number(); 
    e_open = MPE_Log_get_event_number(); 
    MPE_Describe_state(s_init, e_init, "Init", "red");
    MPE_Describe_state(s_define, e_define, "Define", "yellow");
    MPE_Describe_state(s_write, e_write, "Write", "green");
    MPE_Describe_state(s_close, e_close, "Close", "purple");
    MPE_Describe_state(s_open, e_open, "Open", "blue");
    MPE_Start_log();
    MPE_Log_event(s_init, 0, "start init");
#endif /* USE_MPE */

/*     if (!mpi_rank) */
/*     { */
/*        printf("\n*** Testing parallel I/O some more.\n"); */
/*        printf("*** writing a %d x %d x %d file from %d processors...\n",  */
/*               NUM_SLABS, DIMSIZE, DIMSIZE, mpi_size); */
/*     } */

    /* We will write the same slab over and over. */
    for (i = 0; i < DIMSIZE * DIMSIZE; i++)
       data[i] = mpi_rank;

#ifdef USE_MPE
    MPE_Log_event(e_init, 0, "end init");
    MPE_Log_event(s_define, 0, "start define file");
#endif /* USE_MPE */

    /* Create a parallel netcdf-4 file. */
    sprintf(file_name, "%s/%s", TEMP_LARGE, FILE_NAME);
    if (nc_create_par(file_name, NC_PNETCDF, comm, info, &ncid)) ERR;

    /* A global attribute holds the number of processors that created
     * the file. */
    if (nc_put_att_int(ncid, NC_GLOBAL, "num_processors", NC_INT, 1, &mpi_size)) ERR;

    /* Create three dimensions. */
    if (nc_def_dim(ncid, DIM1_NAME, NUM_SLABS, dimids)) ERR;
    if (nc_def_dim(ncid, DIM2_NAME, DIMSIZE, &dimids[1])) ERR;
    if (nc_def_dim(ncid, DIM3_NAME, DIMSIZE, &dimids[2])) ERR;

    /* Create one var. */
    if (nc_def_var(ncid, VAR_NAME, NC_INT, NDIMS, dimids, &varid)) ERR;

    /* Write metadata to file. */
    if (nc_enddef(ncid)) ERR;

#ifdef USE_MPE
    MPE_Log_event(e_define, 0, "end define file");
    if (mpi_rank)
       sleep(mpi_rank);
#endif /* USE_MPE */

/*    if (nc_var_par_access(ncid, varid, NC_COLLECTIVE)) ERR;*/
    if (nc_var_par_access(ncid, varid, NC_INDEPENDENT)) ERR;

    if (!mpi_rank)
       start_time = MPI_Wtime();

    /* Write all the slabs this process is responsible for. */
    for (i = 0; i < NUM_SLABS / mpi_size; i++)
    {
       start[0] = NUM_SLABS / mpi_size * mpi_rank + i;

#ifdef USE_MPE
       MPE_Log_event(s_write, 0, "start write slab");
#endif /* USE_MPE */

       /* Write one slab of data. */
       if (nc_put_vara_int(ncid, varid, start, count, data)) ERR;

#ifdef USE_MPE
       MPE_Log_event(e_write, 0, "end write file");
#endif /* USE_MPE */
    }

    if (!mpi_rank)
    {
       total_time = MPI_Wtime() - start_time;
/*       printf("num_proc\ttime(s)\n");*/
       printf("%d\t%g\t%g\n", mpi_size, total_time, DIMSIZE * DIMSIZE * NUM_SLABS * sizeof(int) / total_time);
    }

#ifdef USE_MPE
    MPE_Log_event(s_close, 0, "start close file");
#endif /* USE_MPE */

    /* Close the netcdf file. */
    if (nc_close(ncid))	ERR;
    
#ifdef USE_MPE
    MPE_Log_event(e_close, 0, "end close file");
#endif /* USE_MPE */

    /* Reopen the file and check it. */
    if (nc_open_par(file_name, NC_NOWRITE|NC_PNETCDF, comm, info, &ncid)) ERR;
    if (nc_inq(ncid, &ndims_in, &nvars_in, &natts_in, &unlimdimid_in)) ERR;
    if (ndims_in != NDIMS || nvars_in != 1 || natts_in != 1 || 
        unlimdimid_in != -1) ERR;

    /* Read all the slabs this process is responsible for. */
    for (i = 0; i < NUM_SLABS / mpi_size; i++)
    {
       start[0] = NUM_SLABS / mpi_size * mpi_rank + i;

#ifdef USE_MPE
       MPE_Log_event(s_read, 0, "start read slab");
#endif /* USE_MPE */

       /* Read one slab of data. */
       if (nc_get_vara_int(ncid, varid, start, count, data_in)) ERR;
       
       /* Check data. */
       for (j = 0; j < DIMSIZE * DIMSIZE; j++)
	  if (data_in[j] != mpi_rank) 
	  {
	     ERR;
	     break;
	  }

#ifdef USE_MPE
       MPE_Log_event(e_read, 0, "end read file");
#endif /* USE_MPE */
    }

#ifdef USE_MPE
    MPE_Log_event(s_close, 0, "start close file");
#endif /* USE_MPE */

    /* Close the netcdf file. */
    if (nc_close(ncid))	ERR;
    
#ifdef USE_MPE
    MPE_Log_event(e_close, 0, "end close file");
#endif /* USE_MPE */

    /* Delete this large file. */
    remove(file_name); 

    /* Shut down MPI. */
    MPI_Finalize();

/*     if (!mpi_rank) */
/*     { */
/*        SUMMARIZE_ERR; */
/*        FINAL_RESULTS; */
/*     } */
    return total_err;
}
