/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C test: 
 *  Write >2GB data per process
 *  Then read them all and check if they are correct. 
 *
 * How to run: mpirun -np <N> big_file <steps>
 * Output: big_file.bp
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include "adios.h"
#include "adios_read.h"

#ifdef DMALLOC
#include "dmalloc.h"
#endif

#define log(...) fprintf (stderr, "[rank=%3.3d, line %d]: ", rank, __LINE__); fprintf (stderr, __VA_ARGS__); fflush(stderr);
#define printE(...) fprintf (stderr, "[rank=%3.3d, line %d]: ERROR: ", rank, __LINE__); fprintf (stderr, __VA_ARGS__); fflush(stderr);

int NSTEPS = 1;
static const char FILENAME[] = "big_file.bp";
#define VALUE(rank, step) (step * 1000 + rank + 1)

/* Variables to write */
int  *a2;

/* 2.5GB = 32*20 * 1M * sizeof(int) */
/* 4.25GB = 32*34 * 1M * sizeof(int) */
static const int ldim1 = 1024*32;
static const int ldim2 = 1024*34;
int gdim1, gdim2;
int offs1, offs2;

int64_t       m_adios_group;

/* Variables to read */
int  *r2;

MPI_Comm    comm = MPI_COMM_WORLD;
int rank;
int size;

void alloc_vars()
{
    size_t n;

    n = (size_t)ldim1 * (size_t)ldim2;
    log ("Allocate 2 arrays of %llu integers...\n",n);
    a2  = (int*) malloc (n * sizeof(int));
    r2  = (int*) malloc (n * sizeof(int));
}

void set_gdim()
{
    gdim1 = size*ldim1;
    gdim2 = ldim2;
}

void set_offsets ()
{
    offs1 = rank*ldim1;
    offs2 = 0;
}

void set_vars(int step)
{
    size_t n, i;
    int v = VALUE(rank, step);

    set_offsets();

    n = (size_t)ldim1 * (size_t)ldim2;
    log ("  Fill up array of %llu elements to value %d...\n",n,v);
    for (i=0; i<n; i++) a2[i] = v;
}

void fini_vars()
{
    int i;
    free (a2);
    free (r2);
}

void Usage() 
{
    printf("Usage: big_file <nsteps>\n" 
            "    <nsteps>:  Number of write cycles (to same file)\n");
}

void define_vars ();
int write_file (int step);
int read_file ();

int main (int argc, char ** argv) 
{
    int err,i ; 

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &size);

    if (argc < 2) { Usage(); return 1; }

    errno = 0;
    i = strtol (argv[1], NULL, 10);
    if (errno || i < 1) { printf("Invalid 1st argument %s\n", argv[1]); Usage(); return 1;}
    NSTEPS = i;

    alloc_vars();
    adios_init_noxml (comm);
    adios_allocate_buffer (ADIOS_BUFFER_ALLOC_NOW, 4400);
    err = adios_read_init_method(ADIOS_READ_METHOD_BP, comm, "verbose=2");
    if (err) {
        printE ("%s\n", adios_errmsg());
    }

    adios_declare_group (&m_adios_group, "bigdata", "iter", adios_flag_yes);
    adios_select_method (m_adios_group, "MPI", "", "");


    define_vars();
    set_gdim();
    
    for (i=0; i<NSTEPS; i++) {
        if (!err) {
            err = write_file (i); 
        }
    }

    if (!err)
        err = read_file (); 

    adios_read_finalize_method (ADIOS_READ_METHOD_BP);
    adios_finalize (rank);
    fini_vars();
    MPI_Finalize ();
    return err;
}

void define_vars ()
{
    int i, block;

    adios_define_var (m_adios_group, "ldim1", "", adios_integer, 0, 0, 0);
    adios_define_var (m_adios_group, "ldim2", "", adios_integer, 0, 0, 0);
    adios_define_var (m_adios_group, "gdim1", "", adios_integer, 0, 0, 0);
    adios_define_var (m_adios_group, "gdim2", "", adios_integer, 0, 0, 0);
    adios_define_var (m_adios_group, "offs1", "", adios_integer, 0, 0, 0);
    adios_define_var (m_adios_group, "offs2", "", adios_integer, 0, 0, 0);

    adios_define_var (m_adios_group, "data", "", adios_integer, 
            "iter,ldim1,ldim2",
            "gdim1,gdim2",
            "offs1,offs2");
}

int NVARS=1; 
int NBLOCKS=1;
int write_file (int step) 
{
    int64_t       fh;
    uint64_t       groupsize=0, totalsize;
    int           block, v, i;
    double        tb, te;

    log ("Write step %d to %s\n", step, FILENAME);
    adios_open (&fh, "bigdata", FILENAME, (step ? "a" : "w"), comm);
    
    groupsize  = (4 + NBLOCKS*2) * sizeof(int);             // dimensions 
    log ("  groupsize calculated = %llu\n", groupsize);
    groupsize += ldim1 * ldim2 * sizeof(int);     // 2D  blocks
    //groupsize +=1024;
    log ("  groupsize calculated = %llu\n", groupsize);

    adios_group_size (fh, groupsize, &totalsize);
    log ("  groupsize %llu, totalsize %llu\n", groupsize, totalsize);

    v = VALUE(rank, step);
    log ("  Write data to %s\n", FILENAME);
    set_vars (step);

    tb = MPI_Wtime();
    adios_write (fh, "gdim1", &gdim1);
    adios_write (fh, "gdim2", &gdim2);
    adios_write (fh, "ldim1", &ldim1);
    adios_write (fh, "ldim2", &ldim2);
    adios_write (fh, "offs1", &offs1);
    adios_write (fh, "offs2", &offs2);
    adios_write (fh, "data", a2);
    adios_close (fh);
    te = MPI_Wtime();

    if (rank==0) {
        log ("  Write time for step %d was %6.3lf seconds\n", rank, step, te-tb);
    }
    MPI_Barrier (comm);
    return 0;
}

#if 1
#define CHECK_VARINFO(VARNAME, NDIM, NSTEPS) \
    vi = adios_inq_var (f, VARNAME); \
    if (vi == NULL) { \
        printE ("No such variable: %s\n", VARNAME); \
        err = 101; \
        goto endread; \
    } \
    if (vi->ndim != NDIM) { \
        printE ("Variable %s has %d dimensions, but expected %d\n", VARNAME, vi->ndim, NDIM); \
        err = 102; \
        goto endread; \
    } \
    if (vi->nsteps != NSTEPS) { \
        printE ("Variable %s has %d steps, but expected %d\n", VARNAME, vi->nsteps, NSTEPS); \
        err = 103; \
        /*goto endread; */\
    } \
    adios_free_varinfo (vi);

#define CHECK_SCALAR(VARNAME, VAR, VALUE, STEP) \
    if (VAR != VALUE) { \
        printE (#VARNAME " step %d: wrote %d but read %d\n",STEP,VALUE,VAR);\
        err = 104; \
        /*goto endread;*/\
    }

#define CHECK_ARRAY(VARNAME,A,N,VALUE,STEP,i) \
    for (i=0;i<N;i++) \
        if (A[i] != VALUE) { \
            printE ("%s[%d] step %d: wrote %d but read %d\n",VARNAME,i,STEP,VALUE,A[i]);\
            err = 104; \
            /*goto endread;*/\
            break; \
        }

#define CHECK_2DARRAY(VARNAME,A,DIM1,DIM2,VALUE,STEP,i,j,k) \
    k=0; \
    for (i=0;i<DIM1;i++) {\
        for (j=0;j<DIM2;j++) {\
            if (A[k] != VALUE) { \
                printE ("%s[%d,%d] step %d: wrote %d but read %d\n",VARNAME,i,j,STEP,VALUE,A[k]);\
                err = 104; \
               /*goto endread;*/\
               i=DIM1; /*break to end */\
               break; \
            }\
            k++; \
        }\
    }

void reset_readvars()
{
    size_t n;
    
    n = (size_t)ldim1 * (size_t)ldim2;
    memset (r2,  0, n*sizeof(int));
}

int read_file ()
{
    ADIOS_SELECTION *sel2;
    ADIOS_FILE * f;
    ADIOS_VARINFO * vi;
    int err=0,v,n;
    int block,step,var; // loop variables
    size_t iMacro,jMacro,kMacro; // loop variable in macros
    double        tb, te, tsched;
    double        tsb, ts; // time for just scheduling for one step/block

    uint64_t start[2] = {offs1,offs2};
    uint64_t count[2] = {ldim1,ldim2};
    uint64_t ndim;
    
    reset_readvars();

    log ("Read and check data in %s\n", FILENAME);
    f = adios_read_open_file (FILENAME, ADIOS_READ_METHOD_BP, comm);
    if (f == NULL) {
        printE ("Error at opening file: %s\n", rank, adios_errmsg());
        return 1;
    }


    log ("  Check variable definitions... %s\n", FILENAME);
    tb = MPI_Wtime();
    CHECK_VARINFO("data", 2, NSTEPS)
    MPI_Barrier (comm);
    te = MPI_Wtime();
    if (rank==0) {
        log ("  Time to check all vars' info: %6.3lf seconds\n", te-tb);
    }

    log ("  Check variable content...\n");
    for (step=0; step<NSTEPS; step++) {
        tb = MPI_Wtime();
        ts = 0;
        v = VALUE(rank,step);
        log ("    Step %d expected value=%d\n", step, v);
        set_offsets();
        start[0] = offs1;
        start[1] = offs2;
        log ("    Step %d bounding box start={%llu,%llu}, count={%llu,%llu}\n", 
             step, start[0], start[1], count[0], count[1]);
        sel2 = adios_selection_boundingbox (2, start, count); 

        tsb = MPI_Wtime();
        adios_schedule_read (f, sel2, "data", step, 1, r2);
        ts += MPI_Wtime() - tsb;
        adios_perform_reads (f, 1);
        CHECK_2DARRAY ("data",  r2, count[0], count[1], v, step, iMacro, jMacro, kMacro)

        adios_selection_delete (sel2);
        MPI_Barrier (comm);
        te = MPI_Wtime();
        if (rank==0) {
            log ("  Read time for step %d was %6.3lfs, scheduling reads was %6.3lfs\n", 
                 step, te-tb, ts);
        }
    } 


endread:


    adios_read_close(f);
    MPI_Barrier (comm);
    return err;
}
#endif
