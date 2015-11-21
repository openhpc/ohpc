/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C test: 
 *  Write a huge number of variables
 *  Then read them all and check if they are correct. 
 *
 * How to run: mpirun -np <N> many_vars <nvars> <blocks per process> <steps>
 * Output: many_vars.bp
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

int NVARS = 1;
int NBLOCKS = 1;
int NSTEPS = 1;
static const char FILENAME[] = "many_vars.bp";
#define VALUE(rank, step, block) (step * 10000 + 10*rank + block)

/* Variables to write */
int  *a2;

static const int ldim1 = 5;
static const int ldim2 = 5;
int gdim1, gdim2;
int offs1, offs2;

int64_t       m_adios_group;

/* Variables to read */
int  *r2;

MPI_Comm    comm = MPI_COMM_WORLD;
int rank;
int size;
char ** varnames;

void alloc_vars()
{
    int n,i;

    n = ldim1 * ldim2;
    a2  = (int*) malloc (n * sizeof(int));
    r2  = (int*) malloc (n * sizeof(int));
    varnames = (char**) malloc (NVARS * sizeof(char*));
    for (i=0; i<NVARS; i++) {
        varnames[i] = (char*) malloc (16);
    }

    /* make varnames like v001,v002,.. */
    int digit=1, d=10;
    while (NVARS/d > 0) {
        d *= 10;
        digit++;
    }

    char fmt[16];
    sprintf (fmt, "v%%%d.%dd",digit,digit);
    printf ("ftm=[%s]\n", fmt);
    for (i=0; i<NVARS; i++) {
        //sprintf(varnames[i], "v%-*d", digit, i);
        sprintf(varnames[i], fmt, i);
    }
    printf ("varname[0]=%s\n", varnames[0]);
    printf ("varname[%d]=%s\n", NVARS-1, varnames[NVARS-1]);
}

void set_gdim()
{
    gdim1 = size*ldim1;
    gdim2 = NBLOCKS*ldim2;
}

void set_offsets (int block)
{
    offs1 = rank*ldim1;
    offs2 = block*ldim2;
}

void set_vars(int step, int block)
{
    int n, i;
    int v = VALUE(rank, step, block);

    set_offsets(block);

    n = ldim1 * ldim2;
    for (i=0; i<n; i++) a2[i] = v;
}

void fini_vars()
{
    int i;
    free (a2);
    free (r2);
    for (i=0; i<NVARS; i++) {
        free(varnames[i]);
    }
    free(varnames);
}

void Usage() 
{
    printf("Usage: many_vars <nvars> <nblocks> <nsteps>\n" 
            "    <nvars>:   Number of variables to generate\n"
            "    <nblocks>: Number of blocks per process to write\n"
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
    NVARS = i;

    if (argc > 2) {
        i = strtol (argv[2], NULL, 10);
        if (errno || i < 1) { printf("Invalid 2nd argument %s\n", argv[2]); Usage(); return 1;}
        NBLOCKS = i;
    }

    if (argc > 3) {
        i = strtol (argv[3], NULL, 10);
        if (errno || i < 1) { printf("Invalid 3rd argument %s\n", argv[3]); Usage(); return 1;}
        NSTEPS = i;
    }

    alloc_vars();
    adios_init_noxml (comm);
    adios_allocate_buffer (ADIOS_BUFFER_ALLOC_NOW, 100);
    err = adios_read_init_method(ADIOS_READ_METHOD_BP, comm, "verbose=2");
    if (err) {
        printE ("%s\n", adios_errmsg());
    }

    adios_declare_group (&m_adios_group, "multiblock", "iter", adios_flag_yes);
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

    adios_free_group (m_adios_group);
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

    for (block=0; block<NBLOCKS; block++) {
        adios_define_var (m_adios_group, "offs1", "", adios_integer, 0, 0, 0);
        adios_define_var (m_adios_group, "offs2", "", adios_integer, 0, 0, 0);

        for (i=0; i<NVARS; i++) {
            adios_define_var (m_adios_group, varnames[i], "", adios_integer, 
                    "iter,ldim1,ldim2",
                    "gdim1,gdim2",
                    "offs1,offs2");
        }
    }
}

int write_file (int step) 
{
    int64_t       fh;
    uint64_t       groupsize=0, totalsize;
    int           block, v, i;
    double        tb, te;

    log ("Write step %d to %s\n", step, FILENAME);
    tb = MPI_Wtime();
    adios_open (&fh, "multiblock", FILENAME, (step ? "a" : "w"), comm);
    
    groupsize  = (4 + NBLOCKS*2) * sizeof(int);             // dimensions 
    groupsize += NVARS * NBLOCKS * ldim1 * ldim2 * sizeof(int);     // 2D  blocks
    //groupsize +=1024;

    adios_group_size (fh, groupsize, &totalsize);
    log ("  groupsize %lld, totalsize %lld\n", groupsize, totalsize);

    adios_write (fh, "gdim1", &gdim1);
    adios_write (fh, "gdim2", &gdim2);
    adios_write (fh, "ldim1", &ldim1);
    adios_write (fh, "ldim2", &ldim2);

    for (block=0; block<NBLOCKS; block++) {
        v = VALUE(rank, step, block);
        log ("  Write block %d, value %d to %s\n", block, v, FILENAME);
        set_vars (step, block);

        adios_write (fh, "offs1", &offs1);
        adios_write (fh, "offs2", &offs2);

        for (i=0; i<NVARS; i++) {
            adios_write (fh, varnames[i], a2);
        }
    }

    adios_close (fh);
    te = MPI_Wtime();
    if (rank==0) {
        log ("  Write time for step %d was %6.3lf seconds\n", step, te-tb);
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

#define CHECK_ARRAY(VARNAME,A,N,VALUE,STEP,BLOCK,i) \
    for (i=0;i<N;i++) \
        if (A[i] != VALUE) { \
            printE ("%s[%d] step %d block %d: wrote %d but read %d\n",VARNAME,i,STEP,BLOCK,VALUE,A[i]);\
            err = 104; \
            /*goto endread;*/\
            break; \
        }

void reset_readvars()
{
    int n;
    
    n = ldim1 * ldim2;
    memset (r2,  -1, n*sizeof(int));
}

int read_file ()
{
    ADIOS_SELECTION *sel2;
    ADIOS_FILE * f;
    ADIOS_VARINFO * vi;
    int err=0,v,n;
    int block,step,var; // loop variables
    int iMacro; // loop variable in macros
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
    for (var=0; var<NVARS; var++) {
        CHECK_VARINFO(varnames[var], 2, NSTEPS)
    }
    MPI_Barrier (comm);
    te = MPI_Wtime();
    if (rank==0) {
        log ("  Time to check all vars' info: %6.3lf seconds\n", te-tb);
    }

    log ("  Check variable content...\n");
    for (step=0; step<NSTEPS; step++) {
        tb = MPI_Wtime();
        ts = 0;
        for (block=0; block<NBLOCKS; block++) {
            v = VALUE(rank,step,block);
            set_offsets(block);
            start[0] = offs1;
            start[1] = offs2;
            sel2 = adios_selection_boundingbox (2, start, count); 
            log ("    Step %d block %d: value=%d\n", step, block, v);

            tsb = MPI_Wtime();
            for (var=0; var<NVARS; var++) {
                adios_schedule_read (f, sel2, varnames[var], step, 1, r2);
                //adios_perform_reads (f, 1);
                //CHECK_ARRAY (varnames[var],  r2,  ldim1*ldim2, v, step, block, iMacro)
            }
            ts += MPI_Wtime() - tsb;
            adios_perform_reads (f, 1);
            adios_selection_delete (sel2);
        }
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
