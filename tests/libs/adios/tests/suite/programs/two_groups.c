/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C test: 
 *  Write two groups at alternative steps, different variables (a vs b) 
 *  Then read them all and check if they are correct. 
 *
 * How to run: mpirun -np <N> write_alternate
 * Output: write_alternate.bp
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "adios.h"
#include "adios_read.h"

#ifdef DMALLOC
#include "dmalloc.h"
#endif

#define log(...) fprintf (stderr, "[rank=%3.3d, line %d]: ", rank, __LINE__); fprintf (stderr, __VA_ARGS__); fflush(stderr);
#define printE(...) fprintf (stderr, "[rank=%3.3d, line %d]: ERROR: ", rank, __LINE__); fprintf (stderr, __VA_ARGS__); fflush(stderr);

static const char FILENAME[] = "two_groups.bp";
#define VALUE(rank, step) (rank * 100 + step)
#define VALUE0(step) (step)

/* Variables to write */
int a0;

/* Variables to read */
int ra, rb, rc;

MPI_Comm    comm = MPI_COMM_WORLD;
int rank;
int size;

int write_group (int i);
int read_file ();
int read_by_group ();

int main (int argc, char ** argv) 
{
    int err,i ; 

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &size);

    adios_init ("two_groups.xml", comm);
    err = adios_read_init_method(ADIOS_READ_METHOD_BP, comm, "verbose=2");
    if (err) {
        printE ("%s\n", adios_errmsg());
    }

    for (i=0; i<2; i++) { 
        if (!err) {
            a0 = i;
            err = write_group (i); 
        }
    }

    if (!err)
        err = read_file (); 

    if (!err)
        err = read_by_group ();

    adios_finalize (rank);
    MPI_Finalize ();
    return err;
}


int write_group (int step)
{
    int64_t       fh;
    uint64_t       groupsize=0, totalsize;

    log ("Write group %d to %s\n", step, FILENAME);
    adios_open (&fh, (step==1 ? "g2" : "g1"), FILENAME, (step==0 ? "w" : "a"), comm);
    
    groupsize  = sizeof(int);  // a scalar only

    adios_group_size (fh, groupsize, &totalsize);
    if (step == 0) {
        adios_write (fh, "a0", &a0);
    } else if (step == 1) {
        adios_write (fh, "b0", &a0);
    } else {
        adios_write (fh, "c0", &a0);
    }

    adios_close (fh);
    MPI_Barrier (comm);
    return 0;
}


#define CHECK_VARINFO(VARNAME, NDIM, NSTEPS) \
    vi = adios_inq_var (f, VARNAME); \
    if (vi == NULL) { \
        printE ("No such variable " VARNAME "\n"); \
        err = 101; \
        goto endread; \
    } \
    if (vi->ndim != NDIM) { \
        printE ("Variable " VARNAME " has %d dimensions, but expected %d\n", vi->ndim, NDIM); \
        err = 102; \
        goto endread; \
    } \
    if (vi->nsteps != NSTEPS) { \
        printE ("Variable " VARNAME " has %d steps, but expected %d\n", vi->nsteps, NSTEPS); \
        err = 103; \
        /*goto endread; */\
    } \
    adios_free_varinfo (vi);

#define CHECK_ATTR(ATTRNAME, VAL) \
    vi = adios_get_attr (f, ATTRNAME, atype, asize); \
    if (vi == NULL) { \
        printE ("No such variable " VARNAME "\n"); \
        err = 101; \
        goto endread; \
    } \
    if (vi->ndim != NDIM) { \
        printE ("Variable " VARNAME " has %d dimensions, but expected %d\n", vi->ndim, NDIM); \
        err = 102; \
        goto endread; \
    } \
    if (vi->nsteps != NSTEPS) { \
        printE ("Variable " VARNAME " has %d steps, but expected %d\n", vi->nsteps, NSTEPS); \
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
            printE (#VARNAME "[%d] step %d: wrote %d but read %d\n",i,STEP,VALUE,A[i]);\
            err = 104; \
            /*goto endread;*/\
            break; \
        }

void reset_readvars()
{
    ra  = -1;
    rb  = -1;
    rc  = -1;
}

int read_file ()
{
    ADIOS_SELECTION *sel0;
    ADIOS_FILE * f;
    ADIOS_VARINFO * vi;
    int err=0,i;

    uint64_t start[3] = {0,0,0};
    uint64_t count[3] = {1,1,1};
    uint64_t ndim;
    
    reset_readvars();

    log ("Read and check data in %s\n", FILENAME);
    f = adios_read_open_file (FILENAME, ADIOS_READ_METHOD_BP, comm);
    if (f == NULL) {
        printE ("Error at opening file: %s\n", rank, adios_errmsg());
        return 1;
    }

    sel0 = adios_selection_boundingbox (0, start, count); 

    log ("  Check variable definitions... %s\n", FILENAME);

    CHECK_VARINFO("a0", 0, 1)
    CHECK_VARINFO("b0", 0, 1)
    //CHECK_VARINFO("c0", 0, 1)

    log ("  Check variables a0,b0...\n");
    adios_schedule_read (f, sel0, "a0",  0, 1, &ra);
    adios_schedule_read (f, sel0, "b0",  0, 1, &rb);
    //adios_schedule_read (f, sel0, "c0",  0, 1, &rc);
    adios_perform_reads (f, 1);

    CHECK_SCALAR (a0,  ra,  0, i)
    CHECK_SCALAR (b0,  rb,  1, i)
    //CHECK_SCALAR (c0,  rc,  2, i)


endread:

    adios_selection_delete (sel0);

    adios_read_close(f);
    MPI_Barrier (comm);
    return err;
}


int read_by_group ()
{
    ADIOS_SELECTION *sel0;
    ADIOS_FILE * f;
    ADIOS_VARINFO * vi;
    int err=0,i;

    uint64_t start[3] = {0,0,0};
    uint64_t count[3] = {1,1,1};
    uint64_t ndim;
    
    reset_readvars();

    log ("Read and check data in %s\n", FILENAME);
    f = adios_read_open_file (FILENAME, ADIOS_READ_METHOD_BP, comm);
    if (f == NULL) {
        printE ("Error at opening file: %s\n", rank, adios_errmsg());
        return 1;
    }

    sel0 = adios_selection_boundingbox (0, start, count); 

    log ("  Limit view to first group only\n");
    err = adios_group_view (f, 0);
    if (err) {
        printE ("Error in adios_group_view: %s\n", rank, adios_errmsg());
        goto endread;
    }

    log ("  Check variable definitions... %s\n", FILENAME);

    CHECK_VARINFO("a0", 0, 1)
    //CHECK_VARINFO("c0", 0, 1)

    log ("  Check variables a0...\n");
    adios_schedule_read (f, sel0, "a0",  0, 1, &ra);
    //adios_schedule_read (f, sel0, "c0",  0, 1, &rc);
    adios_perform_reads (f, 1);

    CHECK_SCALAR (a0,  ra,  0, i)
    //CHECK_SCALAR (c0,  rc,  2, i)


    log ("  Limit view to second group only\n");
    err = adios_group_view (f, 1);
    if (err) {
        printE ("Error in adios_group_view: %s\n", rank, adios_errmsg());
        goto endread;
    }

    log ("  Check variable definitions... %s\n", FILENAME);

    CHECK_VARINFO("b0", 0, 1)

    log ("  Check variables b0...\n");
    adios_schedule_read (f, sel0, "b0",  0, 1, &rb);
    adios_perform_reads (f, 1);

    CHECK_SCALAR (b0,  rb,  1, i)

endread:

    adios_selection_delete (sel0);

    adios_read_close(f);
    MPI_Barrier (comm);
    return err;
}



