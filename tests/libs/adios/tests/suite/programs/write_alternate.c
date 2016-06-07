/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C test: 
 *  Write a* and b* variables at alternative steps, while c* at every step.
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

static const int NSTEPS = 5;
static const char FILENAME[] = "write_alternate.bp";
#define VALUE(rank, step) (rank * 100 + step)
#define VALUE0(step) (step)

/* Variables to write */
int a0;
int  *a1;
int  *a2;
int  *a3;

/* Variables to read */
int r0, rt0;
int  *r1, *rt1, *r2, *r3;

static const int ldim1 = 7;
static const int ldim2 = 5;
static const int ldim3 = 3;
int gdim1, gdim2, gdim3;
int offs1, offs2, offs3;

MPI_Comm    comm = MPI_COMM_WORLD;
int rank;
int size;

void alloc_vars()
{
    int n;

    gdim1 = size*ldim1;
    gdim2 = ldim2;
    gdim3 = ldim3;

    offs1 = rank*ldim1;
    offs2 = 0;
    offs3 = 0;

    n = ldim1;
    a1  = (int*) malloc (n * sizeof(int));
    r1  = (int*) malloc (n * sizeof(int));
    rt1 = (int*) malloc (n * sizeof(int));

    n = ldim1 * ldim2;
    a2  = (int*) malloc (n * sizeof(int));
    r2  = (int*) malloc (n * sizeof(int));

    n = ldim1 * ldim2 * ldim3;
    a3  = (int*) malloc (n * sizeof(int));
    r3  = (int*) malloc (n * sizeof(int));
}

void set_vars(int step)
{
    int n, i;
    int v = VALUE(rank, step);

    a0 = v;

    n = ldim1;
    for (i=0; i<n; i++) a1[i] = v;

    n = ldim1 * ldim2;
    for (i=0; i<n; i++) a2[i] = v;

    n = ldim1 * ldim2 * ldim3;
    for (i=0; i<n; i++) a3[i] = v;
}

void fini_vars()
{
    free (a1);
    free (r1);
    free (rt1);
    free (a2);
    free (r2);
    free (a3);
    free (r3);
}


int write_file (int step);
int read_file ();

int main (int argc, char ** argv) 
{
    int err,i ; 

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &size);

    alloc_vars();
    adios_init ("write_alternate.xml", comm);
    err = adios_read_init_method(ADIOS_READ_METHOD_BP, comm, "verbose=2");
    if (err) {
        printE ("%s\n", adios_errmsg());
    }

    
    for (i=0; i<NSTEPS; i++) {
        if (!err) {
            set_vars (i);
            err = write_file (i); 
        }
    }

    if (!err)
        err = read_file (); 

    if (!err)
        err = read_stream (); 

    adios_finalize (rank);
    fini_vars();
    MPI_Finalize ();
    return err;
}


int write_file (int step) 
{
    int64_t       fh;
    uint64_t       groupsize=0, totalsize;

    log ("Write step %d to %s\n", step, FILENAME);
    adios_open (&fh, "abc", FILENAME, (step ? "a" : "w"), comm);
    
    groupsize  = 9 * sizeof(int);                           // dimensions 
    groupsize += 3 * sizeof(int);                           // scalars 
    groupsize += 3 * ldim1 * sizeof(int);                   // 1D 
    groupsize += 3 * ldim1 * ldim2 * sizeof(int);           // 2D 
    groupsize += 3 * ldim1 * ldim2 * ldim3 * sizeof(int);   // 3D

    adios_group_size (fh, groupsize, &totalsize);

    adios_write (fh, "gdim1", &gdim1);
    adios_write (fh, "gdim2", &gdim2);
    adios_write (fh, "gdim3", &gdim3);
    adios_write (fh, "ldim1", &ldim1);
    adios_write (fh, "ldim2", &ldim2);
    adios_write (fh, "ldim3", &ldim3);
    adios_write (fh, "offs1", &offs1);
    adios_write (fh, "offs2", &offs2);
    adios_write (fh, "offs3", &offs3);

    if (step % 2 == 0) {
        adios_write (fh, "a0", &a0);
        adios_write (fh, "at0", &a0);
        adios_write (fh, "a1", a1);
        adios_write (fh, "at1", a1);
        adios_write (fh, "a2", a2);
        adios_write (fh, "a3", a3);
    } else {
        adios_write (fh, "b0", &a0);
        adios_write (fh, "bt0", &a0);
        adios_write (fh, "b1", a1);
        adios_write (fh, "bt1", a1);
        adios_write (fh, "b2", a2);
        adios_write (fh, "b3", a3);
    }
    adios_write (fh, "c0", &a0);
    adios_write (fh, "ct0", &a0);
    adios_write (fh, "c1", a1);
    adios_write (fh, "ct1", a1);
    adios_write (fh, "c2", a2);
    adios_write (fh, "c3", a3);

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
    int n;
    
    r0  = -1;
    rt0 = -1;

    n = ldim1;
    memset (r1,  -1, n*sizeof(int));
    memset (rt1, -1, n*sizeof(int));

    n = ldim1 * ldim2;
    memset (r2,  -1, n*sizeof(int));

    n = ldim1 * ldim2 * ldim3;
    memset (r3,  -1, n*sizeof(int));
}

int read_file ()
{
    ADIOS_SELECTION *sel0,*sel1,*sel2,*sel3;
    ADIOS_FILE * f;
    ADIOS_VARINFO * vi;
    int err=0,v,v0,i,n;
    int nsteps_a, nsteps_b, nsteps_c;
    int iMacro; // loop variable in macros

    uint64_t start[3] = {offs1,offs2,offs3};
    uint64_t count[3] = {ldim1,ldim2,ldim3};
    uint64_t ndim;
    
    reset_readvars();

    log ("Read and check data in %s\n", FILENAME);
    f = adios_read_open_file (FILENAME, ADIOS_READ_METHOD_BP, comm);
    if (f == NULL) {
        printE ("Error at opening file: %s\n", rank, adios_errmsg());
        return 1;
    }

    sel0 = adios_selection_boundingbox (0, start, count); 
    sel1 = adios_selection_boundingbox (1, start, count); 
    sel2 = adios_selection_boundingbox (2, start, count); 
    sel3 = adios_selection_boundingbox (3, start, count); 

    log ("  Check variable definitions... %s\n", FILENAME);
    nsteps_a = NSTEPS / 2 + NSTEPS % 2;
    nsteps_b = NSTEPS / 2;
    nsteps_c = NSTEPS;

    CHECK_VARINFO("a0", 0, nsteps_a)
    CHECK_VARINFO("b0", 0, nsteps_b)
    CHECK_VARINFO("c0", 0, nsteps_c)
    CHECK_VARINFO("at0", 0, nsteps_a)
    CHECK_VARINFO("bt0", 0, nsteps_b)
    CHECK_VARINFO("ct0", 0, nsteps_c)
    CHECK_VARINFO("a1", 1, nsteps_a)
    CHECK_VARINFO("b1", 1, nsteps_b)
    CHECK_VARINFO("c1", 1, nsteps_c)
    CHECK_VARINFO("at1", 1, nsteps_a)
    CHECK_VARINFO("bt1", 1, nsteps_b)
    CHECK_VARINFO("ct1", 1, nsteps_c)
    CHECK_VARINFO("a2", 2, nsteps_a)
    CHECK_VARINFO("b2", 2, nsteps_b)
    CHECK_VARINFO("c2", 2, nsteps_c)
    CHECK_VARINFO("a3", 3, nsteps_a)
    CHECK_VARINFO("b3", 3, nsteps_b)
    CHECK_VARINFO("c3", 3, nsteps_c)

    log ("  Check variables c0,ct0,c1,ct1,c2,c3, steps=%d...\n",nsteps_c);
    for (i=0; i<nsteps_c; i++) {
        v = VALUE(rank,i);
        v0 = VALUE0(i);
        log ("    Step %d value %d\n", i, v);

        adios_schedule_read (f, sel0, "c0",  i, 1, &r0);
        adios_schedule_read (f, sel0, "ct0", i, 1, &rt0);
        adios_schedule_read (f, sel1, "c1",  i, 1, r1);
        adios_schedule_read (f, sel1, "ct1", i, 1, rt1);
        adios_schedule_read (f, sel2, "c2",  i, 1, r2);
        adios_schedule_read (f, sel3, "c3",  i, 1, r3);
        adios_perform_reads (f, 1);

        CHECK_SCALAR (c0,  r0,  v0, i) // scalar is from writer rank 0, not this rank!
        CHECK_SCALAR (ct0, rt0, v0, i) // so value is v0 at step 'i', not v
        CHECK_ARRAY (c1,  r1,  ldim1, v, i, iMacro)
        CHECK_ARRAY (ct1, rt1, ldim1, v, i, iMacro)
        CHECK_ARRAY (c2,  r2,  ldim1*ldim2, v, i, iMacro)
        CHECK_ARRAY (c3,  r3,  ldim1*ldim2*ldim3, v, i, iMacro)
    } 

    log ("  Check variables a0,at0,a1,at1,a2,a3, steps=%d...\n",nsteps_a);
    for (i=0; i<nsteps_a; i++) {
        v = VALUE(rank,i*2);
        v0 = VALUE0(i*2);
        log ("    Step %d value %d\n", i, v);

        adios_schedule_read (f, sel0, "a0",  i, 1, &r0);
        adios_schedule_read (f, sel0, "at0", i, 1, &rt0);
        adios_schedule_read (f, sel1, "a1",  i, 1, r1);
        adios_schedule_read (f, sel1, "at1", i, 1, rt1);
        adios_schedule_read (f, sel2, "a2",  i, 1, r2);
        adios_schedule_read (f, sel3, "a3",  i, 1, r3);
        adios_perform_reads (f, 1);

        CHECK_SCALAR (a0,  r0,  v0, i)
        CHECK_SCALAR (at0, rt0, v0, i)
        CHECK_ARRAY (a1,  r1,  ldim1, v, i, iMacro)
        CHECK_ARRAY (at1, rt1, ldim1, v, i, iMacro)
        CHECK_ARRAY (a2,  r2,  ldim1*ldim2, v, i, iMacro)
        CHECK_ARRAY (a3,  r3,  ldim1*ldim2*ldim3, v, i, iMacro)
    } 

    log ("  Check variables b0,bt0,b1,bt1,b2,b3, steps=%d...\n",nsteps_b);
    for (i=0; i<nsteps_b; i++) {
        v = VALUE(rank,i*2)+1;
        v0 = VALUE0(i*2)+1;
        log ("    Step %d value %d\n", i, v);

        adios_schedule_read (f, sel0, "b0",  i, 1, &r0);
        adios_schedule_read (f, sel0, "bt0", i, 1, &rt0);
        adios_schedule_read (f, sel1, "b1",  i, 1, r1);
        adios_schedule_read (f, sel1, "bt1", i, 1, rt1);
        adios_schedule_read (f, sel2, "b2",  i, 1, r2);
        adios_schedule_read (f, sel3, "b3",  i, 1, r3);
        adios_perform_reads (f, 1);

        CHECK_SCALAR (b0,  r0,  v0, i)
        CHECK_SCALAR (bt0, rt0, v0, i)
        CHECK_ARRAY (b1,  r1,  ldim1, v, i, iMacro)
        CHECK_ARRAY (bt1, rt1, ldim1, v, i, iMacro)
        CHECK_ARRAY (b2,  r2,  ldim1*ldim2, v, i, iMacro)
        CHECK_ARRAY (b3,  r3,  ldim1*ldim2*ldim3, v, i, iMacro)
    } 

endread:

    adios_selection_delete (sel0);
    adios_selection_delete (sel1);
    adios_selection_delete (sel2);
    adios_selection_delete (sel3);

    adios_read_close(f);
    MPI_Barrier (comm);
    return err;
}





int read_stream ()
{
    ADIOS_SELECTION *sel0,*sel1,*sel2,*sel3;
    ADIOS_FILE * f;
    ADIOS_VARINFO * vi;
    int err=0,v,v0,i,n;
    int nsteps_a, nsteps_b, nsteps_c;
    int iMacro; // loop variable in macros

    uint64_t start[3] = {offs1,offs2,offs3};
    uint64_t count[3] = {ldim1,ldim2,ldim3};
    uint64_t ndim;
    
    reset_readvars();

    log ("Read as stream and check data in %s\n", FILENAME);
    f = adios_read_open (FILENAME, ADIOS_READ_METHOD_BP, comm,
                         ADIOS_LOCKMODE_NONE, 0.0);
    if (f == NULL) {
        printE ("Error at opening file as stream: %s\n", rank, adios_errmsg());
        return 1;
    }

    sel0 = adios_selection_boundingbox (0, start, count); 
    sel1 = adios_selection_boundingbox (1, start, count); 
    sel2 = adios_selection_boundingbox (2, start, count); 
    sel3 = adios_selection_boundingbox (3, start, count); 

    n = 0;
    while (n < NSTEPS && adios_errno != err_end_of_stream) {
        log ("  Step %d\n", n);

        log ("    Check variable definitions... %s\n", FILENAME);
        if (n%2 == 0) {
            CHECK_VARINFO("a0", 0, 1)
            CHECK_VARINFO("at0", 0, 1)
            CHECK_VARINFO("a1", 1, 1)
            CHECK_VARINFO("at1", 1, 1)
            CHECK_VARINFO("a2", 2, 1)
            CHECK_VARINFO("a3", 3, 1)
        } else {
            CHECK_VARINFO("b0", 0, 1)
            CHECK_VARINFO("bt0", 0, 1)
            CHECK_VARINFO("b1", 1, 1)
            CHECK_VARINFO("bt1", 1, 1)
            CHECK_VARINFO("b2", 2, 1)
            CHECK_VARINFO("b3", 3, 1)
        }
        CHECK_VARINFO("c0", 0, 1)
        CHECK_VARINFO("ct0", 0, 1)
        CHECK_VARINFO("c1", 1, 1)
        CHECK_VARINFO("ct1", 1, 1)
        CHECK_VARINFO("c2", 2, 1)
        CHECK_VARINFO("c3", 3, 1)


        v = VALUE(rank,n);
        v0 = VALUE0(n);
        log ("    Check variables c0,ct0,c1,ct1,c2,c3... Step %d value %d\n", n, v);

        adios_schedule_read (f, sel0, "c0",  0, 1, &r0);
        adios_schedule_read (f, sel0, "ct0", 0, 1, &rt0);
        adios_schedule_read (f, sel1, "c1",  0, 1, r1);
        adios_schedule_read (f, sel1, "ct1", 0, 1, rt1);
        adios_schedule_read (f, sel2, "c2",  0, 1, r2);
        adios_schedule_read (f, sel3, "c3",  0, 1, r3);
        adios_perform_reads (f, 1);

        CHECK_SCALAR (c0,  r0,  v0, n) // scalar is from writer rank 0, not this rank!
        CHECK_SCALAR (ct0, rt0, v0, n) // so value is v0 at 'n', not v
        CHECK_ARRAY (c1,  r1,  ldim1, v, n, iMacro)
        CHECK_ARRAY (ct1, rt1, ldim1, v, n, iMacro)
        CHECK_ARRAY (c2,  r2,  ldim1*ldim2, v, n, iMacro)
        CHECK_ARRAY (c3,  r3,  ldim1*ldim2*ldim3, v, n, iMacro)


        if (n%2 == 0) {
            v = VALUE(rank,n);
            v0 = VALUE0(n);
            log ("    Check variables a0,at0,a1,at1,a2,a3... Step %d value %d\n", n, v);

            adios_schedule_read (f, sel0, "a0",  0, 1, &r0);
            adios_schedule_read (f, sel0, "at0", 0, 1, &rt0);
            adios_schedule_read (f, sel1, "a1",  0, 1, r1);
            adios_schedule_read (f, sel1, "at1", 0, 1, rt1);
            adios_schedule_read (f, sel2, "a2",  0, 1, r2);
            adios_schedule_read (f, sel3, "a3",  0, 1, r3);
            adios_perform_reads (f, 1);

            CHECK_SCALAR (a0,  r0,  v0, n)
            CHECK_SCALAR (at0, rt0, v0, n)
            CHECK_ARRAY (a1,  r1,  ldim1, v, n, iMacro)
            CHECK_ARRAY (at1, rt1, ldim1, v, n, iMacro)
            CHECK_ARRAY (a2,  r2,  ldim1*ldim2, v, n, iMacro)
            CHECK_ARRAY (a3,  r3,  ldim1*ldim2*ldim3, v, n, iMacro)
        }


        if (n%2 == 1) {
            v = VALUE(rank,n);
            v0 = VALUE0(n);
            log ("    Check variables b0,bt0,b1,bt1,b2,b3... Step %d value %d\n", n, v);

            adios_schedule_read (f, sel0, "b0",  0, 1, &r0);
            adios_schedule_read (f, sel0, "bt0", 0, 1, &rt0);
            adios_schedule_read (f, sel1, "b1",  0, 1, r1);
            adios_schedule_read (f, sel1, "bt1", 0, 1, rt1);
            adios_schedule_read (f, sel2, "b2",  0, 1, r2);
            adios_schedule_read (f, sel3, "b3",  0, 1, r3);
            adios_perform_reads (f, 1);

            CHECK_SCALAR (b0,  r0,  v0, n)
            CHECK_SCALAR (bt0, rt0, v0, n)
            CHECK_ARRAY (b1,  r1,  ldim1, v, n, iMacro)
            CHECK_ARRAY (bt1, rt1, ldim1, v, n, iMacro)
            CHECK_ARRAY (b2,  r2,  ldim1*ldim2, v, n, iMacro)
            CHECK_ARRAY (b3,  r3,  ldim1*ldim2*ldim3, v, n, iMacro)
        }

        if (adios_advance_step (f, 0, 0.0) >= 0)
            n = f->current_step;
        else
            n++; //just to end the loop
    } 

endread:

    adios_selection_delete (sel0);
    adios_selection_delete (sel1);
    adios_selection_delete (sel2);
    adios_selection_delete (sel3);

    adios_read_close(f);
    MPI_Barrier (comm);
    return err;
}
