/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C test: 
 *  Write variables with different paths
 *  Then read them all and check if they are correct. 
 *
 * How to run: mpirun -np <N> set_path
 * Output: set_path.bp
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

static const char FILENAME[] = "set_path.bp";
#define VALUE(rank,varid) (100*varid+rank)
#define VALUE0(varid) VALUE(0,varid)

/* Variables to write */
int s0, s1, s2;
int  *a0, *a1, *a2;

/* Variables to read */
int rs0, rs1, rs2;
int  *ra0, *ra1, *ra2;

static const int ldim1 = 7;
int gdim1;
int offs1;

MPI_Comm    comm = MPI_COMM_WORLD;
int rank;
int size;

void alloc_vars()
{
    int n;

    gdim1 = size*ldim1;
    offs1 = rank*ldim1;

    n = ldim1;
    a0  = (int*) malloc (n * sizeof(int));
    a1  = (int*) malloc (n * sizeof(int));
    a2  = (int*) malloc (n * sizeof(int));
    ra0  = (int*) malloc (n * sizeof(int));
    ra1  = (int*) malloc (n * sizeof(int));
    ra2  = (int*) malloc (n * sizeof(int));
}

void set_vars(int rank)
{
    int n, i;
    int v0, v1, v2;
    
    n = ldim1;

    s0 = VALUE(rank,0);
    s1 = VALUE(rank,1);
    s2 = VALUE(rank,2);

    for (i=0; i<n; i++) {
        a0[i] = s0;
        a1[i] = s1;
        a2[i] = s2;
    }
}

void fini_vars()
{
    free (a0);
    free (a1);
    free (a2);
    free (ra0);
    free (ra1);
    free (ra2);
}


int write_file ();
int read_file ();

int main (int argc, char ** argv) 
{
    int err,i ; 

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &size);

    alloc_vars();
    adios_init ("set_path.xml", comm);
    err = adios_read_init_method(ADIOS_READ_METHOD_BP, comm, "verbose=2");
    if (err) {
        printE ("%s\n", adios_errmsg());
    }


    if (!err) {
        set_vars (rank);
        err = write_file (); 
    }

    if (!err)
        err = read_file (); 

    adios_finalize (rank);
    fini_vars();
    MPI_Finalize ();
    return err;
}

char *getpath (int varid) 
{
    static char newpath[128];
    sprintf (newpath, "/data/vars_%d", varid);
    return newpath;
}

int write_file () 
{
    int64_t       fh;
    uint64_t      groupsize=0, totalsize;
    char          *newpath, *oldpath;
    char          varpath[128];

    log ("Write to %s\n", FILENAME);
    adios_open (&fh, "abc", FILENAME, "w", comm);
    
    groupsize  = 3 * sizeof(int);                           // dimensions 
    groupsize += 3 * sizeof(int);                           // scalars 
    groupsize += 3 * ldim1 * sizeof(int);                   // 1D 
    groupsize += 1000; // add for extra metadata of triplicated variables, which is not expected from the xml itself

    adios_group_size (fh, groupsize, &totalsize);
    log ("Group size: data only = %lld, with metadata = %lld\n", groupsize, totalsize);

    adios_write (fh, "gdim1", &gdim1);
    adios_write (fh, "ldim1", &ldim1);
    adios_write (fh, "offs1", &offs1);

    newpath = getpath(0);
    log ("  Change path of all variables to %s\n", newpath);
    adios_set_path (fh, newpath);
    log ("    Write s0\n", varpath);
    adios_write (fh, "s0", &s0);
    log ("    Write a1\n", varpath);
    adios_write (fh, "a1", a0);

    newpath = getpath(1);
    log ("  Change path of all variables to %s\n", newpath);
    adios_set_path (fh, newpath);
    log ("    Write s0\n", varpath);
    adios_write (fh, "s0", &s1);
    log ("    Write a1\n", varpath);
    adios_write (fh, "a1", a1);

    newpath = getpath(2);
    log ("  Change path of all variables to %s\n", newpath);
    adios_set_path (fh, newpath);
    log ("    Write s0\n", varpath);
    adios_write (fh, "s0", &s2);
    log ("    Write a1\n", varpath);
    adios_write (fh, "a1", a2);

    adios_close (fh);
    MPI_Barrier (comm);
    return 0;
}


#define CHECK_VARINFO(VARNAME, NDIM, NSTEPS) \
    log ("    Check variable %s ...\n", VARNAME) \
    vi = adios_inq_var (f, VARNAME); \
    if (vi == NULL) { \
        printE ("No such variable %s\n", VARNAME); \
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
        printE ("%s step %d: wrote %d but read %d\n",VARNAME,STEP,VALUE,VAR);\
        err = 104; \
        /*goto endread;*/\
    }

#define CHECK_ARRAY(VARNAME,A,N,VALUE,STEP,i) \
    for (i=0;i<N;i++) \
        if (A[i] != VALUE) { \
            printE ("%s [%d] step %d: wrote %d but read %d\n",VARNAME,i,STEP,VALUE,A[i]);\
            err = 104; \
            /*goto endread;*/\
            break; \
        }

void reset_readvars()
{
    int n;
    
    rs0  = -1;
    rs1  = -1;
    rs2  = -1;

    n = ldim1;
    memset (ra0,  -1, n*sizeof(int));
    memset (ra1,  -1, n*sizeof(int));
    memset (ra2,  -1, n*sizeof(int));
}

int read_file ()
{
    ADIOS_SELECTION *sel0,*sel1;
    ADIOS_FILE * f;
    ADIOS_VARINFO * vi;
    int err=0,v,v0,i,n;
    int iMacro; // loop variable in macros
    char *newpath;
    char varpath[128];

    uint64_t start[1] = {offs1};
    uint64_t count[1] = {ldim1};
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


    log ("  Check variable definitions... %s\n", FILENAME);
    sprintf (varpath, "%s/s0", getpath(0));
    CHECK_VARINFO(varpath, 0, 1)
    sprintf (varpath, "%s/s0", getpath(1));
    CHECK_VARINFO(varpath, 0, 1)
    sprintf (varpath, "%s/s0", getpath(2));
    CHECK_VARINFO(varpath, 0, 1)

    sprintf (varpath, "%s/a1", getpath(0));
    CHECK_VARINFO(varpath, 1, 1)
    sprintf (varpath, "%s/a1", getpath(1));
    CHECK_VARINFO(varpath, 1, 1)
    sprintf (varpath, "%s/a1", getpath(2));
    CHECK_VARINFO(varpath, 1, 1)

    log ("  Read variables ...\n");

    sprintf (varpath, "%s/s0", getpath(0));
    adios_schedule_read (f, sel0, varpath,  0, 1, &rs0);
    sprintf (varpath, "%s/s0", getpath(1));
    adios_schedule_read (f, sel0, varpath,  0, 1, &rs1);
    sprintf (varpath, "%s/s0", getpath(2));
    adios_schedule_read (f, sel0, varpath,  0, 1, &rs2);

    sprintf (varpath, "%s/a1", getpath(0));
    adios_schedule_read (f, sel1, varpath,  0, 1, ra0);
    sprintf (varpath, "%s/a1", getpath(1));
    adios_schedule_read (f, sel1, varpath,  0, 1, ra1);
    sprintf (varpath, "%s/a1", getpath(2));
    adios_schedule_read (f, sel1, varpath,  0, 1, ra2);

    adios_perform_reads (f, 1);

    log ("  Check variable content ...\n");

    v0 = VALUE0(0); // scalar is from writer rank 0, not this rank!
    sprintf (varpath, "%s/s0", getpath(0));
    CHECK_SCALAR (varpath,  rs0,  v0, i) 
    v0 = VALUE0(1); // scalar is from writer rank 0, not this rank!
    sprintf (varpath, "%s/s0", getpath(1));
    CHECK_SCALAR (varpath,  rs1,  v0, i) 
    v0 = VALUE0(2); // scalar is from writer rank 0, not this rank!
    sprintf (varpath, "%s/s0", getpath(2));
    CHECK_SCALAR (varpath,  rs2,  v0, i) 

    v = VALUE(rank,0);
    sprintf (varpath, "%s/a1", getpath(0));
    CHECK_ARRAY (varpath,  ra0, ldim1, v, 0, iMacro)
    v = VALUE(rank,1);
    sprintf (varpath, "%s/a1", getpath(1));
    CHECK_ARRAY (varpath,  ra1, ldim1, v, 0, iMacro)
    v = VALUE(rank,2);
    sprintf (varpath, "%s/a1", getpath(2));
    CHECK_ARRAY (varpath,  ra2, ldim1, v, 0, iMacro)

endread:

    adios_selection_delete (sel0);
    adios_selection_delete (sel1);

    adios_read_close(f);
    MPI_Barrier (comm);
    return err;
}





