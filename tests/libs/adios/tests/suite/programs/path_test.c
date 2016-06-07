/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C test: write then read variables with different paths possible
 *
 * How to run: mpirun -np <N> path_test
 * Output: path_test.bp
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

/* Variables to write */
int   *a1, *a2, *a3, *a4, *a5, *a6, *a7;

/* Variables to read */
int   *r1, *r2, *r3, *r4, *r5, *r6, *r7;

int ldim1 = 1;
int ldim2 = 2;
int ldim3 = 3;
int ldim4 = 4;
int ldim5 = 5;
int ldim6 = 6;
int ldim7 = 7;
int gdim1, gdim2, gdim3, gdim4, gdim5, gdim6, gdim7;
int offs1, offs2, offs3, offs4, offs5, offs6, offs7;

MPI_Comm    comm = MPI_COMM_WORLD;
int rank;
int size;

void init_vars()
{
    int i;
    int v = rank%10;

    gdim1 = size*ldim1;
    gdim2 = size*ldim2;
    gdim3 = size*ldim3;
    gdim4 = size*ldim4;
    gdim5 = size*ldim5;
    gdim6 = size*ldim6;
    gdim7 = size*ldim7;

    offs1 = rank*ldim1;
    offs2 = rank*ldim2;
    offs3 = rank*ldim3;
    offs4 = rank*ldim4;
    offs5 = rank*ldim5;
    offs6 = rank*ldim6;
    offs7 = rank*ldim7;

    a1 = (int*)   malloc (ldim1 * sizeof(int));
    a2 = (int*)   malloc (ldim2 * sizeof(int));
    a3 = (int*)   malloc (ldim3 * sizeof(int));
    a4 = (int*)   malloc (ldim4 * sizeof(int));
    a5 = (int*)   malloc (ldim5 * sizeof(int));
    a6 = (int*)   malloc (ldim6 * sizeof(int));
    a7 = (int*)   malloc (ldim7 * sizeof(int));
    r1 = (int*)   malloc (ldim1 * sizeof(int));
    r2 = (int*)   malloc (ldim2 * sizeof(int));
    r3 = (int*)   malloc (ldim3 * sizeof(int));
    r4 = (int*)   malloc (ldim4 * sizeof(int));
    r5 = (int*)   malloc (ldim5 * sizeof(int));
    r6 = (int*)   malloc (ldim6 * sizeof(int));
    r7 = (int*)   malloc (ldim7 * sizeof(int));

    for (i=0; i<ldim1; i++)  a1[i] = 10+v;
    for (i=0; i<ldim2; i++)  a2[i] = 20+v;
    for (i=0; i<ldim3; i++)  a3[i] = 30+v;
    for (i=0; i<ldim4; i++)  a4[i] = 40+v;
    for (i=0; i<ldim5; i++)  a5[i] = 50+v;
    for (i=0; i<ldim6; i++)  a6[i] = 60+v;
    for (i=0; i<ldim7; i++)  a7[i] = 70+v;
}

void fini_vars()
{
    free (a1);
    free (a2);
    free (a3);
    free (a4);
    free (a5);
    free (a6);
    free (a7);
    free (r1);
    free (r2);
    free (r3);
    free (r4);
    free (r5);
    free (r6);
    free (r7);
}


int write_file (char *fname);
int read_file (char *fname);

int main (int argc, char ** argv) 
{
    int err; 

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &size);

    init_vars();
    adios_init ("path_test.xml", comm);
    err = adios_read_init_method(ADIOS_READ_METHOD_BP, comm, "verbose=2");
    if (err) {
        printE ("%s\n", adios_errmsg());
    }

    if (!err)
        err = write_file ("path_test_1.bp"); 
    if (!err)
        err = read_file ("path_test_1.bp"); 
    if (!err)
        err = write_file ("path_test_2.bp"); 
    if (!err)
        err = read_file ("path_test_2.bp"); 
    adios_finalize (rank);
    fini_vars();
    MPI_Finalize ();
    return err;
}


int write_file (char *fname) 
{
    int64_t       fh;
    uint64_t       groupsize=0, totalsize;

    log ("Write data to %s\n", fname);
    adios_open (&fh, "pathtest", fname, "w", comm);
    
    groupsize  = 21*4;                       // dimensions 
    groupsize += 4 * ldim1;                  // a1 integer array 
    groupsize += 4 * ldim2;                  // a2 integer array 
    groupsize += 4 * ldim3;                  // a3 integer array 
    groupsize += 4 * ldim4;                  // a4 integer array 
    groupsize += 4 * ldim5;                  // a5 integer array 
    groupsize += 4 * ldim6;                  // a6 integer array 
    groupsize += 4 * ldim7;                  // a7 integer array 

    adios_group_size (fh, groupsize, &totalsize);

    adios_write (fh, "dir1/gdim1", &gdim1);
    adios_write (fh, "/dir1/gdim2", &gdim2);
    adios_write (fh, "dir1/gdim3", &gdim3);
    adios_write (fh, "/dir1/dir2/gdim4", &gdim4);
    adios_write (fh, "/gdim5", &gdim5);
    adios_write (fh, "gdim6", &gdim6);
    adios_write (fh, "gdim7", &gdim7);
    adios_write (fh, "dir1/ldim1", &ldim1);
    adios_write (fh, "/dir1/ldim2", &ldim2);
    adios_write (fh, "dir1/ldim3", &ldim3);
    adios_write (fh, "/dir1/dir2/ldim4", &ldim4);
    adios_write (fh, "/ldim5", &ldim5);
    adios_write (fh, "ldim6", &ldim6);
    adios_write (fh, "ldim7", &ldim7);
    adios_write (fh, "dir1/offs1", &offs1);
    adios_write (fh, "/dir1/offs2", &offs2);
    adios_write (fh, "dir1/offs3", &offs3);
    adios_write (fh, "/dir1/dir2/offs4", &offs4);
    adios_write (fh, "/offs5", &offs5);
    adios_write (fh, "offs6", &offs6);
    adios_write (fh, "offs7", &offs7);
    adios_write (fh, "dir1/a1", a1);
    adios_write (fh, "/dir1/a2", a2);
    adios_write (fh, "dir1/a3", a3);
    adios_write (fh, "/dir1/dir2/a4", a4);
    adios_write (fh, "/a5", a5);
    adios_write (fh, "a6", a6);
    adios_write (fh, "a7", a7);

    adios_close (fh);
    MPI_Barrier (comm);
    return 0;
}


#define GET_SCALAR(VARNAME) \
    vi = adios_inq_var (f, VARNAME); \
    if (vi == NULL) { \
        printE ("No such variable %s\n", VARNAME); \
        err = 101; \
        goto endread; \
    } \

#define CHECK_SCALAR(VARNAME,VAR,TYPE) \
    GET_SCALAR(VARNAME) \
    if (*(TYPE*)vi->value != VAR) { \
        printE ("Scalar %s as read from file != as written\n", VARNAME); \
        err = 102; \
        goto endread; \
    } \
    adios_free_varinfo (vi);

#define CHECK_ARRAY(A,R,LEN) \
    for (i=0;i<LEN;i++) \
        if (A[i] != R[i]) { \
            printE ("Variable " #A " does not equal in written and read values at position %d\n", i);\
            err = 103; \
            goto endread;\
        }

void reset_rarrays()
{
    memset (r1, -1, ldim1*sizeof(int));
    memset (r2, -1, ldim2*sizeof(int));
    memset (r3, -1, ldim3*sizeof(int));
    memset (r4, -1, ldim4*sizeof(int));
    memset (r5, -1, ldim5*sizeof(int));
    memset (r6, -1, ldim6*sizeof(int));
    memset (r7, -1, ldim7*sizeof(int));
}

int read_file (char *fname)
{
    ADIOS_SELECTION *sel;
    ADIOS_FILE * f;
    ADIOS_VARINFO * vi;
    int err=0,i;

    uint64_t start[1];
    uint64_t count[1];
    
    reset_rarrays();

    log ("Read and check data in %s\n", fname);
    f = adios_read_open_file (fname, ADIOS_READ_METHOD_BP, comm);
    if (f == NULL) {
        printE ("Error at opening file: %s\n", rank, adios_errmsg());
        return 1;
    }

    log ("  Global dimensions... %s\n", fname);
    CHECK_SCALAR ("dir1/gdim1",  gdim1,  int)
    CHECK_SCALAR ("/dir1/gdim2",  gdim2,  int)
    CHECK_SCALAR ("dir1/gdim3",  gdim3,  int)
    CHECK_SCALAR ("/dir1/dir2/gdim4",  gdim4,  int)
    CHECK_SCALAR ("/gdim5",  gdim5,  int)
    CHECK_SCALAR ("gdim6",  gdim6,  int)
    CHECK_SCALAR ("gdim7",  gdim7,  int)

    log ("  Arrays... %s\n", fname);
    log ("    a1...\n");
    start[0] = offs1;
    count[0] = ldim1;
    sel = adios_selection_boundingbox (1, start, count); 
    adios_schedule_read (f, sel, "dir1/a1", 0, 1, r1);
    adios_perform_reads (f, 1);
    adios_selection_delete (sel);
    CHECK_ARRAY (a1, r1, ldim1)

    log ("    a2...\n");
    start[0] = offs2;
    count[0] = ldim2;
    sel = adios_selection_boundingbox (1, start, count); 
    adios_schedule_read (f, sel, "/dir1/a2", 0, 1, r2);
    adios_perform_reads (f, 1);
    adios_selection_delete (sel);
    CHECK_ARRAY (a2, r2, ldim2)

    log ("    a3...\n");
    start[0] = offs3;
    count[0] = ldim3;
    sel = adios_selection_boundingbox (1, start, count); 
    adios_schedule_read (f, sel, "dir1/a3", 0, 1, r3);
    adios_perform_reads (f, 1);
    adios_selection_delete (sel);
    CHECK_ARRAY (a3, r3, ldim3)

    log ("    a4...\n");
    start[0] = offs4;
    count[0] = ldim4;
    sel = adios_selection_boundingbox (1, start, count); 
    adios_schedule_read (f, sel, "/dir1/dir2/a4", 0, 1, r4);
    adios_perform_reads (f, 1);
    adios_selection_delete (sel);
    CHECK_ARRAY (a4, r4, ldim4)

    log ("    a5...\n");
    start[0] = offs5;
    count[0] = ldim5;
    sel = adios_selection_boundingbox (1, start, count); 
    adios_schedule_read (f, sel, "/a5", 0, 1, r5);
    adios_perform_reads (f, 1);
    adios_selection_delete (sel);
    CHECK_ARRAY (a5, r5, ldim5)

    log ("    a6...\n");
    start[0] = offs6;
    count[0] = ldim6;
    sel = adios_selection_boundingbox (1, start, count); 
    adios_schedule_read (f, sel, "a6", 0, 1, r6);
    adios_perform_reads (f, 1);
    adios_selection_delete (sel);
    CHECK_ARRAY (a6, r6, ldim6)

    log ("    a7...\n");
    start[0] = offs7;
    count[0] = ldim7;
    sel = adios_selection_boundingbox (1, start, count); 
    adios_schedule_read (f, sel, "a7", 0, 1, r7);
    adios_perform_reads (f, 1);
    adios_selection_delete (sel);
    CHECK_ARRAY (a7, r7, ldim7)

endread:
    adios_read_close(f);
    MPI_Barrier (comm);
    return err;
}
