/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C Example: write two arrays, using the same dim scalar with different values
 *
 * How to run: ./reuse_dim
 * Output: reuse_dim.bp
 * ADIOS config file: None
 *
 */

/* This example will write out 2 1D arrays, and one scalar NX twice.
 * The dimension of the arrays is NX, but with different values.
 * This test comes from how Maya is using ADIOS to write tens of thousands
 * of arrays (patches), but using the same scalar for dimension 
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


int         NX = 100;
double      *t, *r1, *r2;

/* ADIOS variables declarations for matching gwrite_temperature.ch */
int64_t     m_adios_group;
int64_t     m_adios_file;

MPI_Comm    comm = MPI_COMM_WORLD;
int rank;
int size;


void init_vars()
{
    int i;
    t  = (double *) malloc (NX * sizeof(double));
    r1 = (double *) malloc (NX * sizeof(double));
    r2 = (double *) malloc (NX * sizeof(double));
    for (i = 0; i < NX; i++)
        t[i] = i;
}

void fini_vars()
{
    free (t);
    free (r1);
    free (r2);
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
    adios_init_noxml (comm);
    err = adios_read_init_method(ADIOS_READ_METHOD_BP, comm, "verbose=2");
    if (err) {
        printE ("%s\n", adios_errmsg());
    }

    if (!err)
        err = declare_group ();
    if (!err)
        err = write_file ("reuse_dim.bp");
    if (!err)
        err = read_file ("reuse_dim.bp");

    adios_finalize (rank);
    fini_vars();
    MPI_Finalize ();
    return err;
}


int declare_group ()
{

    adios_allocate_buffer (ADIOS_BUFFER_ALLOC_NOW, 10);

    adios_declare_group (&m_adios_group, "restart", "iter", adios_flag_yes);
    adios_select_method (m_adios_group, "MPI", "verbose=2", "");

    adios_define_var (m_adios_group, "NX"
            ,"", adios_integer
            ,0, 0, 0);

    adios_define_var (m_adios_group, "t1"
            ,"", adios_double
            ,"NX", "100", "0");

    adios_define_var (m_adios_group, "NX"
            ,"", adios_integer
            ,0, 0, 0);

    adios_define_var (m_adios_group, "t2"
            ,"", adios_double
            ,"NX", "100", "0");

    return 0;
}


int write_file (char *fname)
{
	char        filename [256];
	int         rank, size, i, block;
        int         adios_err;
        uint64_t    adios_groupsize, adios_totalsize;
        int64_t     adios_handle;

	strcpy (filename, "reuse_dim.bp");
   
        adios_open (&m_adios_file, "restart", filename, "w", comm);

        adios_groupsize = (2*sizeof(int) + 2*NX*sizeof(double));

        adios_group_size (m_adios_file, adios_groupsize, &adios_totalsize);

        adios_write(m_adios_file, "NX", (void *) &NX);
        adios_write(m_adios_file, "t1", t);

        adios_write(m_adios_file, "NX", (void *) &NX);
        adios_write(m_adios_file, "t2", t);

        adios_close (m_adios_file);

        MPI_Barrier (comm);

        return 0;
}

#define CHECK_ARRAY(A,R,n) \
    for (i=0;i<n;i++) \
        if (A[i] != R[i]) { \
            printE ("Variable " #A " does not equal in written and read values at position %d\n", i);\
            err = 103; \
            goto endread;\
        }

void reset_rarrays()
{
    int i;
    for (i = 0; i < NX; i++) {
        r1[i] = -1;
        r2[i] = -1;
    }
}


int read_file (char *fname)
{
    ADIOS_SELECTION *sel;
    ADIOS_FILE * f;
    ADIOS_VARINFO * vi;
    int err=0,i,n;

    uint64_t start[1] = {0};
    uint64_t count[2] = {NX};
    uint64_t ndim;

    reset_rarrays();

    log ("Read and check data in %s\n", fname);
    f = adios_read_open_file (fname, ADIOS_READ_METHOD_BP, comm);
    if (f == NULL) {
        printE ("Error at opening file: %s\n", rank, adios_errmsg());
        return 1;
    }

    sel = adios_selection_boundingbox (1, start, count);
    adios_schedule_read (f, sel, "t1", 0, 1, r1);
    adios_schedule_read (f, sel, "t2", 0, 1, r2);
    adios_perform_reads (f, 1);
    adios_selection_delete (sel);

    CHECK_ARRAY (t, r1, NX);
    CHECK_ARRAY (t, r2, NX);

endread:
    adios_read_close(f);
    MPI_Barrier (comm);
    return err;
}

