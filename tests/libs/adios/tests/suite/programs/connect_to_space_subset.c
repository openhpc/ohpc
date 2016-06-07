/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C test: 
 *  Connect to staging server from a subset of processes, write data and close.
 *
 * How to run: mpirun -np <N> connect_to_space_subset <M>
 * where 1 <= M <= N
 * Output: exit code 0 = OK, != 0 on error
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

#define log(...) fprintf (stderr, "[rank=%3.3d, line %d]: ", wrank, __LINE__); fprintf (stderr, __VA_ARGS__); fflush(stderr);
#define printE(...) fprintf (stderr, "[rank=%3.3d, line %d]: ERROR: ", wrank, __LINE__); fprintf (stderr, __VA_ARGS__); fflush(stderr);

static const int NSTEPS = 5;
static const char FILENAME[] = "connect.bp";
#define VALUE(rank, step) (rank * 100 + step)
#define VALUE0(step) (step)

/* Variables to write */
int  *a1;

static const int ldim1 = 7;
int gdim1;
int offs1;

MPI_Comm   wcomm = MPI_COMM_WORLD;
MPI_Comm   subcomm;
int wrank, subrank;
int wsize, subsize;

int64_t    m_adios_group;


void alloc_vars()
{
    int n;

    gdim1 = subsize*ldim1;

    offs1 = subrank*ldim1;

    n = ldim1;
    a1  = (int*) malloc (n * sizeof(int));
}

void set_vars(int step)
{
    int n, i;
    int v = VALUE(subrank, step);

    n = ldim1;
    for (i=0; i<n; i++) a1[i] = v;
}

void fini_vars()
{
    free (a1);
}

void Usage()
{
    printf("Usage: mpirun -np <N> connect_to_space_subset <M>\n"
            "    <N>:   Number of processes\n"
            "    <M>:   Number of processes connecting to staging server\n"
            "           1 <= M <= N\n");
}


int write_file (int step);

int main (int argc, char ** argv) 
{
    int err,i,M; 
    int iconnect;

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (wcomm, &wrank);
    MPI_Comm_size (wcomm, &wsize);

    if (argc < 2) { Usage(); return 1; }
    errno = 0;
    M = strtol (argv[1], NULL, 10);
    if (errno || M < 1 || M > wsize) { 
        printE("Invalid 1st argument %s\n", argv[1]); Usage(); return 1;
    }

    iconnect = (wrank >= wsize-M); // connect to server from ranks N-M, N-M+1, ..., N 
    MPI_Comm_split (wcomm, iconnect, wrank+M-wsize, &subcomm);  
    MPI_Comm_rank (subcomm, &subrank);
    MPI_Comm_size (subcomm, &subsize);
    if (iconnect) {
        if (subsize != M) {
            printE ("Something wrong with communicator split: N=%d, M=%d, splitted size=%d\n",
                     wsize, M, subsize);
            return 2;
        }
        log ("connect as subrank %d\n", subrank);
    }

    alloc_vars();
    adios_read_init_method(ADIOS_READ_METHOD_DATASPACES, subcomm, "verbose=4");
    adios_init_noxml (subcomm);
    adios_allocate_buffer (ADIOS_BUFFER_ALLOC_NOW, 1);

    if (iconnect) 
    {
        adios_declare_group (&m_adios_group, "connect", "iter", adios_flag_yes);
        adios_select_method (m_adios_group, "DATASPACES", "verbose=4", "");
        adios_define_var (m_adios_group, "ldim1", "", adios_integer, 0, 0, 0);
        adios_define_var (m_adios_group, "gdim1", "", adios_integer, 0, 0, 0);
        adios_define_var (m_adios_group, "offs1", "", adios_integer, 0, 0, 0);
        adios_define_var (m_adios_group, "a1", "", adios_integer, 
                                         "ldim1", "gdim1", "offs1");
    
        for (i=0; i<NSTEPS; i++) {
            if (!err) {
                set_vars (i);
                err = write_file (i); 
            }
        }
    }
    log ("done with work, sync with others...\n");
    MPI_Barrier (wcomm);
    log ("call adios_finalize...\n");
    adios_finalize (wrank);
    log ("call adios_read_finalize_method...\n");
    adios_read_finalize_method (ADIOS_READ_METHOD_DATASPACES);
    fini_vars();
    MPI_Finalize ();
    return err;
}


int write_file (int step) 
{
    int64_t       fh;
    uint64_t       groupsize=0, totalsize;

    log ("Write step %d to %s\n", step, FILENAME);
    adios_open (&fh, "connect", FILENAME, (step ? "a" : "w"), &subcomm);
    
    groupsize  = 3 * sizeof(int);                           // dimensions 
    groupsize += ldim1 * sizeof(int);                       // 1D 

    adios_group_size (fh, groupsize, &totalsize);

    adios_write (fh, "gdim1", &gdim1);
    adios_write (fh, "ldim1", &ldim1);
    adios_write (fh, "offs1", &offs1);
    adios_write (fh, "a1", a1);

    adios_close (fh);
    MPI_Barrier (subcomm);
    return 0;
}


