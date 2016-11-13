/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#include <stdio.h>
#include <string.h>
#include "mpi.h"
#include "adios.h"

/*************************************************************/
/*          Example of writing arrays in ADIOS               */
/*                                                           */
/*        Similar example is manual/2_adios_write.c          */
/*************************************************************/
int main (int argc, char ** argv) 
{
    char        filename [256];
    int         rank, i, j;
    int         NX = 10, NY = 100; 
    double      t[NX][NY];
    int         p[NX];
    MPI_Comm    comm = MPI_COMM_WORLD;

    uint64_t    adios_groupsize, adios_totalsize;
    int64_t     adios_handle;

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (comm, &rank);

    for (i = 0; i < NX; i++)
        for (j = 0; j< NY; j++)
            t[i][j] = rank * NX + i + j*(1.0/NY);

    for (i = 0; i < NX; i++)
        p[i] = rank * NX + i;

    strcpy (filename, "arrays.bp");
    adios_init ("arrays.xml", comm);
    adios_open (&adios_handle, "arrays", filename, "w", comm);
#include "gwrite_arrays.ch"
    adios_close (adios_handle);

    MPI_Barrier (comm);

    adios_finalize (rank);

    MPI_Finalize ();
    return 0;
}
