/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C Example: write a global array from N processors using MPI_AGGREGATE.
 *
 * How to run: mpirun -np <N> adios_amr_write
 * Output: adios_amr_write.bp
 * ADIOS config file: adios_amr_write.xml
 *
*/
#include <stdio.h>
#include <string.h>
#include "adios.h"
int main (int argc, char ** argv) 
{
    char        filename [256];
    int         rank, size, i, pattern, npe_1, npe_2;
    int         l1, l2, o1, o2, g1, g2; 
    double      t1, t2;
    MPI_Comm    comm = MPI_COMM_WORLD;

    /* ADIOS variables declarations for matching gwrite_temperature.ch */
    int         adios_err;
    uint64_t    adios_groupsize, adios_totalsize;
    int64_t     adios_handle;

    MPI_Init (&argc, &argv);

    if (argc != 2)
    {
        printf ("Invalid command line arguments.\n");
        exit (1);
    }

    pattern = atoi (argv[1]);

    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &size);

    g1 = 32;
    g2 = 16;

    if (pattern == 1)
    {
        l1 = 1;
        l2 = g2;
    }
    else if (pattern == 2)
    {
        /* 4x4 array */
        l1 = 4;
        l2 = 4;
    }
    else
    {
        printf ("wrong argument.\n");
    }

    npe_1 = g1 / l1;
    npe_2 = g2 / l2;

    o1 = (rank / npe_2) * l1;
    o2 = (rank % npe_2) * l2;

    double t[l1*l2];

    for (i = 0; i < l1 * l2; i++)
    {
        t[i] = o1 * g2 + o2 + (i / l2) * g2 + i % l2;
    }

    strcpy (filename, "adios_amr_write.bp");

    adios_init ("adios_amr_write.xml", comm);

    adios_open (&adios_handle, "temperature", filename, "w", comm);

    adios_groupsize = 4  * 8\
                    + 8 * l1 * l2;
    adios_group_size (adios_handle, adios_groupsize, &adios_totalsize);
    adios_write (adios_handle, "l1", &l1);
    adios_write (adios_handle, "l2", &l2);
    adios_write (adios_handle, "o1", &o1);
    adios_write (adios_handle, "o2", &o2);
    adios_write (adios_handle, "g1", &g1);
    adios_write (adios_handle, "g2", &g2);
    adios_write (adios_handle, "temperature", t);

    adios_close (adios_handle);

    adios_finalize (rank);

    MPI_Finalize ();

    return 0;
}
