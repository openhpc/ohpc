/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* This code is used to test staged-read method.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "adios_read.h"

#ifdef DMALLOC
#include "dmalloc.h"
#endif

enum pattern 
{
    PATTERN_1 = 1,
    PATTERN_2,
    PATTERN_3,
    PATTERN_4,
    PATTERN_5
};

int main (int argc, char ** argv) 
{
    char filename [256];
    int rank, size, i, j, NX, pattern, err;
    MPI_Comm    comm = MPI_COMM_WORLD;
    void * data = NULL, * data1 = NULL, * data2 = NULL;
    uint64_t start[2], count[2], bytes_read = 0, slice_size;
    ADIOS_SELECTION *sel;
    ADIOS_FILE * f;
    ADIOS_VARINFO * vi;

    MPI_Init (&argc, &argv);

    if (argc != 2)
    {
        printf ("Invalid command line arguments.\n");
        exit (1);
    }

    pattern = atoi (argv[1]);
    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &size);

    err = adios_read_init_method(ADIOS_READ_METHOD_BP_AGGREGATE, comm, "verbose=2");

    f = adios_read_open_file ("adios_amr_write.bp", ADIOS_READ_METHOD_BP_AGGREGATE, comm);
    if (!f)
    {
        printf ("Error at opening file: %s\n", adios_errmsg());
        return 1;
    }


    ADIOS_VARINFO * v = adios_inq_var (f, "temperature");
    switch (pattern)
    {
        case PATTERN_1:
            slice_size = v->dims[0]/size;

            start[0] = rank * slice_size;
            count[0] = slice_size;
            if (rank == size - 1)
            {
                slice_size = slice_size + v->dims[0] % size;
            }

            start[1] = 0;
            count[1] = v->dims[1];

            data = malloc (slice_size * v->dims[1] * sizeof (double));

            break;
        case PATTERN_2:
            start[0] = 0;
            count[0] = v->dims[0];

            slice_size = v->dims[1]/size;

            start[1] = slice_size * rank;
            if (rank == size - 1)
            {
                slice_size = slice_size + v->dims[1] % size;
            }
            count[1] = slice_size;

            data = malloc (slice_size * v->dims[0] * sizeof (double));

            break;
        case PATTERN_3:
            start[0] = 0;
            count[0] = 4;

            slice_size = 3;

            start[1] = 2 + slice_size * rank;
            count[1] = slice_size;

            data = malloc (slice_size * count[0] * sizeof (double));

            break;
        case PATTERN_4:
            start[0] = 0;
            count[0] = 32;

            slice_size = 3;

            start[1] = 2 + slice_size * rank;
            count[1] = slice_size;

            data = malloc (slice_size * count[0] * sizeof (double));

            break;
        case PATTERN_5:
            start[0] = 0;
            count[0] = 32;

            slice_size = 3;

            start[1] = 2 + slice_size * rank;
            count[1] = slice_size;

            data = malloc (slice_size * count[0] * sizeof (double));

            break;
        default:
            printf ("wrong pattern value\n");
    }

    assert (data);

    sel = adios_selection_boundingbox (2, start, count);
    adios_schedule_read (f, sel, "temperature", 0, 1, data);
    adios_perform_reads (f, 1);
    adios_selection_delete (sel);

    adios_read_close(f);


    if (pattern != PATTERN_5)
    {
        if (rank == 0)
        {
            for (i = 0; i < count[0]; i++)
            {
                for (j = 0; j < count[1]; j++)
                {
                    printf (" %7.5g", * ((double *)data + i * count[1] + j));
                }

                printf ("\n");
            }
        }
    }
    else
    {
        if (rank == 1)
        {
            for (i = 0; i < count[0]; i++)
            {
                for (j = 0; j < count[1]; j++)
                {
                    printf (" %7.5g", * ((double *)data + i * count[1] + j));
                }

                printf ("\n");
            }
        }
    }

    free (data);
    adios_free_varinfo (v);

    adios_read_finalize_method (ADIOS_READ_METHOD_BP_AGGREGATE);
    MPI_Finalize ();

    return 0;
}
