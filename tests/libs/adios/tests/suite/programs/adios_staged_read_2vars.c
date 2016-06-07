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

#define ADIOS_USE_READ_API_1
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
    int rank, size, i, j, NX, pattern;
    MPI_Comm comm = MPI_COMM_WORLD;
    void * data1 = NULL, * data2 = NULL;
    uint64_t start1[2], count1[2], bytes_read = 0, slice_size;
    uint64_t start2[2], count2[2];

    MPI_Init (&argc, &argv);

    if (argc != 2)
    {
        printf ("Invalid command line arguments.\n");
        exit (1);
    }

    pattern = atoi (argv[1]);
    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &size);

    /* set the read method to staged read */
    adios_set_read_method (ADIOS_READ_METHOD_BP_AGGREGATE);

    ADIOS_FILE * f = adios_fopen ("adios_amr_write_2vars.bp", comm);

    if (!f)
    {
        printf ("%s\n", adios_errmsg());
        return -1;
    }

    ADIOS_GROUP * g = adios_gopen (f, "temperature");
    if (!g)
    {
        printf ("%s\n", adios_errmsg());
        return -1;
    }

    ADIOS_VARINFO * v1 = adios_inq_var (g, "temperature");
    ADIOS_VARINFO * v2 = adios_inq_var (g, "pressure");

    switch (pattern)
    {
        case PATTERN_1:
            /* temperature */
            slice_size = v1->dims[0]/size;

            start1[0] = rank * slice_size;
            count1[0] = slice_size;
            if (rank == size - 1)
            {
                slice_size = slice_size + v1->dims[0] % size;
            }

            start1[1] = 0;
            count1[1] = v1->dims[1];

            /* pressure */
            slice_size = v2->dims[0]/size;

            start2[0] = rank * slice_size;
            count2[0] = slice_size;
            if (rank == size - 1)
            {
                slice_size = slice_size + v2->dims[0] % size;
            }

            start2[1] = 0;
            count2[1] = v2->dims[1];

            break;
        case PATTERN_2:
            /* temperature */
            start1[0] = 0;
            count1[0] = v1->dims[0];

            slice_size = v1->dims[1]/size;

            start1[1] = slice_size * rank;
            if (rank == size - 1)
            {
                slice_size = slice_size + v1->dims[1] % size;
            }
            count1[1] = slice_size;

            /* pressure */
            start2[0] = 0;
            count2[0] = v2->dims[0];

            slice_size = v2->dims[1]/size;

            start2[1] = slice_size * rank;
            if (rank == size - 1)
            {
                slice_size = slice_size + v2->dims[1] % size;
            }
            count2[1] = slice_size;

            break;
        case PATTERN_3:
            /* temperature */
            start1[0] = 0;
            count1[0] = 4;

            slice_size = 3;

            start1[1] = 2 + slice_size * rank;
            count1[1] = slice_size;

            /* pressure */
            start2[0] = 0;
            count2[0] = 4;

            slice_size = 3;

            start2[1] = 2 + slice_size * rank;
            count2[1] = slice_size;

            break;
        case PATTERN_4:
            /* temperature */
            start1[0] = 0;
            count1[0] = 32;

            slice_size = 3;

            start1[1] = 2 + slice_size * rank;
            count1[1] = slice_size;

            /* pressure */
            start2[0] = 0;
            count2[0] = 32;

            slice_size = 3;

            start2[1] = 2 + slice_size * rank;
            count2[1] = slice_size;

            break;
        case PATTERN_5:
            /* temperature */
            start1[0] = 0;
            count1[0] = 32;

            slice_size = 3;

            start1[1] = 2 + slice_size * rank;
            count1[1] = slice_size;

            /* pressure */
            start2[0] = 0;
            count2[0] = 32;

            slice_size = 3;

            start2[1] = 2 + slice_size * rank;
            count2[1] = slice_size;

            break;
        default:
            printf ("wrong pattern value\n");
    }

    data1 = malloc (count1[0] * count1[1] * sizeof (double));
    data2 = malloc (count2[0] * count2[1] * sizeof (int));
    assert (data1);
    assert (data2);

    adios_read_var (g, "temperature", start1, count1, data1);
    adios_read_var (g, "pressure", start2, count2, data2);

    adios_gclose (g);

    adios_fclose (f);

    if (pattern != PATTERN_5)
    {
        if (rank == 0)
        {
            for (i = 0; i < count1[0]; i++)
            {
                for (j = 0; j < count1[1]; j++)
                {
                    printf (" %7.5g", * ((double *)data1 + i * count1[1] + j));
                }

                printf ("\n");
            }

            printf ("\n");

            for (i = 0; i < count2[0]; i++)
            {
                for (j = 0; j < count2[1]; j++)
                {
                    printf (" %7d", * ((int *)data2 + i * count2[1] + j));
                }

                printf ("\n");
            }
        }
    }
    else
    {
        if (rank == 1)
        {
            for (i = 0; i < count1[0]; i++)
            {
                for (j = 0; j < count1[1]; j++)
                {
                    printf (" %7.5g", * ((double *)data1 + i * count1[1] + j));
                }

                printf ("\n");
            }

            printf ("\n");

            for (i = 0; i < count2[0]; i++)
            {
                for (j = 0; j < count2[1]; j++)
                {
                    printf (" %7d", * ((int *)data2 + i * count2[1] + j));
                }

                printf ("\n");
            }

        }
    }

    free (data1);
    free (data2);
    adios_free_varinfo (v1);
    adios_free_varinfo (v2);

    MPI_Finalize ();

    return 0;
}
