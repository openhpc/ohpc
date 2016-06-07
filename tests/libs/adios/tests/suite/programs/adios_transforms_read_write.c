/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C Example: write a global array from N processors using MPI_AGGREGATE.
 *
 * How to run: mpirun -np <N> adios_transform_read_write
 * Output: adios_transform_write.bp
 * ADIOS config file: adios_transform_write.xml
 *
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "adios.h"
#include "adios_read.h"
#include <stdint.h>
#include <assert.h>

const int ntransforms = 7;
const char varname_xform [][256] = { "t_none", 
                                     "t_identity", 
                                     "t_zlib",
                                     "t_szip",
                                     "t_bzip2",
                                     "t_aplod", 
                                     "t_isobar"
};

int find_var (ADIOS_FILE *f, char *name)
{
    int i;
    for (i=0; i<f->nvars; i++)
    {
        // skip the leading / in the file's varname list in the comparison
        if (!strcmp(name, f->var_namelist[i]))
            return 1;
    }
    return 0;
}


int read_box (char *filename)
{

    double                  *data_xform [ntransforms];
    MPI_Comm                comm                = MPI_COMM_WORLD;

    enum ADIOS_READ_METHOD        method  = ADIOS_READ_METHOD_BP;
    ADIOS_FILE              *f       = adios_read_open_file (filename, method, comm);
    ADIOS_VARINFO           *varinfo = adios_inq_var (f, varname_xform [0]);
    ADIOS_SELECTION         *sel1;

    assert (varinfo);
    adios_inq_var_blockinfo (f, varinfo);

    uint32_t                nblocks  = varinfo->sum_nblocks;
    uint32_t                ndim     = varinfo->ndim;
    uint64_t                *pg_dims = varinfo->blockinfo [0].count;

    uint64_t npoints = 0;
    uint64_t *starts  = (uint64_t *) calloc (varinfo->ndim, sizeof (uint64_t));
    uint64_t *counts  = (uint64_t *) calloc (varinfo->ndim, sizeof (uint64_t));

    uint64_t data_size = 0;
    int rank;
    int i = 0;
    int retval = 0;

    MPI_Comm_rank (comm, &rank);
    srand (rank);

    assert (varinfo->ndim);

    npoints = 1;

    // Generate the start offsets and lengths for each dimension
    for (i = 0; i < varinfo->ndim; i ++) {

        // Not really a good way to generate random numbers. But will do for
        // now
        starts [i] = rand () % ((varinfo->dims [i] + 1) / 2);
        counts [i] = rand () % ((varinfo->dims [i] + 1) / 2) + 1;

        npoints *= counts [i];

        // printf ("[%d] %d %lld %lld\n", rank, i, starts [i], counts [i]);
    }

    sel1 = adios_selection_boundingbox (varinfo->ndim, starts, counts);

    // Allocate memory for reading in the transformed data and schedule reads
    // into the buffer
    for (i = 0; i < ntransforms; i ++) {
        if (find_var (f, varname_xform [i])) {
            printf ("\n\t %s variable exists, schedule to read...", varname_xform [i]);
            data_xform [i]    = malloc (npoints * sizeof (double));
            adios_schedule_read (f, sel1, varname_xform [i], 0, 1, data_xform [i]);
        } else {
            printf ("\n\t %s variable is not in the file, skip.", varname_xform [i]);
            data_xform [i] = NULL;
        }
    }
    printf ("\n");

    // Read them all at once
    adios_perform_reads (f, 1);

    data_size = npoints * sizeof (double);

    // Output for each transform must match that of no transform 
    for (i = 1; i < ntransforms; i ++) {
        if (data_xform [i]) {
            if (memcmp (data_xform [0], data_xform [i], data_size) != 0) {
                printf ("\n\t %s --- Failed", varname_xform [i]);
                retval = 1;
            } else {          
                printf ("\n\t %s --- OK", varname_xform [i]);
            }
            free (data_xform [i]);
        }
    }

    printf ("\n");

    free (data_xform [0]);
    adios_selection_delete (sel1);

    free (starts);
    free (counts);

    adios_free_varinfo (varinfo);
    adios_read_close (f);
    adios_read_finalize_method (ADIOS_READ_METHOD_BP);

    return retval;
}

int main (int argc, char *argv [])
{
    char        filename [256];
    int         rank, size, i, retval;
    int         l1, l2, o1, o2, g1, g2;
    MPI_Comm    comm = MPI_COMM_WORLD;

    int         adios_err;
    uint64_t    adios_groupsize, adios_totalsize;
    int64_t     adios_handle;
    double *t;

    uint32_t timestep = 0;

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &size);




    strcpy (filename, "adios_transforms_read_write.bp");

    adios_init ("adios_transforms.xml", comm);

    o1 = rank;
    o2 = 0;
    l1 = 1;
    l2 = 64;

    g1 = size;
    g2 = 64;

    t = (double *) malloc (l1*l2*sizeof(double));

    // Write out 'size' timesteps. During the reads each rank would read in its
    // own timestep
    for (timestep = 0; timestep < size; timestep ++) {

        if (timestep == 0) {
            adios_open (&adios_handle, "transform", filename, "w", comm);
        } else {
            adios_open (&adios_handle, "transform", filename, "a", comm);
        }

        adios_groupsize = 6 * sizeof (int) \
                        + ntransforms * sizeof (double) * l1 * l2;

        // Taken from adios_amr_write
        for (i = 0; i < l1 * l2; i++) {
            t[i] = timestep + o1 * g2 + o2 + (i / l2) * g2 + i % l2;
        }

        adios_group_size (adios_handle, adios_groupsize, &adios_totalsize);
        //printf ("\t Per process group size = %lld, with metadata = %lld\n", 
        //        adios_groupsize, adios_totalsize);
        adios_write (adios_handle, "l1", &l1);
        adios_write (adios_handle, "l2", &l2);
        adios_write (adios_handle, "o1", &o1);
        adios_write (adios_handle, "o2", &o2);
        adios_write (adios_handle, "g1", &g1);
        adios_write (adios_handle, "g2", &g2);

        // We want to test whether the arrays written out
        // below can recovered without corruption during reads
        for (i = 0; i < ntransforms; i ++) {
            adios_write (adios_handle, varname_xform [i], t);
        }

        adios_close (adios_handle);
    }

    adios_finalize (rank);

    retval = read_box (filename);

    MPI_Finalize ();

    return retval;
}

