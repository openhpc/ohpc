/************************************************************

  This example shows how to set the fill value for a
  dataset.  The program first sets the fill value to
  FILLVAL, creates a dataset with dimensions of DIM0xDIM1,
  reads from the uninitialized dataset, and outputs the
  contents to the screen.  Next, it writes integers to the
  dataset, reads the data back, and outputs it to the
  screen.  Finally it extends the dataset, reads from it,
  and outputs the result to the screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_d_fillval.h5"
#define DATASET         "DS1"
#define DIM0            4
#define DIM1            7
#define EDIM0           6
#define EDIM1           10
#define CHUNK0          4
#define CHUNK1          4
#define FILLVAL         99

int
main (void)
{
    hid_t           file, space, dset, dcpl;    /* Handles */
    herr_t          status;
    hsize_t         dims[2] = {DIM0, DIM1},
                    extdims[2] = {EDIM0, EDIM1},
                    maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED},
                    chunk[2] = {CHUNK0, CHUNK1};
    int             wdata[DIM0][DIM1],          /* Write buffer */
                    rdata[DIM0][DIM1],          /* Read buffer */
                    rdata2[EDIM0][EDIM1],       /* Read buffer for
                                                   extenstion */
                    fillval,
                    i, j;

    /*
     * Initialize data.
     */
    for (i=0; i<DIM0; i++)
        for (j=0; j<DIM1; j++)
            wdata[i][j] = i * j - j;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create dataspace with unlimited dimensions.
     */
    space = H5Screate_simple (2, dims, maxdims);

    /*
     * Create the dataset creation property list, and set the chunk
     * size.
     */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    status = H5Pset_chunk (dcpl, 2, chunk);

    /*
     * Set the fill value for the dataset.
     */
    fillval = FILLVAL;
    status = H5Pset_fill_value (dcpl, H5T_NATIVE_INT, &fillval);

    /*
     * Set the allocation time to "early".  This way we can be sure
     * that reading from the dataset immediately after creation will
     * return the fill value.
     */
    status = H5Pset_alloc_time (dcpl, H5D_ALLOC_TIME_EARLY);

    /*
     * Create the dataset using the dataset creation property list.
     */
    dset = H5Dcreate (file, DATASET, H5T_STD_I32LE, space, dcpl);

    /*
     * Read values from the dataset, which has not been written to yet.
     */
    status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf ("Dataset before being written to:\n");
    for (i=0; i<dims[0]; i++) {
        printf (" [");
        for (j=0; j<dims[1]; j++)
            printf (" %3d", rdata[i][j]);
        printf ("]\n");
    }

    /*
     * Write the data to the dataset.
     */
    status = H5Dwrite (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                wdata[0]);

    /*
     * Read the data back.
     */
    status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf ("\nDataset after being written to:\n");
    for (i=0; i<dims[0]; i++) {
        printf (" [");
        for (j=0; j<dims[1]; j++)
            printf (" %3d", rdata[i][j]);
        printf ("]\n");
    }

    /*
     * Extend the dataset.
     */
    status = H5Dset_extent (dset, extdims);

    /*
     * Read from the extended dataset.
     */
    status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata2[0]);

    /*
     * Output the data to the screen.
     */
    printf ("\nDataset after extension:\n");
    for (i=0; i<extdims[0]; i++) {
        printf (" [");
        for (j=0; j<extdims[1]; j++)
            printf (" %3d", rdata2[i][j]);
        printf ("]\n");
    }

    /*
     * Close and release resources.
     */
    status = H5Pclose (dcpl);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Fclose (file);

    return 0;
}
