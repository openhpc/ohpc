/************************************************************

  This example shows how to create and extend an unlimited
  dataset.  The program first writes integers to a dataset
  with dataspace dimensions of DIM0xDIM1, then closes the
  file.  Next, it reopens the file, reads back the data,
  outputs it to the screen, extends the dataset, and writes
  new data to the entire extended dataset.  Finally it
  reopens the file again, reads back the data, and utputs it
  to the screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_d_unlimmod.h5"
#define DATASET         "DS1"
#define DIM0            4
#define DIM1            7
#define EDIM0           6
#define EDIM1           10
#define CHUNK0          4
#define CHUNK1          4

int
main (void)
{
    hid_t           file, space, dset, dcpl;    /* Handles */
    herr_t          status;
    hsize_t         dims[2] = {DIM0, DIM1},
                    extdims[2] = {EDIM0, EDIM1},
                    maxdims[2],
                    chunk[2] = {CHUNK0, CHUNK1};
    int             wdata[DIM0][DIM1],          /* Write buffer */
                    wdata2[EDIM0][EDIM1],       /* Write buffer for
                                                   extension */
                    **rdata,                    /* Read buffer */
                    ndims,
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
    maxdims[0] = H5S_UNLIMITED;
    maxdims[1] = H5S_UNLIMITED;
    space = H5Screate_simple (2, dims, maxdims);

    /*
     * Create the dataset creation property list, and set the chunk
     * size.
     */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    status = H5Pset_chunk (dcpl, 2, chunk);

    /*
     * Create the unlimited dataset.
     */
    dset = H5Dcreate (file, DATASET, H5T_STD_I32LE, space, dcpl);

    /*
     * Write the data to the dataset.
     */
    status = H5Dwrite (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                wdata[0]);

    /*
     * Close and release resources.
     */
    status = H5Pclose (dcpl);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Fclose (file);


    /*
     * In this next section we read back the data, extend the dataset,
     * and write new data to the entire dataset.
     */

    /*
     * Open file and dataset using the default properties.
     */
    file = H5Fopen (FILE, H5F_ACC_RDWR, H5P_DEFAULT);
    dset = H5Dopen (file, DATASET);

    /*
     * Get dataspace and allocate memory for read buffer.  This is a
     * two dimensional dataset so the dynamic allocation must be done
     * in steps.
     */
    space = H5Dget_space (dset);
    ndims = H5Sget_simple_extent_dims (space, dims, NULL);

    /*
     * Allocate array of pointers to rows.
     */
    rdata = (int **) malloc (dims[0] * sizeof (int *));

    /*
     * Allocate space for integer data.
     */
    rdata[0] = (int *) malloc (dims[0] * dims[1] * sizeof (int));

    /*
     * Set the rest of the pointers to rows to the correct addresses.
     */
    for (i=1; i<dims[0]; i++)
        rdata[i] = rdata[0] + i * dims[1];

    /*
     * Read the data using the default properties.
     */
    status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf ("Dataset before extension:\n");
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
     * Initialize data for writing to the extended dataset.
     */
    for (i=0; i<EDIM0; i++)
        for (j=0; j<EDIM1; j++)
            wdata2[i][j] = j;

    /*
     * Write the data to the extended dataset.
     */
    status = H5Dwrite (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                wdata2[0]);

    /*
     * Close and release resources.
     */
    free (rdata[0]);
    free(rdata);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Fclose (file);


    /*
     * Now we simply read back the data and output it to the screen.
     */

    /*
     * Open file and dataset using the default properties.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, DATASET);

    /*
     * Get dataspace and allocate memory for the read buffer as before.
     */
    space = H5Dget_space (dset);
    ndims = H5Sget_simple_extent_dims (space, dims, NULL);
    rdata = (int **) malloc (dims[0] * sizeof (int *));
    rdata[0] = (int *) malloc (dims[0] * dims[1] * sizeof (int));
    for (i=1; i<dims[0]; i++)
        rdata[i] = rdata[0] + i * dims[1];

    /*
     * Read the data using the default properties.
     */
    status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf ("\nDataset after extension:\n");
    for (i=0; i<dims[0]; i++) {
        printf (" [");
        for (j=0; j<dims[1]; j++)
            printf (" %3d", rdata[i][j]);
        printf ("]\n");
    }

    /*
     * Close and release resources.
     */
    free (rdata[0]);
    free(rdata);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Fclose (file);

    return 0;
}
