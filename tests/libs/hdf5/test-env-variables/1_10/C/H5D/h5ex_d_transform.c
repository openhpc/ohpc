/************************************************************

  This example shows how to read and write data to a dataset
  using a data transform expression.  The program first
  writes integers to a dataset using the transform
  expression TRANSFORM, then closes the file.  Next, it
  reopens the file, reads back the data without a transform,
  and outputs the data to the screen.  Finally it reads the
  data using the transform expression RTRANSFORM and outputs
  the results to the screen.

  This file is intended for use with HDF5 Library version 1.8

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_d_transform.h5"
#define DATASET         "DS1"
#define DIM0            4
#define DIM1            7
#define TRANSFORM       "x+1"
#define RTRANSFORM      "x-1"

int
main (void)
{
    hid_t           file, space, dset, dxpl;
                                                /* Handles */
    herr_t          status;
    hsize_t         dims[2] = {DIM0, DIM1};
    int             wdata[DIM0][DIM1],          /* Write buffer */
                    rdata[DIM0][DIM1],          /* Read buffer */
                    i, j;

    /*
     * Initialize data.
     */
    for (i=0; i<DIM0; i++)
        for (j=0; j<DIM1; j++)
            wdata[i][j] = i * j - j;

    /*
     * Output the data to the screen.
     */
    printf ("Original Data:\n");
    for (i=0; i<DIM0; i++) {
        printf (" [");
        for (j=0; j<DIM1; j++)
            printf (" %3d", wdata[i][j]);
        printf ("]\n");
    }

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (2, dims, NULL);

    /*
     * Create the dataset transfer property list and define the
     * transform expression.
     */
    dxpl = H5Pcreate (H5P_DATASET_XFER);
    status = H5Pset_data_transform (dxpl, TRANSFORM);

    /*
     * Create the dataset using the default properties.  Unfortunately
     * we must save as a native type or the transform operation will
     * fail.
     */
    dset = H5Dcreate (file, DATASET, H5T_NATIVE_INT, space, H5P_DEFAULT,
                H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write the data to the dataset using the dataset transfer
     * property list.
     */
    status = H5Dwrite (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, wdata[0]);

    /*
     * Close and release resources.
     */
    status = H5Pclose (dxpl);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Fclose (file);


    /*
     * Now we begin the read section of this example.
     */

    /*
     * Open file and dataset using the default properties.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, DATASET, H5P_DEFAULT);

    /*
     * Read the data using the default properties.
     */
    status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf ("\nData as written with transform \"%s\":\n", TRANSFORM);
    for (i=0; i<DIM0; i++) {
        printf (" [");
        for (j=0; j<DIM1; j++)
            printf (" %3d", rdata[i][j]);
        printf ("]\n");
    }

    /*
     * Create the dataset transfer property list and define the
     * transform expression.
     */
    dxpl = H5Pcreate (H5P_DATASET_XFER);
    status = H5Pset_data_transform (dxpl, RTRANSFORM);

    /*
     * Read the data using the dataset transfer property list.
     */
    status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf ("\nData as written with transform \"%s\" and read with transform \"%s\":\n",
                TRANSFORM, RTRANSFORM);
    for (i=0; i<DIM0; i++) {
        printf (" [");
        for (j=0; j<DIM1; j++)
            printf (" %3d", rdata[i][j]);
        printf ("]\n");
    }

    /*
     * Close and release resources.
     */
    status = H5Pclose (dxpl);
    status = H5Dclose (dset);
    status = H5Fclose (file);

    return 0;
}
