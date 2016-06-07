/************************************************************

  This example shows how to read and write data to an
  external dataset.  The program first writes integers to an
  external dataset with dataspace dimensions of DIM0xDIM1,
  then closes the file.  Next, it reopens the file, reads
  back the data, and outputs the name of the external data
  file and the data to the screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_d_extern.h5"
#define EXTERNAL        "h5ex_d_extern.data"
#define DATASET         "DS1"
#define DIM0            4
#define DIM1            7
#define NAME_BUF_SIZE   32

int
main (void)
{
    hid_t       file, space, dset, dcpl;    /* Handles */
    herr_t      status;
    hsize_t     dims[2] = {DIM0, DIM1};
    char        name[NAME_BUF_SIZE];
    int         wdata[DIM0][DIM1],          /* Write buffer */
                rdata[DIM0][DIM1],          /* Read buffer */
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
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (2, dims, NULL);

    /*
     * Create the dataset creation property list, set the external
     * file.
     */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    status = H5Pset_external (dcpl, EXTERNAL, 0, H5F_UNLIMITED);

    /*
     * Create the external dataset.
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
     * Now we begin the read section of this example.
     */

    /*
     * Open file and dataset using the default properties.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, DATASET);

    /*
     * Retrieve dataset creation property list.
     */
    dcpl = H5Dget_create_plist (dset);

    /*
     * Retrieve and print the name of the external file.  Here we
     * manually set the last field in name to null, in case the name of
     * the file is longer than the buffer.
     */
    status = H5Pget_external (dcpl, 0, NAME_BUF_SIZE, name, NULL, NULL);
    name[NAME_BUF_SIZE-1] = '\0';
    printf ("%s is stored in file: %s\n", DATASET, name);

    /*
     * Read the data using the default properties.
     */
    status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf ("%s:\n", DATASET);
    for (i=0; i<DIM0; i++) {
        printf (" [");
        for (j=0; j<DIM1; j++)
            printf (" %3d", rdata[i][j]);
        printf ("]\n");
    }

    /*
     * Close and release resources.
     */
    status = H5Pclose (dcpl);
    status = H5Dclose (dset);
    status = H5Fclose (file);

    return 0;
}
