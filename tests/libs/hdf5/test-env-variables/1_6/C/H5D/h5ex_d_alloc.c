/************************************************************

  This example shows how to set the space allocation time
  for a dataset.  The program first creates two datasets,
  one with the default allocation time (late) and one with
  early allocation time, and displays whether each has been
  allocated and their allocation size.  Next, it writes data
  to the datasets, and again displays whether each has been
  allocated and their allocation size.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_d_alloc.h5"
#define DATASET1        "DS1"
#define DATASET2        "DS2"
#define DIM0            4
#define DIM1            7
#define FILLVAL         99

int
main (void)
{
    hid_t                   file, space, dset1, dset2, dcpl;
                                                    /* Handles */
    herr_t                  status;
    H5D_space_status_t      space_status;
    hsize_t                 dims[2] = {DIM0, DIM1},
                            storage_size;
    int                     wdata[DIM0][DIM1],      /* Write buffer */
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
     * Create the dataset creation property list, and set the chunk
     * size.
     */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);

    /*
     * Set the allocation time to "early".  This way we can be sure
     * that reading from the dataset immediately after creation will
     * return the fill value.
     */
    status = H5Pset_alloc_time (dcpl, H5D_ALLOC_TIME_EARLY);

    printf ("Creating datasets...\n");
    printf ("%s has allocation time H5D_ALLOC_TIME_LATE\n", DATASET1);
    printf ("%s has allocation time H5D_ALLOC_TIME_EARLY\n\n", DATASET2);

    /*
     * Create the dataset using the dataset creation property list.
     */
    dset1 = H5Dcreate (file, DATASET1, H5T_STD_I32LE, space, H5P_DEFAULT);
    dset2 = H5Dcreate (file, DATASET2, H5T_STD_I32LE, space, dcpl);

    /*
     * Retrieve and print space status and storage size for dset1.
     */
    status = H5Dget_space_status (dset1, &space_status);
    storage_size = H5Dget_storage_size (dset1);
    printf ("Space for %s has%sbeen allocated.\n", DATASET1,
                space_status == H5D_SPACE_STATUS_ALLOCATED ? " " : " not ");
    printf ("Storage size for %s is: %ld bytes.\n", DATASET1,
                (long) storage_size);

    /*
     * Retrieve and print space status and storage size for dset2.
     */
    status = H5Dget_space_status (dset2, &space_status);
    storage_size = H5Dget_storage_size (dset2);
    printf ("Space for %s has%sbeen allocated.\n", DATASET2,
                space_status == H5D_SPACE_STATUS_ALLOCATED ? " " : " not ");
    printf ("Storage size for %s is: %ld bytes.\n", DATASET2,
                (long) storage_size);

    printf ("\nWriting data...\n\n");

    /*
     * Write the data to the datasets.
     */
    status = H5Dwrite (dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                wdata[0]);
    status = H5Dwrite (dset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                wdata[0]);

    /*
     * Retrieve and print space status and storage size for dset1.
     */
    status = H5Dget_space_status (dset1, &space_status);
    storage_size = H5Dget_storage_size (dset1);
    printf ("Space for %s has%sbeen allocated.\n", DATASET1,
                space_status == H5D_SPACE_STATUS_ALLOCATED ? " " : " not ");
    printf ("Storage size for %s is: %ld bytes.\n", DATASET1,
                (long) storage_size);

    /*
     * Retrieve and print space status and storage size for dset2.
     */
    status = H5Dget_space_status (dset2, &space_status);
    storage_size = H5Dget_storage_size (dset2);
    printf ("Space for %s has%sbeen allocated.\n", DATASET2,
                space_status == H5D_SPACE_STATUS_ALLOCATED ? " " : " not ");
    printf ("Storage size for %s is: %ld bytes.\n", DATASET2,
                (long) storage_size);

    /*
     * Close and release resources.
     */
    status = H5Pclose (dcpl);
    status = H5Dclose (dset1);
    status = H5Dclose (dset2);
    status = H5Sclose (space);
    status = H5Fclose (file);

    return 0;
}
