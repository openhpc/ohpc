/************************************************************

  This example shows how to read and write array datatypes
  to a dataset.  The program first writes integers arrays of
  dimension ADIM0xADIM1 to a dataset with a dataspace of
  DIM0, then closes the  file.  Next, it reopens the file,
  reads back the data, and outputs it to the screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_t_array.h5"
#define DATASET         "DS1"
#define DIM0            4
#define ADIM0           3
#define ADIM1           5

int
main (void)
{
    hid_t       file, filetype, memtype, space, dset;
                                                /* Handles */
    herr_t      status;
    hsize_t     dims[1] = {DIM0},
                adims[2] = {ADIM0, ADIM1};
    int         wdata[DIM0][ADIM0][ADIM1],      /* Write buffer */
                ***rdata,                       /* Read buffer */
                ndims,
                i, j, k;

    /*
     * Initialize data.  i is the element in the dataspace, j and k the
     * elements within the array datatype.
     */
    for (i=0; i<DIM0; i++)
        for (j=0; j<ADIM0; j++)
            for (k=0; k<ADIM1; k++)
                wdata[i][j][k] = i * j - j * k + i * k;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create array datatypes for file and memory.
     */
    filetype = H5Tarray_create (H5T_STD_I64LE, 2, adims, NULL);
    memtype = H5Tarray_create (H5T_NATIVE_INT, 2, adims, NULL);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (1, dims, NULL);

    /*
     * Create the dataset and write the array data to it.
     */
    dset = H5Dcreate (file, DATASET, filetype, space, H5P_DEFAULT);
    status = H5Dwrite (dset, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                wdata[0][0]);

    /*
     * Close and release resources.
     */
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Tclose (filetype);
    status = H5Tclose (memtype);
    status = H5Fclose (file);


    /*
     * Now we begin the read section of this example.  Here we assume
     * the dataset and array have the same name and rank, but can have
     * any size.  Therefore we must allocate a new array to read in
     * data using malloc().
     */

    /*
     * Open file and dataset.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, DATASET);

    /*
     * Get the datatype and its dimensions.
     */
    filetype = H5Dget_type (dset);
    ndims = H5Tget_array_dims (filetype, adims, NULL);

    /*
     * Get dataspace and allocate memory for read buffer.  This is a
     * three dimensional dataset when the array datatype is included so
     * the dynamic allocation must be done in steps.
     */
    space = H5Dget_space (dset);
    ndims = H5Sget_simple_extent_dims (space, dims, NULL);

    /*
     * Allocate array of pointers to two-dimensional arrays (the
     * elements of the dataset.
     */
    rdata = (int ***) malloc (dims[0] * sizeof (int **));

    /*
     * Allocate two dimensional array of pointers to rows in the data
     * elements.
     */
    rdata[0] = (int **) malloc (dims[0] * adims[0] * sizeof (int *));

    /*
     * Allocate space for integer data.
     */
    rdata[0][0] = (int *) malloc (dims[0] * adims[0] * adims[1] * sizeof (int));

    /*
     * Set the members of the pointer arrays allocated above to point
     * to the correct locations in their respective arrays.
     */
    for (i=0; i<dims[0]; i++) {
        rdata[i] = rdata[0] + i * adims[0];
        for (j=0; j<adims[0]; j++)
            rdata[i][j] = rdata[0][0] + (adims[0] * adims[1] * i) +
                        (adims[1] * j);
    }

    /*
     * Create the memory datatype.
     */
    memtype = H5Tarray_create (H5T_NATIVE_INT, 2, adims, NULL);

    /*
     * Read the data.
     */
    status = H5Dread (dset, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata[0][0]);

    /*
     * Output the data to the screen.
     */
    for (i=0; i<dims[0]; i++) {
        printf ("%s[%d]:\n", DATASET, i);
        for (j=0; j<adims[0]; j++) {
            printf (" [");
            for (k=0; k<adims[1]; k++)
                printf (" %3d", rdata[i][j][k]);
            printf ("]\n");
        }
        printf("\n");
    }

    /*
     * Close and release resources.
     */
    free (rdata[0][0]);
    free (rdata[0]);
    free (rdata);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Tclose (filetype);
    status = H5Tclose (memtype);
    status = H5Fclose (file);

    return 0;
}
