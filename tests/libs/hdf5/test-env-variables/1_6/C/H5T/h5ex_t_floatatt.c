/************************************************************

  This example shows how to read and write floating point
  datatypes to an attribute.  The program first writes
  floating point numbers to an attribute with a dataspace of
  DIM0xDIM1, then closes the file.  Next, it reopens the
  file, reads back the data, and outputs it to the screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_t_floatatt.h5"
#define DATASET         "DS1"
#define ATTRIBUTE       "A1"
#define DIM0            4
#define DIM1            7

int
main (void)
{
    hid_t       file, space, dset, attr;            /* Handles */
    herr_t      status;
    hsize_t     dims[2] = {DIM0, DIM1};
    double      wdata[DIM0][DIM1],                  /* Write buffer */
                **rdata;                            /* Read buffer */
    int         ndims,
                i, j;

    /*
     * Initialize data.
     */
    for (i=0; i<DIM0; i++)
        for (j=0; j<DIM1; j++)
            wdata[i][j] = (double) i / (j + 0.5) + j;;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create dataset with a scalar dataspace.
     */
    space = H5Screate (H5S_SCALAR);
    dset = H5Dcreate (file, DATASET, H5T_STD_I32LE, space, H5P_DEFAULT);
    status = H5Sclose (space);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (2, dims, NULL);

    /*
     * Create the attribute and write the floating point data to it.
     * In this example we will save the data as 64 bit little endian
     * IEEE floating point numbers, regardless of the native type.  The
     * HDF5 library automatically converts between different floating
     * point types.
     */
    attr = H5Acreate (dset, ATTRIBUTE, H5T_IEEE_F64LE, space, H5P_DEFAULT);
    status = H5Awrite (attr, H5T_NATIVE_DOUBLE, wdata[0]);

    /*
     * Close and release resources.
     */
    status = H5Aclose (attr);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Fclose (file);


    /*
     * Now we begin the read section of this example.  Here we assume
     * the attribute has the same name and rank, but can have any size.
     * Therefore we must allocate a new array to read in data using
     * malloc().
     */

    /*
     * Open file, dataset, and attribute.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, DATASET);
    attr = H5Aopen_name (dset, ATTRIBUTE);

    /*
     * Get dataspace and allocate memory for read buffer.  This is a
     * two dimensional attribute so the dynamic allocation must be done
     * in steps.
     */
    space = H5Aget_space (attr);
    ndims = H5Sget_simple_extent_dims (space, dims, NULL);

    /*
     * Allocate array of pointers to rows.
     */
    rdata = (double **) malloc (dims[0] * sizeof (double *));

    /*
     * Allocate space for floating point data.
     */
    rdata[0] = (double *) malloc (dims[0] * dims[1] * sizeof (double));

    /*
     * Set the rest of the pointers to rows to the correct addresses.
     */
    for (i=1; i<dims[0]; i++)
        rdata[i] = rdata[0] + i * dims[1];

    /*
     * Read the data.
     */
    status = H5Aread (attr, H5T_NATIVE_DOUBLE, rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf ("%s:\n", ATTRIBUTE);
    for (i=0; i<dims[0]; i++) {
        printf (" [");
        for (j=0; j<dims[1]; j++)
            printf (" %6.4f", rdata[i][j]);
        printf ("]\n");
    }

    /*
     * Close and release resources.
     */
    free (rdata[0]);
    free (rdata);
    status = H5Aclose (attr);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Fclose (file);

    return 0;
}
