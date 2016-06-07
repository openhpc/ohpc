/************************************************************

  This example shows how to read and write opaque datatypes
  to a dataset.  The program first writes opaque data to a
  dataset with a dataspace of DIM0, then closes the file.
  Next, it reopens the file, reads back the data, and
  outputs it to the screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_t_opaque.h5"
#define DATASET         "DS1"
#define DIM0            4
#define LEN             7

int
main (void)
{
    hid_t       file, space, dtype, dset;   /* Handles */
    herr_t      status;
    hsize_t     dims[1] = {DIM0};
    size_t      len;
    char        wdata[DIM0*LEN],            /* Write buffer */
                *rdata,                     /* Read buffer */
                str[LEN] = "OPAQUE",
                *tag;
    int         ndims,
                i, j;

    /*
     * Initialize data.
     */
    for (i=0; i<DIM0; i++) {
        for (j=0; j<LEN-1; j++)
            wdata[j + i * LEN] = str[j];
        wdata[LEN - 1 + i * LEN] = (char) i + '0';
    }

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create opaque datatype and set the tag to something appropriate.
     * For this example we will write and view the data as a character
     * array.
     */
    dtype = H5Tcreate (H5T_OPAQUE, LEN);
    status = H5Tset_tag (dtype, "Character array");

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (1, dims, NULL);

    /*
     * Create the dataset and write the opaque data to it.
     */
    dset = H5Dcreate (file, DATASET, dtype, space, H5P_DEFAULT);
    status = H5Dwrite (dset, dtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);

    /*
     * Close and release resources.
     */
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Tclose (dtype);
    status = H5Fclose (file);


    /*
     * Now we begin the read section of this example.  Here we assume
     * the dataset has the same name and rank, but can have any size.
     * Therefore we must allocate a new array to read in data using
     * malloc().
     */

    /*
     * Open file and dataset.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, DATASET);

    /*
     * Get datatype and properties for the datatype.  Note that H5Tget_tag
     * allocates space for the string in tag, so we must remember to free() it
     * later.
     */
    dtype = H5Dget_type (dset);
    len = H5Tget_size (dtype);
    tag = H5Tget_tag (dtype);

    /*
     * Get dataspace and allocate memory for read buffer.
     */
    space = H5Dget_space (dset);
    ndims = H5Sget_simple_extent_dims (space, dims, NULL);
    rdata = (char *) malloc (dims[0] * len);

    /*
     * Read the data.
     */
    status = H5Dread (dset, dtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);

    /*
     * Output the data to the screen.
     */
    printf ("Datatype tag for %s is: \"%s\"\n", DATASET, tag);
    for (i=0; i<dims[0]; i++) {
        printf ("%s[%u]: ", DATASET, i);
        for (j=0; j<len; j++)
            printf ("%c", rdata[j + i * len]);
        printf ("\n");
    }

    /*
     * Close and release resources.
     */
    free (rdata);
    free (tag);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Tclose (dtype);
    status = H5Fclose (file);

    return 0;
}
