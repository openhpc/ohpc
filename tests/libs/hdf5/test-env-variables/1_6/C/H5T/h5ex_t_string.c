/************************************************************

  This example shows how to read and write string datatypes
  to a dataset.  The program first writes strings to a
  dataset with a dataspace of DIM0, then closes the file.
  Next, it reopens the file, reads back the data, and
  outputs it to the screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_t_string.h5"
#define DATASET         "DS1"
#define DIM0            4
#define SDIM            8

int
main (void)
{
    hid_t       file, filetype, memtype, space, dset;
                                            /* Handles */
    herr_t      status;
    hsize_t     dims[1] = {DIM0};
    size_t      sdim;
    char        wdata[DIM0][SDIM] = {"Parting", "is such", "sweet", "sorrow."},
                                            /* Write buffer */
                **rdata;                    /* Read buffer */
    int         ndims,
                i;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create file and memory datatypes.  For this example we will save
     * the strings as FORTRAN strings, therefore they do not need space
     * for the null terminator in the file.
     */
    filetype = H5Tcopy (H5T_FORTRAN_S1);
    status = H5Tset_size (filetype, SDIM - 1);
    memtype = H5Tcopy (H5T_C_S1);
    status = H5Tset_size (memtype, SDIM);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (1, dims, NULL);

    /*
     * Create the dataset and write the string data to it.
     */
    dset = H5Dcreate (file, DATASET, filetype, space, H5P_DEFAULT);
    status = H5Dwrite (dset, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata[0]);

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
     * the dataset and string have the same name and rank, but can have
     * any size.  Therefore we must allocate a new array to read in
     * data using malloc().
     */

    /*
     * Open file and dataset.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, DATASET);

    /*
     * Get the datatype and its size.
     */
    filetype = H5Dget_type (dset);
    sdim = H5Tget_size (filetype);
    sdim++;                         /* Make room for null terminator */

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
    rdata = (char **) malloc (dims[0] * sizeof (char *));

    /*
     * Allocate space for integer data.
     */
    rdata[0] = (char *) malloc (dims[0] * sdim * sizeof (char));

    /*
     * Set the rest of the pointers to rows to the correct addresses.
     */
    for (i=1; i<dims[0]; i++)
        rdata[i] = rdata[0] + i * sdim;

    /*
     * Create the memory datatype.
     */
    memtype = H5Tcopy (H5T_C_S1);
    status = H5Tset_size (memtype, sdim);

    /*
     * Read the data.
     */
    status = H5Dread (dset, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]);

    /*
     * Output the data to the screen.
     */
    for (i=0; i<dims[0]; i++)
        printf ("%s[%d]: %s\n", DATASET, i, rdata[i]);

    /*
     * Close and release resources.
     */
    free (rdata[0]);
    free (rdata);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Tclose (filetype);
    status = H5Tclose (memtype);
    status = H5Fclose (file);

    return 0;
}
