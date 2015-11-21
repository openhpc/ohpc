/************************************************************

  This example shows how to read and write variable-length
  string datatypes to a dataset.  The program first writes
  variable-length strings to a dataset with a dataspace of
  DIM0, then closes the file.  Next, it reopens the file,
  reads back the data, and outputs it to the screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_t_vlstring.h5"
#define DATASET         "DS1"
#define DIM0            4

int
main (void)
{
    hid_t       file, filetype, memtype, space, dset;
                                            /* Handles */
    herr_t      status;
    hsize_t     dims[1] = {DIM0};
    char        *wdata[DIM0] = {"Parting", "is such", "sweet", "sorrow."},
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
     * the strings as FORTRAN strings.
     */
    filetype = H5Tcopy (H5T_FORTRAN_S1);
    status = H5Tset_size (filetype, H5T_VARIABLE);
    memtype = H5Tcopy (H5T_C_S1);
    status = H5Tset_size (memtype, H5T_VARIABLE);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (1, dims, NULL);

    /*
     * Create the dataset and write the variable-length string data to
     * it.
     */
    dset = H5Dcreate (file, DATASET, filetype, space, H5P_DEFAULT);
    status = H5Dwrite (dset, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);

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
     * Get the datatype.
     */
    filetype = H5Dget_type (dset);

    /*
     * Get dataspace and allocate memory for read buffer.
     */
    space = H5Dget_space (dset);
    ndims = H5Sget_simple_extent_dims (space, dims, NULL);
    rdata = (char **) malloc (dims[0] * sizeof (char *));

    /*
     * Create the memory datatype.
     */
    memtype = H5Tcopy (H5T_C_S1);
    status = H5Tset_size (memtype, H5T_VARIABLE);

    /*
     * Read the data.
     */
    status = H5Dread (dset, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);

    /*
     * Output the data to the screen.
     */
    for (i=0; i<dims[0]; i++)
        printf ("%s[%d]: %s\n", DATASET, i, rdata[i]);

    /*
     * Close and release resources.  Note that H5Dvlen_reclaim works
     * for variable-length strings as well as variable-length arrays.
     * Also note that we must still free the array of pointers stored
     * in rdata, as H5Tvlen_reclaim only frees the data these point to.
     */
    status = H5Dvlen_reclaim (memtype, space, H5P_DEFAULT, rdata);
    free (rdata);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Tclose (filetype);
    status = H5Tclose (memtype);
    status = H5Fclose (file);

    return 0;
}
