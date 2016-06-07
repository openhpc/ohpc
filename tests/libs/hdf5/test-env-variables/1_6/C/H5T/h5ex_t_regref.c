/************************************************************

  This example shows how to read and write region references
  to a dataset.  The program first creates a dataset
  containing characters and writes references to region of
  the dataset to a new dataset with a dataspace of DIM0,
  then closes the file.  Next, it reopens the file,
  dereferences the references, and outputs the referenced
  regions to the screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_t_regref.h5"
#define DATASET         "DS1"
#define DATASET2        "DS2"
#define DIM0            2
#define DS2DIM0         3
#define DS2DIM1         16

int
main (void)
{
    hid_t               file, space, memspace, dset, dset2;
                                                    /* Handles */
    herr_t              status;
    hsize_t             dims[1] = {DIM0},
                        dims2[2] = {DS2DIM0, DS2DIM1},
                        coords[4][2] = { {0,  1},
                                         {2, 11},
                                         {1,  0},
                                         {2,  4} },
                        start[2] = {0, 0},
                        stride[2] = {2, 11},
                        count[2] = {2, 2},
                        block[2] = {1, 3};
    hssize_t            npoints;
    hdset_reg_ref_t     wdata[DIM0],                /* Write buffer */
                        *rdata;                     /* Read buffer */
    ssize_t             size;
    char                wdata2[DS2DIM0][DS2DIM1] = {"The quick brown",
                                                    "fox jumps over ",
                                                    "the 5 lazy dogs"},
                        *rdata2,
                        *name;
    int                 ndims,
                        i;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create a dataset with character data.
     */
    space = H5Screate_simple (2, dims2, NULL);
    dset2 = H5Dcreate (file, DATASET2, H5T_STD_I8LE, space, H5P_DEFAULT);
    status = H5Dwrite (dset2, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                wdata2);

    /*
     * Create reference to a list of elements in dset2.
     */
    status = H5Sselect_elements (space, H5S_SELECT_SET, 4, coords[0]);
    status = H5Rcreate (&wdata[0], file, DATASET2, H5R_DATASET_REGION, space);

    /*
     * Create reference to a hyperslab in dset2, close dataspace.
     */
    status = H5Sselect_hyperslab (space, H5S_SELECT_SET, start, stride, count,
                block);
    status = H5Rcreate (&wdata[1], file, DATASET2, H5R_DATASET_REGION, space);
    status = H5Sclose (space);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (1, dims, NULL);

    /*
     * Create the dataset and write the region references to it.
     */
    dset = H5Dcreate (file, DATASET, H5T_STD_REF_DSETREG, space, H5P_DEFAULT);
    status = H5Dwrite (dset, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                wdata);

    /*
     * Close and release resources.
     */
    status = H5Dclose (dset);
    status = H5Dclose (dset2);
    status = H5Sclose (space);
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
     * Get dataspace and allocate memory for read buffer.
     */
    space = H5Dget_space (dset);
    ndims = H5Sget_simple_extent_dims (space, dims, NULL);
    rdata = (hdset_reg_ref_t *) malloc (dims[0] * sizeof (hdset_reg_ref_t));
    status = H5Sclose (space);

    /*
     * Read the data.
     */
    status = H5Dread (dset, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata);

    /*
     * Output the data to the screen.
     */
    for (i=0; i<dims[0]; i++) {
        printf ("%s[%d]:\n  ->", DATASET, i);

        /*
         * Open the referenced object, retrieve its region as a
         * dataspace selection.
         */
        dset2 = H5Rdereference (dset, H5R_DATASET_REGION, &rdata[i]);
        space = H5Rget_region (dset, H5R_DATASET_REGION, &rdata[i]);

        /*
         * Get the length of the object's name, allocate space, then
         * retrieve the name.
         */
        size = 1 + H5Iget_name (dset2, NULL, 0);
        name = (char *) malloc (size);
        size = 1 + H5Iget_name (dset2, name, size);
        if (size <= 1)
            name[0]='\0';

        /*
         * Allocate space for the read buffer.  We will only allocate
         * enough space for the selection, plus a null terminator.  The
         * read buffer will be 1-dimensional.
         */
        npoints = H5Sget_select_npoints (space);
        rdata2 = (char *) malloc (npoints + 1);

        /*
         * Read the dataset region, and add a null terminator so we can
         * print it as a string.
         */
        memspace = H5Screate_simple (1, (hsize_t *) &npoints, NULL);
        status = H5Dread (dset2, H5T_NATIVE_CHAR, memspace, space, H5P_DEFAULT,
                    rdata2);
        rdata2[npoints] = '\0';

        /*
         * Print the name and region data, close and release resources.
         */
        printf (" %s: %s\n", name, rdata2);
        free (rdata2);
        free (name);
        status = H5Sclose (space);
        status = H5Sclose (memspace);
        status = H5Dclose (dset2);
    }

    /*
     * Close and release resources.
     */
    free (rdata);
    status = H5Dclose (dset);
    status = H5Fclose (file);

    return 0;
}
