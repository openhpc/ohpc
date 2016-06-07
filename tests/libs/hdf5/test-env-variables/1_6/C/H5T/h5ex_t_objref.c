/************************************************************

  This example shows how to read and write object references
  to a dataset.  The program first creates objects in the
  file and writes references to those objects to a dataset
  with a dataspace of DIM0, then closes the file.  Next, it
  reopens the file, dereferences the references, and outputs
  the names of their targets to the screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_t_objref.h5"
#define DATASET         "DS1"
#define DIM0            2

int
main (void)
{
    hid_t       file, space, dset, obj;     /* Handles */
    herr_t      status;
    hsize_t     dims[1] = {DIM0};
    hobj_ref_t  wdata[DIM0],                /* Write buffer */
                *rdata;                     /* Read buffer */
    H5G_obj_t   objtype;
    ssize_t     size;
    char        *name;
    int         ndims,
                i;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create a dataset with a scalar dataspace.
     */
    space = H5Screate (H5S_SCALAR);
    obj = H5Dcreate (file, "DS2", H5T_STD_I32LE, space, H5P_DEFAULT);
    status = H5Dclose (obj);
    status = H5Sclose (space);

    /*
     * Create a group.
     */
    obj = H5Gcreate (file, "G1", H5P_DEFAULT);
    status = H5Gclose (obj);

    /*
     * Create references to the previously created objects.  Passing -1
     * as space_id causes this parameter to be ignored.  Other values
     * besides valid dataspaces result in an error.
     */
    status = H5Rcreate (&wdata[0], file, "G1", H5R_OBJECT, -1);
    status = H5Rcreate (&wdata[1], file, "DS2", H5R_OBJECT, -1);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (1, dims, NULL);

    /*
     * Create the dataset and write the object references to it.
     */
    dset = H5Dcreate (file, DATASET, H5T_STD_REF_OBJ, space, H5P_DEFAULT);
    status = H5Dwrite (dset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                wdata);

    /*
     * Close and release resources.
     */
    status = H5Dclose (dset);
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
    rdata = (hobj_ref_t *) malloc (dims[0] * sizeof (hobj_ref_t));

    /*
     * Read the data.
     */
    status = H5Dread (dset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata);

    /*
     * Output the data to the screen.
     */
    for (i=0; i<dims[0]; i++) {
        printf ("%s[%d]:\n  ->", DATASET, i);
        /*
         * Open the referenced object, get its name and type.
         */
        obj = H5Rdereference (dset, H5R_OBJECT, &rdata[i]);
        objtype = H5Rget_obj_type (dset, H5R_OBJECT, &rdata[i]);

        /*
         * Get the length of the name, allocate space, then retrieve
         * the name.
         */
        size = 1 + H5Iget_name (obj, NULL, 0);
        if (size > 1) {
            name = (char *) malloc (size);
            size = 1 + H5Iget_name (obj, name, size);
        }

        /*
         * Print the object type and close the object.
         */
        switch (objtype) {
            case H5G_GROUP:
                printf ("Group");
                status = H5Gclose (obj);
                break;
            case H5G_DATASET:
                printf ("Dataset");
                status = H5Dclose (obj);
                break;
            case H5G_TYPE:
                printf ("Named Datatype");
                status = H5Tclose (obj);
        }

        /*
         * Print the name and deallocate space for the name.
         */
        if (size > 1) {
            printf (": %s", name);
            free (name);
        }
        printf ("\n");
    }

    /*
     * Close and release resources.
     */
    free (rdata);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Fclose (file);

    return 0;
}
