/************************************************************

  This example shows how to iterate over group members using
  H5Giterate.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>

#define FILE       "h5ex_g_iterate.h5"

/*
 * Operator function to be called by H5Giterate.
 */
herr_t op_func (hid_t loc_id, const char *name, void *operator_data);

int
main (void)
{
    hid_t           file;           /* Handle */
    herr_t          status;

    /*
     * Open file.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);

    /*
     * Begin iteration.
     */
    printf ("Objects in root group:\n");
    status = H5Giterate (file, "/", NULL, op_func, NULL);

    /*
     * Close and release resources.
     */
    status = H5Fclose (file);

    return 0;
}


/************************************************************

  Operator function.  Prints the name and type of the object
  being examined.

 ************************************************************/
herr_t op_func (hid_t loc_id, const char *name, void *operator_data)
{
    herr_t          status;
    H5G_stat_t      statbuf;

    /*
     * Get type of the object and display its name and type.
     * The name of the object is passed to this function by
     * the Library.
     */
    status = H5Gget_objinfo (loc_id, name, 0, &statbuf);
    switch (statbuf.type) {
        case H5G_GROUP:
            printf ("  Group: %s\n", name);
            break;
        case H5G_DATASET:
            printf ("  Dataset: %s\n", name);
            break;
        case H5G_TYPE:
            printf ("  Datatype: %s\n", name);
            break;
        default:
            printf ( "  Unknown: %s\n", name);
    }

    return 0;
}
