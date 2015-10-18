/************************************************************

  This example shows how to iterate over group members using
  H5Literate.

  This file is intended for use with HDF5 Library version 1.8

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>

#define FILE       "h5ex_g_iterate.h5"

/*
 * Operator function to be called by H5Literate.
 */
herr_t op_func (hid_t loc_id, const char *name, const H5L_info_t *info,
            void *operator_data);

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
    status = H5Literate (file, H5_INDEX_NAME, H5_ITER_NATIVE, NULL, op_func, NULL);

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
herr_t op_func (hid_t loc_id, const char *name, const H5L_info_t *info,
            void *operator_data)
{
    herr_t          status;
    H5O_info_t      infobuf;

    /*
     * Get type of the object and display its name and type.
     * The name of the object is passed to this function by
     * the Library.
     */
    status = H5Oget_info_by_name (loc_id, name, &infobuf, H5P_DEFAULT);
    switch (infobuf.type) {
        case H5O_TYPE_GROUP:
            printf ("  Group: %s\n", name);
            break;
        case H5O_TYPE_DATASET:
            printf ("  Dataset: %s\n", name);
            break;
        case H5O_TYPE_NAMED_DATATYPE:
            printf ("  Datatype: %s\n", name);
            break;
        default:
            printf ( "  Unknown: %s\n", name);
    }

    return 0;
}
