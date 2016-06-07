/************************************************************

  This example shows how to create intermediate groups with
  a single call to H5Gcreate.

  This file is intended for use with HDF5 Library version 1.8

 ************************************************************/

#include "hdf5.h"

#define FILE            "h5ex_g_intermediate.h5"

/*
 * Operator function to be called by H5Ovisit.
 */
herr_t op_func (hid_t loc_id, const char *name, const H5O_info_t *info,
            void *operator_data);

int
main(void)
{
    hid_t       file, group, gcpl;      /* Handles */
    herr_t      status;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create group creation property list and set it to allow creation
     * of intermediate groups.
     */
    gcpl = H5Pcreate (H5P_LINK_CREATE);
    status = H5Pset_create_intermediate_group (gcpl, 1);

    /*
     * Create the group /G1/G2/G3.  Note that /G1 and /G1/G2 do not
     * exist yet.  This call would cause an error if we did not use the
     * previously created property list.
     */
    group = H5Gcreate (file, "/G1/G2/G3", gcpl, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Print all the objects in the files to show that intermediate
     * groups have been created.  See h5ex_g_visit for more information
     * on how to use H5Ovisit.
     */
    printf ("Objects in the file:\n");
    status = H5Ovisit (file, H5_INDEX_NAME, H5_ITER_NATIVE, op_func, NULL);

    /*
     * Close and release resources.
     */
    status = H5Pclose (gcpl);
    status = H5Gclose (group);
    status = H5Fclose (file);

    return 0;
}


/************************************************************

  Operator function for H5Ovisit.  This function prints the
  name and type of the object passed to it.

 ************************************************************/
herr_t op_func (hid_t loc_id, const char *name, const H5O_info_t *info,
            void *operator_data)
{
    printf ("/");               /* Print root group in object path */

    /*
     * Check if the current object is the root group, and if not print
     * the full path name and type.
     */
    if (name[0] == '.')         /* Root group, do not print '.' */
        printf ("  (Group)\n");
    else
        switch (info->type) {
            case H5O_TYPE_GROUP:
                printf ("%s  (Group)\n", name);
                break;
            case H5O_TYPE_DATASET:
                printf ("%s  (Dataset)\n", name);
                break;
            case H5O_TYPE_NAMED_DATATYPE:
                printf ("%s  (Datatype)\n", name);
                break;
            default:
                printf ("%s  (Unknown)\n", name);
        }

    return 0;
}
