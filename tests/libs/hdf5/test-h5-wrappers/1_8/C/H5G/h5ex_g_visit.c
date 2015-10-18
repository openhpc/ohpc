/************************************************************

  This example shows how to recursively traverse a file
  using H5Ovisit and H5Lvisit.  The program prints all of
  the objects in the file specified in FILE, then prints all
  of the links in that file.  

  This file is intended for use with HDF5 Library version 1.8

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>

#define FILE       "h5ex_g_visit.h5"

/*
 * Operator function to be called by H5Ovisit.
 */
herr_t op_func (hid_t loc_id, const char *name, const H5O_info_t *info,
            void *operator_data);

/*
 * Operator function to be called by H5Lvisit.
 */
herr_t op_func_L (hid_t loc_id, const char *name, const H5L_info_t *info,
            void *operator_data);

int
main (void)
{
    hid_t           file;           /* Handle */
    herr_t          status;

    /*
     * Open file
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);

    /*
     * Begin iteration using H5Ovisit
     */
    printf ("Objects in the file:\n");
    status = H5Ovisit (file, H5_INDEX_NAME, H5_ITER_NATIVE, op_func, NULL);

    /*
     * Repeat the same process using H5Lvisit
     */
    printf ("\nLinks in the file:\n");
    status = H5Lvisit (file, H5_INDEX_NAME, H5_ITER_NATIVE, op_func_L, NULL);

    /*
     * Close and release resources.
     */
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


/************************************************************

  Operator function for H5Lvisit.  This function simply
  retrieves the info for the object the current link points
  to, and calls the operator function for H5Ovisit.

 ************************************************************/
herr_t op_func_L (hid_t loc_id, const char *name, const H5L_info_t *info,
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
    return op_func (loc_id, name, &infobuf, operator_data);
}
