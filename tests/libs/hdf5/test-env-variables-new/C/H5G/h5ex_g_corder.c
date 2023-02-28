/************************************************************

  This example shows how to track links in a group by
  creation order.  The program creates a series of groups,
  then reads back their names: first in alphabetical order,
  then in creation order.

  This file is intended for use with HDF5 Library version 1.8

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE       "h5ex_g_corder.h5"

int
main (void)
{
    hid_t       file, group, subgroup, gcpl;        /* Handles */
    herr_t      status;
    H5G_info_t  ginfo;
    ssize_t     size;                               /* Size of name */
    hsize_t     i;                                  /* Index */
    char        *name;                              /* Output buffer */

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create group creation property list and enable link creation
     * order tracking.  Attempting to track by creation order in a
     * group that does not have this property set will result in an
     * error.
     */
    gcpl = H5Pcreate (H5P_GROUP_CREATE);
    status = H5Pset_link_creation_order( gcpl, H5P_CRT_ORDER_TRACKED |
                H5P_CRT_ORDER_INDEXED );

    /*
     * Create primary group using the property list.
     */
    group = H5Gcreate (file, "index_group", H5P_DEFAULT, gcpl, H5P_DEFAULT);

    /*
     * Create subgroups in the primary group.  These will be tracked
     * by creation order.  Note that these groups do not have to have
     * the creation order tracking property set.
     */
    subgroup = H5Gcreate (group, "H", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Gclose (subgroup);
    subgroup = H5Gcreate (group, "D", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Gclose (subgroup);
    subgroup = H5Gcreate (group, "F", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Gclose (subgroup);
    subgroup = H5Gcreate (group, "5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Gclose (subgroup);

    /*
     * Get group info.
     */
    status = H5Gget_info (group, &ginfo);

    /*
     * Traverse links in the primary group using alphabetical indices
     * (H5_INDEX_NAME).
     */
    printf("Traversing group using alphabetical indices:\n\n");
    for (i=0; i<ginfo.nlinks; i++) {

        /*
         * Get size of name, add 1 for null terminator.
         */
        size = 1 + H5Lget_name_by_idx (group, ".", H5_INDEX_NAME, H5_ITER_INC,
                    i, NULL, 0, H5P_DEFAULT);

        /*
         * Allocate storage for name.
         */
        name = (char *) malloc (size);

        /*
         * Retrieve name, print it, and free the previously allocated
         * space.
         */
        size = H5Lget_name_by_idx (group, ".", H5_INDEX_NAME, H5_ITER_INC, i, name,
                    (size_t) size, H5P_DEFAULT);
        printf ("Index %d: %s\n", (int) i, name);
        free (name);
    }

    /*
     * Traverse links in the primary group by creation order
     * (H5_INDEX_CRT_ORDER).
     */
    printf("\nTraversing group using creation order indices:\n\n");
    for (i=0; i<ginfo.nlinks; i++) {

        /*
         * Get size of name, add 1 for null terminator.
         */
        size = 1 + H5Lget_name_by_idx (group, ".", H5_INDEX_CRT_ORDER,
                    H5_ITER_INC, i, NULL, 0, H5P_DEFAULT);

        /*
         * Allocate storage for name.
         */
        name = (char *) malloc (size);

        /*
         * Retrieve name, print it, and free the previously allocated
         * space.
         */
        size = H5Lget_name_by_idx (group, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, i,
                    name, (size_t) size, H5P_DEFAULT);
        printf ("Index %d: %s\n", (int) i, name);
        free (name);
    }

    /*
     * Close and release resources.
     */
    status = H5Pclose (gcpl);
    status = H5Gclose (group);
    status = H5Fclose (file);

    return 0;
}
