/************************************************************

  This example shows how to set the conditions for
  conversion between compact and dense (indexed) groups.

  This file is intended for use with HDF5 Library version 1.8

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>

#define FILE        "h5ex_g_phase.h5"
#define MAX_GROUPS  7
#define MAX_COMPACT 5
#define MIN_DENSE   3

int
main (void)
{
    hid_t       file, group, subgroup, fapl, gcpl;      /* Handles */
    herr_t      status;
    H5G_info_t  ginfo;
    char        name[3]="G0";                  /* Name of subgroup */
    unsigned    i;

    /*
     * Set file access property list to allow the latest file format.
     * This will allow the library to create new format groups.
     */
    fapl = H5Pcreate (H5P_FILE_ACCESS);
    status = H5Pset_libver_bounds (fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);

    /*
     * Create group access property list and set the phase change
     * conditions.  In this example we lowered the conversion threshold
     * to simplify the output, though this may not be optimal.
     */
    gcpl = H5Pcreate (H5P_GROUP_CREATE);
    status = H5Pset_link_phase_change (gcpl, MAX_COMPACT, MIN_DENSE);

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    /*
     * Create primary group.
     */
    group = H5Gcreate (file, name, H5P_DEFAULT, gcpl, H5P_DEFAULT);

    /*
     * Add subgroups to "group" one at a time, print the storage type
     * for "group" after each subgroup is created.
     */
    for (i=1; i<=MAX_GROUPS; i++) {

        /*
         * Define the subgroup name and create the subgroup.
         */
        name[1] = ((char) i) + '0';     /* G1, G2, G3 etc. */
        subgroup = H5Gcreate (group, name, H5P_DEFAULT, H5P_DEFAULT,
                    H5P_DEFAULT);
        status = H5Gclose (subgroup);

        /*
         * Obtain the group info and print the group storage type
         */
        status = H5Gget_info (group, &ginfo);
        printf ("%d Group%s: Storage type is ", (int) ginfo.nlinks,
                    ginfo.nlinks == 1 ? " " : "s");
        switch (ginfo.storage_type) {
            case H5G_STORAGE_TYPE_COMPACT:
                printf ("H5G_STORAGE_TYPE_COMPACT\n"); /* New compact format */
                break;
            case H5G_STORAGE_TYPE_DENSE:
                printf ("H5G_STORAGE_TYPE_DENSE\n"); /* New dense (indexed) format */
                break;
            case H5G_STORAGE_TYPE_SYMBOL_TABLE:
                printf ("H5G_STORAGE_TYPE_SYMBOL_TABLE\n"); /* Original format */
                break;
            case H5G_STORAGE_TYPE_UNKNOWN:
                printf ("H5G_STORAGE_TYPE_UNKNOWN\n"); /* Unknown format */
        }
    }

    printf("\n");

    /*
     * Delete subgroups one at a time, print the storage type for
     * "group" after each subgroup is deleted.
     */
    for (i=MAX_GROUPS; i>=1; i--) {

        /*
         * Define the subgroup name and delete the subgroup.
         */
        name[1] = ((char) i) + '0';     /* G1, G2, G3 etc. */
        status = H5Ldelete (group, name, H5P_DEFAULT);

        /*
         * Obtain the group info and print the group storage type
         */
        status = H5Gget_info (group, &ginfo);
        printf ("%d Group%s: Storage type is ", (int) ginfo.nlinks,
                    ginfo.nlinks == 1 ? " " : "s");
        switch (ginfo.storage_type) {
            case H5G_STORAGE_TYPE_COMPACT:
                printf ("H5G_STORAGE_TYPE_COMPACT\n"); /* New compact format */
                break;
            case H5G_STORAGE_TYPE_DENSE:
                printf ("H5G_STORAGE_TYPE_DENSE\n"); /* New dense (indexed) format */
                break;
            case H5G_STORAGE_TYPE_SYMBOL_TABLE:
                printf ("H5G_STORAGE_TYPE_SYMBOL_TABLE\n"); /* Original format */
                break;
            case H5G_STORAGE_TYPE_UNKNOWN:
                printf ("H5G_STORAGE_TYPE_UNKNOWN\n"); /* Unknown format */
        }
    }

    /*
     * Close and release resources.
     */
    status = H5Pclose (fapl);
    status = H5Pclose (gcpl);
    status = H5Gclose (group);
    status = H5Fclose (file);

    return 0;
}
