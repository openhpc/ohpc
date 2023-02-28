/************************************************************

  This example shows how to create "compact-or-indexed"
  format groups, new to 1.8.  This example also illustrates
  the space savings of compact groups by creating 2 files
  which are identical except for the group format, and
  displaying the file size of each.  Both files have one
  empty group in the root group.

  This file is intended for use with HDF5 Library version 1.8

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>

#define FILE1       "h5ex_g_compact1.h5"
#define FILE2       "h5ex_g_compact2.h5"
#define GROUP       "G1"

int
main (void)
{
    hid_t       file, group, fapl;         /* Handles */
    herr_t      status;
    H5G_info_t  ginfo;
    hsize_t     size;

    /*
     * Create file 1.  This file will use original format groups.
     */
    file = H5Fcreate (FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    group = H5Gcreate (file, GROUP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Obtain the group info and print the group storage type.
     */
    status = H5Gget_info (group, &ginfo);
    printf ("Group storage type for %s is: ", FILE1);
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

    /*
     * Close and re-open file.  Needed to get the correct file size.
     */
    status = H5Gclose (group);
    status = H5Fclose (file);
    file = H5Fopen (FILE1, H5F_ACC_RDONLY, H5P_DEFAULT);

    /*
     * Obtain and print the file size.
     */
    status = H5Fget_filesize (file, &size);
    printf ("File size for %s is: %d bytes\n\n", FILE1, (int)size);

    /*
     * Close FILE1.
     */
    status = H5Fclose (file);

    /*
     * Set file access property list to allow the latest file format.
     * This will allow the library to create new compact format groups.
     */
    fapl = H5Pcreate (H5P_FILE_ACCESS);
    status = H5Pset_libver_bounds (fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);

    /*
     * Create file 2 using the new file access property list.
     */
    file = H5Fcreate (FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl );
    group = H5Gcreate (file, GROUP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Obtain the group info and print the group storage type.
     */
    status = H5Gget_info (group, &ginfo);
    printf ("Group storage type for %s is: ", FILE2);
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

    /*
     * Close and re-open file.  Needed to get the correct file size.
     */
    status = H5Gclose (group);
    status = H5Fclose (file);
    file = H5Fopen (FILE2, H5F_ACC_RDONLY, fapl);

    /*
     * Obtain and print the file size.
     */
    status = H5Fget_filesize (file, &size);
    printf ("File size for %s is: %d bytes\n", FILE2, (int)size);
    printf ("\n");

    /*
     * Close and release resources.
     */
    status = H5Pclose (fapl);
    status = H5Fclose (file);

    return 0;
}
