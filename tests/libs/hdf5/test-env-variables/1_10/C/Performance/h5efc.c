/************************************************************

  This example shows how to use the external file cache.

  This file is intended for use with HDF5 Library version
  1.8.7 or newer

 ************************************************************/

#include "hdf5.h"

#define FILE            "h5efc.h5"
#define EXT_FILE1       "h5efc1.h5"
#define EXT_FILE2       "h5efc2.h5"
#define EXT_FILE3       "h5efc3.h5"

int
main(void)
{
    hid_t       file1, file2, group, fapl;      /* Handles */
    herr_t      status;

    /*
     * Create file access property list and set it to allow caching of open
     * files visited through external links.
     */
    fapl = H5Pcreate (H5P_FILE_ACCESS);
    status = H5Pset_elink_file_cache_size (fapl, 8);

    /*
     * Create a new file using the file access property list.
     */
    file1 = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    /*
     * Create files to serve as targets for external links.
     */
    file2 = H5Fcreate (EXT_FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Fclose (file2);
    file2 = H5Fcreate (EXT_FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Fclose (file2);
    file2 = H5Fcreate (EXT_FILE3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Fclose (file2);

    /*
     * Create external links to the target files.
     */
    status = H5Lcreate_external (EXT_FILE1, "/", file1, "link_to_1",
            H5P_DEFAULT, H5P_DEFAULT);
    status = H5Lcreate_external (EXT_FILE2, "/", file1, "link_to_2",
            H5P_DEFAULT, H5P_DEFAULT);
    status = H5Lcreate_external (EXT_FILE3, "/", file1, "link_to_3",
            H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Open and close the targets of all three external links (these will be the
     * root groups of the target files).  The target files should be held open
     * by the root file's external file cache after traversal.
     */
    group = H5Gopen (file1, "/link_to_1", H5P_DEFAULT);
    status = H5Gclose(group);
    group = H5Gopen (file1, "/link_to_2", H5P_DEFAULT);
    status = H5Gclose(group);
    group = H5Gopen (file1, "/link_to_3", H5P_DEFAULT);
    status = H5Gclose(group);

    /*
     * Open and close the targets of all three external links again.  The target
     * files should already be held open by the root file's external file cache,
     * so the library will not actually have to issue an "open" system call.
     */
    group = H5Gopen (file1, "/link_to_1", H5P_DEFAULT);
    status = H5Gclose(group);
    group = H5Gopen (file1, "/link_to_2", H5P_DEFAULT);
    status = H5Gclose(group);
    group = H5Gopen (file1, "/link_to_3", H5P_DEFAULT);
    status = H5Gclose(group);

    /*
     * Release the root file's external file cache.  This will close all the
     * external link target files.
     */
    status = H5Frelease_file_cache(file1);

    /*
     * Close and release resources.
     */
    status = H5Pclose (fapl);
    status = H5Fclose (file1);

    return 0;
}
