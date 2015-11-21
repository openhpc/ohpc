/************************************************************

  This example shows how to commit a named datatype to a
  file, and read back that datatype.  The program first
  defines a compound datatype, commits it to a file, then
  closes the file.  Next, it reopens the file, opens the
  datatype, and outputs the names of its fields to the
  screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_t_commit.h5"
#define DATATYPE        "Sensor_Type"

int
main (void)
{
    hid_t           file, filetype, strtype;
                                            /* Handles */
    herr_t          status;
    H5T_class_t     typeclass;
    char            *name;
    int             nmembs,
                    i;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create variable-length string datatype.
     */
    strtype = H5Tcopy (H5T_C_S1);
    status = H5Tset_size (strtype, H5T_VARIABLE);

    /*
     * Create the compound datatype.  Because the standard types we are
     * using may have different sizes than the corresponding native
     * types, we must manually calculate the offset of each member.
     */
    filetype = H5Tcreate (H5T_COMPOUND, 8 + sizeof (char *) + 8 + 8);
    status = H5Tinsert (filetype, "Serial number", 0, H5T_STD_I64BE);
    status = H5Tinsert (filetype, "Location", 8, strtype);
    status = H5Tinsert (filetype, "Temperature (F)", 8 + sizeof (char *),
                H5T_IEEE_F64BE);
    status = H5Tinsert (filetype, "Pressure (inHg)", 8 + sizeof (char *) + 8,
                H5T_IEEE_F64BE);

    /*
     * Commit the compound datatype to the file, creating a named
     * datatype.
     */
    status = H5Tcommit (file, DATATYPE, filetype);

    /*
     * Close and release resources.
     */
    status = H5Tclose (filetype);
    status = H5Tclose (strtype);
    status = H5Fclose (file);


    /*
     * Now we begin the read section of this example.
     */

    /*
     * Open file.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);

    /*
     * Open the named datatype.
     */
    filetype = H5Topen (file, DATATYPE);

    /*
     * Output the data to the screen.
     */
    printf ("Named datatype: %s:\n", DATATYPE);
    /*
     * Get datatype class.  If it isn't compound, we won't print
     * anything.
     */
    typeclass = H5Tget_class (filetype);
    if (typeclass == H5T_COMPOUND) {
        printf ("   Class: H5T_COMPOUND\n");
        nmembs = H5Tget_nmembers (filetype);
        /*
         * Iterate over compound datatype members.
         */
        for (i=0; i<nmembs; i++) {
            /*
             * Get the member name and print it.  Note that
             * H5Tget_member_name allocates space for the string in
             * name, so we must free() it after use.
             */
            name = H5Tget_member_name (filetype, i);
            printf ("   %s\n", name);
            free (name);
        }
    }

    /*
     * Close and release resources.
     */
    status = H5Tclose (filetype);
    status = H5Fclose (file);

    return 0;
}
