/************************************************************

  This example shows how to read and write enumerated
  datatypes to a dataset.  The program first writes
  enumerated values to a dataset with a dataspace of
  DIM0xDIM1, then closes the file.  Next, it reopens the
  file, reads back the data, and outputs it to the screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_t_enum.h5"
#define DATASET         "DS1"
#define DIM0            4
#define DIM1            7
#define F_BASET         H5T_STD_I16BE       /* File base type */
#define M_BASET         H5T_NATIVE_INT      /* Memory base type */
#define NAME_BUF_SIZE   16

typedef enum {
    SOLID,
    LIQUID,
    GAS,
    PLASMA
} phase_t;                                  /* Enumerated type */

int
main (void)
{
    hid_t       file, filetype, memtype, space, dset;
                                            /* Handles */
    herr_t      status;
    hsize_t     dims[2] = {DIM0, DIM1};
    phase_t     wdata[DIM0][DIM1],          /* Write buffer */
                **rdata,                    /* Read buffer */
                val;
    char        *names[4] = {"SOLID", "LIQUID", "GAS", "PLASMA"},
                name[NAME_BUF_SIZE];
    int         ndims,
                i, j;

    /*
     * Initialize data.
     */
    for (i=0; i<DIM0; i++)
        for (j=0; j<DIM1; j++)
            wdata[i][j] = (phase_t) ( (i + 1) * j - j) % (int) (PLASMA + 1);

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create the enumerated datatypes for file and memory.  This
     * process is simplified if native types are used for the file,
     * as only one type must be defined.
     */
    filetype = H5Tenum_create (F_BASET);
    memtype = H5Tenum_create (M_BASET);

    for (i = (int) SOLID; i <= (int) PLASMA; i++) {
        /*
         * Insert enumerated value for memtype.
         */
        val = (phase_t) i;
        status = H5Tenum_insert (memtype, names[i], &val);
        /*
         * Insert enumerated value for filetype.  We must first convert
         * the numerical value val to the base type of the destination.
         */
        status = H5Tconvert (M_BASET, F_BASET, 1, &val, NULL, H5P_DEFAULT);
        status = H5Tenum_insert (filetype, names[i], &val);
    }

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (2, dims, NULL);

    /*
     * Create the dataset and write the enumerated data to it.
     */
    dset = H5Dcreate (file, DATASET, filetype, space, H5P_DEFAULT);
    status = H5Dwrite (dset, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata[0]);

    /*
     * Close and release resources.
     */
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Tclose (filetype);
    status = H5Fclose (file);


    /*
     * Now we begin the read section of this example.  Here we assume
     * the dataset has the same name and rank, but can have any size.
     * Therefore we must allocate a new array to read in data using
     * malloc().  For simplicity, we do not rebuild memtype.
     */

    /*
     * Open file and dataset.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, DATASET);

    /*
     * Get dataspace and allocate memory for read buffer.  This is a
     * two dimensional dataset so the dynamic allocation must be done
     * in steps.
     */
    space = H5Dget_space (dset);
    ndims = H5Sget_simple_extent_dims (space, dims, NULL);

    /*
     * Allocate array of pointers to rows.
     */
    rdata = (phase_t **) malloc (dims[0] * sizeof (phase_t *));

    /*
     * Allocate space for enumerated data.
     */
    rdata[0] = (phase_t *) malloc (dims[0] * dims[1] * sizeof (phase_t));

    /*
     * Set the rest of the pointers to rows to the correct addresses.
     */
    for (i=1; i<dims[0]; i++)
        rdata[i] = rdata[0] + i * dims[1];

    /*
     * Read the data.
     */
    status = H5Dread (dset, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf ("%s:\n", DATASET);
    for (i=0; i<dims[0]; i++) {
        printf (" [");
        for (j=0; j<dims[1]; j++) {

            /*
             * Get the name of the enumeration member.
             */
            status = H5Tenum_nameof (memtype, &rdata[i][j], name,
                        NAME_BUF_SIZE);
            printf (" %-6s", name);
        }
        printf ("]\n");
    }

    /*
     * Close and release resources.
     */
    free (rdata[0]);
    free (rdata);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Tclose (memtype);
    status = H5Fclose (file);

    return 0;
}
