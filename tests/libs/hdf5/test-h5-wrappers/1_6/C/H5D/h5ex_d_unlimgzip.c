/************************************************************

  This example shows how to create and extend an unlimited
  dataset with gzip compression.  The program first writes
  integers to a gzip compressed dataset with dataspace
  dimensions of DIM0xDIM1, then closes the file.  Next, it
  reopens the file, reads back the data, outputs it to the
  screen, extends the dataset, and writes new data to the
  extended portions of the dataset.  Finally it reopens the
  file again, reads back the data, and outputs it to the
  screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_d_unlimgzip.h5"
#define DATASET         "DS1"
#define DIM0            4
#define DIM1            7
#define EDIM0           6
#define EDIM1           10
#define CHUNK0          4
#define CHUNK1          4

int
main (void)
{
    hid_t           file, space, dset, dcpl;    /* Handles */
    herr_t          status;
    htri_t          avail;
    H5Z_filter_t    filter_type;
    hsize_t         dims[2] = {DIM0, DIM1},
                    extdims[2] = {EDIM0, EDIM1},
                    maxdims[2],
                    chunk[2] = {CHUNK0, CHUNK1},
                    start[2],
                    count[2];
    size_t          nelmts;
    unsigned int    flags,
                    filter_info;
    int             wdata[DIM0][DIM1],          /* Write buffer */
                    wdata2[EDIM0][EDIM1],       /* Write buffer for
                                                   extension */
                    **rdata,                    /* Read buffer */
                    ndims,
                    i, j;

    /*
     * Check if gzip compression is available and can be used for both
     * compression and decompression.  Normally we do not perform error
     * checking in these examples for the sake of clarity, but in this
     * case we will make an exception because this filter is an
     * optional part of the hdf5 library.
     */
    avail = H5Zfilter_avail(H5Z_FILTER_DEFLATE);
    if (!avail) {
        printf ("gzip filter not available.\n");
        return 1;
    }
    status = H5Zget_filter_info (H5Z_FILTER_DEFLATE, &filter_info);
    if ( !(filter_info & H5Z_FILTER_CONFIG_ENCODE_ENABLED) ||
                !(filter_info & H5Z_FILTER_CONFIG_DECODE_ENABLED) ) {
        printf ("gzip filter not available for encoding and decoding.\n");
        return 1;
    }

    /*
     * Initialize data.
     */
    for (i=0; i<DIM0; i++)
        for (j=0; j<DIM1; j++)
            wdata[i][j] = i * j - j;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create dataspace with unlimited dimensions.
     */
    maxdims[0] = H5S_UNLIMITED;
    maxdims[1] = H5S_UNLIMITED;
    space = H5Screate_simple (2, dims, maxdims);

    /*
     * Create the dataset creation property list, add the gzip
     * compression filter and set the chunk size.
     */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    status = H5Pset_deflate (dcpl, 9);
    status = H5Pset_chunk (dcpl, 2, chunk);

    /*
     * Create the compressed unlimited dataset.
     */
    dset = H5Dcreate (file, DATASET, H5T_STD_I32LE, space, dcpl);

    /*
     * Write the data to the dataset.
     */
    status = H5Dwrite (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                wdata[0]);

    /*
     * Close and release resources.
     */
    status = H5Pclose (dcpl);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Fclose (file);


    /*
     * In this next section we read back the data, extend the dataset,
     * and write new data to the extended portions.
     */

    /*
     * Open file and dataset using the default properties.
     */
    file = H5Fopen (FILE, H5F_ACC_RDWR, H5P_DEFAULT);
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
    rdata = (int **) malloc (dims[0] * sizeof (int *));

    /*
     * Allocate space for integer data.
     */
    rdata[0] = (int *) malloc (dims[0] * dims[1] * sizeof (int));

    /*
     * Set the rest of the pointers to rows to the correct addresses.
     */
    for (i=1; i<dims[0]; i++)
        rdata[i] = rdata[0] + i * dims[1];

    /*
     * Read the data using the default properties.
     */
    status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf ("Dataset before extension:\n");
    for (i=0; i<dims[0]; i++) {
        printf (" [");
        for (j=0; j<dims[1]; j++)
            printf (" %3d", rdata[i][j]);
        printf ("]\n");
    }

    status = H5Sclose (space);

    /*
     * Extend the dataset.
     */
    status = H5Dextend (dset, extdims);

    /*
     * Retrieve the dataspace for the newly extended dataset.
     */
    space = H5Dget_space (dset);

    /*
     * Initialize data for writing to the extended dataset.
     */
    for (i=0; i<EDIM0; i++)
        for (j=0; j<EDIM1; j++)
            wdata2[i][j] = j;

    /*
     * Select the entire dataspace.
     */
    status = H5Sselect_all (space);

    /*
     * Subtract a hyperslab reflecting the original dimensions from the
     * selection.  The selection now contains only the newly extended
     * portions of the dataset.
     */
    start[0] = 0;
    start[1] = 0;
    count[0] = dims[0];
    count[1] = dims[1];
    status = H5Sselect_hyperslab (space, H5S_SELECT_NOTB, start, NULL, count,
                NULL);

    /*
     * Write the data to the selected portion of the dataset.
     */
    status = H5Dwrite (dset, H5T_NATIVE_INT, H5S_ALL, space, H5P_DEFAULT,
                wdata2[0]);

    /*
     * Close and release resources.
     */
    free (rdata[0]);
    free(rdata);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Fclose (file);


    /*
     * Now we simply read back the data and output it to the screen.
     */

    /*
     * Open file and dataset using the default properties.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, DATASET);

    /*
     * Retrieve dataset creation property list.
     */
    dcpl = H5Dget_create_plist (dset);

    /*
     * Retrieve and print the filter type.  Here we only retrieve the
     * first filter because we know that we only added one filter.
     */
    nelmts = 0;
    filter_type = H5Pget_filter (dcpl, 0, &flags, &nelmts, NULL, 0, NULL);
    printf ("\nFilter type is: ");
    switch (filter_type) {
        case H5Z_FILTER_DEFLATE:
            printf ("H5Z_FILTER_DEFLATE\n");
            break;
        case H5Z_FILTER_SHUFFLE:
            printf ("H5Z_FILTER_SHUFFLE\n");
            break;
        case H5Z_FILTER_FLETCHER32:
            printf ("H5Z_FILTER_FLETCHER32\n");
            break;
        case H5Z_FILTER_SZIP:
            printf ("H5Z_FILTER_SZIP\n");
    }

    /*
     * Get dataspace and allocate memory for the read buffer as before.
     */
    space = H5Dget_space (dset);
    ndims = H5Sget_simple_extent_dims (space, dims, NULL);
    rdata = (int **) malloc (dims[0] * sizeof (int *));
    rdata[0] = (int *) malloc (dims[0] * dims[1] * sizeof (int));
    for (i=1; i<dims[0]; i++)
        rdata[i] = rdata[0] + i * dims[1];

    /*
     * Read the data using the default properties.
     */
    status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata[0]);

    /*
     * Output the data to the screen.
     */
    printf ("Dataset after extension:\n");
    for (i=0; i<dims[0]; i++) {
        printf (" [");
        for (j=0; j<dims[1]; j++)
            printf (" %3d", rdata[i][j]);
        printf ("]\n");
    }

    /*
     * Close and release resources.
     */
    free (rdata[0]);
    free(rdata);
    status = H5Pclose (dcpl);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Fclose (file);

    return 0;
}
