/************************************************************

  This example shows how to read and write data to a dataset
  using szip compression.    The program first checks if
  szip compression is available, then if it is it writes
  integers to a dataset using szip, then closes the file.
  Next, it reopens the file, reads back the data, and
  outputs the type of compression and the maximum value in
  the dataset to the screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/


#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_d_szip.h5"
#define DATASET         "DS1"
#define DIM0            32
#define DIM1            64
#define CHUNK0          4
#define CHUNK1          8

int
main (void)
{
    hid_t           file, space, dset, dcpl;    /* Handles */
    herr_t          status;
    htri_t          avail;
    H5Z_filter_t    filter_type;
    hsize_t         dims[2] = {DIM0, DIM1},
                    chunk[2] = {CHUNK0, CHUNK1};
    size_t          nelmts;
    unsigned int    flags,
                    filter_info;
    int             wdata[DIM0][DIM1],          /* Write buffer */
                    rdata[DIM0][DIM1],          /* Read buffer */
                    max,
                    i, j;

    /*
     * Check if szip compression is available and can be used for both
     * compression and decompression.  Normally we do not perform error
     * checking in these examples for the sake of clarity, but in this
     * case we will make an exception because this filter is an
     * optional part of the hdf5 library.
     */
    avail = H5Zfilter_avail(H5Z_FILTER_SZIP);
    if (!avail) {
        printf ("szip filter not available.\n");
        return 1;
    }
    status = H5Zget_filter_info (H5Z_FILTER_SZIP, &filter_info);
    if ( !(filter_info & H5Z_FILTER_CONFIG_ENCODE_ENABLED) ||
                !(filter_info & H5Z_FILTER_CONFIG_DECODE_ENABLED) ) {
        printf ("szip filter not available for encoding and decoding.\n");
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
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (2, dims, NULL);

    /*
     * Create the dataset creation property list, add the szip
     * compression filter and set the chunk size.
     */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    status = H5Pset_szip (dcpl, H5_SZIP_NN_OPTION_MASK, 8);
    status = H5Pset_chunk (dcpl, 2, chunk);

    /*
     * Create the dataset.
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
     * Now we begin the read section of this example.
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
    printf ("Filter type is: ");
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
     * Read the data using the default properties.
     */
    status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata[0]);

    /*
     * Find the maximum value in the dataset, to verify that it was
     * read correctly.
     */
    max = rdata[0][0];
    for (i=0; i<DIM0; i++)
        for (j = 0; j<DIM1; j++)
            if (max < rdata[i][j])
                max=rdata[i][j];

    /*
     * Print the maximum value.
     */
    printf ("Maximum value in %s is: %d\n", DATASET, max);

    /*
     * Close and release resources.
     */
    status = H5Pclose (dcpl);
    status = H5Dclose (dset);
    status = H5Fclose (file);

    return 0;
}
