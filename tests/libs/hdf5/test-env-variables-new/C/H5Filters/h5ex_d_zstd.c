/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of the HDF5 ZSTD filter plugin source.  The full       *
 * copyright notice, including terms governing use, modification, and        *
 * terms governing use, modification, and redistribution, is contained in    *
 * the file COPYING, which can be found at the root of the ZSTD source code *
 * distribution tree.  If you do not have access to this file, you may       *
 * request a copy from help@hdfgroup.org.                                    *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/************************************************************

  This example shows how to write data and read it from a dataset
  using zstd compression.
  zstd filter is not available in HDF5.
  The example uses a new feature available in HDF5 version 1.8.11
  to discover, load and register filters at run time.

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_d_zstd.h5"
#define DATASET         "DS1"
#define DIM0            512
#define DIM1            1024
#define NUM_IMAGES      2
#define CHUNK0          1
#define CHUNK1          DIM0
#define CHUNK2          DIM1
#define H5Z_FILTER_ZSTD 32015

int
main (void)
{
    hid_t           file_id = -1;    /* Handles */
    hid_t           space_id = -1;    /* Handles */
    hid_t           dset_id = -1;    /* Handles */
    hid_t           dcpl_id = -1;    /* Handles */
    herr_t          status;
    htri_t          avail;
    H5Z_filter_t    filter_id = 0;
    char            filter_name[80];
    hsize_t         dims[3] = {NUM_IMAGES, DIM0, DIM1},
                    chunk[3] = {CHUNK0, CHUNK1, CHUNK2};
    size_t          nelmts = 1;                /* number of elements in cd_values */
    unsigned int    flags;
    unsigned        filter_config;
    size_t          data_size = DIM0 * DIM1 * NUM_IMAGES;

    /* ZSTD filter optionally uses 1 parameter */
    /* Number of columns */
    /* Number of rows */
    const unsigned int cd_values[1] = {0};     /* zstd default level is 3 */
    unsigned int       values_out[1] = {99};
    unsigned char   *wdata,          /* Write buffer */
                    *rdata;          /* Read buffer */
    int             num_diff = 0;
    hsize_t         i;
    int             ret_value = 1;

    wdata = (unsigned char*) malloc(sizeof(unsigned char) * data_size);
    rdata = (unsigned char*) malloc(sizeof(unsigned char) * data_size);

    for(i = 0; i < data_size; i++){
        wdata[i] = i;
    }

    /*
     * Create a new file using the default properties.
     */
    file_id = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (file_id < 0) goto done;

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space_id = H5Screate_simple (3, dims, NULL);
    if (space_id < 0) goto done;

    /*
     * Create the dataset creation property list, add the gzip
     * compression filter and set the chunk size.
     */
    dcpl_id = H5Pcreate (H5P_DATASET_CREATE);
    if (dcpl_id < 0) goto done;

    status = H5Pset_filter (dcpl_id, H5Z_FILTER_ZSTD, H5Z_FLAG_MANDATORY, nelmts, cd_values);
    if (status < 0) goto done;

    /*
     * Check that filter is registered with the library now.
     * If it is registered, retrieve filter's configuration.
     */
    avail = H5Zfilter_avail(H5Z_FILTER_ZSTD);
    if (avail) {
        status = H5Zget_filter_info (H5Z_FILTER_ZSTD, &filter_config);
        if ( (filter_config & H5Z_FILTER_CONFIG_ENCODE_ENABLED) &&
                (filter_config & H5Z_FILTER_CONFIG_DECODE_ENABLED) )
            printf ("zstd filter is available for encoding and decoding.\n");
    }
    else {
        printf ("H5Zfilter_avail - not found.\n");
        goto done;
    }
    status = H5Pset_chunk (dcpl_id, 3, chunk);
    if (status < 0) printf ("failed to set chunk.\n");

    /*
     * Create the dataset.
     */
    printf ("....Create dataset ................\n");
    dset_id = H5Dcreate (file_id, DATASET, H5T_NATIVE_UINT8, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    if (dset_id < 0) {
        printf ("failed to create dataset.\n");
        goto done;
    }

    /*
     * Write the data to the dataset.
     */
    printf ("....Writing zstd compressed data ................\n");
    status = H5Dwrite (dset_id, H5T_NATIVE_UINT8, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    if (status < 0) printf ("failed to write data.\n");

    /*
     * Close and release resources.
     */
    H5Dclose (dset_id);
    dset_id = -1;
    H5Pclose (dcpl_id);
    dcpl_id = -1;
    H5Sclose (space_id);
    space_id = -1;
    H5Fclose (file_id);
    file_id = -1;
    status = H5close();
    if (status < 0) {
        printf ("/nFAILED to close library/n");
        goto done;
    }


    printf ("....Close the file and reopen for reading ........\n");
    /*
     * Now we begin the read section of this example.
     */

    /*
     * Open file and dataset using the default properties.
     */
    file_id = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    if (file_id < 0) goto done;

    dset_id = H5Dopen (file_id, DATASET, H5P_DEFAULT);
    if (dset_id < 0) goto done;

    /*
     * Retrieve dataset creation property list.
     */
    dcpl_id = H5Dget_create_plist (dset_id);
    if (dcpl_id < 0) goto done;

    /*
     * Retrieve and print the filter id, compression level and filter's name for zstd.
     */
    filter_id = H5Pget_filter2 (dcpl_id, (unsigned) 0, &flags, &nelmts, values_out, sizeof(filter_name), filter_name, NULL);
    printf ("Filter info is available from the dataset creation property \n ");
    printf ("  Filter identifier is ");
    switch (filter_id) {
        case H5Z_FILTER_ZSTD:
            printf ("%d\n", filter_id);
            printf ("   Number of parameters is %d with the value %u\n", nelmts, values_out[0]);
            printf ("   To find more about the filter check %s\n", filter_name);
            break;
        default:
            printf ("Not expected filter\n");
            break;
    }

    /*
     * Read the data using the default properties.
     */
    printf ("....Reading zstd compressed data ................\n");
    status = H5Dread (dset_id, H5T_NATIVE_UINT8, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    if (status < 0) printf ("failed to read data.\n");

    /*
     * Find the maximum value in the dataset, to verify that it was
     * read correctly.
     */
    for (i=0; i<data_size; i++) {
        /*printf("%d \n", rdata[i]); */
        if (rdata[i] != wdata[i])
            num_diff++;
    }
    /*
     * Print the number of differences.
     */
    printf ("ZSTD number of differing array elements=%d\n", num_diff);
    /*
     * Check that filter is registered with the library now.
     */
    avail = H5Zfilter_avail(H5Z_FILTER_ZSTD);
    if (avail)
        printf ("zstd filter is available now since H5Dread triggered loading of the filter.\n");

    ret_value = 0;

done:
    free (rdata);
    free (wdata);
    /*
     * Close and release resources.
     */
    if (dcpl_id >= 0) H5Pclose (dcpl_id);
    if (dset_id >= 0) H5Dclose (dset_id);
    if (space_id >= 0) H5Sclose (space_id);
    if (file_id >= 0) H5Fclose (file_id);

    return ret_value;
}
