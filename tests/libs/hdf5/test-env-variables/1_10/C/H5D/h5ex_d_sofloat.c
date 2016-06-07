/************************************************************

  This example shows how to read and write data to a dataset
  using the Scale-Offset filter.  The program first checks
  if the Scale-Offset filter is available, then if it is it
  writes floating point numbers to a dataset using
  Scale-Offset, then closes the file Next, it reopens the
  file, reads back the data, and outputs the type of filter
  and the maximum value in the dataset to the screen.

  This file is intended for use with HDF5 Library version 1.8

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_d_sofloat.h5"
#define DATASET         "DS1"
#define DIM0            32
#define DIM1            64
#define CHUNK0          4
#define CHUNK1          8

int
main (void)
{
    hid_t           file, space, dset, dcpl;
                                                /* Handles */
    herr_t          status;
    htri_t          avail;
    H5Z_filter_t    filter_type;
    hsize_t         dims[2] = {DIM0, DIM1},
                    chunk[2] = {CHUNK0, CHUNK1};
    size_t          nelmts;
    unsigned int    flags,
                    filter_info;
    double          wdata[DIM0][DIM1],          /* Write buffer */
                    rdata[DIM0][DIM1],          /* Read buffer */
                    max,
                    min;
    int             i, j;

    /*
     * Check if Scale-Offset compression is available and can be used
     * for both compression and decompression.  Normally we do not
     * perform error checking in these examples for the sake of
     * clarity, but in this case we will make an exception because this
     * filter is an optional part of the hdf5 library.
     */
    avail = H5Zfilter_avail(H5Z_FILTER_SCALEOFFSET);
    if (!avail) {
        printf ("Scale-Offset filter not available.\n");
        return 1;
    }
    status = H5Zget_filter_info (H5Z_FILTER_SCALEOFFSET, &filter_info);
    if ( !(filter_info & H5Z_FILTER_CONFIG_ENCODE_ENABLED) ||
                !(filter_info & H5Z_FILTER_CONFIG_DECODE_ENABLED) ) {
        printf ("Scale-Offset filter not available for encoding and decoding.\n");
        return 1;
    }

    /*
     * Initialize data.
     */
    for (i=0; i<DIM0; i++)
        for (j=0; j<DIM1; j++)
            wdata[i][j] = (double) (i + 1) / (j + 0.3) + j;

    /*
     * Find the maximum value in the dataset, to verify that it was
     * read correctly.
     */
    max = wdata[0][0];
    min = wdata[0][0];
    for (i=0; i<DIM0; i++)
        for (j=0; j<DIM1; j++) {
            if (max < wdata[i][j])
                max = wdata[i][j];
            if (min > wdata[i][j])
                min = wdata[i][j];
        }

    /*
     * Print the maximum value.
     */
    printf ("Maximum value in write buffer is: %f\n", max);
    printf ("Minimum value in write buffer is: %f\n", min);

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
     * Create the dataset creation property list, add the Scale-Offset
     * filter and set the chunk size.
     */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    status = H5Pset_scaleoffset (dcpl, H5Z_SO_FLOAT_DSCALE, 2);
    status = H5Pset_chunk (dcpl, 2, chunk);

    /*
     * Create the dataset.
     */
    dset = H5Dcreate (file, DATASET, H5T_IEEE_F64LE, space, H5P_DEFAULT, dcpl,
                H5P_DEFAULT);

    /*
     * Write the data to the dataset.
     */
    status = H5Dwrite (dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
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
    dset = H5Dopen (file, DATASET, H5P_DEFAULT);

    /*
     * Retrieve dataset creation property list.
     */
    dcpl = H5Dget_create_plist (dset);

    /*
     * Retrieve and print the filter type.  Here we only retrieve the
     * first filter because we know that we only added one filter.
     */
    nelmts = 0;
    filter_type = H5Pget_filter (dcpl, 0, &flags, &nelmts, NULL, 0, NULL, &filter_info);
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
            break;
        case H5Z_FILTER_NBIT:
            printf ("H5Z_FILTER_NBIT\n");
            break;
        case H5Z_FILTER_SCALEOFFSET:
            printf ("H5Z_FILTER_SCALEOFFSET\n");
    }

    /*
     * Read the data using the default properties.
     */
    status = H5Dread (dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                rdata[0]);

    /*
     * Find the maximum value in the dataset, to verify that it was
     * read correctly.
     */
    max = rdata[0][0];
    min = rdata[0][0];
    for (i=0; i<DIM0; i++)
        for (j=0; j<DIM1; j++) {
            if (max < rdata[i][j])
                max = rdata[i][j];
            if (min > rdata[i][j])
                min = rdata[i][j];
        }

    /*
     * Print the maximum value.
     */
    printf ("Maximum value in %s is: %f\n", DATASET, max);
    printf ("Minimum value in %s is: %f\n", DATASET, min);

    /*
     * Close and release resources.
     */
    status = H5Pclose (dcpl);
    status = H5Dclose (dset);
    status = H5Fclose (file);

    return 0;
}
