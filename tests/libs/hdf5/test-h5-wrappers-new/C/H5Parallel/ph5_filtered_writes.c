/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Example of using the parallel HDF5 library to write to datasets
 * with filters applied to them.
 *
 * If the HDF5_NOCLEANUP environment variable is set, the file that
 * this example creates will not be removed as the example finishes.
 *
 * The need of requirement of parallel file prefix is that in general
 * the current working directory in which compiling is done, is not suitable
 * for parallel I/O and there is no standard pathname for parallel file
 * systems. In some cases, the parallel file name may even need some
 * parallel file type prefix such as: "pfs:/GF/...".  Therefore, this
 * example parses the HDF5_PARAPREFIX environment variable for a prefix,
 * if one is needed.
 */

#include <stdlib.h>

#include "hdf5.h"

#if defined(H5_HAVE_PARALLEL) && defined(H5_HAVE_PARALLEL_FILTERED_WRITES)

#define EXAMPLE_FILE       "ph5_filtered_writes.h5"
#define EXAMPLE_DSET1_NAME "DSET1"
#define EXAMPLE_DSET2_NAME "DSET2"

#define EXAMPLE_DSET_DIMS           2
#define EXAMPLE_DSET_CHUNK_DIM_SIZE 10

/* Dataset datatype */
#define HDF5_DATATYPE H5T_NATIVE_INT
typedef int C_DATATYPE;

#ifndef PATH_MAX
#define PATH_MAX 512
#endif

/* Global variables */
int mpi_rank, mpi_size;

/*
 * Routine to set an HDF5 filter on the given DCPL
 */
static void
set_filter(hid_t dcpl_id)
{
    htri_t filter_avail;

    /*
     * Check if 'deflate' filter is available
     */
    filter_avail = H5Zfilter_avail(H5Z_FILTER_DEFLATE);
    if (filter_avail < 0)
        return;
    else if (filter_avail) {
        /*
         * Set 'deflate' filter with reasonable
         * compression level on DCPL
         */
        H5Pset_deflate(dcpl_id, 6);
    }
    else {
        /*
         * Set Fletcher32 checksum filter on DCPL
         * since it is always available in HDF5
         */
        H5Pset_fletcher32(dcpl_id);
    }
}

/*
 * Routine to fill a data buffer with data. Assumes
 * dimension rank is 2 and data is stored contiguous.
 */
void
fill_databuf(hsize_t start[], hsize_t count[], hsize_t stride[], C_DATATYPE *data)
{
    C_DATATYPE *dataptr = data;
    hsize_t     i, j;

    /* Use MPI rank value for data */
    for (i = 0; i < count[0]; i++) {
        for (j = 0; j < count[1]; j++) {
            *dataptr++ = mpi_rank;
        }
    }
}

/* Cleanup created file */
static void
cleanup(char *filename)
{
    hbool_t do_cleanup = getenv(HDF5_NOCLEANUP) ? 0 : 1;

    if (do_cleanup)
        MPI_File_delete(filename, MPI_INFO_NULL);
}

/*
 * Routine to write to a dataset in a fashion
 * where no chunks in the dataset are written
 * to by more than 1 MPI rank. This will
 * generally give the best performance as the
 * MPI ranks will need the least amount of
 * inter-process communication.
 */
static void
write_dataset_no_overlap(hid_t file_id, hid_t dxpl_id)
{
    C_DATATYPE data[EXAMPLE_DSET_CHUNK_DIM_SIZE][4 * EXAMPLE_DSET_CHUNK_DIM_SIZE];
    hsize_t    dataset_dims[EXAMPLE_DSET_DIMS];
    hsize_t    chunk_dims[EXAMPLE_DSET_DIMS];
    hsize_t    start[EXAMPLE_DSET_DIMS];
    hsize_t    stride[EXAMPLE_DSET_DIMS];
    hsize_t    count[EXAMPLE_DSET_DIMS];
    size_t     i, j;
    hid_t      dset_id        = H5I_INVALID_HID;
    hid_t      dcpl_id        = H5I_INVALID_HID;
    hid_t      file_dataspace = H5I_INVALID_HID;

    /*
     * ------------------------------------
     * Setup Dataset Creation Property List
     * ------------------------------------
     */

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);

    /*
     * REQUIRED: Dataset chunking must be enabled to
     *           apply a data filter to the dataset.
     *           Chunks in the dataset are of size
     *           EXAMPLE_DSET_CHUNK_DIM_SIZE x EXAMPLE_DSET_CHUNK_DIM_SIZE.
     */
    chunk_dims[0] = EXAMPLE_DSET_CHUNK_DIM_SIZE;
    chunk_dims[1] = EXAMPLE_DSET_CHUNK_DIM_SIZE;
    H5Pset_chunk(dcpl_id, EXAMPLE_DSET_DIMS, chunk_dims);

    /* Set filter to be applied to created datasets */
    set_filter(dcpl_id);

    /*
     * ------------------------------------
     * Define the dimensions of the dataset
     * and create it
     * ------------------------------------
     */

    /*
     * Create a dataset composed of 4 chunks
     * per MPI rank. The first dataset dimension
     * scales according to the number of MPI ranks.
     * The second dataset dimension stays fixed
     * according to the chunk size.
     */
    dataset_dims[0] = EXAMPLE_DSET_CHUNK_DIM_SIZE * mpi_size;
    dataset_dims[1] = 4 * EXAMPLE_DSET_CHUNK_DIM_SIZE;

    file_dataspace = H5Screate_simple(EXAMPLE_DSET_DIMS, dataset_dims, NULL);

    /* Create the dataset */
    dset_id = H5Dcreate2(file_id, EXAMPLE_DSET1_NAME, HDF5_DATATYPE, file_dataspace, H5P_DEFAULT, dcpl_id,
                         H5P_DEFAULT);

    /*
     * ------------------------------------
     * Setup selection in the dataset for
     * each MPI rank
     * ------------------------------------
     */

    /*
     * Each MPI rank's selection covers a
     * single chunk in the first dataset
     * dimension. Each MPI rank's selection
     * covers 4 chunks in the second dataset
     * dimension. This leads to each MPI rank
     * writing to 4 chunks of the dataset.
     */
    start[0]  = mpi_rank * EXAMPLE_DSET_CHUNK_DIM_SIZE;
    start[1]  = 0;
    stride[0] = 1;
    stride[1] = 1;
    count[0]  = EXAMPLE_DSET_CHUNK_DIM_SIZE;
    count[1]  = 4 * EXAMPLE_DSET_CHUNK_DIM_SIZE;

    H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL);

    /*
     * --------------------------------------
     * Fill data buffer with MPI rank's rank
     * value to make it easy to see which
     * part of the dataset each rank wrote to
     * --------------------------------------
     */

    fill_databuf(start, count, stride, &data[0][0]);

    /*
     * ---------------------------------
     * Write to the dataset collectively
     * ---------------------------------
     */

    H5Dwrite(dset_id, HDF5_DATATYPE, H5S_BLOCK, file_dataspace, dxpl_id, data);

    /*
     * --------------
     * Close HDF5 IDs
     * --------------
     */

    H5Sclose(file_dataspace);
    H5Pclose(dcpl_id);
    H5Dclose(dset_id);
}

/*
 * Routine to write to a dataset in a fashion
 * where every chunk in the dataset is written
 * to by every MPI rank. This will generally
 * give the worst performance as the MPI ranks
 * will need the most amount of inter-process
 * communication.
 */
static void
write_dataset_overlap(hid_t file_id, hid_t dxpl_id)
{
    C_DATATYPE *data = NULL;
    hsize_t     dataset_dims[EXAMPLE_DSET_DIMS];
    hsize_t     chunk_dims[EXAMPLE_DSET_DIMS];
    hsize_t     start[EXAMPLE_DSET_DIMS];
    hsize_t     stride[EXAMPLE_DSET_DIMS];
    hsize_t     count[EXAMPLE_DSET_DIMS];
    size_t      i, j;
    hid_t       dset_id        = H5I_INVALID_HID;
    hid_t       dcpl_id        = H5I_INVALID_HID;
    hid_t       file_dataspace = H5I_INVALID_HID;

    /*
     * ------------------------------------
     * Setup Dataset Creation Property List
     * ------------------------------------
     */

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);

    /*
     * REQUIRED: Dataset chunking must be enabled to
     *           apply a data filter to the dataset.
     *           Chunks in the dataset are of size
     *           mpi_size x EXAMPLE_DSET_CHUNK_DIM_SIZE.
     */
    chunk_dims[0] = mpi_size;
    chunk_dims[1] = EXAMPLE_DSET_CHUNK_DIM_SIZE;
    H5Pset_chunk(dcpl_id, EXAMPLE_DSET_DIMS, chunk_dims);

    /* Set filter to be applied to created datasets */
    set_filter(dcpl_id);

    /*
     * ------------------------------------
     * Define the dimensions of the dataset
     * and create it
     * ------------------------------------
     */

    /*
     * Create a dataset composed of N chunks,
     * where N is the number of MPI ranks. The
     * first dataset dimension scales according
     * to the number of MPI ranks. The second
     * dataset dimension stays fixed according
     * to the chunk size.
     */
    dataset_dims[0] = mpi_size * chunk_dims[0];
    dataset_dims[1] = EXAMPLE_DSET_CHUNK_DIM_SIZE;

    file_dataspace = H5Screate_simple(EXAMPLE_DSET_DIMS, dataset_dims, NULL);

    /* Create the dataset */
    dset_id = H5Dcreate2(file_id, EXAMPLE_DSET2_NAME, HDF5_DATATYPE, file_dataspace, H5P_DEFAULT, dcpl_id,
                         H5P_DEFAULT);

    /*
     * ------------------------------------
     * Setup selection in the dataset for
     * each MPI rank
     * ------------------------------------
     */

    /*
     * Each MPI rank's selection covers
     * part of every chunk in the first
     * dimension. Each MPI rank's selection
     * covers all of every chunk in the
     * second dimension. This leads to
     * each MPI rank writing an equal
     * amount of data to every chunk
     * in the dataset.
     */
    start[0]  = mpi_rank;
    start[1]  = 0;
    stride[0] = chunk_dims[0];
    stride[1] = 1;
    count[0]  = mpi_size;
    count[1]  = EXAMPLE_DSET_CHUNK_DIM_SIZE;

    H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL);

    /*
     * --------------------------------------
     * Fill data buffer with MPI rank's rank
     * value to make it easy to see which
     * part of the dataset each rank wrote to
     * --------------------------------------
     */

    data = malloc(mpi_size * EXAMPLE_DSET_CHUNK_DIM_SIZE * sizeof(C_DATATYPE));

    fill_databuf(start, count, stride, data);

    /*
     * ---------------------------------
     * Write to the dataset collectively
     * ---------------------------------
     */

    H5Dwrite(dset_id, HDF5_DATATYPE, H5S_BLOCK, file_dataspace, dxpl_id, data);

    free(data);

    /*
     * --------------
     * Close HDF5 IDs
     * --------------
     */

    H5Sclose(file_dataspace);
    H5Pclose(dcpl_id);
    H5Dclose(dset_id);
}

int
main(int argc, char **argv)
{
    MPI_Comm comm       = MPI_COMM_WORLD;
    MPI_Info info       = MPI_INFO_NULL;
    hid_t    file_id    = H5I_INVALID_HID;
    hid_t    fapl_id    = H5I_INVALID_HID;
    hid_t    dxpl_id    = H5I_INVALID_HID;
    char    *par_prefix = NULL;
    char     filename[PATH_MAX];

    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);

    /*
     * ----------------------------------
     * Start parallel access to HDF5 file
     * ----------------------------------
     */

    /* Setup File Access Property List with parallel I/O access */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(fapl_id, comm, info);

    /*
     * OPTIONAL: Set collective metadata reads on FAPL to allow
     *           parallel writes to filtered datasets to perform
     *           better at scale. While not strictly necessary,
     *           this is generally recommended.
     */
    H5Pset_all_coll_metadata_ops(fapl_id, true);

    /*
     * OPTIONAL: Set the latest file format version for HDF5 in
     *           order to gain access to different dataset chunk
     *           index types and better data encoding methods.
     *           While not strictly necessary, this is generally
     *           recommended.
     */
    H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);

    /* Parse any parallel prefix and create filename */
    par_prefix = getenv("HDF5_PARAPREFIX");

    snprintf(filename, PATH_MAX, "%s%s%s", par_prefix ? par_prefix : "", par_prefix ? "/" : "", EXAMPLE_FILE);

    /* Create HDF5 file */
    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);

    /*
     * --------------------------------------
     * Setup Dataset Transfer Property List
     * with collective I/O
     * --------------------------------------
     */

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);

    /*
     * REQUIRED: Setup collective I/O for the dataset
     *           write operations. Parallel writes to
     *           filtered datasets MUST be collective,
     *           even if some ranks have no data to
     *           contribute to the write operation.
     *
     *           Refer to the 'ph5_filtered_writes_no_sel'
     *           example to see how to setup a dataset
     *           write when one or more MPI ranks have
     *           no data to contribute to the write
     *           operation.
     */
    H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE);

    /*
     * --------------------------------
     * Create and write to each dataset
     * --------------------------------
     */

    /*
     * Write to a dataset in a fashion where no
     * chunks in the dataset are written to by
     * more than 1 MPI rank. This will generally
     * give the best performance as the MPI ranks
     * will need the least amount of inter-process
     * communication.
     */
    write_dataset_no_overlap(file_id, dxpl_id);

    /*
     * Write to a dataset in a fashion where
     * every chunk in the dataset is written
     * to by every MPI rank. This will generally
     * give the worst performance as the MPI ranks
     * will need the most amount of inter-process
     * communication.
     */
    write_dataset_overlap(file_id, dxpl_id);

    /*
     * ------------------
     * Close all HDF5 IDs
     * ------------------
     */

    H5Pclose(dxpl_id);
    H5Pclose(fapl_id);
    H5Fclose(file_id);

    printf("PHDF5 example finished with no errors\n");

    /*
     * ------------------------------------
     * Cleanup created HDF5 file and finish
     * ------------------------------------
     */

    cleanup(filename);

    MPI_Finalize();

    return 0;
}

#else

int
main(void)
{
    printf("HDF5 not configured with parallel support or parallel filtered writes are disabled!\n");
    return 0;
}

#endif
