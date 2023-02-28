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
 * Example of using HDF5's Subfiling VFD to write to an
 * HDF5 file that is striped across multiple subfiles
 *
 * If the HDF5_NOCLEANUP environment variable is set, the
 * files that this example creates will not be removed as
 * the example finishes.
 *
 * In general, the current working directory in which compiling
 * is done, is not suitable for parallel I/O and there is no
 * standard pathname for parallel file systems. In some cases,
 * the parallel file name may even need some parallel file type
 * prefix such as: "pfs:/GF/...".  Therefore, this example parses
 * the HDF5_PARAPREFIX environment variable for a prefix, if one
 * is needed.
 */

#include <stdlib.h>

#include "hdf5.h"

#if defined(H5_HAVE_PARALLEL) && defined(H5_HAVE_SUBFILING_VFD)

#define EXAMPLE_FILE  "h5_subfiling_default_example.h5"
#define EXAMPLE_FILE2 "h5_subfiling_custom_example.h5"
#define EXAMPLE_FILE3 "h5_subfiling_precreate_example.h5"

#define EXAMPLE_DSET_NAME "DSET"
#define EXAMPLE_DSET_DIMS 2

/* Have each MPI rank write 16MiB of data */
#define EXAMPLE_DSET_NY 4194304

/* Dataset datatype */
#define EXAMPLE_DSET_DATATYPE H5T_NATIVE_INT
typedef int EXAMPLE_DSET_C_DATATYPE;

/* Cleanup created files */
static void
cleanup(char *filename, hid_t fapl_id)
{
    hbool_t do_cleanup = getenv(HDF5_NOCLEANUP) ? 0 : 1;

    if (do_cleanup)
        H5Fdelete(filename, fapl_id);
}

/*
 * An example of using the HDF5 Subfiling VFD with
 * its default settings of 1 subfile per node, with
 * a stripe size of 32MiB
 */
static void
subfiling_write_default(hid_t fapl_id, int mpi_size, int mpi_rank)
{
    EXAMPLE_DSET_C_DATATYPE *data;
    hsize_t                  dset_dims[EXAMPLE_DSET_DIMS];
    hsize_t                  start[EXAMPLE_DSET_DIMS];
    hsize_t                  count[EXAMPLE_DSET_DIMS];
    hid_t                    file_id;
    hid_t                    subfiling_fapl;
    hid_t                    dset_id;
    hid_t                    filespace;
    char                     filename[512];
    char                    *par_prefix;

    /*
     * Make a copy of the FAPL so we don't disturb
     * it for the other examples
     */
    subfiling_fapl = H5Pcopy(fapl_id);

    /*
     * Set Subfiling VFD on FAPL using default settings
     * (use IOC VFD, 1 IOC per node, 32MiB stripe size)
     *
     * Note that all of Subfiling's configuration settings
     * can be adjusted with environment variables as well
     * in this case.
     */
    H5Pset_fapl_subfiling(subfiling_fapl, NULL);

    /*
     * OPTIONAL: Set alignment of objects in HDF5 file to
     *           be equal to the Subfiling stripe size.
     *           Choosing a Subfiling stripe size and HDF5
     *           object alignment value that are some
     *           multiple of the disk block size can
     *           generally help performance by ensuring
     *           that I/O is well-aligned and doesn't
     *           excessively cross stripe boundaries.
     *
     *           Note that this option can substantially
     *           increase the size of the resulting HDF5
     *           files, so it is a good idea to keep an eye
     *           on this.
     */
    H5Pset_alignment(subfiling_fapl, 0, 33554432); /* Align to default 32MiB stripe size */

    /* Parse any parallel prefix and create filename */
    par_prefix = getenv("HDF5_PARAPREFIX");

    snprintf(filename, sizeof(filename), "%s%s%s", par_prefix ? par_prefix : "", par_prefix ? "/" : "",
             EXAMPLE_FILE);

    /*
     * Create a new file collectively
     */
    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, subfiling_fapl);

    /*
     * Create the dataspace for the dataset. The first
     * dimension varies with the number of MPI ranks
     * while the second dimension is fixed.
     */
    dset_dims[0] = mpi_size;
    dset_dims[1] = EXAMPLE_DSET_NY;
    filespace    = H5Screate_simple(EXAMPLE_DSET_DIMS, dset_dims, NULL);

    /*
     * Create the dataset with default properties
     */
    dset_id = H5Dcreate2(file_id, EXAMPLE_DSET_NAME, EXAMPLE_DSET_DATATYPE, filespace, H5P_DEFAULT,
                         H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Each MPI rank writes from a contiguous memory
     * region to the hyperslab in the file
     */
    start[0] = mpi_rank;
    start[1] = 0;
    count[0] = 1;
    count[1] = dset_dims[1];
    H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL);

    /*
     * Initialize data buffer
     */
    data = malloc(count[0] * count[1] * sizeof(EXAMPLE_DSET_C_DATATYPE));
    for (size_t i = 0; i < count[0] * count[1]; i++) {
        data[i] = mpi_rank + i;
    }

    /*
     * Write to dataset
     */
    H5Dwrite(dset_id, EXAMPLE_DSET_DATATYPE, H5S_BLOCK, filespace, H5P_DEFAULT, data);

    /*
     * Close/release resources.
     */

    free(data);

    H5Dclose(dset_id);
    H5Sclose(filespace);
    H5Fclose(file_id);

    cleanup(EXAMPLE_FILE, subfiling_fapl);

    H5Pclose(subfiling_fapl);
}

/*
 * An example of using the HDF5 Subfiling VFD with
 * custom settings
 */
static void
subfiling_write_custom(hid_t fapl_id, int mpi_size, int mpi_rank)
{
    EXAMPLE_DSET_C_DATATYPE *data;
    H5FD_subfiling_config_t  subf_config;
    H5FD_ioc_config_t        ioc_config;
    hsize_t                  dset_dims[EXAMPLE_DSET_DIMS];
    hsize_t                  start[EXAMPLE_DSET_DIMS];
    hsize_t                  count[EXAMPLE_DSET_DIMS];
    hid_t                    file_id;
    hid_t                    subfiling_fapl;
    hid_t                    dset_id;
    hid_t                    filespace;
    char                     filename[512];
    char                    *par_prefix;

    /*
     * Make a copy of the FAPL so we don't disturb
     * it for the other examples
     */
    subfiling_fapl = H5Pcopy(fapl_id);

    /*
     * Get a default Subfiling and IOC configuration
     */
    H5Pget_fapl_subfiling(subfiling_fapl, &subf_config);
    H5Pget_fapl_ioc(subfiling_fapl, &ioc_config);

    /*
     * Set Subfiling configuration to use a 1MiB
     * stripe size and the SELECT_IOC_EVERY_NTH_RANK
     * selection method. By default, without a setting
     * in the H5FD_SUBFILING_IOC_SELECTION_CRITERIA
     * environment variable, this will use every MPI
     * rank as an I/O concentrator.
     */
    subf_config.shared_cfg.stripe_size   = 1048576;
    subf_config.shared_cfg.ioc_selection = SELECT_IOC_EVERY_NTH_RANK;

    /*
     * Set IOC configuration to use 2 worker threads
     * per IOC instead of the default setting and
     * update IOC configuration with new subfiling
     * configuration.
     */
    ioc_config.thread_pool_size = 2;

    /*
     * Set our new configuration on the IOC
     * FAPL used for Subfiling
     */
    H5Pset_fapl_ioc(subf_config.ioc_fapl_id, &ioc_config);

    /*
     * Finally, set our new Subfiling configuration
     * on the original FAPL
     */
    H5Pset_fapl_subfiling(subfiling_fapl, &subf_config);

    /*
     * OPTIONAL: Set alignment of objects in HDF5 file to
     *           be equal to the Subfiling stripe size.
     *           Choosing a Subfiling stripe size and HDF5
     *           object alignment value that are some
     *           multiple of the disk block size can
     *           generally help performance by ensuring
     *           that I/O is well-aligned and doesn't
     *           excessively cross stripe boundaries.
     *
     *           Note that this option can substantially
     *           increase the size of the resulting HDF5
     *           files, so it is a good idea to keep an eye
     *           on this.
     */
    H5Pset_alignment(subfiling_fapl, 0, 1048576); /* Align to custom 1MiB stripe size */

    /* Parse any parallel prefix and create filename */
    par_prefix = getenv("HDF5_PARAPREFIX");

    snprintf(filename, sizeof(filename), "%s%s%s", par_prefix ? par_prefix : "", par_prefix ? "/" : "",
             EXAMPLE_FILE2);

    /*
     * Create a new file collectively
     */
    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, subfiling_fapl);

    /*
     * Create the dataspace for the dataset. The first
     * dimension varies with the number of MPI ranks
     * while the second dimension is fixed.
     */
    dset_dims[0] = mpi_size;
    dset_dims[1] = EXAMPLE_DSET_NY;
    filespace    = H5Screate_simple(EXAMPLE_DSET_DIMS, dset_dims, NULL);

    /*
     * Create the dataset with default properties
     */
    dset_id = H5Dcreate2(file_id, EXAMPLE_DSET_NAME, EXAMPLE_DSET_DATATYPE, filespace, H5P_DEFAULT,
                         H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Each MPI rank writes from a contiguous memory
     * region to the hyperslab in the file
     */
    start[0] = mpi_rank;
    start[1] = 0;
    count[0] = 1;
    count[1] = dset_dims[1];
    H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL);

    /*
     * Initialize data buffer
     */
    data = malloc(count[0] * count[1] * sizeof(EXAMPLE_DSET_C_DATATYPE));
    for (size_t i = 0; i < count[0] * count[1]; i++) {
        data[i] = mpi_rank + i;
    }

    /*
     * Write to dataset
     */
    H5Dwrite(dset_id, EXAMPLE_DSET_DATATYPE, H5S_BLOCK, filespace, H5P_DEFAULT, data);

    /*
     * Close/release resources.
     */

    free(data);

    H5Dclose(dset_id);
    H5Sclose(filespace);
    H5Fclose(file_id);

    cleanup(EXAMPLE_FILE2, subfiling_fapl);

    H5Pclose(subfiling_fapl);
}

/*
 * An example of pre-creating an HDF5 file on MPI rank
 * 0 when using the HDF5 Subfiling VFD. In this case,
 * the subfiling stripe count must be set so that rank
 * 0 knows how many subfiles to pre-create.
 */
static void
subfiling_write_precreate(hid_t fapl_id, int mpi_size, int mpi_rank)
{
    EXAMPLE_DSET_C_DATATYPE *data;
    H5FD_subfiling_config_t  subf_config;
    hsize_t                  dset_dims[EXAMPLE_DSET_DIMS];
    hsize_t                  start[EXAMPLE_DSET_DIMS];
    hsize_t                  count[EXAMPLE_DSET_DIMS];
    hid_t                    file_id;
    hid_t                    subfiling_fapl;
    hid_t                    dset_id;
    hid_t                    filespace;
    char                     filename[512];
    char                    *par_prefix;

    /*
     * Make a copy of the FAPL so we don't disturb
     * it for the other examples
     */
    subfiling_fapl = H5Pcopy(fapl_id);

    /*
     * Get a default Subfiling and IOC configuration
     */
    H5Pget_fapl_subfiling(subfiling_fapl, &subf_config);

    /*
     * Set the Subfiling stripe count so that rank
     * 0 knows how many subfiles the logical HDF5
     * file should consist of. In this case, use
     * 5 subfiles with a default stripe size of
     * 32MiB.
     */
    subf_config.shared_cfg.stripe_count = 5;

    /*
     * OPTIONAL: Set alignment of objects in HDF5 file to
     *           be equal to the Subfiling stripe size.
     *           Choosing a Subfiling stripe size and HDF5
     *           object alignment value that are some
     *           multiple of the disk block size can
     *           generally help performance by ensuring
     *           that I/O is well-aligned and doesn't
     *           excessively cross stripe boundaries.
     *
     *           Note that this option can substantially
     *           increase the size of the resulting HDF5
     *           files, so it is a good idea to keep an eye
     *           on this.
     */
    H5Pset_alignment(subfiling_fapl, 0, 1048576); /* Align to custom 1MiB stripe size */

    /* Parse any parallel prefix and create filename */
    par_prefix = getenv("HDF5_PARAPREFIX");

    snprintf(filename, sizeof(filename), "%s%s%s", par_prefix ? par_prefix : "", par_prefix ? "/" : "",
             EXAMPLE_FILE3);

    /* Set dataset dimensionality */
    dset_dims[0] = mpi_size;
    dset_dims[1] = EXAMPLE_DSET_NY;

    if (mpi_rank == 0) {
        /*
         * Make sure only this rank opens the file
         */
        H5Pset_mpi_params(subfiling_fapl, MPI_COMM_SELF, MPI_INFO_NULL);

        /*
         * Set the Subfiling VFD on our FAPL using
         * our custom configuration
         */
        H5Pset_fapl_subfiling(subfiling_fapl, &subf_config);

        /*
         * Create a new file on rank 0
         */
        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, subfiling_fapl);

        /*
         * Create the dataspace for the dataset. The first
         * dimension varies with the number of MPI ranks
         * while the second dimension is fixed.
         */
        filespace = H5Screate_simple(EXAMPLE_DSET_DIMS, dset_dims, NULL);

        /*
         * Create the dataset with default properties
         */
        dset_id = H5Dcreate2(file_id, EXAMPLE_DSET_NAME, EXAMPLE_DSET_DATATYPE, filespace, H5P_DEFAULT,
                             H5P_DEFAULT, H5P_DEFAULT);

        /*
         * Initialize data buffer
         */
        data = malloc(dset_dims[0] * dset_dims[1] * sizeof(EXAMPLE_DSET_C_DATATYPE));
        for (size_t i = 0; i < dset_dims[0] * dset_dims[1]; i++) {
            data[i] = i;
        }

        /*
         * Rank 0 writes to the whole dataset
         */
        H5Dwrite(dset_id, EXAMPLE_DSET_DATATYPE, H5S_BLOCK, filespace, H5P_DEFAULT, data);

        /*
         * Close/release resources.
         */

        free(data);

        H5Dclose(dset_id);
        H5Sclose(filespace);
        H5Fclose(file_id);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    /*
     * Use all MPI ranks to re-open the file and
     * read back the dataset that was created
     */
    H5Pset_mpi_params(subfiling_fapl, MPI_COMM_WORLD, MPI_INFO_NULL);

    /*
     * Use the same subfiling configuration as rank 0
     * used to create the file
     */
    H5Pset_fapl_subfiling(subfiling_fapl, &subf_config);

    /*
     * Re-open the file on all ranks
     */
    file_id = H5Fopen(filename, H5F_ACC_RDONLY, subfiling_fapl);

    /*
     * Open the dataset that was created
     */
    dset_id = H5Dopen2(file_id, EXAMPLE_DSET_NAME, H5P_DEFAULT);

    /*
     * Initialize data buffer
     */
    data = malloc(dset_dims[0] * dset_dims[1] * sizeof(EXAMPLE_DSET_C_DATATYPE));

    /*
     * Read the dataset on all ranks
     */
    H5Dread(dset_id, EXAMPLE_DSET_DATATYPE, H5S_BLOCK, H5S_ALL, H5P_DEFAULT, data);

    /*
     * Close/release resources.
     */

    free(data);

    H5Dclose(dset_id);
    H5Fclose(file_id);

    cleanup(EXAMPLE_FILE3, subfiling_fapl);

    H5Pclose(subfiling_fapl);
}

int
main(int argc, char **argv)
{
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;
    hid_t    fapl_id;
    int      mpi_size;
    int      mpi_rank;
    int      mpi_thread_required = MPI_THREAD_MULTIPLE;
    int      mpi_thread_provided = 0;

    /* HDF5 Subfiling VFD requires MPI_Init_thread with MPI_THREAD_MULTIPLE */
    MPI_Init_thread(&argc, &argv, mpi_thread_required, &mpi_thread_provided);
    if (mpi_thread_provided < mpi_thread_required) {
        printf("MPI_THREAD_MULTIPLE not supported\n");
        MPI_Abort(comm, -1);
    }

    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);

    /*
     * Set up File Access Property List with MPI
     * parameters for the Subfiling VFD to use
     */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_mpi_params(fapl_id, comm, info);

    /* Use Subfiling VFD with default settings */
    subfiling_write_default(fapl_id, mpi_size, mpi_rank);

    /* Use Subfiling VFD with custom settings */
    subfiling_write_custom(fapl_id, mpi_size, mpi_rank);

    /*
     * Use Subfiling VFD to precreate the HDF5
     * file on MPI rank 0
     */
    subfiling_write_precreate(fapl_id, mpi_size, mpi_rank);

    H5Pclose(fapl_id);

    if (mpi_rank == 0)
        printf("PHDF5 example finished with no errors\n");

    MPI_Finalize();

    return 0;
}

#else

/* dummy program since HDF5 is not parallel-enabled */
int
main(void)
{
    printf(
        "Example program cannot run - HDF5 must be built with parallel support and Subfiling VFD support\n");
    return 0;
}

#endif /* H5_HAVE_PARALLEL && H5_HAVE_SUBFILING_VFD */
