/*  
 *  This example creates an HDF5 file.
 */
 
#include "hdf5.h"

int
main (int argc, char **argv)
{
    char *out_file;
    out_file = argv[1];
    /*
     * HDF5 APIs definitions
     */     
    hid_t       file_id;         /* file and dataset identifiers */
    hid_t   plist_id;        /* property list identifier( access template) */
    herr_t  status;

    /*
     * MPI variables
     */
    int mpi_size, mpi_rank;
    MPI_Comm comm  = MPI_COMM_WORLD;
    MPI_Info info  = MPI_INFO_NULL;

    /*
     * Initialize MPI
     */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);  
 
    /* 
     * Set up file access property list with parallel I/O access
     */
     plist_id = H5Pcreate(H5P_FILE_ACCESS);
     H5Pset_fapl_mpio(plist_id, comm, info);

    /*
     * Create a new file collectively.
     */
    file_id = H5Fcreate(out_file, H5F_ACC_TRUNC, H5P_DEFAULT, plist_id);

    /*
     * Close property list.
     */
    H5Pclose(plist_id);

    /*
     * Close the file.
     */
    H5Fclose(file_id);
 
    MPI_Finalize();

    return 0;
}     
