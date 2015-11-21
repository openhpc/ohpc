/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <stdlib.h>
#include <string.h>
#include "hdf5.h"
#include "h5slab.h"

/* Read the chunks in a row pattern.
 *
 * Created by Albert Cheng and Christian Chilan 2010/7/13.
 */
int main (int argc, char **argv) {
    hid_t   file_id, dset_id, filespace, memspace, fapl, dxpl;
    hsize_t dimsf[2], count[2], offset[2], chunk_dims[2]={CX, CY};
    char    *data, table[RC];               
    unsigned long i, j, cx;

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    dxpl = H5Pcreate(H5P_DATASET_XFER);
    fapl = dxpl = H5P_DEFAULT;
    file_id = H5Fopen(argv[1], H5F_ACC_RDONLY, fapl);
    dset_id = H5Dopen(file_id, "dataset1", H5P_DEFAULT);
    filespace = H5Dget_space(dset_id); 
    count[0] = CX;
    count[1] = NY;
    memspace = H5Screate_simple(2, count, NULL);

    data = (char *)malloc(count[0]*count[1]*sizeof(char));
    for (i=0; i<RC; i++){
        offset[0] = i*CX;
        offset[1] = 0;
        H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL, count, NULL);
        H5Dread(dset_id, H5T_NATIVE_CHAR, memspace, filespace, dxpl, data);
    
    }
    free(data);
    H5Dclose(dset_id);
    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Pclose(dxpl);
    H5Pclose(fapl);
    H5Fclose(file_id);
    return 0;
}     

