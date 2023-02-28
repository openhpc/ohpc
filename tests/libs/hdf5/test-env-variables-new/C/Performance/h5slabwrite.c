/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* This example shows different data writing patterns to generate data files
 * that will exhibit substantially different data read speed. The files
 * contain 2-D chunk storage datasets.
 * The 2 writing patterns are:
 * 1. Random--chunks are wriitten in the random order.
 * 2. ByRow--chunks are written by row order.
 */

#include <stdlib.h>
#include <string.h>
#include "hdf5.h"
#include "h5slab.h"

/* Write the chunks in the row order.  This provides good and bad read
 * performance if the read pattern is by row and by column respectivly.
 *
 * Created by Albert Cheng and Christian Chilan 2010/7/13.
 */
int
createfilebyrow(void)
{
    hid_t   file_id, dset_id, filespace, memspace, fapl, dxpl, dcpl;
    hsize_t dimsf[2], count[2], offset[2], chunk_dims[2]={CX, CY};
    char     *data, dataval, table[RC];               
    unsigned long i, j, l, cx;
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    dxpl = H5Pcreate(H5P_DATASET_XFER);
    H5Pset_chunk ( dcpl, 2, chunk_dims);
    fapl = dxpl = H5P_DEFAULT;
    file_id = H5Fcreate("row_alloc.h5", H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    dimsf[0] = NX;
    dimsf[1] = NY;
    filespace = H5Screate_simple(2, dimsf, NULL); 
    dset_id = H5Dcreate(file_id, "dataset1", H5T_NATIVE_CHAR, filespace,
			H5P_DEFAULT, dcpl, H5P_DEFAULT);
    count[0] = CX;
    count[1] = NY;
    memspace = H5Screate_simple(2, count, NULL);

    data = (char *)malloc(count[0]*count[1]*sizeof(char));
    
    /* writing the whole chunked rows each time. */
    for (l=0; l<RC; l++){

        offset[0] = l*CX;
        offset[1] = 0;

	/* fill with values according to row number */
	for (i=0; i<count[0]; i++)
	    for (j=0; j<count[1]; j++)
		data[i*count[1]+j]=l;

        H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL, count, NULL);
        H5Dwrite(dset_id, H5T_NATIVE_CHAR, memspace, filespace, dxpl, data);
    } 
 
    free(data);
    H5Dclose(dset_id);
    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Pclose(dxpl);
    H5Pclose(dcpl);
    H5Pclose(fapl);
    H5Fclose(file_id);
    return 0;
}

/* Write the chunks in a random pattern.  This provides a read performance
 * worse than when the chunks are written and read in the same order, whether
 * it is by row or by column.
 *
 * Created by Albert Cheng and Christian Chilan 2010/7/13.
 */
int
createfilerandom (void)
{
    hid_t   file_id, dset_id, filespace, memspace, fapl, dxpl, dcpl;
    hsize_t dimsf[2], count[2], offset[2], chunk_dims[2]={CX, CY};
    char     *data, table[RC][CC];               
    unsigned long i, j, cx, cy;
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    dxpl = H5Pcreate(H5P_DATASET_XFER);
    H5Pset_chunk ( dcpl, 2, chunk_dims);
    fapl = dxpl = H5P_DEFAULT;
    file_id = H5Fcreate("random_alloc.h5", H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    dimsf[0] = NX;
    dimsf[1] = NY;
    filespace = H5Screate_simple(2, dimsf, NULL); 
    dset_id = H5Dcreate(file_id, "dataset1", H5T_NATIVE_CHAR, filespace,
			H5P_DEFAULT, dcpl, H5P_DEFAULT);
    count[0] = CX;
    count[1] = CY;
    memspace = H5Screate_simple(2, count, NULL);
    data = (char *)malloc(count[0]*count[1]*sizeof(char));

    for (i=0; i<RC; i++)
        for (j=0; j<CC; j++)
            table[i][j]=0;

    for (i=0; i< RC*CC; i++) { 
        do{
            cx=rand()%RC;
            cy=rand()%CC;
        }while (table[cx][cy]);

        for (j=0; j < count[0]*count[1]; j++){
            data[j] = cx+cy;
        }

        table[cx][cy]=1;
        
        offset[0] = cx*CX;
        offset[1] = cy*CY;

        H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL, count, NULL);
        H5Dwrite(dset_id, H5T_NATIVE_CHAR, memspace, filespace, dxpl, data);
        
    } 
 
    free(data);
    H5Dclose(dset_id);
    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Pclose(dxpl);
    H5Pclose(dcpl);
    H5Pclose(fapl);
    H5Fclose(file_id);
    return 0;
}     

int main (int argc, char **argv) {
    createfilebyrow();
    createfilerandom();
}     

