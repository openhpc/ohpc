/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

// mpi
#include "mpi.h"

#include "adios.h"

int main (int argc, char ** argv)
{
    char * type_name = "testbp";
    char * filename = "testbp_c.bp";
    int64_t io_handle;  // io handle
    MPI_Comm comm = MPI_COMM_WORLD;
    int rank;

    int dim1 = 10;   // dimension 1
    int dim2 = 7; 
    int dim3 = 6;
    int dim4 = 3;
    int dim5 = 5;
    int i,j,k,l,m; // loop counters for dims
    int time_start = 27; // arbitrary number to put into bp file


    int int_1D[dim1];
    int int_2D[dim1][dim2];
    int int_3D[dim1][dim2][dim3];
    int int_4D[dim1][dim2][dim3][dim4];
    int int_5D[dim1][dim2][dim3][dim4][dim5];

    int node = 0;

    uint64_t total;

    int vi, vj, vk, vl;
    for (i = 0; i < dim1; i++) {
        int_1D[i] = i;
        vi = i*dim2;
        // 2D arrays
        for (j = 0; j < dim2; j++) {
            int_2D[i][j] = vi + j;
            vj = (vi + j)*dim3;
            // 3D array
            for (k = 0; k < dim3; k++) {
                int_3D[i][j][k] = vj + k;
                vk = (vj+k)*dim4;
                // 4D array
                for (l = 0; l < dim4; l++) {
                    int_4D[i][j][k][l] = vk + l;
                    // 5D array
                    vl = (vk+l)*dim5;
                    for (m = 0; m < dim5; m++) {
                        int_5D[i][j][k][l][m] = vl + m;
                    }
                }
            }
        }
    }

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (comm, &rank);
    if (adios_init ("testbp_c.xml", comm))
        return -1;

    adios_open (&io_handle, type_name, filename, "w", comm);
    adios_group_size (io_handle, 32                             // 4 integers 
                                + 4 * dim1                      // int_1D
                                + 4 * dim1 * dim2               // int_2D
                                + 4 * dim1 * dim2 * dim3        // int_3D
                                + 4 * dim1 * dim2 * dim3 * dim4 // int_4D
                                + 4 * dim1 * dim2 * dim3 * dim4 * dim5// int_4D
                     ,&total
                     );
    adios_write (io_handle, "dim1", &dim1);
    adios_write (io_handle, "dim2", &dim2);
    adios_write (io_handle, "dim3", &dim3);
    adios_write (io_handle, "dim4", &dim4);
    adios_write (io_handle, "dim5", &dim5);

    adios_write (io_handle, "int_1D", int_1D);
    adios_write (io_handle, "int_2D", int_2D);
    adios_write (io_handle, "int_3D", int_3D);
    adios_write (io_handle, "int_4D", int_4D);
    adios_write (io_handle, "int_5D", int_5D);

    adios_write (io_handle, "time_start", &time_start);

    adios_close (io_handle);

    printf ("rank: %d write completed\n", rank);

    MPI_Barrier (MPI_COMM_WORLD);

    adios_finalize (node);
    MPI_Finalize ();

    return 0;
}
