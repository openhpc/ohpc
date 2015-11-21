/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/*************************************************************/
/*          Example of reading arrays in ADIOS               */
/*    which were written from the same number of processors  */
/*                                                           */
/*        Similar example is manual/2_adios_read.c           */
/*************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "mpi.h"
#include "adios.h"
#include "adios_read.h"

int main (int argc, char ** argv) 
{
    int         rank, j;
    int         NX, NY; 
    double      *t;
    MPI_Comm    comm = MPI_COMM_WORLD;

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (comm, &rank);

    adios_read_init_method(ADIOS_READ_METHOD_FLEXPATH, comm, "");

    ADIOS_SELECTION global_range_select;
    //if(rank == 0){
    global_range_select.type=ADIOS_SELECTION_BOUNDINGBOX;
    global_range_select.u.bb.start = malloc(sizeof(uint64_t)*2);
    global_range_select.u.bb.count = malloc(sizeof(uint64_t)*2);
    (global_range_select.u.bb.start)[0] = 0;
    (global_range_select.u.bb.count)[0] = 4;
    (global_range_select.u.bb.start)[1] = 0;
    (global_range_select.u.bb.count)[1] = 40;
    global_range_select.u.bb.ndim = 2;

    ADIOS_SELECTION scalar_block_select;
    scalar_block_select.type = ADIOS_SELECTION_WRITEBLOCK;
    scalar_block_select.u.block.index = rank;
    //fprintf(stderr, "app got here\n");
    /* schedule_read of a scalar. */    
    int test_scalar = -1;
    ADIOS_FILE* afile = adios_read_open("arrays", 
                                         ADIOS_READ_METHOD_FLEXPATH, 
                                         comm,
                                         ADIOS_LOCKMODE_NONE, 0.0);
    
    int ii = 0;
    while(adios_errno != err_end_of_stream){
        
        /* get a bounding box - rank 0 for now*/
        ADIOS_VARINFO* nx_info = adios_inq_var( afile, "NX");
        ADIOS_VARINFO* ny_info = adios_inq_var( afile, "NY");

        if(nx_info->value) {
            NX = *((int *)nx_info->value);
        }
        if(ny_info->value){
            NY= *((int*)ny_info->value);
        }
    
        //printf("\trank=%d: NX=%d\n", rank, NX);
        //printf("\trank=%d: NY=%d\n", rank, NY);
    
        /* Allocate space for the arrays */
        int nelem = 160;
        int arr_size = sizeof(double) * nelem;
        t = (double *) malloc (arr_size);
        memset(t, 0, arr_size);
        //fprintf(stderr, "t %p\n", t);
      
        /* Read the arrays */        
        adios_schedule_read (afile, 
                             &global_range_select, 
                             "var_2d_array", 
                             0, 1, t);
	adios_schedule_read (afile,
			     &scalar_block_select,
			     "test_scalar",
			     0, 1, &test_scalar);

        adios_perform_reads (afile, 1);                
    
        //sleep(20);
    
        printf("Rank=%d: test_scalar: %d step: %d, t[0,5+x] = [%6.2f", rank, test_scalar, ii, t[0]);
        for(j=0; j<nelem; j++) {
            printf(", %6.2f", t[j]);
        }
        printf("]\n");
        adios_release_step(afile);
        adios_advance_step(afile, 0, 30);
        ii++;
        //MPI_Barrier (comm);
        //sleep(1);
    }
    //
    adios_read_close(afile);

    adios_read_finalize_method(ADIOS_READ_METHOD_FLEXPATH);

    //MPI_Finalize ();

    return 0;
}
