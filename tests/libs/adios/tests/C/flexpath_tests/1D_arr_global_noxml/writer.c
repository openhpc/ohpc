/**
 * arrays_write.c
 *
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 *
 *  Created on: Jul 1, 2013
 *  Author: Magda Slawinska aka Magic Magg magg dot gatech at gmail.com
 *
 * This is an example of writing a 1D array of doubles. Each process
 * writes the NX sized array of doubles.
 */

#include "mpi.h"
#include "adios.h"
#include "adios_read.h"  // for adios_errno

#include "misc.h"
#include "utils.h"
#include "cfg.h"
#include "test_common.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>


int main(int argc, char ** argv){
	char filename[256];           // the name of the file to write data and compare with flexpath
	int  rank=0, size=0;
	int  NX = NX_DIM;             // size of 1D array we will write
	double t[NX_DIM];             // this will contain the variables to be tranpsorted
	MPI_Comm  comm = MPI_COMM_WORLD; // required for ADIOS

	int64_t 	adios_handle;   // the ADIOS file handler
	int retval;

	struct adios_tsprt_opts adios_opts;
	int err_count = 0;

	GET_ENTRY_OPTIONS(adios_opts, "Runs writers. It is recommended to run as many writers as readers.");


	// sanity check
	assert(NX==NX_DIM);

	// where I will write the data
	strcpy(filename, FILE_NAME);

	// ADIOS initialization
	MPI_Init(&argc, &argv);
	MPI_Comm_rank (comm, &rank);
	MPI_Comm_size (comm, &size);

	// From sources it just returns 1 (2013-07-16, whatever)
	adios_init_noxml(comm);

	// returns 0 (buffer allocated) or 1 (seems everything fine)
	// I guess size of the buffer in MB
	adios_allocate_buffer(ADIOS_BUFFER_ALLOC_NOW, 20);

	// this will hold the group id for all variables defined within this group
	int64_t	adios_grp;

	// now declare a group
	adios_declare_group(&adios_grp, "temperature", "iter", adios_flag_yes);

	SET_ERROR_IF_ZERO(adios_select_method(adios_grp, adios_opts.transport, "", ""), err_count);
	RET_IF_ERROR(err_count, rank);


	// I am defining here a global array - global bounds is the size
	// of global array for all writers; within that array there is
	// an offset from which each rank has its space
	int global_bounds= NX * size;
	int		offsets= NX * rank;


	adios_define_var (adios_grp, "NX", "", adios_integer, 0, 0, 0);
	adios_define_var (adios_grp, "size", "", adios_integer, 0, 0, 0);
	adios_define_var (adios_grp, "rank", "", adios_integer, 0, 0, 0);
	adios_define_var (adios_grp, "global_bounds", "", adios_integer, 0, 0, 0);
	adios_define_var (adios_grp, "offsets", "", adios_integer, 0, 0, 0);
	// NX - local var dimensions, global_bounds - global dimension, offsets - variable local offsets
	adios_define_var (adios_grp, "var_1d_array", "", adios_double, "NX", "global_bounds", "offsets");

	// open our group and transport method associated with it
	adios_open (&adios_handle, "temperature", FILE_NAME, "w", comm);
	// NX, size, rank, global_bounds, offsets,var_1d_array
	uint64_t adios_groupsize = 4 + 4 + 4 + 4 + 4 + NX * 8;
	uint64_t adios_totalsize = 0;

	retval=adios_group_size (adios_handle, adios_groupsize, &adios_totalsize);
	fprintf(stderr, "Rank=%d adios_group_size(): adios_groupsize=%lld, adios_totalsize=%lld, retval=%d\n",
				rank, adios_groupsize, adios_totalsize, retval);

	// init the array that I will transport over the sea
	if (gen_1D_array(t, NX, rank) == DIAG_ERR){
		printf("ERROR: Generating 1D array. Quitting ...\n");
		return DIAG_ERR;
	}

	// write; don't check errors for simplicity reasons
	adios_write(adios_handle, "NX", &NX);
	adios_write(adios_handle, "size", &size);
	adios_write(adios_handle, "rank", &rank);
	adios_write(adios_handle, "global_bounds", &global_bounds);
	adios_write(adios_handle, "offsets", &offsets);
	adios_write(adios_handle, "var_1d_array", t);

	fprintf(stderr, "Rank=%d committed write\n", rank);

	adios_close(adios_handle);

	// clean and finalize the system
	adios_finalize(rank);
	MPI_Finalize();

	return 0;
}

