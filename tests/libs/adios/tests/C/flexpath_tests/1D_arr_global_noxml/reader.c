/**
 * arrays_read.c
 *
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 *
 * Created on: Jul 1, 2013
 * Author: Magda Slawinska aka Magic Magg magg dot gatech at gmail.com
 *
 * This is a test for the FlexPath method based on examples/C/flexpath_arrays
 */

#include "mpi.h"
#include "adios.h"
#include "adios_read.h"

#include "misc.h"
#include "utils.h"
#include "test_common.h"
#include "cfg.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>


// for printing the values of the variable
#define STR_BUFFER_SIZE 3000



int main (int argc, char **argv){
	int rank =0, size =0;
	int NX = 0;
	double *t = NULL;
	// this is an array we expect as a reference array
	double *t_ref = NULL;
	MPI_Comm comm = MPI_COMM_WORLD;
	diag_t diag = DIAG_OK;  // to store the diagnostic information
	struct test_info test_result = {TEST_PASSED, "1D_arr_global_noxml"};
	struct err_counts err = { 0, 0};
	struct adios_tsprt_opts adios_opts;

	GET_ENTRY_OPTIONS(adios_opts,  "Runs readers. It is recommended to run as many readers as writers.");

	// adios read initialization
	MPI_Init( &argc, &argv);
	MPI_Comm_rank (comm, &rank);

	// depending on the method
	SET_ERROR_IF_NOT_ZERO(adios_read_init_method(adios_opts.method, comm, adios_opts.adios_options), err.adios);
	RET_IF_ERROR(err.adios, rank);


	// I will be working with streams so the lock mode is necessary,
	// return immediately if the stream unavailable
	ADIOS_FILE *adios_handle = adios_read_open(FILE_NAME, adios_opts.method, comm, ADIOS_LOCKMODE_NONE, 0.0);
	if ( !adios_handle){
		p_error("Quitting ... (%d) %s\n", adios_errno, adios_errmsg());
		return DIAG_ERR;
	}

	// define portions of data how they will be read
	ADIOS_SELECTION *sel = NULL;
	ADIOS_VARINFO *avi = NULL;


	// for storing the variables
	char buf[STR_BUFFER_SIZE];

	int step = 0;

	// read how many processors wrote that array
	avi = adios_inq_var (adios_handle, "size");
	if (!avi){
		p_error("rank %d: Quitting ... (%d) %s\n", rank, adios_errno, adios_errmsg());
		diag = DIAG_ERR;
		goto close_adios;
	}
	size = *((int*)avi->value);
	adios_free_varinfo(avi);
	avi = NULL;

	// if I run the more readers than writers; just release
	// the excessive readers
	if (rank >= size){
		p_info("rank %d: I am an excessive rank. Nothing to read ...\n", rank);
		diag = DIAG_OK;
		goto close_adios;
	}

	// read the size of the array
	avi = adios_inq_var (adios_handle, "NX");
	if (!avi){
		p_error("rank %d: Quitting ... (%d) %s\n", rank, adios_errno, adios_errmsg());
		diag = DIAG_ERR;
		goto close_adios;
	}

	// I expect a scalar that will tell me the size of an array
	assert(0 == avi->ndim);
	assert(adios_integer == avi->type);
	NX = *((int*)avi->value);
	// I don't need that variable any more
	adios_free_varinfo(avi);
	assert(NX_DIM == NX);
	avi = NULL;


	// this will define the slice that we want to read; each rank should
	// read its own slice written by a corresponding writer rank
	uint64_t count[1] = { NX };
	uint64_t start[1] = { 0 };
	start[0] = rank*NX;

	sel = adios_selection_boundingbox(1,start, count);
	if( !sel ){
		p_error("rank %d: Quitting ... (%d) %s\n", rank, adios_errno, adios_errmsg());
		diag = DIAG_ERR;
		goto close_adios;
	}

	// make the reference array with reference values I expect to get
	t_ref = calloc(NX, sizeof(double));
	if (gen_1D_array(t_ref, NX, rank) == DIAG_ERR){
		p_error("Generating 1D array. Quitting ...\n");
		diag = DIAG_ERR;
		goto close_adios;
	}

	// allocate the memory for the actual array to be read
	t = calloc(NX, sizeof(double));

	if (adios_schedule_read(adios_handle, sel, "var_1d_array",0,1,t) != 0){
		p_error("rank %d: Quitting ...(%d) %s\n", rank, adios_errno, adios_errmsg());
		diag = DIAG_ERR;
		goto just_clean;
	}

	// not sure if this assumption is correct; difficult to find in the ADIOS sources
	if (adios_perform_reads(adios_handle, 1) != 0){
		p_error("rank %d: Quitting ...(%d) %s\n", rank, adios_errno, adios_errmsg());
		diag = DIAG_ERR;
		goto just_clean;
	}

	sprintf(buf, "Rank %d: var_1d_array: step %d: t: ", rank, step);

	int i = 0;
	for(i=0; i < NX; ++i){
		if( t[i] != t_ref[i] ){
			p_test_failed("%s: rank %d: for t[%d] (expected %.1f, got %.1f)\n", test_result.name, rank,  i, t_ref[i], t[i] );
			test_result.result = TEST_FAILED;
			break;
		}
	}

	if (TEST_PASSED == test_result.result)
		p_test_passed("%s: rank %d\n", test_result.name, rank);

just_clean:
	// clean everything
	adios_selection_delete(sel);
	sel = NULL;
	free(t);
	t = NULL;
	free(t_ref);
	t_ref = NULL;

close_adios:
	CLOSE_ADIOS_READER(adios_handle, adios_opts.method);

	if ((DIAG_OK == diag) && (TEST_PASSED == test_result.result)) {
		return 0;
	} else {
		return 1;
	}
}
