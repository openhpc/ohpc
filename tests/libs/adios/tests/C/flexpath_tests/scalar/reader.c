/**
 * reader.c
 *
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 *
 * Created on: Jul 1, 2013
 * Author: Magda Slawinska aka Magic Magg magg dot gatech at gmail.com
 *
 * The process reads an integer scalar. It should be equal to its rank.
 * Excessive processes (if rank > the number of writers) quit. It is recommended,
 * however, to run as many readers as writers.
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



int main (int argc, char **argv){
	int rank =0, size =0;
	int my_scalar = -1;         // this will hold what I got from the writer
	MPI_Comm comm = MPI_COMM_WORLD;
	diag_t diag = DIAG_OK;  // to store the diagnostic information
	struct test_info test_result = { TEST_PASSED, "scalar" };
	struct err_counts err = { 0, 0};
	struct adios_tsprt_opts adios_opts;

	GET_ENTRY_OPTIONS(adios_opts, "Runs readers. It is recommended to run as many readers as writers.");

	// adios read initialization
	MPI_Init( &argc, &argv);
	MPI_Comm_rank (comm, &rank);

	// choose the right method depending on the method
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

	// if I run more readers than writers; just release
	// the excessive readers
	if (rank >= size){
		p_info("rank %d: I am an excessive rank. Nothing to read ...\n", rank);
		// diag should be DIAG_OK
		goto close_adios;
	}

	// this is the index of the written block
	sel = adios_selection_writeblock(rank);
	if( !sel ){
		p_error("rank %d: Quitting ... (%d) %s\n", rank, adios_errno, adios_errmsg());
		diag = DIAG_ERR;
		goto close_adios;
	}

	// TODO as of 2013-07-08, I was told that err_end_of_stream doesn't work
	// as it supposed to work
	//while(adios_errno != err_end_of_stream){

		if (adios_schedule_read(adios_handle, sel, "lucky_scalar",0,1,&my_scalar) != 0){
			p_error("rank %d: Quitting ...(%d) %s\n", rank, adios_errno, adios_errmsg());
			adios_selection_delete(sel);
			sel = NULL;
			CLOSE_ADIOS_READER(adios_handle, adios_opts.method);
			return DIAG_ERR;
		}

		// not sure if this assumption is correct; difficult to find in the ADIOS sources
		if (adios_perform_reads(adios_handle, 1) != 0){
			p_error("rank %d: Quitting ...(%d) %s\n", rank, adios_errno, adios_errmsg());
			adios_selection_delete(sel);
			sel = NULL;
			CLOSE_ADIOS_READER(adios_handle, adios_opts.method);
			return DIAG_ERR;
		}

		if( rank == my_scalar){
			p_test_passed("%s: rank %d\n", test_result.name, rank);
			test_result.result = TEST_PASSED;
		} else {
			p_test_failed("%s: rank %d: my_scalar=%d. (rank != my_scalar)\n", test_result.name, rank,  my_scalar);
			test_result.result = TEST_FAILED;
		}
	//}

		// clean everything
		adios_selection_delete(sel);
		sel = NULL;
close_adios:
		CLOSE_ADIOS_READER(adios_handle, adios_opts.method);

	return diag;
}
