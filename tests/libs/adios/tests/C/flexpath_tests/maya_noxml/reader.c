/**
 * reader.c
 *
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 *
 * Created on: Jul 19, 2013
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

/*
 * This is what the reader expects to read.
 *
 $ bpls test.bp
  unsigned integer    /P                              scalar
  unsigned integer    /patch_id                       scalar
  unsigned integer    /shape_dim_x                    scalar
  unsigned integer    /shape_dim_y                    scalar
  unsigned integer    /shape_dim_z                    scalar
  double              /data/maya_gf_var0              {27, 48, 89, 116}
  integer             /level/maya_gf_var0             {27}
  integer             /carpet_mglevel/maya_gf_var0    {27}
  integer             /timestep/maya_gf_var0          {27}
  integer             /group_timelevel/maya_gf_var0   {27}
  double              /time/maya_gf_var0              {27}
  integer             /cctk_bbox/maya_gf_var0         {27, 6}
  integer             /cctk_nghostzones/maya_gf_var0  {27, 3}
  double              /origin/maya_gf_var0            {27, 3}
  double              /delta/maya_gf_var0             {27, 3}
  integer             /iorigin/maya_gf_var0           {27, 3}
  unsigned long long  /shape/maya_gf_var0             {27, 3}
*/

// for printing the values of the variable
#define STR_BUFFER_SIZE 100

/**
 * wrapper for scheduling adios reads; this macro assumes existence
 * quite a few important variables; please take a look and be careful
 * how to use it
 *
 * @param path_str The path to the variable
 * @param out_buf  The output buffer
 */
#define READ_FULLPATH(path_str, out_buf) \
	sprintf(fullpath, "%s%s", path_str, fullname);  \
	SET_ERROR_IF_NOT_ZERO(adios_schedule_read(adios_handle, sel, fullpath,0,1, out_buf), error_counts.adios); \
	BREAK_IF_ERROR(error_counts.adios);



int main (int argc, char **argv){
	int rank =0;
	MPI_Comm comm = MPI_COMM_WORLD;
	diag_t diag = DIAG_OK;  // to store the diagnostic information
	struct test_info test_result = { TEST_PASSED, "maya_noxml" };
	struct err_counts error_counts = {0, 0};
	struct adios_tsprt_opts adios_opts;

	GET_ENTRY_OPTIONS(adios_opts, "Runs readers.");

	// adios read initialization
	MPI_Init( &argc, &argv);
	MPI_Comm_rank (comm, &rank);

	// depending on the method
	SET_ERROR_IF_NOT_ZERO(adios_read_init_method(adios_opts.method, comm, adios_opts.adios_options), error_counts.adios);
	RET_IF_ERROR(error_counts.adios, rank);

	// I will be working with streams so the lock mode is necessary,
	// return immediately if the stream unavailable
	ADIOS_FILE *adios_handle = adios_read_open(FILE_NAME, adios_opts.method, comm, ADIOS_LOCKMODE_NONE, 0.0);
	if ( !adios_handle){
		p_error("Quitting ... (%d) %s\n", adios_errno, adios_errmsg());
		return DIAG_ERR;
	}

	// now I will try to read what I got from the checkpoint
	// define portions of data how they will be read
	ADIOS_SELECTION *sel = NULL;
	ADIOS_VARINFO *avi = NULL;

	avi = adios_inq_var(adios_handle, "P");
	if (!avi){
		p_error("rank %d: Quitting ... (%d) %s\n", rank, adios_errno, adios_errmsg());
		CLOSE_ADIOS_READER(adios_handle, adios_opts.method);
		return DIAG_ERR;
	}

	if( GLOBAL_PATCH_COUNT != *(int*)avi->value){
		p_test_failed("%s: rank %d: global_patch_count (got %d)\n", test_result.name, rank,  *(int*)avi->value );
		test_result.result = TEST_FAILED;
		// clean everything
		adios_free_varinfo(avi);
		avi = NULL;
		CLOSE_ADIOS_READER(adios_handle, adios_opts.method);
		return DIAG_ERR;
	}

	// for holding the name of the maya variable
	char fullname[MAYA_VAR_BUF_SIZE];
	int i = 0;
	// for storing shape_dim_x, shape_dim_y, shape_dim_z
	int shape_max_dims[3];

	// space for the data (size will depend on the shape_dim_x, etc)
	double *data = NULL;
	uint32_t patch_id = 0;
	int32_t level = 0;
	int32_t carpet_mglevel = 0;
	int32_t timestep = 0;
	int32_t grp_tl = 0;
	double time_attr = 0.0;
	int32_t cctk_bbox[6];
	int32_t cctk_nghostzones[3];
	double origin[3];
	double delta[3];
	int32_t iorigin[3];
	uint64_t shape[3];

	// I think these are global variables, so shouldn't be a problem

	// first schedule reading of the entire variables (so NULL selection)
	// reading the patch_id doesn't make much sense as it seems that only the
	// very first value is written out so we will get 0
	SET_ERROR_IF_NOT_ZERO(adios_schedule_read(adios_handle, NULL, "patch_id",0,1, &patch_id), error_counts.adios);
	RET_AND_CLOSE_ADIOS_READER_IF_ERROR(error_counts.adios, rank, adios_handle, adios_opts.method);

	SET_ERROR_IF_NOT_ZERO(adios_schedule_read(adios_handle, NULL, "shape_dim_x",0,1, &shape_max_dims[0]), error_counts.adios);
	RET_AND_CLOSE_ADIOS_READER_IF_ERROR(error_counts.adios, rank, adios_handle, adios_opts.method);

	SET_ERROR_IF_NOT_ZERO(adios_schedule_read(adios_handle, NULL, "shape_dim_y",0,1, &shape_max_dims[1]), error_counts.adios);
	RET_AND_CLOSE_ADIOS_READER_IF_ERROR(error_counts.adios, rank, adios_handle, adios_opts.method);

	SET_ERROR_IF_NOT_ZERO(adios_schedule_read(adios_handle, NULL, "shape_dim_z",0,1, &shape_max_dims[2]), error_counts.adios);
	RET_AND_CLOSE_ADIOS_READER_IF_ERROR(error_counts.adios, rank, adios_handle, adios_opts.method);

	// not sure if this assumption is correct; difficult to find in the ADIOS sources
	SET_ERROR_IF_NOT_ZERO(adios_perform_reads(adios_handle, 1), error_counts.adios);
	RET_AND_CLOSE_ADIOS_READER_IF_ERROR(error_counts.adios, rank, adios_handle, adios_opts.method);

	// now test if what I read is what I supposed to get; see comment with
	// adios_schedule_read(patch_id)
	TEST_INT_EQUAL(0, patch_id, error_counts.test, test_result.result);
	RET_AND_CLOSE_ADIOS_READER_IF_TEST_FAILED(test_result, rank, adios_handle, adios_opts.method);

	TEST_INT_EQUAL(MAYA_SHAPE_MAX_X, shape_max_dims[0], error_counts.test, test_result.result);
	RET_AND_CLOSE_ADIOS_READER_IF_TEST_FAILED(test_result, rank, adios_handle, adios_opts.method);

	TEST_INT_EQUAL(MAYA_SHAPE_MAX_Y, shape_max_dims[1], error_counts.test, test_result.result);
	RET_AND_CLOSE_ADIOS_READER_IF_TEST_FAILED(test_result, rank, adios_handle, adios_opts.method);

	TEST_INT_EQUAL(MAYA_SHAPE_MAX_Z, shape_max_dims[2], error_counts.test, test_result.result);
	RET_AND_CLOSE_ADIOS_READER_IF_TEST_FAILED(test_result, rank, adios_handle, adios_opts.method);

	// so here I assume that our data buffer is fixed, and I allocate
	// space for that buffer, and consult the reader but the
	// size of the output data depends on the shape_max_dims that I have
	// already tested that I read as expected
	int data_size = 0;

	if (get_data_size(shape_max_dims, 3, &data_size) != DIAG_OK){
		RET_IF_ERROR(1, rank);
	}
	// just in case
	assert(shape_max_dims[0] * shape_max_dims[1] *shape_max_dims[2] * 8 == data_size);
	data = (double *) malloc(data_size);
	if( !data ){
		RET_IF_ERROR(1, rank);
	}

	// the data are the same for all variables so fill the reference data
	double * ref_data = (double *) malloc(data_size);
	if( !(ref_data) || (set_value(ref_data, shape_max_dims[0] * shape_max_dims[1] *shape_max_dims[2], (double) rank) != DIAG_OK) ){
		free(data);
		RET_IF_ERROR(1, rank);
	}


	// for storing the name of the variable
	char fullpath[STR_BUFFER_SIZE];
	char * levelpath = "/level/";
	char * datapath = "/data/";
	char * mglevelpath = "/carpet_mglevel/";
	char * timesteppath ="/timestep/";
	char * group_timelevelpath = "/group_timelevel/";
	char * timepath = "/time/";
	char * cbbpath = "/cctk_bbox/";
	char * cngzpath = "/cctk_nghostzones/";
	char * originpath = "/origin/";
	char * deltapath = "/delta/";
	char * ioriginpath = "/iorigin/";
	char * shapepath = "/shape/";

	// read all variables with attributes
	for(i = 0; i < MAYA_GRID_FUNC_COUNT; ++i){
		int k = 0;

		// clean the buffers to hold output data
		level = 0;
		carpet_mglevel = 0;
		timestep = 0;
		grp_tl = 0;
		time_attr = 0.0;
		for(k = 0 ; k < 3; ++k){
			cctk_bbox[k] = 0;
			cctk_nghostzones[k] = 0;
			origin[k] = 0;
			delta[k] = 0;
			iorigin[k] = 0;
			shape[k] = 0;
		}
		for(k =3; k < 6; ++k){
			cctk_bbox[k] = 0;
		}

		memset(data, 0, data_size);
		// generate the name of maya variable
		gen_maya_var_name(fullname, MAYA_VAR_BUF_SIZE, MAYA_GF_VAR_PFX, i);

		// now I need to play carefully with the selections, as ADIOS
		// does not provide any help if you messed up with selections

		uint64_t count_1D = 1;
		// this is assumed that the data are in this patch - we have to know
		// how it was written out i-th grid function was outputted in i
		uint64_t start_1D = i;

		sel = adios_selection_boundingbox(1, &start_1D, &count_1D );

		READ_FULLPATH(levelpath, &level);
		READ_FULLPATH(mglevelpath, &carpet_mglevel);
		READ_FULLPATH(timesteppath, &timestep);
		READ_FULLPATH(group_timelevelpath, &grp_tl);
		READ_FULLPATH(timepath, &time_attr);

		adios_selection_delete(sel);
		sel = NULL;

		// now more complex variables
		uint64_t count_2D[] = {1, 6};
		// this is assumed that the data are in this patch - we have to know
		// how it was written out i-th grid function was outputted in i-th patch
		// e.g., bpls -d test.bp -s"1,0" -c/cctk_bbox/maya_gf_var1 -n 12
		uint64_t start_2D[] = {i, 0};

		sel = adios_selection_boundingbox(2, start_2D, count_2D );

		READ_FULLPATH(cbbpath, cctk_bbox);

		adios_selection_delete(sel);
		sel = NULL;

		// now there is a deal with a 3 element vectors
		count_2D[1] = 3;   // this is a 3 elem vector

		sel = adios_selection_boundingbox(2, start_2D, count_2D );
		READ_FULLPATH(cngzpath, cctk_nghostzones);
		READ_FULLPATH(originpath, origin);
		READ_FULLPATH(deltapath, delta);
		READ_FULLPATH(ioriginpath, iorigin);
		READ_FULLPATH(shapepath, shape);

		adios_selection_delete(sel);
		sel = NULL;

		SET_ERROR_IF_NOT_ZERO(adios_perform_reads(adios_handle, 1), error_counts.adios);
		RET_AND_CLOSE_ADIOS_READER_IF_ERROR(error_counts.adios, rank, adios_handle, adios_opts.method);

		// now tests if I get what I expected to get
		TEST_INT_EQUAL(i, level, error_counts.test, test_result.result);
		BREAK_IF_ERROR(error_counts.test);

		TEST_INT_EQUAL(i%2, carpet_mglevel, error_counts.test, test_result.result);
		BREAK_IF_ERROR(error_counts.test);

		TEST_INT_EQUAL(26, timestep, error_counts.test, test_result.result);
		BREAK_IF_ERROR(error_counts.test);

		TEST_INT_EQUAL(i%3, grp_tl, error_counts.test, test_result.result);
		BREAK_IF_ERROR(error_counts.test);

		TEST_DOUBLE_EQUAL(13.0, time_attr, error_counts.test, test_result.result);
		BREAK_IF_ERROR(error_counts.test);

		for(k = 0; k < 6 ; ++k ){
			TEST_INT_EQUAL(13, cctk_bbox[k], error_counts.test, test_result.result);
			BREAK_IF_ERROR(error_counts.test);
		}
		BREAK_IF_ERROR(error_counts.test);

		uint64_t shape_ref[] = {MAYA_SHAPE_X, MAYA_SHAPE_Y, MAYA_SHAPE_Z};

		for(k = 0; k < 3; ++k){
			TEST_INT_EQUAL(14, cctk_nghostzones[k], error_counts.test, test_result.result);
			BREAK_IF_ERROR(error_counts.test);

			TEST_DOUBLE_EQUAL(15.0, origin[k], error_counts.test, test_result.result );
			BREAK_IF_ERROR(error_counts.test);

			TEST_DOUBLE_EQUAL(15.0, delta[k], error_counts.test, test_result.result );
			BREAK_IF_ERROR(error_counts.test);

			TEST_INT_EQUAL(14, iorigin[k], error_counts.test, test_result.result);
			BREAK_IF_ERROR(error_counts.test);

			TEST_LONG_EQUAL(shape_ref[k], shape[k], error_counts.test, test_result.result);
			BREAK_IF_ERROR(error_counts.test);
		}
		BREAK_IF_ERROR(error_counts.test);

		// now schedule and perform  reading the data; you have to know
		// the value of shape[] to read the correct slice of the /data space
		//  double              /data/maya_gf_var1              {27, 48, 89, 116}
		// the sizes are the maximum values; in Maya we assume that the
		// actual size are for this particular variable are stored in /shape attribute
		// so for this test case they are in shape[]; and hopefully
		// reading tests passed so I can use shape[] to define the appropriate
		// selection
		// now more complex variables
		uint64_t count_4D[4];
		count_4D[0] = 1; // 1 element in the patch dimension
		count_4D[1] = shape[0];
		count_4D[2] = shape[1];
		count_4D[3] = shape[2];
		// this is assumed that the data are in this patch - we have to know
		// how it was written out i-th grid function was outputted in i-th patch
		// e.g., bpls -d test.bp -s"1,0,0,0" -c"1,shape[0],shape[1],shape[2] /data/maya_gf_var1 -n 12
		uint64_t start_4D[] = {i, 0, 0, 0};

		sel = adios_selection_boundingbox(4, start_4D, count_4D );
		READ_FULLPATH(datapath, data);
		adios_selection_delete(sel);
		sel = NULL;

		SET_ERROR_IF_NOT_ZERO(adios_perform_reads(adios_handle, 1), error_counts.adios);
		RET_AND_CLOSE_ADIOS_READER_IF_ERROR(error_counts.adios, rank, adios_handle, adios_opts.method);

		// now compare what I got
		for(k = 0 ; k < shape[0] *shape[1] *shape[2]; ++k){
			TEST_DOUBLE_EQUAL(ref_data[k], data[k], error_counts.test, test_result.result);
			BREAK_IF_ERROR(error_counts.test);
		}
		BREAK_IF_ERROR(error_counts.test);
	}

	adios_free_varinfo(avi);
	avi = NULL;

	if (TEST_PASSED == test_result.result)
		p_test_passed("%s: rank %d\n", test_result.name, rank);

	CLOSE_ADIOS_READER(adios_handle, adios_opts.method);

	return diag;
}
