/**
 * reader.c
 *
 *  Created on: Aug 21, 2013
 *  Author: Magda Slawinska aka Magic Magg magg dot gatech at gmail.com
 */

#include <string.h>
#include "mpi.h"
#include "adios.h"
#include "adios_read.h"

#include "misc.h"
#include "utils.h"
#include "test_common.h"
#include "cfg.h"


/**
 * wrapper for scheduling adios reads; this macro assumes existence of
 * quite a few important variables; please take a look and be careful
 * how to use it
 *
 * @param path_str The path to the variable
 * @param out_buf  The output buffer
 */
#define READ_FULLPATH(attribute, grid_func_name, out_buf) \
	char fullpath[STR_BUFFER_SIZE]; \
	sprintf(fullpath, "%s%s", attribute, grid_func_name);  \
	SET_ERROR_IF_NOT_ZERO(adios_schedule_read(adios_handle, sel, fullpath,0,10, out_buf), error_counts.adios);


// for printing the values of the variable
#define STR_BUFFER_SIZE 100

int main (int argc, char **argv){
	int rank =0;
	MPI_Comm comm = MPI_COMM_WORLD;
	struct err_counts err = {0, 0};
	struct test_info test_result = {TEST_PASSED, "maya_append"};
	struct adios_tsprt_opts adios_opts;

	GET_ENTRY_OPTIONS(adios_opts, "Runs readers. As many as you want to.");

	// adios read initialization
	MPI_Init( &argc, &argv);
	MPI_Comm_rank (comm, &rank);

	// depending on the method
	SET_ERROR_IF_NOT_ZERO(adios_read_init_method(adios_opts.method, comm, adios_opts.adios_options), err.adios);
	RET_IF_ERROR(err.adios, rank);


	// I will be working with streams so the lock mode is necessary,
	// return immediately if the stream unavailable
	ADIOS_FILE *adios_handle = adios_read_open_file(FILE_NAME, adios_opts.method, comm);
	if ( !adios_handle){
		p_error("Quitting ... (%d) %s\n", adios_errno, adios_errmsg());
		return DIAG_ERR;
	}

	int i = 0;
	// I will only support reading TIMESTEP_COUNT integers for the level value
	int level[TIMESTEP_COUNT];
	int level_scalar[TIMESTEP_COUNT];
	int cctk_bbox[TIMESTEP_COUNT * 6];
	double *data = malloc(8 * 11* 12*13 * TIMESTEP_COUNT);

	memset(level, 0, sizeof(int) * TIMESTEP_COUNT);
	memset(cctk_bbox, 0, sizeof(int) * TIMESTEP_COUNT*6);
	memset(data, 0, sizeof(double) * 11* 12*13 * TIMESTEP_COUNT);

	// selection should be NULL or as a single variable
	// just say that you want to take different steps
	adios_schedule_read(adios_handle, NULL, "/level/maya_gf_var",0,10, level);
	adios_schedule_read(adios_handle, NULL, "/scalar/maya_gf_var", 0, 10, level_scalar);
	adios_schedule_read(adios_handle, NULL, "/cctk_bbox/maya_gf_var", 0, 10, cctk_bbox);

	// now I will try to read with a single variable but with multiple steps
	ADIOS_SELECTION *sel = NULL;
	uint64_t start_3D[] = {0, 0, 0};
	uint64_t count_3D[] = {11, 12, 13};
	sel = adios_selection_boundingbox(3, start_3D, count_3D );
	adios_schedule_read(adios_handle, sel, "/data/maya_gf_var", 0, 10, data);
	adios_selection_delete(sel);
	sel = NULL;

	SET_ERROR_IF_NOT_ZERO(adios_perform_reads(adios_handle, 1), err.adios);

	int j = 0;
	if (err.adios){
		test_result.result = TEST_FAILED;
	} else {
		// reference data
		int level_ref[TIMESTEP_COUNT];
		gen_1D_array_int(level_ref, TIMESTEP_COUNT, rank);

		int cctk_bbox_ref[6];
		gen_1D_array_int(cctk_bbox_ref, 6, rank);

		double * data_ref =  (double *)malloc(11*12*13*sizeof(double));
		gen_1D_array2(data_ref, 11*12*13, rank);

		// compare with reference values
		for( i = 0; i < TIMESTEP_COUNT; ++i){
			TEST_INT_EQUAL(level_ref[i], level[i], err.test, test_result.result);
			BREAK_IF_ERROR(err.test);
			TEST_INT_EQUAL(level_ref[i], level_scalar[i], err.test, test_result.result);
			BREAK_IF_ERROR(err.test);

			for( j = 0; j < 6; ++j){
				TEST_INT_EQUAL(cctk_bbox_ref[j], cctk_bbox[i*6 + j], err.test, test_result.result);
				BREAK_IF_ERROR(err.test);
			}
			BREAK_IF_ERROR(err.test);

			for( j = 0; j < 11 * 12 * 13; ++j){
				TEST_DOUBLE_EQUAL(data_ref[j], data[i * 11 * 12 * 13 +j], err.test, test_result.result);
				BREAK_IF_ERROR(err.test);
			}
			BREAK_IF_ERROR(err.test);
		}

		free(data_ref);
		data_ref = NULL;
	}

	if (TEST_PASSED == test_result.result){
		p_test_passed("%s: rank %d\n", test_result.name, rank);
	}

	free(data);
	data = NULL;
	CLOSE_ADIOS_READER(adios_handle, adios_opts.method);

	return DIAG_OK;
}
