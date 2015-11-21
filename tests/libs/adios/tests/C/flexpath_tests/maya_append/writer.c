/**
 * writer.c
 *
 *  Created on: Aug 21, 2013
 *  Author: Magda Slawinska aka Magic Magg magg dot gatech at gmail.com
 */

#include "mpi.h"
#include "adios.h"
#include "adios_read.h"  // for adios_errno

#include "misc.h"
#include "test_common.h"
#include "utils.h"
#include "cfg.h"

#include <string.h>

// for printing the values of the variable
#define STR_BUFFER_SIZE 100

/**
 * wrapper for writes; this macro assumes existence fullpath
 * quite a few important variables; please take a look and be careful
 * how to use it
 *
 * I simulate Maya var name which consists of the attribute and the
 * grid function
 *
 * @param attribute The attribute (part of adios variable name or path)
 * @param grid_func_name The name of the grid function
 * @param var The var to be written out
 *
 */
#define WRITE_VAR(attribute, grid_func_name, var) \
	do { \
		sprintf(fullpath, "%s%s", attribute, grid_func_name); \
    	adios_write(adios_handle, fullpath, var); \
	} while (0)


int main(int argc, char ** argv){
	int rank = 0, size = 0;
	MPI_Comm comm = MPI_COMM_WORLD; // required for ADIOS
	int64_t adios_handle;   		// the ADIOS file handler
	int retval;

	struct adios_tsprt_opts adios_opts;
	int err_count = 0;

	GET_ENTRY_OPTIONS(adios_opts, "Runs writers. As many as you want to.");

	// ADIOS initialization
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &size);

	// From sources it just returns 1 (2013-07-16, whatever)
	adios_init_noxml(comm);

	// returns 0 (buffer allocated) or 1 (seems everything fine)
	// I guess size of the buffer in MB
	adios_allocate_buffer(ADIOS_BUFFER_ALLOC_NOW, ADS_BUFFER_SIZE);

	// this will hold the group id for all variables defined within this group
	int64_t	adios_grp = 0;

	// now declare a group and variables you will have in the group
	adios_declare_group(&adios_grp, GRP_NAME, "", adios_flag_no);

	uint64_t adios_groupsize = 0;

    adios_define_var (adios_grp, MAYA_GF_VAR_PFX, "/level", adios_integer, "1", "1", "0");
    adios_groupsize += 4;

    adios_define_var (adios_grp, MAYA_GF_VAR_PFX, "/scalar", adios_integer, "", "", "");
    adios_groupsize += sizeof(uint64_t);

    adios_define_var (adios_grp, MAYA_GF_VAR_PFX, "/cctk_bbox", adios_integer, "6", "6", "0" );
    adios_groupsize += (4 * 2 * 3);

    adios_define_var (adios_grp, MAYA_GF_VAR_PFX, "/data", adios_double, "11,12,13", "11,12,13", "0,0,0");
    adios_groupsize += (8 * 11 * 12 *13);


	SET_ERROR_IF_ZERO(adios_select_method(adios_grp, adios_opts.transport, "", ""), err_count);
	RET_IF_ERROR(err_count, rank);


    int i = 0;

    // these are values that will be written
    int level[TIMESTEP_COUNT];
    gen_1D_array_int(level, TIMESTEP_COUNT, rank);

    int cctk_bbox[6];
    gen_1D_array_int(cctk_bbox, 6, rank);

    double * data =  (double *)malloc(11*12*13*sizeof(double));
    gen_1D_array2(data, 11*12*13, rank);


    // required by WRITE_VAR macro (see the macro)
    char fullpath[STR_BUFFER_SIZE];

    // actually adios_write() doesn't write anything; the actual write
    // is performed on or after adios_close() so having
    // adios_open and adios_close out of the for loop will not produce
    // the desired effect i.e., appending to values
    for(i = 0; i < TIMESTEP_COUNT; ++i){
    	// open our group and transport method associated with it
    	adios_open (&adios_handle, GRP_NAME, FILE_NAME, "a", comm);

    	uint64_t adios_totalsize = 0;
    	retval=adios_group_size (adios_handle, adios_groupsize, &adios_totalsize);
    	fprintf(stderr, "Rank=%d adios_group_size(): adios_groupsize=%lld, adios_totalsize=%lld, retval=%d\n",
    				rank, adios_groupsize, adios_totalsize, retval);
    	WRITE_VAR("/level/", MAYA_GF_VAR_PFX, &level[i]);
    	WRITE_VAR("/scalar/", MAYA_GF_VAR_PFX, &level[i]);
    	WRITE_VAR("/cctk_bbox/", MAYA_GF_VAR_PFX, cctk_bbox);
    	WRITE_VAR("/data/", MAYA_GF_VAR_PFX, data);

    	fprintf(stderr, "Rank=%d committed write\n", rank);
    	adios_close(adios_handle);
    }

    free(data);
    data = NULL;
    adios_finalize(rank);
    MPI_Finalize();

	return 0;
}
