/**
 * writer.c
 *
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 *
 *  Created on: Jul 19, 2013
 *  Author: Magda Slawinska aka Magic Magg magg dot gatech at gmail.com
 *
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

// for printing the values of the variable
#define STR_BUFFER_SIZE 100

/**
 * wrapper for writes; this macro assumes existence
 * quite a few important variables; please take a look and be careful
 * how to use it
 *
 * @param path_str The path to the variable
 * @param variable  The var to be written out
 */
#define WRITE_FULLPATH(path_str, var) \
	sprintf(fullpath, "%s%s", path_str, fullname); \
	adios_write(adios_handle, fullpath, var);


int main(int argc, char ** argv){
	int  rank=0, size=0;
	MPI_Comm  comm = MPI_COMM_WORLD; // required for ADIOS
	int64_t 	adios_handle;   // the ADIOS file handler
	int retval;
	struct adios_tsprt_opts adios_opts;
	int err_count = 0;

	GET_ENTRY_OPTIONS(adios_opts, "Runs writers.");

	// ADIOS initialization
	MPI_Init(&argc, &argv);
	MPI_Comm_rank (comm, &rank);
	MPI_Comm_size (comm, &size);

	// From sources it just returns 1 (2013-07-16, whatever)
	adios_init_noxml(comm);

	// returns 0 (buffer allocated) or 1 (seems everything fine)
	// I guess size of the buffer in MB
	adios_allocate_buffer(ADIOS_BUFFER_ALLOC_NOW, ADS_BUFFER_SIZE);

	// this will hold the group id for all variables defined within this group
	int64_t	adios_grp = 0;

	// now declare a group
	adios_declare_group(&adios_grp, "carpet_checkpoint", "", adios_flag_no);

	uint64_t adios_groupsize = 0;

    adios_define_var (adios_grp, "P", "", adios_unsigned_integer, "", "", "");
    adios_groupsize += sizeof(uint64_t);

    int i = 0;
    // the dimension; for grid functions it should be 3, for scalars should be 1
    int dim = 0;
    // for holding the name of the maya variable
    char fullname[MAYA_VAR_BUF_SIZE];
    // the shape values
    // the max box across all patches
    int max_shape[3] = {MAYA_SHAPE_MAX_X, MAYA_SHAPE_MAX_Y, MAYA_SHAPE_MAX_Z};
    // the actual shape for a particular patch
    int shape[3] = {MAYA_SHAPE_X, MAYA_SHAPE_Y, MAYA_SHAPE_Z};
    // the size of the data I intend to write as a meat for the checkpoint
    int data_size = 0;
    if (get_data_size(shape, 3, &data_size) != DIAG_OK){
    	p_error("Quitting ...\n");
    	return DIAG_ERR;
    }
    // you need to provide it for the defining the ADIOS group
    int max_data_size = 0;
    if (get_data_size(max_shape, 3, &max_data_size) != DIAG_OK){
    	p_error("Quitting ...\n");
    	return DIAG_ERR;
    }

    dim = 3;

    // first define variable, since I am using no XML api
    for(i = 0; i < MAYA_GRID_FUNC_COUNT; ++i ){
    	// this is common for grid functions and scalars
        adios_define_var (adios_grp, "patch_id", "", adios_unsigned_integer, "", "", "");
        adios_groupsize += sizeof (uint64_t);

        adios_define_var (adios_grp, "shape_dim_x", "", adios_unsigned_integer, "", "", "");
        adios_define_var (adios_grp, "shape_dim_y", "", adios_unsigned_integer, "", "", "");
        adios_define_var (adios_grp, "shape_dim_z", "", adios_unsigned_integer, "", "", "");
        adios_groupsize += 3*sizeof (uint64_t); // can be bigger

        // I simulate writing grid functions
        char * dimensions="1,shape_dim_x,shape_dim_y,shape_dim_z";
        // global dimensions should be, I suppose greater than shap_dim_xxx
        // i.e., shape_dim_x <= 48, shape_dim_y <= 89, shape_dim_z <= 116
        char * global_dimensions=GLOBAL_DIMENSIONS;
        char * offsets="patch_id,0,0,0";
        // the name of maya variable
        memset(fullname, 0, MAYA_VAR_BUF_SIZE);
        sprintf(fullname, MAYA_GF_VAR_PFX "%d", i );

        // I think this is for dataset size for doubles
        adios_groupsize += max_data_size;
        adios_define_var (adios_grp, fullname, "/data", adios_double, dimensions, global_dimensions, offsets);

        adios_groupsize += (4 + 4 + 4 + 4 + 8);
        adios_define_var (adios_grp, fullname, "/level", adios_integer, "1", "P", "patch_id") ;
        adios_define_var (adios_grp, fullname, "/carpet_mglevel", adios_integer, "1", "P", "patch_id");
        adios_define_var (adios_grp, fullname, "/timestep", adios_integer, "1", "P", "patch_id");
        adios_define_var (adios_grp, fullname, "/group_timelevel", adios_integer, "1", "P", "patch_id");
        adios_define_var (adios_grp, fullname, "/time", adios_double, "1", "P", "patch_id");

        char ndim[16];
        char global_dims[18];
        char local_offsets[25];

        adios_groupsize += (4 * 2 * dim);
        sprintf(ndim, "1,%d", 2 * dim);
        sprintf(global_dims, "P,%d", 2 * dim);
        sprintf(local_offsets, "patch_id,0");
        adios_define_var(adios_grp, fullname, "/cctk_bbox", adios_integer, ndim, global_dims, local_offsets);

        adios_groupsize += (4 * dim);
        sprintf(ndim, "1,%d", dim);
        sprintf(global_dims, "P,%d", dim);
        adios_define_var(adios_grp, fullname, "/cctk_nghostzones",adios_integer, ndim, global_dims, local_offsets);

        adios_groupsize += (8 * dim);
        sprintf(ndim, "1,%d", dim);
        sprintf(global_dims, "P,%d", dim);
        adios_define_var(adios_grp, fullname, "/origin", adios_double, ndim, global_dims, local_offsets);

        adios_groupsize += (8 * dim);
        adios_define_var(adios_grp, fullname, "/delta", adios_double, ndim, global_dims, local_offsets);

        adios_groupsize += (4 * dim);
        adios_define_var(adios_grp, fullname, "/iorigin", adios_integer, ndim, global_dims, local_offsets);

        adios_groupsize += (sizeof(uint64_t) * dim);
        adios_define_var(adios_grp, fullname, "/shape", adios_unsigned_long,ndim, global_dims, local_offsets);
    }

    SET_ERROR_IF_ZERO(adios_select_method(adios_grp, adios_opts.transport, "", ""), err_count);
    RET_IF_ERROR(err_count, rank);

	// open our group and transport method associated with it
	adios_open (&adios_handle, "carpet_checkpoint", FILE_NAME, "w", comm);
	uint64_t adios_totalsize = 0;

	retval=adios_group_size (adios_handle, adios_groupsize, &adios_totalsize);
	fprintf(stderr, "Rank=%d adios_group_size(): adios_groupsize=%lld, adios_totalsize=%lld, retval=%d\n",
				rank, adios_groupsize, adios_totalsize, retval);

	printf("Writing checkpoint to file %s using the %s method: group size is %llu, total size is %llu. ", FILE_NAME, adios_opts.transport, adios_groupsize, adios_totalsize);


    // arbitrary, but this is what I am getting from Maya
	// the number of patches I want to write
    const int global_patch_count = GLOBAL_PATCH_COUNT;

	adios_write(adios_handle, "P", (void*)&global_patch_count);

	// now goes adWRiteGroupVar

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

    assert(shape[0] * shape[1] *shape[2] * 8 == data_size);
    assert(max_shape[0] * max_shape[1] * max_shape[2] * 8 == max_data_size);

    // generate data that I will send over the ocean
    // TODO for some reason it can't be shape[] but max_shape; probably
    // the definition of the ADIOS /data var requires the all bytes;
    // otherwise it tries to copy some other data
    double * my_data = (double *) malloc(max_data_size);
    if( set_value(my_data, max_shape[0] * max_shape[1] *max_shape[2], (double) rank) != DIAG_OK ){
    	p_error("with generating data. Quitting\n");
    	return DIAG_ERR;
    }

    int my_patch_index = 0;


    for(i = 0; i < MAYA_GRID_FUNC_COUNT; ++i){

    	// generate the name of maya variable
    	gen_maya_var_name(fullname, MAYA_VAR_BUF_SIZE, MAYA_GF_VAR_PFX, i);

    	// the purpose of writing this variable is to enable
    	// ADIOS to output the rest of variables in the correct place
    	// however, if I try to read the value of patch_id e.g. with
    	// bpls -d /patch_id, I can see only scalar with value 0
    	// if you try to read the /level attribute without changing
    	// patch_id (i.e., keeping it at value equal 0), the level
    	// is always outputed in the same place so I guess the key thing
    	// is adios_define_var("/level", adios_integer, "1", "P", "patch_id")
    	// and that's why if you want to see level for different patch
    	// you need to have adios_write(patch_id) here; not above the
    	// for loop
    	// the idea was to write each variable with a designated different patch
    	adios_write (adios_handle, "patch_id", &my_patch_index);
    	my_patch_index++;
    	my_patch_index %= global_patch_count;

    	// these max shape dims; this also tests if we can rewrite the
    	// same variable over and over; as we did this for the Maya
    	adios_write (adios_handle, "shape_dim_x", &max_shape[0]) ;
    	adios_write (adios_handle, "shape_dim_y", &max_shape[1]) ;
    	adios_write (adios_handle, "shape_dim_z", &max_shape[2]) ;

    	// Write the data
    	WRITE_FULLPATH(datapath, my_data);

    	int refinementlevel=i;
    	WRITE_FULLPATH(levelpath, &refinementlevel);

    	int mglevel = i%2;
    	WRITE_FULLPATH(mglevelpath, &mglevel);

    	int ts = 26;
    	WRITE_FULLPATH(timesteppath, &ts);

    	int grp_tl = i%3;
    	WRITE_FULLPATH(group_timelevelpath, &grp_tl);

    	double time_attr = 13.0;
    	WRITE_FULLPATH(timepath, &time_attr);

    	int dim_6_int[6] = {13,13,13,13,13,13};
    	int dim_3_int[3] = {14,14,14};
    	double dim_3_double[3] = {15.0,15.0,15.0 };
    	uint64_t dim_3_uint64_t[3] = {MAYA_SHAPE_X, MAYA_SHAPE_Y, MAYA_SHAPE_Z};

    	WRITE_FULLPATH(cbbpath, dim_6_int);
    	WRITE_FULLPATH(cngzpath, dim_3_int);
    	WRITE_FULLPATH(originpath, dim_3_double);
    	WRITE_FULLPATH(deltapath, dim_3_double);
    	WRITE_FULLPATH(ioriginpath, dim_3_int);

    	// this is the size of adios_unsigned_long @see adios_get_type_size()
    	assert(sizeof(uint64_t) == 8);
    	WRITE_FULLPATH(shapepath, dim_3_uint64_t);
    }

	fprintf(stderr, "Rank=%d committed write\n", rank);

	free(my_data);
	my_data = NULL;

	adios_close(adios_handle);

	// clean and finalize the system
	adios_finalize(rank);
	MPI_Finalize();

	printf("\n");

	return 0;
}

