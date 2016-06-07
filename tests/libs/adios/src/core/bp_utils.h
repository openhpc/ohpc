/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#ifndef __BP_UTILS_H__
#define __BP_UTILS_H__

#include <stdio.h>
#include <sys/types.h>
#include "public/adios_read.h" // ADIOS_FILE*
#include "core/bp_types.h"
#define VARS_MINIHEADER_SIZE 10

BP_PROC * GET_BP_PROC (const ADIOS_FILE * fp);
BP_FILE * GET_BP_FILE (const ADIOS_FILE * fp);
void bp_alloc_aligned (struct adios_bp_buffer_struct_v1 * b, uint64_t size);
void bp_realloc_aligned (struct adios_bp_buffer_struct_v1 * b, uint64_t size);
int bp_get_endianness( uint32_t change_endianness );
int bp_parse_characteristics (struct adios_bp_buffer_struct_v1 * b,
                    struct adios_index_var_struct_v1 ** root,
                uint64_t j);
int bp_get_characteristics_data (void ** ptr_data,
                void * buffer,
                int data_size,
                enum ADIOS_DATATYPES type);
int bp_read_close (struct adios_bp_buffer_struct_v1 * b);
int bp_read_open (const char * filename,
        MPI_Comm comm,
        BP_FILE * fh);
MPI_File * get_BP_file_handle(struct BP_file_handle * l, uint32_t file_index);
void add_BP_file_handle (struct BP_file_handle ** l, struct BP_file_handle * n);
void close_all_BP_files (struct BP_file_handle * l);
int get_time (struct adios_index_var_struct_v1 * v, int step);
int bp_open (const char * fname,
             MPI_Comm comm,
             BP_FILE * fh);
ADIOS_VARINFO * bp_inq_var_byid (const ADIOS_FILE * fp, int varid);
int bp_close (BP_FILE * fh);
int bp_read_minifooter (BP_FILE * bp_struct);
int bp_parse_pgs (BP_FILE * fh);
int bp_parse_attrs (BP_FILE * fh);
int bp_parse_vars (BP_FILE * fh);
int bp_seek_to_step (ADIOS_FILE * fp, int tostep, int show_hidden_attrs);
int64_t get_var_start_index (struct adios_index_var_struct_v1 * v, int t);
int64_t get_var_stop_index (struct adios_index_var_struct_v1 * v, int t);

const char * bp_value_to_string (enum ADIOS_DATATYPES type, void * data);
int bp_get_type_size (enum ADIOS_DATATYPES type, void * var);
int bp_get_dimension_generic(const struct adios_index_characteristic_dims_struct_v1 *dims,
                                    uint64_t *ldims, uint64_t *gdims, uint64_t *offsets);
int bp_get_dimension_characteristics(struct adios_index_characteristic_struct_v1 *ch,
                                    uint64_t *ldims, uint64_t *gdims, uint64_t *offsets);
int bp_get_dimension_generic_notime (const struct adios_index_characteristic_dims_struct_v1 *dims,
                                     uint64_t *ldims, uint64_t *gdims, uint64_t *offsets,
                                     int file_is_fortran);
int bp_get_dimension_characteristics_notime (struct adios_index_characteristic_struct_v1 *ch,
                                            uint64_t *ldims, uint64_t *gdims, uint64_t *offsets,
                                            int file_is_fortran);
void bp_get_dimensions_generic(const ADIOS_FILE *fp, struct adios_index_var_struct_v1 *var_root, int file_is_fortran,
                               int *ndim, uint64_t **dims, int *nsteps, int use_pretransform_dimensions);
void bp_get_dimensions (const ADIOS_FILE *fp, struct adios_index_var_struct_v1 *var_root, int file_is_fortran,
                        int *ndim, uint64_t **dims, int *nsteps);
void bp_get_and_swap_dimensions_generic (const ADIOS_FILE *fp, struct adios_index_var_struct_v1 *var_root, int file_is_fortran,
                                         int *ndim, uint64_t **dims, int *nsteps, int swap_flag, int use_pretransform_dimensions);
void bp_get_and_swap_dimensions (const ADIOS_FILE *fp, struct adios_index_var_struct_v1 *var_root, int file_is_fortran,
                                 int *ndim, uint64_t **dims, int *nsteps, int swap_flag);
int get_var_nsteps (struct adios_index_var_struct_v1 * var_root);
int * get_var_nblocks (struct adios_index_var_struct_v1 * var_root, int nsteps);
void print_process_group_index (
                         struct adios_index_process_group_struct_v1 * pg_root
                         );
/* Return 1 if a < b wrt. the given type, otherwise 0 */
int adios_lt(int type, void *a, void *b);
double bp_value_to_double(enum ADIOS_DATATYPES type, void * data);
int is_fortran_file (BP_FILE * fh);
int has_subfiles (BP_FILE * fh);
struct adios_index_var_struct_v1 * bp_find_var_byid (BP_FILE * fh, int varid);
int is_global_array_generic (const struct adios_index_characteristic_dims_struct_v1 *dims); // NCSU ALACRITY-ADIOS
int is_global_array (struct adios_index_characteristic_struct_v1 * ch);
int check_bp_validity (const char * fname);
int get_num_subfiles (BP_FILE * fh);
#endif
