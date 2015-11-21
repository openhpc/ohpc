/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#ifndef __BP_TYPES_H__
#define __BP_TYPES_H__

#include "public/adios_types.h"
#include "core/adios_bp_v1.h"
#include "core/util.h" /* struct read_request */

#define BP_MAX_RANK 32
#define BP_MAX_NDIMS (BP_MAX_RANK+1)

struct bp_index_pg_struct_v1
{   
    char * group_name;
    enum ADIOS_FLAG adios_host_language_fortran;
    uint32_t process_id; 
    char * time_index_name;
    uint32_t time_index;
    uint64_t offset_in_file;

    struct bp_index_pg_struct_v1 * next;
};

struct bp_minifooter {
    uint64_t time_steps;  /* = fh->tidx_stop - fh->tidx_start + 1 */
    uint64_t pgs_count;
    uint64_t pgs_length;
    uint32_t vars_count;
    uint32_t attrs_count;
    uint64_t vars_length;
    uint64_t attrs_length;
    uint64_t pgs_index_offset;
    uint64_t vars_index_offset;
    uint64_t attrs_index_offset;
    uint32_t version;
    uint32_t change_endianness; // = enum ADIOS_FLAG, 0: unknown!, adios_flag_yes or adios_flag_no
    uint64_t file_size;
} __attribute__((__packed__));

struct BP_file_handle
{
    uint32_t file_index;
    MPI_File fh;
    struct BP_file_handle * next;
};

typedef struct BP_file_handle BP_file_handle_list;

typedef struct BP_FILE {
    MPI_File mpi_fh;
    char * fname; // Main file name is needed to calculate subfile names
    BP_file_handle_list * sfh; // This list links all the subfiles handle together
    MPI_Comm comm;
    struct adios_bp_buffer_struct_v1 * b;
    struct bp_index_pg_struct_v1 * pgs_root;
    struct adios_index_var_struct_v1 * vars_root;
    struct adios_index_attribute_struct_v1 * attrs_root;
    struct adios_index_var_struct_v1 ** vars_table; // To speed up vars_root lookup. Q. Liu, 12-2013.
    struct bp_minifooter mfooter; 
    struct BP_GROUP_VAR * gvar_h;
    struct BP_GROUP_ATTR * gattr_h;
    uint32_t tidx_start;
    uint32_t tidx_stop;
    void * priv;
} BP_FILE;

// save per proc info
typedef struct BP_PROC {
    BP_FILE * fh;
    int streaming;
    int * varid_mapping;
    read_request * local_read_request_list;
    void * b; //internal buffer for chunk reading
    void * priv;
} BP_PROC;

struct BP_GROUP_VAR {
    uint16_t group_count;
    uint16_t group_id; 
    char ** namelist;
    uint32_t *** time_index; 
    uint64_t * pg_offsets;
    char ** var_namelist;
    uint32_t * var_counts_per_group;
    uint64_t ** var_offsets;
};

struct BP_GROUP_ATTR {
    uint16_t group_count;
    uint16_t group_id;
    char ** namelist;
    char ** attr_namelist;
    uint32_t * attr_counts_per_group;
    uint64_t ** attr_offsets;
};

struct BP_GROUP {
    uint16_t group_id;
    uint16_t vars_offset; // start of variables belonging to this group in the list of variables from all groups; old read API used this
    uint32_t vars_count;
    uint16_t attrs_offset; // old read API, this group's attributes in the list of all groups' attrs 
    uint32_t attrs_count;
    struct BP_FILE * fh;
    struct adios_index_var_struct_v1 * vars_root;  /* pointer into the list of BP_FILE.vars_root */
    struct adios_index_attribute_struct_v1 * attrs_root;
};

#endif
