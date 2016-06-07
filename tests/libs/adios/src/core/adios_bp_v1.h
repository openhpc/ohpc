/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#ifndef ADIOS_BP_V1_H
#define ADIOS_BP_V1_H
#include "stdint.h"
#include "unistd.h"
#include "public/adios_types.h"
#include "core/adios_transport_hooks.h"
#include "core/qhashtbl.h"

#define ADIOS_VERSION_BP_FORMAT                      2
#define ADIOS_VERSION_NUM_MASK                       0x000000FF
#define ADIOS_VERSION_HAVE_SUBFILE                   0x00000100
#define ADIOS_VERSION_HAVE_TIME_INDEX_CHARACTERISTIC 0x00000200
enum ADIOS_CHARACTERISTICS
{
     adios_characteristic_value          = 0
    ,adios_characteristic_min            = 1 // This is no longer used. Used to read in older bp file format
    ,adios_characteristic_max            = 2 // This is no longer used. Used to read in older bp file format
    ,adios_characteristic_offset         = 3
    ,adios_characteristic_dimensions     = 4
    ,adios_characteristic_var_id         = 5
    ,adios_characteristic_payload_offset = 6
    ,adios_characteristic_file_index     = 7
    ,adios_characteristic_time_index     = 8
    ,adios_characteristic_bitmap         = 9
    ,adios_characteristic_stat           = 10
    ,adios_characteristic_transform_type = 11
};

#ifndef ADIOS_STAT_LENGTH
    #define ADIOS_STAT_LENGTH 7 
#endif

// NCSU - Adding statistics
enum ADIOS_STAT
{
     adios_statistic_min             = 0
    ,adios_statistic_max             = 1
    ,adios_statistic_cnt             = 2 
    ,adios_statistic_sum             = 3 
    ,adios_statistic_sum_square      = 4 
    ,adios_statistic_hist            = 5 
    ,adios_statistic_finite          = 6 
};

struct adios_bp_buffer_struct_v1
{
    int f;             // the file handle
    uint64_t file_size;
    uint32_t version;

    char * allocated_buff_ptr;  // initial alloc for aligning on 8-byte boundary

    char * buff;
    uint64_t length;
    uint64_t offset;   // buffer_offset

    enum ADIOS_FLAG change_endianness;

    off_t file_offset;
    uint64_t end_of_pgs;          // where the last process group ends
    uint64_t pg_index_offset;     // process groups index starts
    uint64_t pg_size;             // process groups index size
    uint64_t vars_index_offset;   // vars index start
    uint64_t vars_size;           // vars index size
    uint64_t attrs_index_offset;  // attributes index start
    uint64_t attrs_size;          // attributes index size

    uint64_t read_pg_offset;
    uint64_t read_pg_size;
};

struct adios_index_process_group_struct_v1
{
    char * group_name;
    enum ADIOS_FLAG adios_host_language_fortran;
    uint32_t process_id;
    char * time_index_name;
    uint32_t time_index;
    uint64_t offset_in_file;

    struct adios_index_process_group_struct_v1 * next;
};

struct adios_index_characteristic_dims_struct_v1
{
    uint8_t count;
    uint64_t * dims;  // each 3 uint64_t represents one dimension (l, g, o)
};

// NCSU - Generic data for all statistics
struct adios_index_characteristics_stat_struct
{
    void * data;
};

// NCSU - Structure for histogram
struct adios_index_characteristics_hist_struct
{
    double min; //minimum value of histogram ** for when we use complex variables
    double max; //maximum value of histogram
    uint32_t num_breaks; //number of break points for the histogram
    uint32_t * frequencies; //array of frequencies for the histogram
    double * breaks; //breaks array for the histogram, output this to gnuplot
};


struct adios_index_characteristic_transform_struct {
    uint8_t transform_type;

    enum ADIOS_DATATYPES pre_transform_type;
    struct adios_index_characteristic_dims_struct_v1 pre_transform_dimensions;

    uint16_t transform_metadata_len;
    void *transform_metadata;
};

struct adios_index_characteristic_struct_v1
{
    uint64_t offset;  // beginning of the var or attr entry
    struct adios_index_characteristic_dims_struct_v1 dims;
    uint32_t var_id;
    void * value;
    uint64_t payload_offset;   // beginning of the var or attr payload
    uint32_t file_index;  // subfile index
    uint32_t time_index;

    uint32_t bitmap;

    struct adios_index_characteristics_stat_struct ** stats;

    // NCSU ALACRITY-ADIOS - Adding transform-related fields
    /*
    uint8_t transform_type;
    enum ADIOS_DATATYPES pre_transform_type;
    struct adios_index_characteristic_dims_struct_v1 pre_transform_dimensions;
    uint16_t transform_metadata_len;
    void *transform_metadata;
    */
    struct adios_index_characteristic_transform_struct transform;
};

struct adios_index_var_struct_v1
{
    uint32_t id;
    char * group_name;
    char * var_name;
    char * var_path;
    enum ADIOS_DATATYPES type;

    uint64_t characteristics_count;
    uint64_t characteristics_allocated;

    struct adios_index_characteristic_struct_v1 * characteristics;

    struct adios_index_var_struct_v1 * next;
};

struct adios_index_attribute_struct_v1
{
    uint32_t id;
    char * group_name;
    char * attr_name;
    char * attr_path;
    enum ADIOS_DATATYPES type;

    uint64_t characteristics_count;
    uint64_t characteristics_allocated;

    struct adios_index_characteristic_struct_v1 * characteristics;

    struct adios_index_attribute_struct_v1 * next;
};

/* Struct to hold the 3 main pointers of the index:
 * group, variable and attribute indices
 */
struct adios_index_struct_v1
{
    struct adios_index_process_group_struct_v1 * pg_root;
    struct adios_index_var_struct_v1           * vars_root;
    struct adios_index_var_struct_v1           * vars_tail;
    struct adios_index_attribute_struct_v1     * attrs_root;
    struct adios_index_attribute_struct_v1     * attrs_tail;
    qhashtbl_t *hashtbl_vars;  // to speed up merging lists
    qhashtbl_t *hashtbl_attrs; // to speed up merging lists
};

struct adios_method_info_struct_v1
{
    enum ADIOS_IO_METHOD id;
    char * parameters;

    struct adios_method_info_struct_v1 * next;
};

struct adios_dimension_item_struct_v1
{
    uint64_t rank;
    uint32_t var_id;
    enum ADIOS_FLAG time_index;
};

struct adios_dimension_struct_v1
{
    struct adios_dimension_item_struct_v1 dimension;
    struct adios_dimension_item_struct_v1 global_dimension;
    struct adios_dimension_item_struct_v1 local_offset;
    struct adios_dimension_struct_v1 * next;
};

struct adios_process_group_header_struct_v1
{
    enum ADIOS_FLAG host_language_fortran;
    char * name;
    uint32_t coord_var_id;
    char * time_index_name;
    uint32_t time_index;
    uint8_t methods_count;
    struct adios_method_info_struct_v1 * methods;
};

struct adios_vars_header_struct_v1
{
    uint32_t count;
    uint64_t length;
};

struct adios_attributes_header_struct_v1
{
    uint32_t count;
    uint64_t length;
};

struct adios_var_header_struct_v1
{
    uint32_t id;
    char * name;
    char * path;
    enum ADIOS_DATATYPES type;
    enum ADIOS_FLAG is_dim;
    struct adios_dimension_struct_v1 * dims;
    struct adios_index_characteristic_struct_v1 characteristics;
    uint64_t payload_size;
};

struct adios_var_payload_struct_v1
{
    void * payload;
};

struct adios_attribute_struct_v1
{
    uint32_t id;
    char * name;
    char * path;

    enum ADIOS_FLAG is_var;

    uint32_t var_id;

    enum ADIOS_DATATYPES type;
    uint32_t length;
    void * value;
};

void adios_shared_buffer_free (struct adios_bp_buffer_struct_v1 * b);
void adios_buffer_struct_init (struct adios_bp_buffer_struct_v1 * b);
void adios_buffer_struct_clear (struct adios_bp_buffer_struct_v1 * b);

void * adios_dupe_data_scalar (enum ADIOS_DATATYPES type, void * in);

// buff must be 4 bytes
int adios_parse_version (struct adios_bp_buffer_struct_v1 * b
                        ,uint32_t * version
                        );

// buff must be 16 bytes
int adios_parse_index_offsets_v1 (struct adios_bp_buffer_struct_v1 * b);

int adios_parse_process_group_index_v1 (struct adios_bp_buffer_struct_v1 * b
                         ,struct adios_index_process_group_struct_v1 ** pg_root
                         );

int adios_parse_vars_index_v1 (struct adios_bp_buffer_struct_v1 * b
                              ,struct adios_index_var_struct_v1 ** vars_root
                              ,qhashtbl_t *hashtbl_vars
                              ,struct adios_index_var_struct_v1 ** vars_tail
                              );
int adios_parse_attributes_index_v1 (struct adios_bp_buffer_struct_v1 * b
                                    ,struct adios_index_attribute_struct_v1 ** attrs_root
                          );

int adios_parse_process_group_header_v1 (struct adios_bp_buffer_struct_v1 * b
                     ,struct adios_process_group_header_struct_v1 * pg_header
                     );

int adios_parse_vars_header_v1 (struct adios_bp_buffer_struct_v1 * b
                               ,struct adios_vars_header_struct_v1 * vars_header
                               );

int adios_parse_var_data_header_v1 (struct adios_bp_buffer_struct_v1 * b
                                ,struct adios_var_header_struct_v1 * var_header
                                );

int adios_parse_var_data_payload_v1 (struct adios_bp_buffer_struct_v1 * b
                              ,struct adios_var_header_struct_v1 * var_header
                              ,struct adios_var_payload_struct_v1 * var_payload
                              ,uint64_t payload_size
                              );

int adios_parse_attributes_header_v1 (struct adios_bp_buffer_struct_v1 * b
                       ,struct adios_attributes_header_struct_v1 * attrs_header
                       );

int adios_parse_attribute_v1 (struct adios_bp_buffer_struct_v1 * b
                             ,struct adios_attribute_struct_v1 * attribute
                             );
int adios_clear_process_group_header_v1 (
                     struct adios_process_group_header_struct_v1 * pg_header);
int adios_clear_var_header_v1 (struct adios_var_header_struct_v1 * var_header);
int adios_clear_attribute_v1 (struct adios_attribute_struct_v1 * attribute);

// ****************************************************************************
// functions from adios_posix.c used for file reading

void adios_init_buffer_read_version (struct adios_bp_buffer_struct_v1 * b);
void adios_posix_read_version (struct adios_bp_buffer_struct_v1 * b);

void adios_init_buffer_read_index_offsets ( struct adios_bp_buffer_struct_v1 * b);
void adios_posix_read_index_offsets (struct adios_bp_buffer_struct_v1 * b);

void adios_init_buffer_read_process_group_index ( struct adios_bp_buffer_struct_v1 * b);
void adios_posix_read_process_group_index (struct adios_bp_buffer_struct_v1 * b);

void adios_init_buffer_read_process_group (struct adios_bp_buffer_struct_v1 * b);
void adios_init_buffer_read_vars_index (struct adios_bp_buffer_struct_v1 * b);
void adios_posix_read_vars_index (struct adios_bp_buffer_struct_v1 * b);

void adios_init_buffer_read_attributes_index
                                        (struct adios_bp_buffer_struct_v1 * b);
void adios_posix_read_attributes_index (struct adios_bp_buffer_struct_v1 * b);

void adios_init_buffer_read_procss_group (struct adios_bp_buffer_struct_v1 * b);
uint64_t adios_posix_read_process_group (struct adios_bp_buffer_struct_v1 * b);

int adios_posix_open_read_internal (const char * filename
                                   ,const char * base_path
                                   ,struct adios_bp_buffer_struct_v1 * b
                                   );
void adios_posix_close_internal (struct adios_bp_buffer_struct_v1 * b);

// ADIOS statistics related functions
uint64_t adios_get_stat_size (void * data, enum ADIOS_DATATYPES type, enum ADIOS_STAT stat_id);
uint8_t adios_get_stat_set_count (enum ADIOS_DATATYPES type);
#endif
