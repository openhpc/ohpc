/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#ifndef __ADIOS_ERROR_H_
#define __ADIOS_ERROR_H_

#ifdef __cplusplus
extern "C" {
#endif

enum ADIOS_ERRCODES {
    err_no_error                        = 0,
    err_no_memory                       = -1,
    err_file_open_error                 = -2,
    err_file_not_found                  = -3,
    err_invalid_file_pointer            = -4,
    err_invalid_group                   = -5,
    err_invalid_group_struct            = -6,
    err_invalid_varid                   = -7,
    err_invalid_varname                 = -8,
    err_corrupted_variable              = -9,

    err_invalid_attrid                  = -10,
    err_invalid_attrname                = -11,
    err_corrupted_attribute             = -12,
    err_invalid_attribute_reference     = -13,
    err_invalid_timestep                = -14,
    err_no_data_at_timestep             = -15,
    err_time_at_wrong_dimension         = -16,
    err_invalid_read_method             = -17,
    err_connection_failed               = -18,
    err_out_of_bound                    = -19,

    err_operation_not_supported         = -20,
    err_end_of_stream                   = -21, /* stream: reached end of stream, 
                                                  returned by adios_read_open() or
                                                           by adios_advance_step()         */
    err_step_notready                   = -22, /* stream: tried to advance the step, 
                                                  but no new step has arrived yet          */
    err_step_disappeared                = -23, /* stream: tried to advance the step, 
                                                  but next step has already disappeared    */
    err_too_many_files                  = -24, /* some staging methods allow for using only
                                                  a fixed number of different filenames    */

    err_unknown_char                    = -30,

    // XML parsing errors
    err_invalid_host_language           = -50,
    err_global_dim_required             = -51,
    err_global_offset_required          = -52,
    err_invalid_method                  = -53,
    err_invalid_buffer_size             = -54,
    err_missing_config_file             = -55,
    err_expected_read_size_mismatch     = -56,
    err_allocating_buffer_size          = -57,
    err_invalid_xml_doc                 = -58,
    err_no_group_defined                = -59,
    err_no_method_defined               = -60,
    err_no_buffer_defined               = -61,
    err_missing_invalid_group           = -62,
    err_group_method_mismatch           = -63,
    err_dimension_required              = -64,
    err_offset_required                 = -65,
    err_invalid_dimension               = -66,
    err_invalid_global_dimension        = -67,
    err_invalid_offset                  = -68,
    err_invalid_var_as_dimension        = -69,
    err_invalid_type_attr               = -70,
    err_invalid_value_attr              = -71,
    err_histogram_error                 = -72,

    // Write method errors
    err_invalid_file_mode               = -100,
    err_invalid_file_version            = -101,
    err_invalid_data                    = -102,
    err_buffer_overflow                 = -103,
    err_too_many_variables              = -104,
    err_invalid_write_method            = -105,
    err_write_error                     = -106,

    //buffering errors
    err_invalid_buffer                  = -130,
    err_invalid_buffer_version          = -131,
    err_invalid_buffer_index            = -132,
    err_invalid_buffer_group            = -133,
    err_invalid_buffer_vars             = -134,
    err_invalid_buffer_attrs            = -135,
    
    

    //invalid argument to function
    err_invalid_argument                = -140,

    // Mesh reading errors
    err_mesh_file_missing                     = -146, // mesh file is missing
    err_no_matching_mesh_var                  = -147, // no mesh is associated to var
    err_mesh_missing                          = -148, // mesh is missing in meshlist
    err_mesh_name_attr_missing                = -149, // mesh attr is missing
    err_mesh_unstructured_invaid_points       = -150, // points were invalid
    err_mesh_unstructured_invaid_num_points   = -151, // number of points is invalid
    err_mesh_unstructured_invaid_dim_points   = -152, // dim of points is invalid
    err_mesh_unstructured_missing_one_points  = -153, // one of the points is not defined
    err_mesh_unstructured_missing_points      = -154, // points are not defined
    err_mesh_unstructured_missing_ncsets      = -155, // ncsets was not defined 
    err_mesh_unstructured_invalid_ncsets      = -156, // could not read the ncsets value
    err_mesh_unstructured_missing_ccount      = -157, // ccount was not defined
    err_mesh_unstructured_invalid_ccount      = -158, // could not read the ccount value
    err_mesh_unstructured_invalid_ctypes      = -159, // # of cell types invalid
    err_mesh_unstructured_missing_cdata       = -160, // cdata was not defined
    err_mesh_unstructured_invalid_cdata       = -161, // could not read the cdata value 
    err_mesh_unstructured_missing_ctype       = -162, // ctype was not defined
    err_mesh_unstructured_invalid_ctype       = -163, // coule not read ctype value
    err_mesh_unstructured_centering_missing   = -164, // var centering of mesh is missing
    err_mesh_unstructured_centering_invalid   = -165, // var centering of mesh is invalid

    err_mesh_unifrom_invalid_num_dims         = -170, // # dimensions was invalid
    err_mesh_unifrom_invalid_num_max          = -171, // # of maximums was less than ndims
    err_mesh_unifrom_missing_maximum          = -172, // maximums was not defined
    err_mesh_unifrom_invalid_var_type         = -173, // var type is not supported for processing
    err_mesh_unifrom_missing_one_dim          = -174, // one of the dimensions is not defined 
    err_mesh_unifrom_invalid_dim              = -175, // could not read the dim value
    err_mesh_unifrom_missing_dims             = -176, // dimensions are not defined 
    err_mesh_unifrom_max_conflict             = -177, // maximum is not consistant

    err_mesh_recti_invalid_num_dims           = -180, // # dimensions was invalid
    err_mesh_recti_missing_dims               = -181, // dimensions are not defined
    err_mesh_recti_missing_one_dim            = -182, // one of the dimensions is not defined
    err_mesh_recti_invalid_dim                = -183, // could not read the dim value
    err_mesh_recti_invalid_coords             = -184, // coordinates were invalid
    err_mesh_recti_invalid_num_coords         = -185, // # coordinates was invalid
    err_mesh_recti_missing_one_coords         = -186, // one of the coordinates is not defined
    err_mesh_recti_missing_coords             = -187, // coordinates are not defined

    err_mesh_structured_invalid_num_dims      = -190, // # dimensions was invalid
    err_mesh_structured_missing_dims          = -191, // dimensions are not defined
    err_mesh_structured_missing_one_dim       = -192, // one of the dimensions is not defined
    err_mesh_structured_invalid_dim           = -193, // could not read the dim value
    err_mesh_structured_invaid_dim_points     = -194, // # of points were invalid
    err_mesh_structured_invaid_points         = -195, // points were invalid
    err_mesh_structured_missing_one_points    = -196, // one of the points is not defined
    err_mesh_structured_missing_points        = -197, // points are not defined
    err_mesh_structured_invalid_num_points    = -198, // number of points is invalid

    // Transform layer errors
    err_invalid_transform_type                = -300, // unknown transform is requested 

    // Query errors
    err_unsupported_selection                 = -401, // unsupported selection
    err_invalid_query_value                   = -402, // value passed in expression is invalid
    err_incompatible_queries                  = -403, // cannot combine two queries

    // Miscellaneous
    err_fgr                                   = -900, // FGR lib error

    err_unspecified                           = -1000
};

void adios_error (enum ADIOS_ERRCODES errcode, char *fmt, ...);
void adios_error_at_line (enum ADIOS_ERRCODES errcode, const char* filename, unsigned int linenum, char *fmt, ...);

const char* adios_get_last_errmsg (void);
void adios_clear_error(void); // reset adios_errno to err_no_err and clear last errmsg

#ifdef __cplusplus
}
#endif

#endif
