/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/*
 *   Read C API for ADIOS BP format files
 */
#ifndef __ADIOS_READ_V1_DEFS_H__
#define __ADIOS_READ_V1_DEFS_H__

#define ADIOS_FILE ADIOS_FILE_V1
#define ADIOS_GROUP ADIOS_GROUP_V1
#define ADIOS_VARINFO ADIOS_VARINFO_V1
#define ADIOS_HIST ADIOS_HIST_V1
#define ADIOS_VARBLOCK ADIOS_VARBLOCK_V1

#define ADIOS_READ_METHOD ADIOS_READ_METHOD_V1
#define ADIOS_READ_METHOD_BP ADIOS_READ_METHOD_BP_V1
#define ADIOS_READ_METHOD_BP_AGGREGATE ADIOS_READ_METHOD_BP_AGGREGATE_V1
#define ADIOS_READ_METHOD_BP_STAGED ADIOS_READ_METHOD_BP_STAGED_V1
#define ADIOS_READ_METHOD_HDF5 ADIOS_READ_METHOD_HDF5_V1
#define ADIOS_READ_METHOD_DART ADIOS_READ_METHOD_DART_V1
#define ADIOS_READ_METHOD_DIMES ADIOS_READ_METHOD_DIMES_V1
#define ADIOS_READ_METHOD_NSSI ADIOS_READ_METHOD_NSSI_V1
#define ADIOS_READ_METHOD_DATATAP ADIOS_READ_METHOD_DATATAP_V1
#define ADIOS_READ_METHOD_BP_STAGED1 ADIOS_READ_METHOD_BP_STAGED1_V1

#define adios_read_init adios_read_init_v1
#define adios_read_finalize adios_read_finalize_v1
#define adios_set_read_method adios_set_read_method_v1
#define adios_fopen adios_fopen_v1
#define adios_fopen adios_fopen_v1
#define adios_fclose adios_fclose_v1
#define adios_reset_dimension_order adios_reset_dimension_order_v1
#define adios_gopen adios_gopen_v1
#define adios_gopen_byid adios_gopen_byid_v1
#define adios_gclose adios_gclose_v1
#define adios_inq_var adios_inq_var_v1
#define adios_inq_var_byid adios_inq_var_byid_v1
#define adios_stat_cor adios_stat_cor_v1
#define adios_stat_cov adios_stat_cov_v1
#define adios_free_varinfo adios_free_varinfo_v1
#define adios_read_var adios_read_var_v1
#define adios_read_var_byid adios_read_var_byid_v1
#define adios_read_local_var adios_read_local_var_v1
#define adios_get_attr adios_get_attr_v1
#define adios_get_attr_byid adios_get_attr_byid_v1
#define adios_type_to_string adios_type_to_string_v1
#define adios_type_size adios_type_size_v1
#define adios_print_fileinfo adios_print_fileinfo_v1


#define err_end_of_file err_end_of_stream

#endif  /*__ADIOS_READ_VER1_DEFS_H__*/
