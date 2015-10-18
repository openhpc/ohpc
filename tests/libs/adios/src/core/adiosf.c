/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#include "config.h"
#include <string.h>
#include <unistd.h>
#include <stdint.h>

#include "core/adios_internals.h"
#include "core/adios_internals_mxml.h"
#include "core/common_adios.h"
#include "core/adios_transport_hooks.h"
#include "core/futils.h"
#include "core/globals.h"
#include "adios_error.h"
#include "core/adios_logger.h"
#include "core/adios_timing.h"

#ifdef __cplusplus
extern "C"  /* prevent C++ name mangling */
#endif


#ifdef BUILD_WITH_CMAKE
  #include "FC.h"
#endif

extern int adios_errno;

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_set_application_id, ADIOS_SET_APPLICATION_ID) (int *id, int * err)
{
    globals_adios_set_application_id (*id);
    if (err != 0) *err = err_no_error;
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_init, ADIOS_INIT) (const char * config, MPI_Fint * comm, int * err, int config_size)
{
    char * buf1 = 0;

    MPI_Comm c_comm = MPI_Comm_f2c (*comm);
    buf1 = futils_fstr_to_cstr (config, config_size);
    if (buf1 != 0) {
        *err = common_adios_init (buf1, c_comm);
        free (buf1);
    } else {
        *err = adios_errno;
    }
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_init_noxml, ADIOS_INIT_LOCAL) (MPI_Fint * comm, int * err)
{
    MPI_Comm c_comm = MPI_Comm_f2c (*comm);
    *err = common_adios_init_noxml (c_comm);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_finalize, ADIOS_FINALIZE) (int * mype, int * err)
{
    *err = common_adios_finalize (*mype);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_allocate_buffer, ADIOS_ALLOCATE_BUFFER) (int *sizeMB, int * err)
{
    //FIX
    *err = common_adios_allocate_buffer (1 /*NOW*/, *sizeMB);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_open, ADIOS_OPEN) 
    (int64_t * fd, const char * group_name, const char * name
    ,const char * mode, MPI_Fint *comm, int * err
    ,int group_name_size, int name_size, int mode_size
    )
{
    char * buf1 = 0;
    char * buf2 = 0;
    char * buf3 = 0;
    MPI_Comm c_comm = MPI_Comm_f2c (*comm);

    buf1 = futils_fstr_to_cstr (group_name, group_name_size);
    buf2 = futils_fstr_to_cstr (name, name_size);
    buf3 = futils_fstr_to_cstr (mode, mode_size);

    if (buf1 != 0 && buf2 != 0 && buf3 != 0) {
        *err = common_adios_open (fd, buf1, buf2, buf3, c_comm);
        free (buf1);
        free (buf2);
        free (buf3);
    } else {
        *err = adios_errno;
    }
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_group_size, ADIOS_GROUP_SIZE) 
    (int64_t * fd_p, int64_t * data_size
    ,int64_t * total_size, int * err
    )
{
    *err = common_adios_group_size (*fd_p, (uint64_t) *data_size
                            ,(uint64_t *) total_size
                            );
}

///////////////////////////////////////////////////////////////////////////////
#include "stdio.h"
void FC_FUNC_(adios_write_byid, ADIOS_WRITE_BYID)
    (int64_t * fd_p, int64_t * id, void * var
    ,int * err, int var_size
    )
{
    struct adios_file_struct * fd = (struct adios_file_struct *) *fd_p;
    if (!fd)
    {
        adios_error (err_invalid_file_pointer, "Invalid handle passed to adios_write\n");
        *err = adios_errno;
        return;
    }

    struct adios_var_struct * v = (struct adios_var_struct *) *id;
    if (!v)
    {
        adios_error (err_invalid_varid, "Invalid id passed to adios_write_byid\n");
        *err = adios_errno;
        return;
    }

    struct adios_method_list_struct * m = fd->group->methods;

    if (m && m->next == NULL && m->method->m == ADIOS_METHOD_NULL)
    {
        // nothing to do so just return
        *err = err_no_error;
        return;
    }

    if (!var)
    {
        adios_error (err_invalid_data, "Invalid data (NULL pointer) passed to write\n");
        *err = adios_errno;
        return;
    }

    *err = common_adios_write_byid (fd, v, var);
}

/* Name clash resolution: Fortran adios_write_byid is an interface, and its 
   subroutines cannot call adios_write_byid() in this file directly because 
   the Fortran compiler interprets it as the call to the interface name. 
   adios_write_byid_f2c provides the bridge to link the C function with
   the subroutines. 
*/
void FC_FUNC_(adios_write_byid_f2c, ADIOS_WRITE_BYID_F2C) 
    (int64_t * fd_p, int64_t * id, void * var, int * err, int var_size)
{
    FC_FUNC_(adios_write_byid, ADIOS_WRITE) (fd_p, id, var, err, var_size);
}

/* This Fortran api function is a bit different from the C api funcion, but
 * they call the same common_adios_write().
 * Difference: if the variable is string type then we need to convert
 * the void * var to a C string (add \0 to the end)
 * We rely on the assumption/fact that Fortran compilers pass on the 
 * length of a character array in an extra integer argument, even if
 * the C function declares a void* array in the argument list. 
 */
void FC_FUNC_(adios_write, ADIOS_WRITE) 
    (int64_t * fd_p, const char * name, void * var, int * err
    ,int name_size, int var_size
    )
{
    struct adios_file_struct * fd = (struct adios_file_struct *) *fd_p;
    if (!fd)
    {
        adios_error (err_invalid_file_pointer, "Invalid handle passed to adios_write\n");
        *err = adios_errno;
        return;
    }

    struct adios_var_struct * v = fd->group->vars;
    struct adios_method_list_struct * m = fd->group->methods;

    if (m && m->next == NULL && m->method->m == ADIOS_METHOD_NULL)
    {
        // nothing to do so just return
        *err = err_no_error;
        return;
    }

    char * buf1 = 0;
    buf1 = futils_fstr_to_cstr (name, name_size);

    //printf("  -- adios_write: name=[%s] var size = %d\n", buf1, var_size);

    if (!buf1) {
        *err = adios_errno;
        return;
    }

    v = adios_find_var_by_name (fd->group, buf1);

    if (!v)
    {
        adios_error (err_invalid_varname, "Bad var name (ignored): '%s'\n", buf1);
        *err = adios_errno;
        free (buf1);
        return;
    }

    if (fd->mode == adios_mode_read)
    {
        if (   strcasecmp (buf1, fd->group->group_comm)
            && v->is_dim != adios_flag_yes
           )
        {
            adios_error (err_invalid_file_mode, "write attempted on %s in %s.  This was opened for read\n" ,buf1 , fd->name);
            *err = adios_errno;
            free (buf1);
            return;
        }
    }

    /* // Do not check NULL pointer here, it works fine in writing
    if (!var)
    {
        adios_error (err_invalid_data, "Invalid data (NULL pointer) passed to write for variable %s\n", buf1);
        *err = adios_errno;
        free (buf1);
        return;
    }
    */

    if (v->data)
    {
        free (v->data);
        v->data = 0;
    }

    // Q.L. 10-2010. To fix a memory leak problem.
    // NCSU - Clear stats
    if (v->stats)
    {   
        int j, idx;
        int c, count = 1;

        if (v->type == adios_complex || v->type == adios_double_complex)
            count = 3;

        for (c = 0; c < count; c ++)
        {   
            j = idx = 0;
            while (v->bitmap >> j)
            {   
                if (v->bitmap >> j & 1)
                {   
                    if (j == adios_statistic_hist)
                    {   
                        struct adios_index_characteristics_hist_struct * hist =
                            (struct adios_index_characteristics_hist_struct *) v->stats[c][idx].data;
                        free (hist->breaks);
                        free (hist->frequencies);
                        free (hist);
                        v->stats[c][idx].data = 0;
                    }
                    else
                    {   
                        free (v->stats[c][idx].data);
                        v->stats[c][idx].data = 0;
                    }
                    idx ++;
                }
                j ++;
            }
        }
    }

    if (v->dimensions)
    {
        v->data = var;
    }
    else
    {
        uint64_t element_size = adios_get_type_size (v->type, var);

        switch (v->type)
        {
            case adios_byte:
            case adios_short:
            case adios_integer:
            case adios_long:
            case adios_unsigned_byte:
            case adios_unsigned_short:
            case adios_unsigned_integer:
            case adios_unsigned_long:
            case adios_real:
            case adios_double:
            case adios_long_double:
            case adios_complex:
            case adios_double_complex:
                v->data = malloc (element_size);
                if (!v->data)
                {
                    adios_error (err_no_memory, 
                                 "In adios_write, cannot allocate %lld bytes to copy scalar %s\n",
                                 element_size, v->name);
                    *err = adios_errno;
                    free (buf1);
                    return;
                }

                memcpy ((char *) v->data, var, element_size);
                break;
            case adios_string:
                v->data = futils_fstr_to_cstr (var, var_size);
                if (!v->data)
                {
                    adios_error (err_no_memory, 
                                 "In adios_write, cannot allocate %lld bytes to copy string %s\n",
                                 element_size, v->name);
                    *err = adios_errno;
                    free (buf1);
                    return;
                }
                break;

            default:
                v->data = 0;
                break;
        }
    }

    *err = common_adios_write (fd, v, var);
    if (fd->mode == adios_mode_write || fd->mode == adios_mode_append)
    {
        adios_copy_var_written (fd->group, v);
    }

    free (buf1);
}

/* Name clash resolution: Fortran adios_write is an interface, and its subroutines cannot call
   adios_write() in this file directly because the Fortran compiler interprets it as the
   call to the interface name. adios_write_f2c provides the bridge to link the C function with
   the subroutines. 
*/
void FC_FUNC_(adios_write_f2c, ADIOS_WRITE_F2C) 
    (int64_t * fd_p, const char * name, void * var, int * err,int name_size, int var_size)
{
    FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_get_write_buffer, ADIOS_GET_WRITE_BUFFER) 
    (int64_t * fd_p, const char * name
    ,int64_t * size
    ,void ** buffer, int * err, int name_size
    )
{
    char * buf1 = 0;

    buf1 = futils_fstr_to_cstr (name, name_size);

    if (buf1 != 0) {
        *err = common_adios_get_write_buffer (*fd_p, buf1, (uint64_t *) size, buffer);
        free (buf1);
    } else {
        *err = adios_errno;
    }
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_read, ADIOS_READ) 
    (int64_t * fd_p, const char * name, void * buffer
    ,int64_t * buffer_size, int * err, int name_size
    )
{
    char * buf1 = 0;

    buf1 = futils_fstr_to_cstr (name, name_size);

    if (buf1 != 0) {
        *err = common_adios_read (*fd_p, buf1, buffer, *buffer_size);
        free (buf1);
    } else {
        *err = adios_errno;
    }
}

void FC_FUNC_(adios_read_f2c, ADIOS_READ_F2C) 
    (int64_t * fd_p, const char * name, void * buffer,int64_t * buffer_size, int * err, int name_size)
{
    FC_FUNC_(adios_read, ADIOS_READ) 
    (fd_p, name, buffer, buffer_size, err, name_size);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_set_path, ADIOS_SET_PATH) 
    (int64_t * fd_p, const char * path, int * err, int path_size)
{
    char * buf1 = 0;

    buf1 = futils_fstr_to_cstr (path, path_size);

    if (buf1 != 0) {
        *err = common_adios_set_path (*fd_p, buf1);
        free (buf1);
    } else {
        *err = adios_errno;
    }
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_set_path_var, ADIOS_SET_PATH_VAR) 
    (int64_t * fd_p, const char * path, const char * name, int * err, int path_size, int name_size)
{
    char * buf1 = 0;
    char * buf2 = 0;

    buf1 = futils_fstr_to_cstr (path, path_size);
    buf2 = futils_fstr_to_cstr (name, name_size);

    if (buf1 != 0 && buf2 != 0) {
        *err = common_adios_set_path_var (*fd_p, buf1, buf2);
        free (buf1);
        free (buf2);
    } else {
        *err = adios_errno;
    }
}

///////////////////////////////////////////////////////////////////////////////
// hint that we reached the end of an iteration (for asynchronous pacing)
void FC_FUNC_(adios_end_iteration, ADIOS_END_ITERATION) (int * err)
{
    *err = common_adios_end_iteration ();
}

///////////////////////////////////////////////////////////////////////////////
// hint to start communicating
void FC_FUNC_(adios_start_calculation, ADIOS_START_CALCULATION) (int * err)
{
    *err = common_adios_start_calculation ();
}

///////////////////////////////////////////////////////////////////////////////
// hint to stop communicating
void FC_FUNC_(adios_stop_calculation, ADIOS_STOP_CALCULATION) (int * err)
{
    *err = common_adios_stop_calculation ();
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_close, ADIOS_CLOSE) (int64_t * fd_p, int * err)
{
    *err = common_adios_close (*fd_p);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_timing_write_xml, ADIOS_TIMING_WRITE_XML) (int64_t * fd_p, const char* filename, int filename_size)
{
    char * cfile = 0;
    cfile = futils_fstr_to_cstr (filename, filename_size);

    // defined in adios_timing.c
    adios_timing_write_xml_common (*fd_p, cfile);
}



//////////////////////////////////////////////////////////////////////////////
// Methods normally only called by the XML parser
//////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
// adios_common_declare_group is in adios_internals.c
// group a list of vars into a composite group
void FC_FUNC_(adios_declare_group, ADIOS_DECLARE_GROUP) 
    (int64_t * id, const char * name
    ,const char * time_index, enum ADIOS_FLAG *stats
    ,int * err, int name_size, int time_index_size
    )
{
    char * buf1 = 0;
    char * buf2 = 0;

    adios_errno = err_no_error;
    buf1 = futils_fstr_to_cstr (name, name_size);
    buf2 = futils_fstr_to_cstr (time_index, time_index_size);

    if (buf1 != 0 && buf2 != 0) {
        int ret = adios_common_declare_group (id, buf1, adios_flag_yes, "", "", buf2, *stats);
        free (buf1);
        free (buf2);
        if (ret == 1) {
            struct adios_group_struct * g = (struct adios_group_struct *) *id;
            g->all_unique_var_names = adios_flag_no;
        }
    }
    *err = adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
// adios_common_define_var is in adios_internals.c
// declare a single var as an entry in a group
void FC_FUNC_(adios_define_var, ADIOS_DEFINE_VAR) 
    (int64_t * group_id, const char * name
    ,const char * path, int * type
    ,const char * dimensions
    ,const char * global_dimensions
    ,const char * local_offsets, int64_t * id
    ,int name_size, int path_size, int dimensions_size
    ,int global_dimensions_size, int local_offsets_size
    )
{
    char * buf1 = 0;
    char * buf2 = 0;
    char * buf3 = 0;
    char * buf4 = 0;
    char * buf5 = 0;

    adios_errno = err_no_error;
    buf1 = futils_fstr_to_cstr (name, name_size);
    buf2 = futils_fstr_to_cstr (path, path_size);
    buf3 = futils_fstr_to_cstr (dimensions, dimensions_size);
    buf4 = futils_fstr_to_cstr (global_dimensions, global_dimensions_size);
    buf5 = futils_fstr_to_cstr (local_offsets, local_offsets_size);

    if (buf1 != 0 && buf2 != 0) {
        *id = adios_common_define_var (*group_id, buf1, buf2
                                       ,(enum ADIOS_DATATYPES) *type
                                       ,buf3, buf4, buf5);

        free (buf1);
        free (buf2);
        free (buf3);
        free (buf4);
        free (buf5);
    } else {
        *id = 0;
    }
}

// delete all variable definitions from a group
// Use if you want to define a new set of variables for the next output step.
void FC_FUNC_(adios_delete_vardefs, ADIOS_DELETE_VARDEFS) (int64_t *id, int *err)
{
    adios_errno = err_no_error;
    if (id != 0) {
        struct adios_group_struct * g = (struct adios_group_struct *) *id;
        *err = adios_common_delete_vardefs (g);
    } else {
        adios_error (err_invalid_group, "adios_delete_vardefs() called with 0 argument\n");
        *err = adios_errno;
    }
}

///////////////////////////////////////////////////////////////////////////////
// adios_common_set_transform is in adios_internals.c
// set the transform method for the selected variable (default is "none")
void FC_FUNC_(adios_set_transform, ADIOS_SET_TRANSFORM)
    (int64_t * var_id, const char *transform_type_str, int *err, 
     int str_size)
{
    char * buf1 = 0;
    adios_errno = err_no_error;
    buf1 = futils_fstr_to_cstr (transform_type_str, str_size);
    if (buf1 != 0) {
        *err = adios_common_set_transform (*var_id, buf1);
        free (buf1);
    } else {
        *err = adios_errno;
    }
}
///////////////////////////////////////////////////////////////////////////////
// adios_common_define_attribute is in adios_internals.c
void FC_FUNC_(adios_define_attribute, ADIOS_DEFINE_ATTRIBUTE) 
    (int64_t * group, const char * name
    ,const char * path, int * type, const char * value
    ,const char * var, int * err
    ,int name_size, int path_size, int value_size
    ,int var_size
    )
{
    char * buf1 = 0;
    char * buf2 = 0;
    char * buf3 = 0;
    char * buf4 = 0;

    adios_errno = err_no_error;
    buf1 = futils_fstr_to_cstr (name, name_size);
    buf2 = futils_fstr_to_cstr (path, path_size);
    buf3 = futils_fstr_to_cstr (value, value_size);
    buf4 = futils_fstr_to_cstr (var, var_size);

    if (buf1 != 0 && buf2 != 0 && buf3 != 0 && buf4 != 0) {
        adios_common_define_attribute (*group, buf1, buf2
                                      ,(enum ADIOS_DATATYPES) *type, buf3
                                      ,buf4
                                      );
        free (buf1);
        free (buf2);
        free (buf3);
        free (buf4);
    }
    *err = adios_errno;
}

// delete all attribute definitions from a group
// Use if you want to define a new set of attribute for the next output step.
void FC_FUNC_(adios_delete_attrdefs, ADIOS_DELETE_VARDEFS) (int64_t *id, int *err)
{
    adios_errno = err_no_error;
    if (id != 0) {
        struct adios_group_struct * g = (struct adios_group_struct *) *id;
        *err = adios_common_delete_attrdefs (g);
    } else {
        adios_error (err_invalid_group, "adios_delete_attrdefs() called with 0 argument\n");
        *err = adios_errno;
    }
}

///////////////////////////////////////////////////////////////////////////////
// adios_common_select_method is in adios_internals_mxml.c
void FC_FUNC_(adios_select_method, ADIOS_SELECT_METHOD) 
    (int64_t * group, const char * method
    ,const char * parameters, const char * base_path
    ,int * err, int method_size, int parameters_size
    ,int base_path_size
    )
{
    char * buf1 = 0;
    char * buf2 = 0;
    char * buf3 = 0;

    adios_errno = err_no_error;
    buf1 = futils_fstr_to_cstr (method, method_size);
    buf2 = futils_fstr_to_cstr (parameters, parameters_size);
    buf3 = futils_fstr_to_cstr (base_path, base_path_size);

    if (buf1 != 0 && buf2 != 0 && buf3 != 0) {
        struct adios_group_struct * g = (struct adios_group_struct *) (* group);
        adios_common_select_method (0, buf1, buf2, g->name, buf3, 0);

        free (buf1);
        free (buf2);
        free (buf3);
    }
    *err = adios_errno;
}

// no-xml schema API
///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_schema_version, ADIOS_DEFINE_SCHEMA_VERSION)
    (int64_t * group_id, char * schema_version
    ,int schema_version_size)
{
    char * buf1 = 0;
    buf1 = futils_fstr_to_cstr (schema_version, schema_version_size);
    if (buf1 != 0)
    {
//        printf ("call adios_define_schema_version in adiosf.c\n");
//        printf ("schema_version is %s\n", buf1);
        struct adios_group_struct * g = (struct adios_group_struct *) *group_id;
        adios_common_define_schema_version (g, buf1);
    }
    free (buf1);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_var_mesh, ADIOS_DEFINE_VAR_MESH)
    (int64_t * group_id, const char * varname, const char * meshname
    ,int varname_size, int meshname_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    buf1 = futils_fstr_to_cstr (varname, varname_size);
    buf2 = futils_fstr_to_cstr (meshname, meshname_size);

    if (buf1 != 0 && buf2 != 0)
        adios_common_define_var_mesh (*group_id, buf1, buf2, "");        

    free (buf1);
    free (buf2);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_var_centering, ADIOS_DEFINE_VAR_CENTERING)
    (int64_t * group_id, const char * varname, const char * centering
    , int varname_size, int centering_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    buf1 = futils_fstr_to_cstr (varname, varname_size);
    buf2 = futils_fstr_to_cstr (centering, centering_size);
    if (buf1 != 0 && buf2 != 0)
        adios_common_define_var_centering (*group_id, buf1, buf2, "");

    free (buf1);
    free (buf2);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_mesh_timevarying, ADIOS_DEFINE_MESH_TIMEVARYING)
    (const char * timevarying, int64_t * group_id, const char * name
    ,int timevarying_size, int name_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    buf1 = futils_fstr_to_cstr (timevarying, timevarying_size);
    buf2 = futils_fstr_to_cstr (name, name_size);
    if (buf1 != 0 && buf2 != 0)
        adios_common_define_mesh_timeVarying (buf1, *group_id, buf2);

    free (buf1);
    free (buf2);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_mesh_file, ADIOS_DEFINE_MESH_FILE)
    (int64_t * group_id, char * name, char * file
    ,int name_size, int file_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    buf1 = futils_fstr_to_cstr (name, name_size);
    buf2 = futils_fstr_to_cstr (file, file_size);
    if (buf1 != 0 && buf2 != 0)
        adios_common_define_mesh_file (*group_id, buf1, buf2);

    free (buf1);
    free (buf2);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_var_timesteps, ADIOS_DEFINE_VAR_TIMESTEPS)
    (const char * timesteps, int64_t * group_id, const char * name
    ,int timesteps_size, int name_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    buf1 = futils_fstr_to_cstr (timesteps, timesteps_size);
    buf2 = futils_fstr_to_cstr (name, name_size);
    struct adios_group_struct * g = (struct adios_group_struct *) *group_id;
    adios_common_define_var_timesteps (buf1, g, buf2, ""); 
    free (buf1);
    free (buf2);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_var_timescale, ADIOS_DEFINE_VAR_TIMESCALE)
    (const char * timescale, int64_t * group_id, const char * name
    ,int timescale_size, int name_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    buf1 = futils_fstr_to_cstr (timescale, timescale_size);
    buf2 = futils_fstr_to_cstr (name, name_size);
    struct adios_group_struct * g = (struct adios_group_struct *) *group_id;
    adios_common_define_var_timescale (buf1, g, buf2, "");
    free (buf1);
    free (buf2);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_var_timeseriesformat, ADIOS_DEFINE_VAR_TIMESERIESFORMAT)
    (const char * timeseries, int64_t * group_id, const char * name
    ,int timeseries_size, int name_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    buf1 = futils_fstr_to_cstr (timeseries, timeseries_size);
    buf2 = futils_fstr_to_cstr (name, name_size);
    struct adios_group_struct * g = (struct adios_group_struct *) *group_id;
    adios_common_define_var_timeseriesformat (buf1, g, buf2, "");
    free (buf1);
    free (buf2);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_var_hyperslab, ADIOS_DEFINE_VAR_HYPERSLAB)
    (const char * hyperslab, int64_t * group_id, const char * name
    ,int hyperslab_size, int name_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    buf1 = futils_fstr_to_cstr (hyperslab, hyperslab_size);
    buf2 = futils_fstr_to_cstr (name, name_size);
    struct adios_group_struct * g = (struct adios_group_struct *) *group_id;
    adios_common_define_var_hyperslab (buf1, g, buf2, "");
    free (buf1);
    free (buf2);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_mesh_timesteps, ADIOS_DEFINE_MESH_TIMESTEPS)
    (const char * timesteps, int64_t * group_id, const char * name
    ,int timesteps_size, int name_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    buf1 = futils_fstr_to_cstr (timesteps, timesteps_size);
    buf2 = futils_fstr_to_cstr (name, name_size);
    struct adios_group_struct * g = (struct adios_group_struct *) *group_id;
    adios_common_define_mesh_timeSteps (buf1, g, buf2);
    free (buf1);
    free (buf2);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_mesh_timescale, ADIOS_DEFINE_MESH_TIMESCALE)
    (const char * timescale, int64_t * group_id, const char * name
    ,int timescale_size, int name_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    buf1 = futils_fstr_to_cstr (timescale, timescale_size);
    buf2 = futils_fstr_to_cstr (name, name_size);
    struct adios_group_struct * g = (struct adios_group_struct *) *group_id;
    adios_common_define_mesh_timeScale (buf1, g, buf2);
    free (buf1);
    free (buf2);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_mesh_timeseriesformat, ADIOS_DEFINE_MESH_TIMESERIESFORMAT)
    (const char * timeseries, int64_t * group_id, const char * name
    ,int timeseries_size, int name_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    buf1 = futils_fstr_to_cstr (timeseries, timeseries_size);
    buf2 = futils_fstr_to_cstr (name, name_size);
    struct adios_group_struct * g = (struct adios_group_struct *) *group_id;
    adios_common_define_mesh_timeSeriesFormat (buf1, g, buf2);
    free (buf1);
    free (buf2);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_mesh_group, ADIOS_DEFINE_MESH_GROUP)
    (const char * group, int64_t * group_id, const char * name
    ,int group_size, int name_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    buf1 = futils_fstr_to_cstr (group, group_size);
    buf2 = futils_fstr_to_cstr (name, name_size);
    adios_common_define_mesh_group (*group_id, buf2, buf1);
    free (buf1);
    free (buf2);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_mesh_uniform, ADIOS_DEFINE_MESH_UNIFROM)
    (char * dimensions, char * origin, char * spacing, char * maximum, char * nspace
    ,int64_t * group_id, const char * name, int dimensions_size, int origin_size
    ,int spacing_size, int maximum_size, int nspace_size ,int name_size) 
{
    char * buf1 = 0;
    char * buf2 = 0;
    char * buf3 = 0;
    char * buf4 = 0;
    char * buf5 = 0;
    char * buf6 = 0;
    buf1 = futils_fstr_to_cstr (dimensions, dimensions_size);
    buf2 = futils_fstr_to_cstr (origin, origin_size);
    buf3 = futils_fstr_to_cstr (spacing, spacing_size);
    buf4 = futils_fstr_to_cstr (maximum, maximum_size);
    buf5 = futils_fstr_to_cstr (nspace, nspace_size);
    buf6 = futils_fstr_to_cstr (name, name_size);
    adios_common_define_mesh_uniform (buf1, buf2, buf3, buf4, buf5, buf6, *group_id);
    free (buf1);
    free (buf2);
    free (buf3);
    free (buf4);
    free (buf5);
    free (buf6);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_mesh_rectilinear, ADIOS_DEFINE_MESH_RECTILINEAR)
    (char * dimensions, char * coordinates, char * nspace, int64_t * group_id
    ,const char * name,  int dimensions_size, int coordinates_size, int nspace_size
    ,int name_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    char * buf3 = 0;
    char * buf4 = 0;
    buf1 = futils_fstr_to_cstr (dimensions, dimensions_size);
    buf2 = futils_fstr_to_cstr (coordinates, coordinates_size);
    buf3 = futils_fstr_to_cstr (nspace, nspace_size);
    buf4 = futils_fstr_to_cstr (name, name_size);
    adios_common_define_mesh_rectilinear (buf1, buf2, buf3, buf4, *group_id);
    free (buf1);
    free (buf2);
    free (buf3);
    free (buf4);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_mesh_structured, ADIOS_DEFINE_MESH_STRUCTURED)
    (char * dimensions, char * points, char * nspace, int64_t * group_id, const char * name
    ,int dimensions_size, int points_size, int nspace_size, int name_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    char * buf3 = 0;
    char * buf4 = 0;
    buf1 = futils_fstr_to_cstr (dimensions, dimensions_size);
    buf2 = futils_fstr_to_cstr (points, points_size);
    buf3 = futils_fstr_to_cstr (nspace, nspace_size);
    buf4 = futils_fstr_to_cstr (name, name_size);
    adios_common_define_mesh_structured (buf1, buf3, buf2, buf4, *group_id);
    free (buf1);
    free (buf2);
    free (buf3);
    free (buf4);
}

///////////////////////////////////////////////////////////////////////////////
void FC_FUNC_(adios_define_mesh_unstructured, ADIOS_DEFINE_MESH_UNSTRUCTURED)
    (char * points, char * data, char * count, char * cell_type, char * npoints
    ,char * nspace, int64_t * group_id, const char * name, int points_size, int data_size
    ,int count_size, int cell_type_size, int npoints_size, int nspace_size, int name_size)
{
    char * buf1 = 0;
    char * buf2 = 0;
    char * buf3 = 0;
    char * buf4 = 0;
    char * buf5 = 0;
    char * buf6 = 0;
    char * buf7 = 0;
    buf1 = futils_fstr_to_cstr (points, points_size);
    buf2 = futils_fstr_to_cstr (data, data_size);
    buf3 = futils_fstr_to_cstr (count, count_size);
    buf4 = futils_fstr_to_cstr (cell_type, cell_type_size);
    buf5 = futils_fstr_to_cstr (npoints, npoints_size);
    buf6 = futils_fstr_to_cstr (nspace, nspace_size);
    buf7 = futils_fstr_to_cstr (name, name_size);

//    adios_common_define_mesh_unstructured (buf1, buf2, buf3, buf4, buf5, buf6, buf7, *group_id);
    adios_common_define_mesh_unstructured (buf1, buf2, buf3, buf4, buf6, buf5, buf7, *group_id);

    free (buf1);
    free (buf2);
    free (buf3);
    free (buf4);
    free (buf5);
    free (buf6);
    free (buf7);
}

/**************************************************************************/
/*                  Specific function for each data type                  */
/**************************************************************************/
#if 0
/* 
    ADIOS_WRITE 
*/
/* scalars */
void FC_FUNC_(adios_write_int1_d0, ADIOS_WRITE_INT1_D0) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int2_d0, ADIOS_WRITE_INT2_D0) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int4_d0, ADIOS_WRITE_INT4_D0) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int8_d0, ADIOS_WRITE_INT8_D0) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real4_d0, ADIOS_WRITE_REAL4_D0) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real8_d0, ADIOS_WRITE_REAL8_D0) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
//void FC_FUNC_(adios_write_char_d0, ADIOS_WRITE_CHAR_D0) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex8_d0, ADIOS_WRITE_COMPLEX8_D0) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex16_d0, ADIOS_WRITE_COMPLEX16_D0) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical1_d0, ADIOS_WRITE_LOGICAL1_D0) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical2_d0, ADIOS_WRITE_LOGICAL2_D0) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical4_d0, ADIOS_WRITE_LOGICAL4_D0) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical8_d0, ADIOS_WRITE_LOGICAL8_D0) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }


/* 1D */
void FC_FUNC_(adios_write_int1_d1, ADIOS_WRITE_INT1_D1) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int2_d1, ADIOS_WRITE_INT2_D1) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int4_d1, ADIOS_WRITE_INT4_D1) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int8_d1, ADIOS_WRITE_INT8_D1) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real4_d1, ADIOS_WRITE_REAL4_D1) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real8_d1, ADIOS_WRITE_REAL8_D1) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_char_d1, ADIOS_WRITE_CHAR_D1) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex8_d1, ADIOS_WRITE_COMPLEX8_D1) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex16_d1, ADIOS_WRITE_COMPLEX16_D1) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical1_d1, ADIOS_WRITE_LOGICAL1_D1) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical2_d1, ADIOS_WRITE_LOGICAL2_D1) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical4_d1, ADIOS_WRITE_LOGICAL4_D1) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical8_d1, ADIOS_WRITE_LOGICAL8_D1) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }


/* 2D */
void FC_FUNC_(adios_write_int1_d2, ADIOS_WRITE_INT1_D2) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int2_d2, ADIOS_WRITE_INT2_D2) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int4_d2, ADIOS_WRITE_INT4_D2) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int8_d2, ADIOS_WRITE_INT8_D2) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real4_d2, ADIOS_WRITE_REAL4_D2) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real8_d2, ADIOS_WRITE_REAL8_D2) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_char_d2, ADIOS_WRITE_CHAR_D2) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex8_d2, ADIOS_WRITE_COMPLEX8_D2) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex16_d2, ADIOS_WRITE_COMPLEX16_D2) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical1_d2, ADIOS_WRITE_LOGICAL1_D2) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical2_d2, ADIOS_WRITE_LOGICAL2_D2) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical4_d2, ADIOS_WRITE_LOGICAL4_D2) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical8_d2, ADIOS_WRITE_LOGICAL8_D2) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }


/* 3D */
void FC_FUNC_(adios_write_int1_d3, ADIOS_WRITE_INT1_D3) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int2_d3, ADIOS_WRITE_INT2_D3) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int4_d3, ADIOS_WRITE_INT4_D3) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int8_d3, ADIOS_WRITE_INT8_D3) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real4_d3, ADIOS_WRITE_REAL4_D3) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real8_d3, ADIOS_WRITE_REAL8_D3) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_char_d3, ADIOS_WRITE_CHAR_D3) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex8_d3, ADIOS_WRITE_COMPLEX8_D3) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex16_d3, ADIOS_WRITE_COMPLEX16_D3) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical1_d3, ADIOS_WRITE_LOGICAL1_D3) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical2_d3, ADIOS_WRITE_LOGICAL2_D3) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical4_d3, ADIOS_WRITE_LOGICAL4_D3) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical8_d3, ADIOS_WRITE_LOGICAL8_D3) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }


/* 4D */
void FC_FUNC_(adios_write_int1_d4, ADIOS_WRITE_INT1_D4) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int2_d4, ADIOS_WRITE_INT2_D4) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int4_d4, ADIOS_WRITE_INT4_D4) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int8_d4, ADIOS_WRITE_INT8_D4) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real4_d4, ADIOS_WRITE_REAL4_D4) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real8_d4, ADIOS_WRITE_REAL8_D4) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_char_d4, ADIOS_WRITE_CHAR_D4) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex8_d4, ADIOS_WRITE_COMPLEX8_D4) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex16_d4, ADIOS_WRITE_COMPLEX16_D4) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical1_d4, ADIOS_WRITE_LOGICAL1_D4) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical2_d4, ADIOS_WRITE_LOGICAL2_D4) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical4_d4, ADIOS_WRITE_LOGICAL4_D4) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical8_d4, ADIOS_WRITE_LOGICAL8_D4) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }


/* 5D */
void FC_FUNC_(adios_write_int1_d5, ADIOS_WRITE_INT1_D5) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int2_d5, ADIOS_WRITE_INT2_D5) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int4_d5, ADIOS_WRITE_INT4_D5) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int8_d5, ADIOS_WRITE_INT8_D5) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real4_d5, ADIOS_WRITE_REAL4_D5) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real8_d5, ADIOS_WRITE_REAL8_D5) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_char_d5, ADIOS_WRITE_CHAR_D5) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex8_d5, ADIOS_WRITE_COMPLEX8_D5) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex16_d5, ADIOS_WRITE_COMPLEX16_D5) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical1_d5, ADIOS_WRITE_LOGICAL1_D5) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical2_d5, ADIOS_WRITE_LOGICAL2_D5) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical4_d5, ADIOS_WRITE_LOGICAL4_D5) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical8_d5, ADIOS_WRITE_LOGICAL8_D5) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }


/* 6D */
void FC_FUNC_(adios_write_int1_d6, ADIOS_WRITE_INT1_D6) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int2_d6, ADIOS_WRITE_INT2_D6) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int4_d6, ADIOS_WRITE_INT4_D6) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_int8_d6, ADIOS_WRITE_INT8_D6) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real4_d6, ADIOS_WRITE_REAL4_D6) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_real8_d6, ADIOS_WRITE_REAL8_D6) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_char_d6, ADIOS_WRITE_CHAR_D6) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex8_d6, ADIOS_WRITE_COMPLEX8_D6) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_complex16_d6, ADIOS_WRITE_COMPLEX16_D6) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical1_d6, ADIOS_WRITE_LOGICAL1_D6) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical2_d6, ADIOS_WRITE_LOGICAL2_D6) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical4_d6, ADIOS_WRITE_LOGICAL4_D6) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }
void FC_FUNC_(adios_write_logical8_d6, ADIOS_WRITE_LOGICAL8_D6) (int64_t * fd_p, const char * name, void * var, int * err, int name_size, int var_size) { FC_FUNC_(adios_write, ADIOS_WRITE) (fd_p, name, var, err, name_size, var_size); }



/* 
    ADIOS_READ 
*/
/* scalars */
void FC_FUNC_(adios_read_int1_d0, ADIOS_READ_INT1_D0) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int2_d0, ADIOS_READ_INT2_D0) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int4_d0, ADIOS_READ_INT4_D0) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int8_d0, ADIOS_READ_INT8_D0) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real4_d0, ADIOS_READ_REAL4_D0) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real8_d0, ADIOS_READ_REAL8_D0) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
//void FC_FUNC_(adios_read_char_d0, ADIOS_READ_CHAR_D0) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex8_d0, ADIOS_READ_COMPLEX8_D0) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex16_d0, ADIOS_READ_COMPLEX16_D0) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical1_d0, ADIOS_READ_LOGICAL1_D0) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical2_d0, ADIOS_READ_LOGICAL2_D0) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical4_d0, ADIOS_READ_LOGICAL4_D0) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical8_d0, ADIOS_READ_LOGICAL8_D0) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }

/* 1D */
void FC_FUNC_(adios_read_int1_d1, ADIOS_READ_INT1_D1) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int2_d1, ADIOS_READ_INT2_D1) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int4_d1, ADIOS_READ_INT4_D1) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int8_d1, ADIOS_READ_INT8_D1) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real4_d1, ADIOS_READ_REAL4_D1) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real8_d1, ADIOS_READ_REAL8_D1) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_char_d1, ADIOS_READ_CHAR_D1) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex8_d1, ADIOS_READ_COMPLEX8_D1) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex16_d1, ADIOS_READ_COMPLEX16_D1) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical1_d1, ADIOS_READ_LOGICAL1_D1) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical2_d1, ADIOS_READ_LOGICAL2_D1) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical4_d1, ADIOS_READ_LOGICAL4_D1) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical8_d1, ADIOS_READ_LOGICAL8_D1) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }


/* 2D */
void FC_FUNC_(adios_read_int1_d2, ADIOS_READ_INT1_D2) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int2_d2, ADIOS_READ_INT2_D2) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int4_d2, ADIOS_READ_INT4_D2) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int8_d2, ADIOS_READ_INT8_D2) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real4_d2, ADIOS_READ_REAL4_D2) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real8_d2, ADIOS_READ_REAL8_D2) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_char_d2, ADIOS_READ_CHAR_D2) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex8_d2, ADIOS_READ_COMPLEX8_D2) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex16_d2, ADIOS_READ_COMPLEX16_D2) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical1_d2, ADIOS_READ_LOGICAL1_D2) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical2_d2, ADIOS_READ_LOGICAL2_D2) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical4_d2, ADIOS_READ_LOGICAL4_D2) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical8_d2, ADIOS_READ_LOGICAL8_D2) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }


/* 3D */
void FC_FUNC_(adios_read_int1_d3, ADIOS_READ_INT1_D3) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int2_d3, ADIOS_READ_INT2_D3) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int4_d3, ADIOS_READ_INT4_D3) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int8_d3, ADIOS_READ_INT8_D3) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real4_d3, ADIOS_READ_REAL4_D3) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real8_d3, ADIOS_READ_REAL8_D3) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_char_d3, ADIOS_READ_CHAR_D3) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex8_d3, ADIOS_READ_COMPLEX8_D3) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex16_d3, ADIOS_READ_COMPLEX16_D3) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical1_d3, ADIOS_READ_LOGICAL1_D3) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical2_d3, ADIOS_READ_LOGICAL2_D3) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical4_d3, ADIOS_READ_LOGICAL4_D3) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical8_d3, ADIOS_READ_LOGICAL8_D3) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }


/* 4D */
void FC_FUNC_(adios_read_int1_d4, ADIOS_READ_INT1_D4) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int2_d4, ADIOS_READ_INT2_D4) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int4_d4, ADIOS_READ_INT4_D4) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int8_d4, ADIOS_READ_INT8_D4) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real4_d4, ADIOS_READ_REAL4_D4) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real8_d4, ADIOS_READ_REAL8_D4) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_char_d4, ADIOS_READ_CHAR_D4) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex8_d4, ADIOS_READ_COMPLEX8_D4) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex16_d4, ADIOS_READ_COMPLEX16_D4) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical1_d4, ADIOS_READ_LOGICAL1_D4) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical2_d4, ADIOS_READ_LOGICAL2_D4) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical4_d4, ADIOS_READ_LOGICAL4_D4) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical8_d4, ADIOS_READ_LOGICAL8_D4) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }


/* 5D */
void FC_FUNC_(adios_read_int1_d5, ADIOS_READ_INT1_D5) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int2_d5, ADIOS_READ_INT2_D5) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int4_d5, ADIOS_READ_INT4_D5) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int8_d5, ADIOS_READ_INT8_D5) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real4_d5, ADIOS_READ_REAL4_D5) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real8_d5, ADIOS_READ_REAL8_D5) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_char_d5, ADIOS_READ_CHAR_D5) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex8_d5, ADIOS_READ_COMPLEX8_D5) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex16_d5, ADIOS_READ_COMPLEX16_D5) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical1_d5, ADIOS_READ_LOGICAL1_D5) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical2_d5, ADIOS_READ_LOGICAL2_D5) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical4_d5, ADIOS_READ_LOGICAL4_D5) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical8_d5, ADIOS_READ_LOGICAL8_D5) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }


/* 6D */
void FC_FUNC_(adios_read_int1_d6, ADIOS_READ_INT1_D6) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int2_d6, ADIOS_READ_INT2_D6) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int4_d6, ADIOS_READ_INT4_D6) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_int8_d6, ADIOS_READ_INT8_D6) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real4_d6, ADIOS_READ_REAL4_D6) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_real8_d6, ADIOS_READ_REAL8_D6) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_char_d6, ADIOS_READ_CHAR_D6) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex8_d6, ADIOS_READ_COMPLEX8_D6) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_complex16_d6, ADIOS_READ_COMPLEX16_D6) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical1_d6, ADIOS_READ_LOGICAL1_D6) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical2_d6, ADIOS_READ_LOGICAL2_D6) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical4_d6, ADIOS_READ_LOGICAL4_D6) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }
void FC_FUNC_(adios_read_logical8_d6, ADIOS_READ_LOGICAL8_D6) (int64_t * fd_p, const char * name, void * buffer ,int64_t * buffer_size, int * err, int name_size) { FC_FUNC_(adios_read, ADIOS_READ) (fd_p, name, buffer, buffer_size, err, name_size); }


#endif

