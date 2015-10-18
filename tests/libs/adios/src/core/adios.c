/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#include "config.h"
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>

// xml parser
#include <mxml.h>

#include "adios.h"
#include "adios_error.h"
#include "core/globals.h"
#include "core/common_adios.h"
#include "core/adios_bp_v1.h"
#include "core/adios_internals.h"
#include "core/adios_internals_mxml.h"
#include "core/adios_transport_hooks.h"
#include "core/adios_logger.h"
#include "core/adios_timing.h"

#ifdef DMALLOC
#include "dmalloc.h"
#endif

extern struct adios_transport_struct * adios_transports;
extern int adios_errno;

int adios_set_application_id (int id)
{
    globals_adios_set_application_id (id);
    return err_no_error;
}

///////////////////////////////////////////////////////////////////////////////
int adios_init (const char * config, MPI_Comm comm)
{
    return common_adios_init (config, comm);
}


///////////////////////////////////////////////////////////////////////////////
// all XML file pieces will be provided by another series of calls
int adios_init_noxml (MPI_Comm comm)
{
    return common_adios_init_noxml (comm);
}

///////////////////////////////////////////////////////////////////////////////
int adios_finalize (int mype)
{
    return common_adios_finalize (mype);
}

///////////////////////////////////////////////////////////////////////////////
int adios_allocate_buffer (enum ADIOS_BUFFER_ALLOC_WHEN adios_buffer_alloc_when
                          ,uint64_t buffer_size)
{
    return common_adios_allocate_buffer (adios_buffer_alloc_when, buffer_size);
}

///////////////////////////////////////////////////////////////////////////////
int adios_open (int64_t * fd, const char * group_name, const char * name
               ,const char * mode, MPI_Comm comm
               )
{
    return common_adios_open (fd, group_name, name, mode, comm);
}

///////////////////////////////////////////////////////////////////////////////
int adios_group_size (int64_t fd_p, uint64_t data_size
                     ,uint64_t * total_size
                     )
{
    return common_adios_group_size (fd_p, data_size, total_size);
}

///////////////////////////////////////////////////////////////////////////////
/* This C api function is slightly different from adios_write. Instead of
 * passing variable names, user is expected to pass variable id, which is returned
 * from adios_define_var call. Therefore, this function is only used in the no-xml way.
 */
int adios_write_byid (int64_t fd_p, int64_t id, void * var)
{
    return common_adios_write_byid ((struct adios_file_struct *) fd_p, (struct adios_var_struct *) id, var);
}

///////////////////////////////////////////////////////////////////////////////
/* This C api function is a bit different from the Fortran api funcion, but
 * they call the same common_adios_write()
 */
int adios_write (int64_t fd_p, const char * name, void * var)
{
    int retval;
    struct adios_file_struct * fd = (struct adios_file_struct *) fd_p;
    if (!fd)
    {
        adios_error (err_invalid_file_pointer, "Invalid handle passed to adios_write\n");
        return adios_errno;
    }

    struct adios_var_struct * v = fd->group->vars;
    struct adios_method_list_struct * m = fd->group->methods;

    if (m && m->next == NULL && m->method->m == ADIOS_METHOD_NULL)
    {
        // nothing to do so just return OK (=0)
        return err_no_error; 
    }
    log_debug ("%s (%s)\n", __func__, name);
    v = adios_find_var_by_name (fd->group, name);

    if (!v)
    {
        adios_error (err_invalid_varname, "Bad var name (ignored) in adios_write(): '%s'\n", name);

        return adios_errno;
    }

    retval = common_adios_write_byid (fd, v, var);
#if 0
    if (fd->mode == adios_mode_read)
    {
        if (   strcasecmp (name, fd->group->group_comm)
            && v->is_dim != adios_flag_yes
           )
        {
            adios_error (err_invalid_file_mode, "write attempted on %s in %s.  This was opened for read\n" ,name , fd->name);
            return 1;
        }
    }

    if (!var)
    {
        adios_error (err_invalid_data, "Invalid data (NULL pointer) passed to write for variable %s\n", name);

        return 1;
    }

    if (v->data)
    {
        free (v->data);
        v->data = 0;
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

                    return 0;
                }

                memcpy ((char *) v->data, var, element_size);
                break;

            case adios_string:
                v->data = malloc (element_size + 1);
                if (!v->data)
                {
                    adios_error (err_no_memory, 
                                 "In adios_write, cannot allocate %lld bytes to copy string %s\n",
                                 element_size, v->name);

                    return 0;
                }
                ((char *) v->data) [element_size] = 0;
                memcpy ((char *) v->data, var, element_size);
                break;

            default:
                v->data = 0;
                break;
        }
    }

    common_adios_write (fd, v, var);
    // v->data is set to NULL in the above call

    if (fd->mode == adios_mode_write || fd->mode == adios_mode_append) 
    {
        adios_copy_var_written (fd->group, v);
    }
#endif
    return retval;
}


///////////////////////////////////////////////////////////////////////////////
int adios_get_write_buffer (int64_t fd_p, const char * name
                           ,uint64_t * size
                           ,void ** buffer
                           )
{
    return common_adios_get_write_buffer (fd_p, name, size, buffer);
}

///////////////////////////////////////////////////////////////////////////////
int adios_read (int64_t fd_p, const char * name, void * buffer
               ,uint64_t buffer_size
               )
{
    return common_adios_read (fd_p, name, buffer, buffer_size);
}

///////////////////////////////////////////////////////////////////////////////
int adios_set_path (int64_t fd_p, const char * path)
{
    return common_adios_set_path (fd_p, path);
}

///////////////////////////////////////////////////////////////////////////////
int adios_set_path_var (int64_t fd_p, const char * path, const char * name)
{
    return common_adios_set_path_var (fd_p, path, name);
}

///////////////////////////////////////////////////////////////////////////////
// hint that we reached the end of an iteration (for asynchronous pacing)
int adios_end_iteration ()
{
    return common_adios_end_iteration ();
}

///////////////////////////////////////////////////////////////////////////////
// hint to start communicating
int adios_start_calculation ()
{
    return common_adios_start_calculation ();
}

///////////////////////////////////////////////////////////////////////////////
// hint to stop communicating
int adios_stop_calculation ()
{
    return common_adios_stop_calculation ();
}

///////////////////////////////////////////////////////////////////////////////
int adios_close (int64_t fd_p)
{
    int retval;
    struct adios_file_struct * fd = (struct adios_file_struct *) fd_p;
    struct adios_var_struct * v = fd->group->vars;

    retval = common_adios_close (fd_p);

    // Q.L. 10-2010. To fix a memory leak problem.
    while (v) {
        int j, idx;
        int c, count = 1;
        // NCSU - Clear stats
        if (v->stats) {   
    
            if (v->type == adios_complex || v->type == adios_double_complex)
                count = 3;
            else 
                count = 1;

            for (c = 0; c < count; c ++) {   
                j = idx = 0;
                while (v->bitmap >> j) {   
                    if (v->bitmap >> j & 1) {   
                        if (j == adios_statistic_hist) {   
                            struct adios_index_characteristics_hist_struct * hist =
                                (struct adios_index_characteristics_hist_struct *) v->stats[c][idx].data;
                            if (hist) {   
                                free (hist->breaks);
                                free (hist->frequencies);
                                free (hist);
                                v->stats[c][idx].data = 0;
                            }
                        }
                        else {
                            if (v->stats[c][idx].data) {
                                free (v->stats[c][idx].data);
                                v->stats[c][idx].data = 0;
                            }
                        }
                        idx ++;
                    }
                    j ++;
                }
            }
        }

        // NCSU ALACRITY-ADIOS - Clear transform metadata
        // adios_transform_clear_transform_var(v); // Actually, no, we shouldn't free the metadata here, because this happens once a timestep,
                                                   // and this shouldn't be free'd until finalize (it is just overwritten each timestep)

        v = v->next;
    }
    return retval;
}

//////////////////////////////////////////////////////////////////////////////
// Methods normally only called by the XML parser
//////////////////////////////////////////////////////////////////////////////

// adios_common_declare_group is in adios_internals.c

///////////////////////////////////////////////////////////////////////////////
// group a list of vars into a composite group
int adios_declare_group (int64_t * id, const char * name
                        ,const char * time_index
                        ,enum ADIOS_FLAG stats
                        )
{
    int ret;
    adios_errno = err_no_error;
    ret = adios_common_declare_group (id, name, adios_flag_no
                                      ,""
                                      ,""
                                      ,time_index
                                      ,stats
                                      );
    if (ret == 1) {
        struct adios_group_struct * g = (struct adios_group_struct *) *id;
        g->all_unique_var_names = adios_flag_no;
    }
    return adios_errno;
}

int adios_free_group (int64_t id)
{
    adios_errno = err_no_error;
    adios_common_free_group (id);
    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////

// adios_common_define_var is in adios_internals.c

// declare a single var as an entry in a group
int64_t adios_define_var (int64_t group_id, const char * name
                         ,const char * path
                         ,enum ADIOS_DATATYPES type
                         ,const char * dimensions
                         ,const char * global_dimensions
                         ,const char * local_offsets
                         )
{
    adios_errno = err_no_error;
    return adios_common_define_var (group_id, name, path
                                   ,type
                                   ,dimensions
                                   ,global_dimensions, local_offsets
                                   );
}

// delete all variable definitions from a group
// Use if you want to define a new set of variables for the next output step.
int adios_delete_vardefs (int64_t id)
{
    adios_errno = err_no_error;
    if (id != 0) {
        struct adios_group_struct * g = (struct adios_group_struct *) id;
        adios_common_delete_vardefs (g);
    } else {
        adios_error (err_invalid_group, "adios_delete_vardefs() called with 0 argument\n");
    }
    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
// adios_common_set_transform is in adios_internals.c
// set the transform method for the selected variable (default is "none")
int adios_set_transform (int64_t var_id, const char *transform_type_str)
{
    adios_errno = err_no_error;
    return adios_common_set_transform (var_id, transform_type_str);
}


///////////////////////////////////////////////////////////////////////////////

// adios_common_define_attribute is in adios_internals.c

int adios_define_attribute (int64_t group, const char * name
                           ,const char * path, enum ADIOS_DATATYPES type
                           ,const char * value, const char * var
                           )
{
    adios_errno = err_no_error;
    adios_common_define_attribute (group, name, path, type, value, var);
    return adios_errno;
}

// delete all attribute definitions from a group
// Use if you want to define a new set of attributes for the next output step.
int adios_delete_attrdefs (int64_t id)
{
    adios_errno = err_no_error;
    if (id != 0) {
        struct adios_group_struct * g = (struct adios_group_struct *) id;
        adios_common_delete_attrdefs (g);
    } else {
        adios_error (err_invalid_group, "adios_delete_attrdefs() called with 0 argument\n");
    }
    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////

// adios_common_select_method is in adios_internals_mxml.c
int adios_select_method (int64_t group, const char * method
                        ,const char * parameters
                        ,const char * base_path
                        )
{
    adios_errno = err_no_error;
    adios_common_select_method_by_group_id (0, method, parameters, group
                                            ,base_path, 0
                                            );
    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
void adios_timing_write_xml (int64_t fd_p, const char* filename)
{
    // defined in adios_timing.c
    adios_timing_write_xml_common (fd_p, filename);
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_schema_version (int64_t group_id, char * schema_version)
{
    struct adios_group_struct * g = (struct adios_group_struct *) group_id;
    return adios_common_define_schema_version (g, schema_version);
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_var_mesh (int64_t group_id, const char * varname, const char * meshname)
{
    return adios_common_define_var_mesh ( group_id, varname, meshname, ""); 
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_var_centering (int64_t group_id, const char * varname, const char * centering)
{
    return adios_common_define_var_centering (group_id, varname, centering, "");
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_mesh_file (int64_t group_id, char * name, char * file)
{
    return  adios_common_define_mesh_file (group_id, name, file);
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_var_timesteps (const char * timesteps, int64_t group_id, const char * name)
{
    struct adios_group_struct * g = (struct adios_group_struct *) group_id;
    return adios_common_define_var_timesteps (timesteps, g, name, "");
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_var_timescale (const char * timescale, int64_t group_id, const char * name)
{
    struct adios_group_struct * g = (struct adios_group_struct *) group_id;
    return adios_common_define_var_timescale (timescale, g, name, "");
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_var_timeseriesformat (const char * timeseries, int64_t group_id, const char * name)
{
    struct adios_group_struct * g = (struct adios_group_struct *) group_id;
    return adios_common_define_var_timeseriesformat (timeseries, g, name, "");
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_var_hyperslab ( const char * hyperslab, int64_t group_id, const char * name)
{
    struct adios_group_struct * g = (struct adios_group_struct *) group_id;
    return adios_common_define_var_hyperslab (hyperslab, g, name, ""); 
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_mesh_timevarying (const char * timevarying, int64_t group_id, const char * name)
{
    return adios_common_define_mesh_timeVarying (timevarying, group_id, name);
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_mesh_timesteps (const char * timesteps, int64_t group_id, const char * name)
{
    struct adios_group_struct * g = (struct adios_group_struct *) group_id;
    return adios_common_define_mesh_timeSteps (timesteps, g, name);
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_mesh_timescale (const char * timescale, int64_t group_id, const char * name)
{
    struct adios_group_struct * g = (struct adios_group_struct *) group_id;
    return adios_common_define_mesh_timeScale (timescale, g, name);
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_mesh_timeseriesformat (const char * timeseries, int64_t group_id, const char * name)
{
    struct adios_group_struct * g = (struct adios_group_struct *) group_id;
    return adios_common_define_mesh_timeSeriesFormat (timeseries, g, name);
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_mesh_group (const char * group, int64_t group_id, const char * name)
{
    return adios_common_define_mesh_group (group_id, name, group);
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_mesh_uniform (char * dimensions,
                               char * origin,
                               char * spacing,
                               char * maximum,
                               char * nspace,
                               int64_t group_id,
                               const char * name
                              )
{
//    struct adios_group_struct * g = (struct adios_group_struct *) group_id;
    return adios_common_define_mesh_uniform (dimensions, origin, spacing, maximum, nspace, name, group_id);
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_mesh_rectilinear (char * dimensions,
                                   char * coordinates,
                                   char * nspace,
                                   int64_t group_id,
                                   const char * name
                                  )
{
//    struct adios_group_struct * g = (struct adios_group_struct *) group_id;
    return adios_common_define_mesh_rectilinear (dimensions, coordinates, nspace, name, group_id);
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_mesh_structured (char * dimensions,
                                  char * points,
                                  char * nspace,
                                  int64_t group_id,
                                  const char * name
                                 )
{
//    struct adios_group_struct * g = (struct adios_group_struct *) group_id;
    return adios_common_define_mesh_structured (dimensions, nspace, points, name, group_id);
}

///////////////////////////////////////////////////////////////////////////////
int adios_define_mesh_unstructured (char * points,
                                    char * data,
                                    char * count,
                                    char * cell_type,
                                    char * npoints,
                                    char * nspace,
                                    int64_t group_id,
                                    const char * name
                                   )
{
//    struct adios_group_struct * g = (struct adios_group_struct *) group_id;
    return adios_common_define_mesh_unstructured (points, data, count, cell_type, nspace, npoints, name, group_id);
}

