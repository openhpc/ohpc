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
#include <sys/time.h> // gettimeofday
#include <assert.h>

// xml parser
#include <mxml.h>

#include "core/adios_internals.h"
#include "core/adios_internals_mxml.h"
#include "core/common_adios.h"
#include "core/adios_bp_v1.h"
#include "core/buffer.h"
#include "core/adios_transport_hooks.h"
#include "core/adios_logger.h"
#include "core/adios_timing.h"
#include "core/qhashtbl.h"
#include "adios_error.h"

// NCSU ALACRITY-ADIOS
#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_read.h"
#include "core/transforms/adios_transforms_write.h"

#ifdef WITH_NCSU_TIMER
#include "timer.h"
#endif

#ifdef DMALLOC
#include "dmalloc.h"
#endif

extern struct adios_transport_struct * adios_transports;
extern int adios_errno;

///////////////////////////////////////////////////////////////////////////////
int common_adios_init (const char * config, MPI_Comm comm)
{
    // parse the config file
    adios_errno = err_no_error;
    adios_parse_config (config, comm);
    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
// all XML file pieces will be provided by another series of calls
int common_adios_init_noxml (MPI_Comm comm)
{
    adios_errno = err_no_error;
    adios_local_config (comm);
    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
int common_adios_finalize (int mype)
{
    struct adios_method_list_struct * m;

    adios_errno = err_no_error;
    for (m = adios_get_methods (); m; m = m->next)
    {
        if (   m->method->m != ADIOS_METHOD_UNKNOWN
            && m->method->m != ADIOS_METHOD_NULL
            && adios_transports [m->method->m].adios_finalize_fn
           )
        {
            adios_transports [m->method->m].adios_finalize_fn (mype, m->method);
        }
    }

    adios_cleanup ();

#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_finalize ();
#endif

    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
int common_adios_allocate_buffer (enum ADIOS_BUFFER_ALLOC_WHEN adios_buffer_alloc_when
                                 ,uint64_t buffer_size)
{
    adios_errno = err_no_error;
    adios_buffer_size_requested_set (buffer_size * 1024 * 1024);
    adios_buffer_alloc_when_set (adios_buffer_alloc_when);
    adios_set_buffer_size ();
    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
// Drew: used for experiments
uint32_t pinned_timestep = 0;
void adios_pin_timestep(uint32_t ts) {
  pinned_timestep = ts;
}

///////////////////////////////////////////////////////////////////////////////
int common_adios_open (int64_t * fd, const char * group_name
                ,const char * name, const char * file_mode, MPI_Comm comm
               )
{
#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_start ("adios_open_to_close");
    timer_start ("adios_open");
#endif

    int64_t group_id = 0;
    struct adios_file_struct * fd_p = (struct adios_file_struct *)
                                  malloc (sizeof (struct adios_file_struct));
    struct adios_group_struct * g = 0;
    struct adios_method_list_struct * methods = 0;
    enum ADIOS_METHOD_MODE mode;

    adios_errno = err_no_error;
    adios_common_get_group (&group_id, group_name);
    g = (struct adios_group_struct *) group_id;
    methods = g->methods;

    if (!strcasecmp (file_mode, "r"))
        mode = adios_mode_read;
    else
        if (!strcasecmp (file_mode, "w"))
            mode = adios_mode_write;
        else
            if (!strcasecmp (file_mode, "a"))
                mode = adios_mode_append;
            else
                if (!strcasecmp (file_mode, "u"))
                    mode = adios_mode_update;
                else
                {
                    adios_error(err_invalid_file_mode,
                        "adios_open: unknown file mode: %s, supported r,w,a,u\n",
                        file_mode);

                    *fd = 0;

                    return adios_errno;
                }

    fd_p->name = strdup (name);
    fd_p->subfile_index = -1; // subfile index is by default -1
    fd_p->group = g;
    fd_p->mode = mode;
    fd_p->data_size = 0;
    fd_p->buffer = 0;
    fd_p->offset = 0;
    fd_p->bytes_written = 0;
    fd_p->buffer_size = 0;
    fd_p->vars_start = 0;
    fd_p->vars_written = 0;
    fd_p->write_size_bytes = 0;
    fd_p->base_offset = 0;
    fd_p->pg_start_in_file = 0;
    if (comm != MPI_COMM_NULL)
        MPI_Comm_dup(comm, &fd_p->comm);
    else
        fd_p->comm = MPI_COMM_NULL;


#if 1
    /* Time index magic done here */
    if (mode == adios_mode_write)
    {
        /* Traditionally, time=1 at the first step, and for subsequent file
           creations, time increases. Although, each file contains one step,
           the time index indicates that they are in a series.
        */
        g->time_index++;
    }
    else if (mode == adios_mode_append)
    {
        g->time_index++;
    }
    else if (mode == adios_mode_update && g->time_index > 1)
    {
        /* Update from Append differs only in the time index. All methods had
           code for Append, now for Update we decrease the counter by one,
           for all methods. (But do not go below 1).
        */
        g->time_index--;
    }
    /* time starts from 1 not from 0 (traditionally; now no one cares */
    if (g->time_index == 0)
        g->time_index = 1;
#else
    /* old way pre-1.4*/
    if (mode != adios_mode_read)
        g->time_index++;
#endif

    // Drew: for experiments
    if (pinned_timestep > 0)
        g->time_index = pinned_timestep;

    while (methods)
    {
        if (   methods->method->m != ADIOS_METHOD_UNKNOWN
            && methods->method->m != ADIOS_METHOD_NULL
            && adios_transports [methods->method->m].adios_open_fn
           )
        {
            adios_transports [methods->method->m].adios_open_fn
                                                 (fd_p, methods->method, fd_p->comm);
        }

        methods = methods->next;
    }

    *fd = (int64_t) fd_p;

#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_stop ("adios_open");
#endif
    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
static const char ADIOS_ATTR_PATH[] = "/__adios__";

int common_adios_group_size (int64_t fd_p
                     ,uint64_t data_size
                     ,uint64_t * total_size
                     )
{
#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_start ("adios_group_size");
#endif
    adios_errno = err_no_error;
    struct adios_file_struct * fd = (struct adios_file_struct *) fd_p;
    if (!fd)
    {
        adios_error (err_invalid_file_pointer, "Invalid handle passed to adios_group_size\n");
        return adios_errno;
    }
    struct adios_method_list_struct * m = fd->group->methods;
    if (m && m->next == NULL && m->method->m == ADIOS_METHOD_NULL)
    {
        // nothing to do so just return
        fd->shared_buffer = adios_flag_no;
        fd->write_size_bytes = 0;
        fd->buffer = 0;
        *total_size = 0;
#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_stop ("adios_group_size");
#endif
        return err_no_error;
    }

    /* Add ADIOS internal attributes now (should be before calculating the overhead) */
    if (fd->mode != adios_mode_read &&
         (fd->group->process_id == 0 || fd->subfile_index != -1)
       )
    {
        struct timeval tp;
        char epoch[16];
        gettimeofday(&tp, NULL);
        sprintf(epoch, "%d", (int) tp.tv_sec);

        int def_adios_init_attrs = 1;
        // if we append/update, define these attributes only at the first step
        if (fd->mode != adios_mode_write && fd->group->time_index > 1)
            def_adios_init_attrs = 0;

        if (def_adios_init_attrs) {
            log_debug ("Define ADIOS extra attributes, "
                       "time = %d, rank = %d, epoch = %s subfile=%d\n",
                       fd->group->time_index, fd->group->process_id, epoch, fd->subfile_index);

            adios_common_define_attribute ((int64_t)fd->group, "version", ADIOS_ATTR_PATH,
                    adios_string, VERSION, NULL);

            adios_common_define_attribute ((int64_t)fd->group, "create_time_epoch", ADIOS_ATTR_PATH,
                    adios_integer, epoch, NULL);
            adios_common_define_attribute ((int64_t)fd->group, "update_time_epoch", ADIOS_ATTR_PATH,
                    adios_integer, epoch, NULL);
            // id of last attribute is fd->group->member_count
            fd->group->attrid_update_epoch = fd->group->member_count;

        }
        /* FIXME: this code works fine, it does not duplicate the attribute,
           but the index will still contain all copies and the read will see
           only the first one. Thus updating an attribute does not work
           in practice.
         */
        else
        {
            // update attribute of update time (define would duplicate it)
            struct adios_attribute_struct * attr = adios_find_attribute_by_id
                   (fd->group->attributes, fd->group->attrid_update_epoch);
            if (attr) {
                log_debug ("Update ADIOS extra attribute name=%s, "
                           "time = %d, rank = %d, epoch = %s, subfile=%d\n",
                           attr->name, fd->group->time_index, fd->group->process_id,
                           epoch, fd->subfile_index);

                free(attr->value);
                adios_parse_scalar_string (adios_integer, (void *) epoch, &attr->value);
            }
        }
    }

#ifdef ADIOS_TIMERS
    int tv_size = adios_add_timing_variables (fd);
    data_size += tv_size;
#endif

    fd->write_size_bytes = data_size;

    uint64_t overhead = adios_calc_overhead_v1 (fd);

    *total_size = data_size + overhead;

    // try to reserve a buffer using the adios_method_buffer_alloc
    // if it does not give the correct amount, overflow.  Make sure
    // the amount given is big enough for the first part of the file.

    fd->write_size_bytes += overhead;

    // NCSU ALACRITY-ADIOS - Current solution to group_size problem: find
    // the most "expansive" transform method used in the file, and assume
    // all of the data uses that method, for a very rough but safe upper bound.
    //
    // (see the comment in the top of adios_transforms.c, under section
    // 'The "group size" problem,' for more details)
    uint64_t wc_transformed_size = adios_transform_worst_case_transformed_group_size(data_size, fd);
    if (wc_transformed_size > data_size) {
        log_debug("Computed worst-case bound on transformed data for a group size of %llu is %llu; increasing group size to match.\n",
                  data_size, wc_transformed_size);

        fd->write_size_bytes += (wc_transformed_size - data_size);
        *total_size += (wc_transformed_size - data_size);
    }

    uint64_t allocated = adios_method_buffer_alloc (fd->write_size_bytes);
    if (allocated != fd->write_size_bytes)
    {
        fd->shared_buffer = adios_flag_no;

        log_warn ("adios_group_size (%s): Not buffering. "
                  "needs: %llu available: %llu.\n",
                  fd->group->name, fd->write_size_bytes, allocated);
    }
    else
    {
        fd->shared_buffer = adios_flag_yes;
    }

    // Drew: for experiments
    if (pinned_timestep != 0)
        fd->group->time_index = pinned_timestep;

    // call each transport method to coordinate the write and handle
    // if an overflow is detected.
    // now tell each transport attached that it is being written
    while (m)
    {
        enum ADIOS_FLAG should_buffer = adios_flag_yes;
        if (   m->method->m != ADIOS_METHOD_UNKNOWN
            && m->method->m != ADIOS_METHOD_NULL
            && adios_transports [m->method->m].adios_should_buffer_fn
           )
        {
            should_buffer = adios_transports [m->method->m].
                                            adios_should_buffer_fn (fd
                                                                   ,m->method
                                                                   );
        }

        if (should_buffer == adios_flag_no)     // can't write directly since
            fd->shared_buffer = adios_flag_no;  // some might want to share

        m = m->next;
    }

    // Drew: for experiments
    if (pinned_timestep != 0)
        fd->group->time_index = pinned_timestep;

    if (fd->shared_buffer == adios_flag_no)
    {
        adios_method_buffer_free (allocated);
        fd->buffer = 0;
        fd->offset = 0;
        fd->bytes_written = 0;
    }
    else
    {
        fd->buffer = malloc (fd->write_size_bytes);
        fd->buffer_size = fd->write_size_bytes;
        fd->offset = 0;
        fd->bytes_written = 0;
        if (!fd->buffer)
        {
            adios_error (err_no_memory, "Cannot allocate %llu bytes for buffered output.\n",
                    fd->write_size_bytes);

            return adios_errno;
        }
        else
        {
            // write the process group header
            adios_write_process_group_header_v1 (fd, *total_size);

            // setup for writing vars
            adios_write_open_vars_v1 (fd);
        }
    }

#ifdef ADIOS_TIMERS
    adios_write_timing_variables (fd);
#endif


#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_stop ("adios_group_size");
#endif
    // each var will be added to the buffer by the adios_write calls
    // attributes will be added by adios_close

    return adios_errno;
}

int common_adios_write_byid (struct adios_file_struct * fd, struct adios_var_struct * v, void * var)
{
    struct adios_method_list_struct * m = fd->group->methods;

    adios_errno = err_no_error;
    if (m && m->next == NULL && m->method->m == ADIOS_METHOD_NULL)
    {
        return adios_errno;
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
                    return adios_errno;
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
                    return adios_errno;
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

    if (!adios_errno) {
        if (fd->mode == adios_mode_write || fd->mode == adios_mode_append)
        {
            adios_copy_var_written (fd->group, v);
        }
    }

    return adios_errno;
}

static int common_adios_write_transform_helper(struct adios_file_struct * fd, struct adios_var_struct * v) {
    int use_shared_buffer = (fd->shared_buffer == adios_flag_yes);
    int wrote_to_shared_buffer = 0;

    // If we are using the shared buffer, transform the data directly into it
    if (use_shared_buffer) {
        uint16_t header_size = adios_calc_var_overhead_v1(v);
        uint64_t header_offset;
        uint64_t payload_offset;
        uint64_t end_offset;

        // Reserve space for the variable header (it will need to be written after
        // the transform to capture updated metadata)
        header_offset = fd->offset;
        fd->offset += header_size;
        payload_offset = fd->offset;

        // This function will either:
        // a) write to the shared buffer, leave v->data, v->data_size and
        //    v->free_data untouched, and return 1, OR
        // b) write to v->data, set v->data_size and v->free_data, and return 0
        //
        int success = adios_transform_variable_data(fd, v, use_shared_buffer, &wrote_to_shared_buffer);
        if (!success) {
            fd->offset = header_offset;
            return 0;
        }

        // Assumption: we don't change the header size, just contents, in
        // adios_transform_variable_data
        assert(adios_calc_var_overhead_v1(v) == header_size);

        // Store the ending offset of the payload write (if any)
        end_offset = fd->offset;

        // Rewind and write the header back where it should be
        fd->offset = header_offset;
        // var payload sent for sizing information
        adios_write_var_header_v1(fd, v);

        assert(fd->offset == payload_offset);

        // If the data was stored to the shared buffer, update v->data,
        // v->data_size and v->free_data. Else, write the payload to the shared
        // buffer (the other v->* fields have already been updated)
        if (wrote_to_shared_buffer) {
            v->data = fd->buffer + payload_offset;
            v->data_size = end_offset - payload_offset;
            v->free_data = adios_flag_no;

            // Update the buffer back to the end of the header+payload
            fd->offset = end_offset;
        } else {
            // write payload
            adios_write_var_payload_v1 (fd, v);

            // fd->offset now points to the end of the header+payload
        }

        // Success!
        return 1;
    } else {
        int ret = adios_transform_variable_data(fd, v, use_shared_buffer, &wrote_to_shared_buffer);

        assert(!wrote_to_shared_buffer);
        assert(v->data);

        return ret;
    }
}

///////////////////////////////////////////////////////////////////////////////
/* common_adios_write is just a partial implementation. It expects filled out
 * structures. This is because C and Fortran implementations of adios_write are
 * different for some part and this is the common part.
 */
int common_adios_write (struct adios_file_struct * fd, struct adios_var_struct * v, void * var)
{
#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_start ("adios_write");
#endif
    adios_errno = err_no_error;
    struct adios_method_list_struct * m = fd->group->methods;

    // NCSU ALACRITY-ADIOS - Do some processing here depending on the transform
    //   type specified (if any)

    // First, before doing any transform (or none), compute variable statistics,
    // as we can't do this after the data is transformed
    adios_generate_var_characteristics_v1 (fd, v);

    // If no transform is specified, do the normal thing (write to shared
    // buffer immediately, if one exists)
    if (v->transform_type == adios_transform_none)
    {
        if (fd->shared_buffer == adios_flag_yes)
        {
            // var payload sent for sizing information
            adios_write_var_header_v1 (fd, v);

            // write payload
            adios_write_var_payload_v1 (fd, v);
        }
    }
    // Else, do a transform
    else
    {
#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_start ("adios_transform");
#endif
        int success = common_adios_write_transform_helper(fd, v);
        if (success) {
            // Make it appear as if the user had supplied the transformed data
            var = v->data;
        } else {
            log_error("Error: unable to apply transform %s to variable %s; likely ran out of memory, check previous error messages\n", adios_transform_plugin_primary_xml_alias(v->transform_type), v->name);
            // FIXME: Reverse the transform metadata and write raw data as usual
        }
#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_stop ("adios_transform");
#endif
    }

    // now tell each transport attached that it is being written
    while (m)
    {
        if (   m->method->m != ADIOS_METHOD_UNKNOWN
            && m->method->m != ADIOS_METHOD_NULL
            && adios_transports [m->method->m].adios_write_fn
           )
        {
            adios_transports [m->method->m].adios_write_fn
                                   (fd, v, var, m->method);
        }

        m = m->next;
    }

    // NCSU ALACRITY-ADIOS - Free transform-method-allocated data buffers
    if (v->dimensions)
    {
        // TODO: Is this correct? Normally v->data is a user buffer, so we
        //   can't free it. However, after a transform, we probably do need
        //   to free it. We mark this by setting the free_data flag. However,
        //   as this flag is hardly ever used, I don't know whether this is
        //   using the flag correctly or not. Need verification with
        //   Gary/Norbert/someone knowledgable about ADIOS internals.
        if (v->transform_type != adios_transform_none && v->free_data == adios_flag_yes && v->data)
            free(v->data);
        v->data = 0;
    }

    v->write_count++;
#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_stop ("adios_write");
#endif
    // printf ("var: %s written %d\n", v->name, v->write_count);
    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
int common_adios_get_write_buffer (int64_t fd_p, const char * name
                           ,uint64_t * size
                           ,void ** buffer
                           )
{
    struct adios_file_struct * fd = (struct adios_file_struct *) fd_p;
    adios_errno = err_no_error;
    if (!fd)
    {
        adios_error (err_invalid_file_pointer, "Invalid handle passed to adios_group_size\n");
        return adios_errno;
    }
    struct adios_var_struct * v = fd->group->vars;
    struct adios_method_list_struct * m = fd->group->methods;

    v = adios_find_var_by_name (fd->group, name);

    if (!v)
    {
        adios_error (err_invalid_varname, "Bad var name (ignored): '%s' (%c%c%c)\n",
                     name, name[0], name[1], name[2]);
        return adios_errno;
    }

    if (fd->mode == adios_mode_read)
    {
        adios_error (err_invalid_file_mode,
                     "write attempted on %s in %s. This was opened for read\n",
                     name , fd->name);
        return adios_errno;
    }

    // since we are only getting one buffer, get it from the first
    // transport method that can provide it.
    while (m)
    {
        if (   m->method->m != ADIOS_METHOD_UNKNOWN
            && m->method->m != ADIOS_METHOD_NULL
            && adios_transports [m->method->m].adios_get_write_buffer_fn
           )
        {
            adios_transports [m->method->m].adios_get_write_buffer_fn
                                (fd, v, size, buffer, m->method);
            m = 0;
        }
        else
            m = m->next;
    }

    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////

int common_adios_read (int64_t fd_p, const char * name, void * buffer
               ,uint64_t buffer_size
               )
{
    struct adios_file_struct * fd = (struct adios_file_struct *) fd_p;
    adios_errno = err_no_error;
    if (!fd)
    {
        adios_error (err_invalid_file_pointer, "Invalid handle passed to adios_group_size\n");

        return adios_errno;
    }
    struct adios_var_struct * v;
    struct adios_method_list_struct * m = fd->group->methods;

    if (m && m->next == NULL && m->method->m == ADIOS_METHOD_NULL)
    {
        // nothing to do so just return
        return err_no_error;
    }

    if (!(fd->mode == adios_mode_read))
    {
        adios_error (err_invalid_file_mode,
                     "read attempted on %s which was opened for write\n",
                     fd->name);

        return adios_errno;
    }

    v = adios_find_var_by_name (fd->group, name);
    if (v)
    {
        // since can only read from one place into the buffer,
        // read from the first transport method that can
        while (m)
        {
            if (   m->method->m != ADIOS_METHOD_UNKNOWN
                && m->method->m != ADIOS_METHOD_NULL
                && adios_transports [m->method->m].adios_read_fn
               )
            {
                adios_transports [m->method->m].adios_read_fn
                                     (fd, v, buffer, buffer_size, m->method);
                m = 0;
            }
            else
                m = m->next;
    }
    }
    else
    {
        adios_error (err_invalid_varname, "var %s in file %s not found on read\n",
                     name, fd->name);

        return adios_errno;
    }

    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
// OBSOLETE, kept only for backward compatibility
int common_adios_set_path (int64_t fd_p, const char * path)
{
    struct adios_file_struct * fd = (struct adios_file_struct *) fd_p;
    adios_errno = err_no_error;
    if (!fd)
    {
        adios_error (err_invalid_file_pointer, "Invalid handle passed to adios_set_path\n");

        return adios_errno;
    }
    struct adios_group_struct * t = fd->group;
    struct adios_var_struct * v = t->vars;
    struct adios_attribute_struct * a = t->attributes;

    while (v)
    {
        if (v->path)
        {
            free (v->path);
        }

        v->path = strdup (path);

        v = v->next;
    }

    while (a)
    {
        // skip internal attributes
        if (a->path && strstr (a->path, "__adios__"))
        {
            a = a->next;
            continue;
        }

        if (a->path)
        {
            free (a->path);
        }

        a->path = strdup (path);

        a = a->next;
    }

    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
// OBSOLETE, kept only for backward compatibility
// Inconsistent behavior with new ADIOS variable naming
// The variable is not replaced with the new path in the hash table here, so
// it is still found using the old path!
int common_adios_set_path_var (int64_t fd_p, const char * path
                       ,const char * name
                       )
{
    struct adios_file_struct * fd = (struct adios_file_struct *) fd_p;
    adios_errno = err_no_error;
    if (!fd)
    {
        adios_error (err_invalid_file_pointer, "Invalid handle passed to adios_set_path_var\n");

        return adios_errno;
    }
    struct adios_group_struct * t = fd->group;
    struct adios_var_struct * v = t->vars;

    // check for vars and then attributes
    v = adios_find_var_by_name (t, name);

    if (v)
    {
        if (v->path)
        {
            free (v->path);
        }

        v->path = strdup (path);

        /* Possible new behavior: replace the old path with the new path
         * in the hash table so that the variable is found by
         * the new path. Inconsistent with old codes that only used
         * the variable name without the path in adios_write()
         */
        //remove var from hash table by old fullpath...
        //t->hashtbl_vars->remove (t->hashtbl_vars, name);
        // and add it back with new fullpath
        //t->hashtbl_vars->put2 (t->hashtbl_vars, v->path, v->name, v);

    }
    else
    {
        adios_error (err_invalid_varname,
                     "adios_set_path_var (path=%s, var=%s): var not found\n",
                     path, name);

        return adios_errno;
    }

    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
// hint that we reached the end of an iteration (for asynchronous pacing)
int common_adios_end_iteration ()
{
    struct adios_method_list_struct * m;

    adios_errno = err_no_error;
    for (m = adios_get_methods (); m; m = m->next)
    {
        if (   m->method->m != ADIOS_METHOD_UNKNOWN
            && m->method->m != ADIOS_METHOD_NULL
            && adios_transports [m->method->m].adios_end_iteration_fn
           )
        {
            adios_transports [m->method->m].adios_end_iteration_fn
                                                (m->method);
        }
    }

    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
// hint to start communicating
int common_adios_start_calculation ()
{
    struct adios_method_list_struct * m;

    adios_errno = err_no_error;
    for (m = adios_get_methods (); m; m = m->next)
    {
        if (   m->method->m != ADIOS_METHOD_UNKNOWN
            && m->method->m != ADIOS_METHOD_NULL
            && adios_transports [m->method->m].adios_start_calculation_fn
           )
        {
            adios_transports [m->method->m].adios_start_calculation_fn
                                                  (m->method);
        }
    }

    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
// hint to stop communicating
int common_adios_stop_calculation ()
{
    struct adios_method_list_struct * m;

    adios_errno = err_no_error;
    for (m = adios_get_methods (); m; m = m->next)
    {
        if (   m->method->m != ADIOS_METHOD_UNKNOWN
            && m->method->m != ADIOS_METHOD_NULL
            && adios_transports [m->method->m].adios_stop_calculation_fn
           )
        {
            adios_transports [m->method->m].adios_stop_calculation_fn
                                                   (m->method);
        }
    }

    return adios_errno;
}

///////////////////////////////////////////////////////////////////////////////
int common_adios_close (int64_t fd_p)
{
#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_start ("adios_close");
#endif
    adios_errno = err_no_error;

    struct adios_file_struct * fd = (struct adios_file_struct *) fd_p;
    if (!fd)
    {
        adios_error (err_invalid_file_pointer, "Invalid handle passed to adios_close\n");

        return adios_errno;
    }
    struct adios_method_list_struct * m = fd->group->methods;
    if (m && m->next == NULL && m->method->m == ADIOS_METHOD_NULL)
    {
        // nothing to do so just return
#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_stop ("adios_close");
    timer_stop ("adios_open_to_close");
#endif
        return 0;
    }

    struct adios_attribute_struct * a = fd->group->attributes;
    struct adios_var_struct * v = fd->group->vars;

    if (fd->shared_buffer == adios_flag_yes)
    {
        adios_write_close_vars_v1 (fd);

        /* FIXME: this strategy writes all attributes defined in time step 0
           and thus duplicates them in the PGs and in the attribute index.
           For write mode, where files are new, this is good.
           For append/update it is unnecessary and replicates the attributes
           in the index.
           One should write the newly created attributes only in append mode.
        */
        adios_write_open_attributes_v1 (fd);

        if (!fd->group->process_id || fd->subfile_index != -1) {
            // from ADIOS 1.4, only rank 0 writes attributes (or to subfiles)
            while (a) {
                adios_write_attribute_v1 (fd, a);
                a = a->next;
            }
        }

        adios_write_close_attributes_v1 (fd);
    }

    // in order to get the index assembled, we need to do it in the
    // transport once we have collected all of the pieces

    // now tell all of the transports to write the buffer during close
    for (;m; m = m->next)
    {
        if (   m->method->m != ADIOS_METHOD_UNKNOWN
            && m->method->m != ADIOS_METHOD_NULL
            && adios_transports [m->method->m].adios_close_fn
           )
        {
            adios_transports [m->method->m].adios_close_fn
                                 (fd, m->method);
        }
    }

    if (fd->shared_buffer == adios_flag_yes)
    {
        adios_method_buffer_free (fd->write_size_bytes);
        free (fd->buffer);
        fd->buffer_size = 0;
        fd->buffer = 0;
        fd->offset = 0;
    }

    while (v)
    {
        v->write_offset = 0;
        if (v->data)
        {
            free (v->data);
            v->data = 0;
        }

        v = v->next;
    }

    while (fd->group->vars_written)
    {
        if (fd->group->vars_written->name)
            free (fd->group->vars_written->name);
        if (fd->group->vars_written->path)
            free (fd->group->vars_written->path);

        while (fd->group->vars_written->dimensions)
        {
            struct adios_dimension_struct * dimensions
                            = fd->group->vars_written->dimensions->next;

            free (fd->group->vars_written->dimensions);
            fd->group->vars_written->dimensions = dimensions;
        }

        // NCSU - Clear stat
        if (fd->group->vars_written->stats)
        {
            uint8_t j = 0, idx = 0;
            uint8_t c = 0, count = adios_get_stat_set_count(fd->group->vars_written->type);

            for (c = 0; c < count; c ++)
            {
                while (fd->group->vars_written->bitmap >> j)
                {
                    if ((fd->group->vars_written->bitmap >> j) & 1)
                    {
                        if (j == adios_statistic_hist)
                        {
                            struct adios_hist_struct * hist = (struct adios_hist_struct *) (fd->group->vars_written->stats[c][idx].data);
                            free (hist->breaks);
                            free (hist->frequencies);
                            free (hist);
                        }
                        else
                            free (fd->group->vars_written->stats[c][idx].data);

                        idx ++;
                    }
                    j ++;
                }
                free (fd->group->vars_written->stats[c]);
            }
            free (fd->group->vars_written->stats);
        }

        // NCSU ALACRITY-ADIOS - Clear transform metadata
        adios_transform_clear_transform_var(fd->group->vars_written);

        if (fd->group->vars_written->data)
            free (fd->group->vars_written->data);

        v = fd->group->vars_written->next;
        free (fd->group->vars_written);
        fd->group->vars_written = v;
    }


#ifdef ADIOS_TIMER_EVENTS
    char* extension = ".perf";
    int name_len = strlen (fd->name);
    int fn_len = name_len + strlen (extension) + 1;
    char* fn = (char*) malloc (sizeof (char) * fn_len);
    
    sprintf (fn, "%s%s", fd->name, extension);

    adios_timing_write_xml_common (fd_p, fn);
#endif


    if (fd->name)
    {
        free (fd->name);
        fd->name = 0;
    }

    if (fd->comm != MPI_COMM_NULL) {
        MPI_Comm_free (&fd->comm);
    }

    free ((void *) fd_p);
#if defined(WITH_NCSU_TIMER) && defined(TIMER_LEVEL) && (TIMER_LEVEL <= 0)
    timer_stop ("adios_close");
    timer_stop ("adios_open_to_close");
//    printf ("Timers, ");
//    printf ("%d, ", fd->group->process_id);
//    printf ("%d, ", fd->group->time_index);
//    printf ("%lf, ", timer_get_total_interval ("adios_open" ));
//    printf ("%lf, ", timer_get_total_interval ("adios_group_size"));
//    printf ("%lf, ", timer_get_total_interval ("adios_transform" ));
//    printf ("%lf, ", timer_get_total_interval ("adios_write" ));
//    printf ("%lf\n", timer_get_total_interval ("adios_close"     ));
//    timer_reset_timers ();

    printf("[TIMERS] Proc: %d Time: %d ", fd->group->process_id, fd->group->time_index);
    int i;
    timer_result_t *results = timer_get_results_sorted();
    for (i = 0; i < timer_get_num_timers(); i++) {
        printf("%s: %0.4lf ", results[i].name, results[i].time);
    }
    printf("\n");
    free(results);

    //timer_reset_timers ();
#endif


    return adios_errno;
}

//////////////////////////////////////////////////////////////////////////////
// Methods normally only called by the XML parser
//////////////////////////////////////////////////////////////////////////////

// adios_common_declare_group is in adios_internals.c
// adios_common_define_var is in adios_internals.c
// adios_common_define_attribute is in adios_internals.c
// adios_common_select_method is in adios_internals.c
