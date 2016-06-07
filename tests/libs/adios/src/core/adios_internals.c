/*
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#include "config.h" /* VERSION_xxx */
#include <math.h>
#include <string.h>
#include <ctype.h>  /* isdigit() */
#include <unistd.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <arpa/inet.h>
#include <stdint.h>
#include <sys/stat.h>
#include <assert.h>

#include "adios.h"
#include "core/adios_internals.h"
#include "core/adios_bp_v1.h"
#include "core/qhashtbl.h"
#include "core/adios_logger.h"

#ifdef DMALLOC
#include "dmalloc.h"
#endif

// NCSU ALACRITY-ADIOS - Added header file
#include "core/transforms/adios_transforms_common.h"
#include "core/transforms/adios_transforms_hooks.h"
#include "core/transforms/adios_transforms_read.h"
#include "core/transforms/adios_transforms_write.h"
#include "core/transforms/adios_transforms_specparse.h"

struct adios_method_list_struct * adios_methods = 0;
struct adios_group_list_struct * adios_groups = 0;

int adios_int_is_var (const char * temp) // 1 == yes, 0 == no
{
    if (!temp)
        return 1;

    if (*temp == '-' || isdigit (*temp))
    {
        temp++;
        while (*temp)
        {
            if (isdigit (*temp))
                temp++;
            else
                return 1;
        }
    }
    else
        return 1;

    return 0;
}

int adios_int_is_num (char * temp) // 1 == yes, 0 == no
{
    char * extra = 0;

    strtod (temp, &extra);

    if (extra)
        return 0;
    else
        return 1;

    return 0;
}

/** Find a variable in the hash table and return the pointer to the var struct.
 *  A full path is required (to match var->path+"/"+var->name)
 */
struct adios_var_struct * adios_find_var_by_name (struct adios_group_struct * g,
                                                  const char * fullpath)
{
    // Find variable in the hash table
    return  (struct adios_var_struct *)
            g->hashtbl_vars->get (g->hashtbl_vars, fullpath);
}

/*
struct adios_var_struct * adios_find_var_by_name (struct adios_var_struct * root
        ,const char * name
        ,enum ADIOS_FLAG unique_names
        )
{
    int done = 0;
    struct adios_var_struct * var = 0;

    if (!name)
    {
        done = 1;
        root = 0;
    }

    while (!done && root)
    {
        char * compare_name = root->name;
        char * compare_name_path = root->name;
        if (unique_names == adios_flag_no)
        {
            compare_name_path = malloc (  strlen (root->name)
                    + strlen (root->path)
                    + 2 // null term and '/'
                    );
            if (!strcmp (root->path, "/"))
                sprintf (compare_name_path, "/%s", root->name);
            else
                sprintf (compare_name_path, "%s/%s", root->path, root->name);
        }

        if (   !strcasecmp (name, compare_name)
                || (   unique_names == adios_flag_no
                    && !strcasecmp (name, compare_name_path)
                   )
           )
        {
            done = 1;
            var = root;
        }
        else
        {
            root = root->next;
        }

        if (unique_names == adios_flag_no)
        {
            free (compare_name_path);
        }
    }

    return var;
}
*/

struct adios_attribute_struct * adios_find_attribute_by_name
(struct adios_attribute_struct * root
 ,const char * name
 ,enum ADIOS_FLAG unique_names
 )
{
    int done = 0;
    struct adios_attribute_struct * attr = 0;

    if (!name)
    {
        done = 1;
        root = 0;
    }

    while (!done && root)
    {
        char * compare_name = root->name;
        char * compare_name_path = root->name;
        if (unique_names == adios_flag_no)
        {
            compare_name_path = malloc (  strlen (root->name)
                    + strlen (root->path)
                    + 2 // null term and '/'
                    );
            if (!root->path || !root->path[0])
                sprintf (compare_name_path, "%s", root->name);
            else if (!strcmp (root->path, "/"))
                sprintf (compare_name_path, "/%s", root->name);
            else
                sprintf (compare_name_path, "%s/%s", root->path, root->name);
        }

        if (   !strcasecmp (name, compare_name)
                || (   unique_names == adios_flag_no
                    && !strcasecmp (name, compare_name_path)
                   )
           )
        {
            done = 1;
            attr = root;
        }
        else
        {
            root = root->next;
        }

        if (unique_names == adios_flag_no)
        {
            free (compare_name_path);
        }
    }

    return attr;
}

struct adios_var_struct * adios_find_var_by_id (struct adios_var_struct * root
        ,uint32_t id
        )
{
    while (root)
    {
        if (root->id == id)
            return root;
        else
            root = root->next;
    }

    return NULL;
}

struct adios_attribute_struct * adios_find_attribute_by_id
(struct adios_attribute_struct * root
 ,uint32_t id
 )
{
    while (root)
    {
        if (root->id == id)
            return root;
        else
            root = root->next;
    }

    return NULL;
}

int adios_parse_dimension (const char * dimension
        ,const char * global_dimension
        ,const char * local_offset
        ,struct adios_group_struct * g
        ,struct adios_dimension_struct * dim
        )
{
    if (!dimension)
    {
        adios_error (err_dimension_required, "adios_parse_dimension: dimension not provided\n");

        return 0;
    }

    /* Get the local dimension */
    // one of the three fields below will be set, the other two remain 0
    dim->dimension.rank = 0;
    dim->dimension.var = NULL;
    dim->dimension.attr = NULL;

    dim->dimension.time_index = adios_flag_no;
    if ( g->time_index_name &&
         !strcasecmp (g->time_index_name, dimension)
       )
    {
        /* this is time dimension */
        dim->dimension.time_index = adios_flag_yes;
    }
    else if (adios_int_is_var (dimension))
    {
        struct adios_var_struct * var = 0;
        dim->dimension.rank = 0;
        var = adios_find_var_by_name (g, dimension);
        if (!var)
        {
            struct adios_attribute_struct * attr = 0;
            attr = adios_find_attribute_by_name (g->attributes, dimension
                    ,g->all_unique_var_names
                    );

            if (!attr)
            {
                adios_error (err_invalid_dimension,
                        "config.xml: invalid var dimension: %s\n",
                        dimension);

                return 0;
            }
            else
            {
                if (attr->var)
                {
                    switch (attr->var->type)
                    {
                        case adios_string:
                        case adios_real:
                        case adios_double:
                        case adios_long_double:
                        case adios_complex:
                        case adios_double_complex:
                            adios_error (err_invalid_var_as_dimension,
                                    "config.xml: dimension defining var %s "
                                    "pointed by attribute %s "
                                    "has an invalid type: %s\n",
                                    attr->var->name,
                                    attr->name,
                                    adios_type_to_string_int (attr->var->type));
                            return 0;

                        default: // the integral numeric types are all fine
                            break;
                    }
                    attr->var->is_dim = adios_flag_yes;
                }
                else
                {
                    switch (attr->type)
                    {
                        case adios_string:
                        case adios_real:
                        case adios_double:
                        case adios_long_double:
                        case adios_complex:
                        case adios_double_complex:
                            adios_error (err_invalid_var_as_dimension,
                                    "config.xml: dimension defining var %s "
                                    "pointed by attribute %s "
                                    "has an invalid type: %s\n",
                                    attr->var->name,
                                    attr->name,
                                    adios_type_to_string_int (attr->type));
                            return 0;

                        default: // the integral numeric types are all fine
                            break;
                    }
                }
                dim->dimension.attr = attr;
            }
        }
        else
        {
            switch (var->type)
            {
                case adios_string:
                case adios_real:
                case adios_double:
                case adios_long_double:
                case adios_complex:
                case adios_double_complex:
                    adios_error (err_invalid_var_as_dimension,
                            "config.xml: dimension defining var %s "
                            "has an invalid type: %s\n",
                            var->name,
                            adios_type_to_string_int (var->type));
                    return 0;

                default: // the integral numeric types are all fine
                    break;
            }

            dim->dimension.var = var;
            var->is_dim = adios_flag_yes;
        }
    }
    else
    {
        dim->dimension.rank = atoi (dimension);
    }

    if (!global_dimension)
    {
        adios_error (err_global_dim_required,
                "adios_parse_dimension: global_dimension not provided\n");

        return 0;
    }

    /* Get the global dimension */
    dim->global_dimension.rank = 0;
    dim->global_dimension.var  = NULL;
    dim->global_dimension.attr = NULL;
    if (adios_int_is_var (global_dimension))
    {
        struct adios_var_struct * var = 0;
        var = adios_find_var_by_name (g, global_dimension);
        if (!var)
        {
            struct adios_attribute_struct * attr = 0;
            attr = adios_find_attribute_by_name (g->attributes, global_dimension
                    ,g->all_unique_var_names
                    );

            if (!attr)
            {
                /* FIXME: Is time dimension allowed for global dim definition?
                 * What is this code doing here? */
                if (   g->time_index_name
                        && !strcasecmp (g->time_index_name, global_dimension)
                   )
                {
                    dim->global_dimension.time_index = adios_flag_yes;
                }
                else
                {
                    adios_error (err_invalid_global_dimension,
                            "config.xml: invalid global-bounds dimension: %s\n",
                            global_dimension);

                    return 0;
                }
            }
            else
            {
                if (attr->var)
                {
                    switch (attr->var->type)
                    {
                        case adios_string:
                        case adios_real:
                        case adios_double:
                        case adios_long_double:
                        case adios_complex:
                        case adios_double_complex:
                            adios_error (err_invalid_var_as_dimension,
                                    "config.xml: global dimension defining var %s "
                                    "pointed by attribute %s "
                                    "has an invalid type: %s\n",
                                    attr->var->name,
                                    attr->name,
                                    adios_type_to_string_int (attr->var->type));
                            return 0;

                        default: // the integral numeric types are all fine
                            break;
                    }
                    attr->var->is_dim = adios_flag_yes;
                }
                else
                {
                    switch (attr->type)
                    {
                        case adios_string:
                        case adios_real:
                        case adios_double:
                        case adios_long_double:
                        case adios_complex:
                        case adios_double_complex:
                            adios_error (err_invalid_var_as_dimension,
                                    "config.xml: global dimension defining var %s "
                                    "pointed by attribute %s "
                                    "has an invalid type: %s\n",
                                    attr->var->name,
                                    attr->name,
                                    adios_type_to_string_int (attr->var->type));
                            return 0;

                        default: // the integral numeric types are all fine
                            break;
                    }
                }
                dim->global_dimension.attr = attr;
            }
        }
        else
        {
            switch (var->type)
            {
                case adios_string:
                case adios_real:
                case adios_double:
                case adios_long_double:
                case adios_complex:
                case adios_double_complex:
                    adios_error (err_invalid_var_as_dimension,
                            "config.xml: global dimension defining var %s "
                            "has an invalid type: %s\n",
                            var->name,
                            adios_type_to_string_int (var->type));
                    return 0;

                default: // the integral numeric types are all fine
                    break;
            }
            var->is_dim = adios_flag_yes;
            dim->global_dimension.var = var;
        }
    }
    else
    {
        dim->global_dimension.rank = strtol (global_dimension, NULL, 10);
    }

    if (!local_offset)
    {
        adios_error (err_offset_required, "adios_parse_dimension: local-offset not provided\n");

        return 0;
    }

    /* Get the local offset */
    dim->local_offset.rank = 0;
    dim->local_offset.var  = NULL;
    dim->local_offset.attr = NULL;
    if (adios_int_is_var (local_offset))
    {
        struct adios_var_struct * var = 0;
        var = adios_find_var_by_name (g, local_offset);
        if (!var)
        {
            struct adios_attribute_struct * attr = 0;
            attr = adios_find_attribute_by_name (g->attributes, local_offset
                    ,g->all_unique_var_names
                    );

            if (!attr)
            {
                /* FIXME: Is time dimension allowed for offset definition?
                 * What is this code doing here? */

                if (   g->time_index_name
                        && !strcasecmp (g->time_index_name, local_offset)
                   )
                {
                    dim->local_offset.time_index = adios_flag_yes;
                }
                else
                {
                    adios_error (err_invalid_offset,
                            "config.xml: invalid var local_offset: %s\n",
                            local_offset);

                    return 0;
                }
            }
            else
            {
                if (attr->var)
                {
                    switch (attr->var->type)
                    {
                        case adios_string:
                        case adios_real:
                        case adios_double:
                        case adios_long_double:
                        case adios_complex:
                        case adios_double_complex:
                            adios_error (err_invalid_var_as_dimension,
                                    "config.xml: offset defining var %s "
                                    "pointed by attribute %s "
                                    "has an invalid type: %s\n",
                                    attr->var->name,
                                    attr->name,
                                    adios_type_to_string_int (attr->var->type));
                            return 0;

                        default: // the integral numeric types are all fine
                            break;
                    }
                    attr->var->is_dim = adios_flag_yes;
                }
                else
                {
                    switch (attr->type)
                    {
                        case adios_string:
                        case adios_real:
                        case adios_double:
                        case adios_long_double:
                        case adios_complex:
                        case adios_double_complex:
                            adios_error (err_invalid_var_as_dimension,
                                    "config.xml: offset defining var %s "
                                    "pointed by attribute %s "
                                    "has an invalid type: %s\n",
                                    attr->var->name,
                                    attr->name,
                                    adios_type_to_string_int (attr->var->type));
                            return 0;

                        default: // the integral numeric types are all fine
                            break;
                    }
                }
                dim->local_offset.attr = attr;
            }
        }
        else
        {
            switch (var->type)
            {
                case adios_string:
                case adios_real:
                case adios_double:
                case adios_long_double:
                case adios_complex:
                case adios_double_complex:
                    adios_error (err_invalid_var_as_dimension,
                            "config.xml: offset defining var %s "
                            "has an invalid type: %s\n",
                            var->name,
                            adios_type_to_string_int (var->type));
                    return 0;

                default: // the integral numeric types are all fine
                    break;
            }
            var->is_dim = adios_flag_yes;
            dim->local_offset.var = var;
        }
    }
    else
    {
        dim->local_offset.rank = strtol (local_offset, NULL, 10);
    }

    return 1;
}

struct adios_method_list_struct * adios_get_methods ()
{
    return adios_methods;
}

struct adios_group_list_struct * adios_get_groups ()
{
    return adios_groups;
}


int adios_parse_scalar_string (enum ADIOS_DATATYPES type, char * value, void ** out)
{
    char * end;

    switch (type)
    {
        case adios_byte:
        case adios_short:
        case adios_integer:
            {
                int errno_save = errno;
                long t = strtol (value, &end, 10);
                if (errno != errno_save || (end != 0 && *end != '\0'))
                {
                    adios_error (err_invalid_argument,
                            "value: '%s' not valid integer\n",value);
                    return 0;
                }
                else
                {
                    switch (type)
                    {
                        case adios_byte:
                            if (t < SCHAR_MIN || t > SCHAR_MAX)
                            {
                                adios_error (err_out_of_bound,
                                        "type is %s, value "
                                        "is out of range: '%s'\n",
                                        adios_type_to_string_int (type),
                                        value);
                                return 0;
                            }
                            else
                            {
                                *out = malloc (1);
                                *((int8_t *) *out) = t;

                                return 1;
                            }
                        case adios_short:
                            if (t < SHRT_MIN || t > SHRT_MAX)
                            {
                                adios_error (err_out_of_bound,
                                        "type is %s, value "
                                        "is out of range: '%s'\n",
                                        adios_type_to_string_int (type),
                                        value);
                                return 0;
                            }
                            else
                            {
                                *out = malloc (2);
                                *((int16_t *) *out) = t;

                                return 1;
                            }
                        case adios_integer:
                            if (t < INT_MIN || t > INT_MAX)
                            {
                                adios_error (err_out_of_bound,
                                        "type is %s, value "
                                        "is out of range: '%s'\n",
                                        adios_type_to_string_int (type),
                                        value);
                                return 0;
                            }
                            else
                            {
                                *out = malloc (4);
                                *((int32_t *) *out) = t;

                                return 1;
                            }
                        default:
                                return 1;
                    }
                }
            }
        case adios_long:
            {
                int errno_save = errno;
                int64_t t = strtoll (value, &end, 10);
                if (errno != errno_save || (end != 0 && *end != '\0'))
                {
                    adios_error (err_out_of_bound,
                            "type is %s, value "
                            "is out of range: '%s'\n",
                            adios_type_to_string_int (type),
                            value);
                    return 0;
                }
                else
                {
                    *out = malloc (8);
                    *((int64_t *) *out) = t;

                    return 1;
                }
            }
        case adios_unsigned_byte:
        case adios_unsigned_short:
        case adios_unsigned_integer:
            {
                int errno_save = errno;
                unsigned long t = strtoul (value, &end, 10);
                if (errno != errno_save || (end != 0 && *end != '\0'))
                {
                    adios_error (err_invalid_argument,
                            "value: '%s' not valid integer\n", value);
                    return 0;
                }
                else
                {
                    switch (type)
                    {
                        case adios_unsigned_byte:
                            if (t > UCHAR_MAX)
                            {
                                adios_error (err_out_of_bound,
                                        "type is %s, value "
                                        "is out of range: '%s'\n",
                                        adios_type_to_string_int (type),
                                        value);
                                return 0;
                            }
                            else
                            {
                                *out = malloc (1);
                                *((uint8_t *) *out) = t;

                                return 1;
                            }
                        case adios_unsigned_short:
                            if (t > USHRT_MAX)
                            {
                                adios_error (err_out_of_bound,
                                        "type is %s, value "
                                        "is out of range: '%s'\n",
                                        adios_type_to_string_int (type),
                                        value);
                                return 0;
                            }
                            else
                            {
                                *out = malloc (2);
                                *((uint16_t *) *out) = t;

                                return 1;
                            }
                        case adios_unsigned_integer:
                            if (t > UINT_MAX)
                            {
                                adios_error (err_out_of_bound,
                                        "type is %s, value "
                                        "is out of range: '%s'\n",
                                        adios_type_to_string_int (type),
                                        value);
                                return 0;
                            }
                            else
                            {
                                *out = malloc (4);
                                *((uint32_t *) *out) = t;

                                return 1;
                            }
                        default:
                                return 1;
                    }
                }
            }
        case adios_unsigned_long:
            {
                int errno_save = errno;
                uint64_t t = strtoull (value, &end, 10);
                if (errno != errno_save || (end != 0 && *end != '\0'))
                {
                    adios_error (err_out_of_bound,
                            "type is %s, value "
                            "is out of range: '%s'\n",
                            adios_type_to_string_int (type),
                            value);
                    return 0;
                }
                else
                {
                    *out = malloc (8);
                    *((uint64_t *) *out) = t;

                    return 1;
                }
            }
        case adios_real:
            {
                int errno_save = errno;
                float t = strtof (value, &end);
                if (errno != errno_save || (end != 0 && *end != '\0'))
                {
                    adios_error (err_out_of_bound,
                            "type is %s, value "
                            "is out of range: '%s'\n",
                            adios_type_to_string_int (type),
                            value);
                    return 0;
                }
                else
                {
                    *out = malloc (4);
                    *((float *) *out) = t;

                    return 1;
                }
            }
        case adios_double:
            {
                int errno_save = errno;
                double t = strtod (value, &end);
                if (errno != errno_save || (end != 0 && *end != '\0'))
                {
                    adios_error (err_out_of_bound,
                            "type is %s, value "
                            "is out of range: '%s'\n",
                            adios_type_to_string_int (type),
                            value);
                    return 0;
                }
                else
                {
                    *out = malloc (8);
                    *((double *) *out) = t;

                    return 1;
                }
            }
        case adios_long_double:
            {
                int errno_save = errno;
                long double t = strtold (value, &end);
                if (errno != errno_save || (end != 0 && *end != '\0'))
                {
                    adios_error (err_out_of_bound,
                            "type is %s, value "
                            "is out of range: '%s'\n",
                            adios_type_to_string_int (type),
                            value);
                    return 0;
                }
                else
                {
                    *out = malloc (16);
                    *((long double *) *out) = t;
                }
            }
        case adios_string:
            {
                *out = (void *) strdup (value);

                return 1;
            }
        case adios_complex:
            {
                adios_error (err_unspecified,
                        "adios_parse_scalar_string: adios_complex type "
                        "validation needs to be implemented\n");
                return 1;
            }
        case adios_double_complex:
            {
                adios_error (err_unspecified,
                        "adios_parse_scalar_string: adios_double_complex type "
                        "validation needs to be implemented\n");
                return 1;
            }

        case adios_unknown:
        default:
            adios_error (err_unspecified,
                    "adios_parse_scalar_string: unknown type cannot be validated\n");

            return 0;
    }

    return 1;
}

int adios_common_define_attribute (int64_t group, const char * name
        ,const char * path
        ,enum ADIOS_DATATYPES type
        ,const char * value
        ,const char * var
        )
{
    struct adios_group_struct * g = (struct adios_group_struct *) group;
    struct adios_attribute_struct * attr = (struct adios_attribute_struct *)
        malloc (sizeof (struct adios_attribute_struct));

    attr->name = strdup (name);
    if (path) 
        attr->path = strdup (path);
    else
        attr->path = strdup (""); // not null but empty path
    if (value)
    {
        if (type == adios_unknown)
        {
            adios_error (err_invalid_type_attr,
                    "config.xml: attribute element %s has invalid "
                    "type attribute\n",
                    name);

            free (attr->name);
            free (attr->path);
            free (attr);

            return 0;
        }
        attr->type = type;
        if (adios_parse_scalar_string (type, (void *) value, &attr->value))
        {
            attr->var = 0;
        }
        else
        {
            adios_error (err_invalid_value_attr,
                    "config.xml: attribute element %s has invalid "
                    "value attribute: '%s'\n",
                    name, value);

            free (attr->value);
            free (attr->name);
            free (attr->path);
            free (attr);

            return 0;
        }
    }
    else
    {
        attr->value = 0;
        attr->type = adios_unknown;
        attr->var = adios_find_var_by_name (g, var);

        if (attr->var == 0)
        {
            adios_error (err_invalid_varname,
                    "config.xml: attribute element %s references "
                    "var %s that has not been defined.\n",
                    name, var);

            free (attr->name);
            free (attr->path);
            free (attr);

            return 0;
        }
    }

    attr->next = 0;
    attr->write_offset = 0;

    adios_append_attribute (&g->attributes, attr, ++g->member_count);

    return 1;
}


/* define an attribute by passing the value directly, not by a string */
int adios_common_define_attribute_byvalue (int64_t group, const char * name
        ,const char * path
        ,enum ADIOS_DATATYPES type
        ,void * value
        )
{
    struct adios_group_struct * g = (struct adios_group_struct *) group;
    struct adios_attribute_struct * attr = (struct adios_attribute_struct *)
        malloc (sizeof (struct adios_attribute_struct));
    uint64_t size;

    attr->name = strdup (name);
    attr->path = strdup (path);
    if (value)
    {
        if (type == adios_unknown)
        {
            adios_error (err_invalid_type_attr,
                    "config.xml: attribute element %s has invalid "
                    "type attribute\n",
                    name);

            free (attr->name);
            free (attr->path);
            free (attr);

            return 0;
        }
        attr->type = type;
        size = adios_get_type_size (attr->type, value);
        if (size > 0)
        {
            attr->value = malloc (size);
            memcpy (attr->value, value, size);
            attr->var = 0;
        }
        else
        {
            adios_error (err_invalid_value_attr,
                    "Attribute element %s has invalid "
                    "value attribute\n", name);

            free (attr->value);
            free (attr->name);
            free (attr->path);
            free (attr);

            return 0;
        }
    }
    else
    {
        adios_error (err_invalid_value_attr,
                "Attribute element %s has invalid "
                "value attribute\n", name);

        free (attr->name);
        free (attr->path);
        free (attr);

        return 0;
    }

    attr->next = 0;
    attr->write_offset = 0;

    adios_append_attribute (&g->attributes, attr, ++g->member_count);

    return 1;
}

/*void adios_extract_string (char ** out, const char * in, int size)
  {
  if (in && out)
  {
 *out = malloc (strlen (in) + 1);
 strcpy (*out, in);
 }
// for some Fortran implementations, we get a size for a string.
// for others (like PGI), we don't and it isn't null terminated
// unless we do it explicitly.  Assume that it is null terminated
// for now.
//
#if 0
int i = 0;
memcpy (out, in, size);
while (i < size)
{
if (out [i] == ' ')
{
out [i] = 0;
return;
}
else
i++;
}
out [i] = 0;
#endif
}*/

void adios_append_method (struct adios_method_struct * method)
{
    struct adios_method_list_struct ** root = &adios_methods;

    while (root)
    {
        if (!*root)
        {
            struct adios_method_list_struct * new_node =
                (struct adios_method_list_struct *)
                malloc (sizeof (struct adios_method_list_struct));

            if (!new_node)
            {
                adios_error (err_no_memory, "out of memory in adios_append_method\n");
            }
            new_node->method = method;
            new_node->next = 0;

            *root = new_node;
            root = 0;
        }
        else
        {
            root = &(*root)->next;
        }
    }
}

void adios_add_method_to_group (struct adios_method_list_struct ** root
        ,struct adios_method_struct * method
        )
{
    while (root)
    {
        if (!*root)
        {
            struct adios_method_list_struct * new_node =
                (struct adios_method_list_struct *)
                malloc (sizeof (struct adios_method_list_struct));

            if (!new_node)
            {
                adios_error (err_no_memory, "out of memory in adios_add_method_to_group\n");
            }
            new_node->method = method;
            new_node->next = 0;

            *root = new_node;
            root = 0;
        }
        else
        {
            root = &(*root)->next;
        }
    }
}

void adios_append_group (struct adios_group_struct * group)
{
    struct adios_group_list_struct ** root = &adios_groups;
    int id = 1;

    while (root)
    {
        if (!*root)
        {
            struct adios_group_list_struct * new_node =
                (struct adios_group_list_struct *)
                malloc (sizeof (struct adios_group_list_struct));

            if (!new_node)
            {
                adios_error (err_no_memory, "out of memory in adios_append_group\n");
            }
            group->id = id;
            new_node->group = group;
            new_node->next = 0;

            *root = new_node;
            root = 0;
        }
        else
        {
            root = &(*root)->next;
            id++;
        }
    }
}

static void adios_append_var (struct adios_group_struct * g, struct adios_var_struct * var)
{
    assert(g);

    /* Note: many routines parse the list of variables through the ->next pointer with
     * "while (v) { ...; v=v->next }
     * So we need have NULL as next in the last variable
     */
    if (!g->vars) {
        // first variable: g->vars     : V => (null)
        //                 g->vars_tail: V => (null)
        var->next = NULL;
        g->vars = var;      // V => (null)
        g->vars_tail = var; // V => (null)
    } else {
       var->next = NULL;
       // append var to tail
       g->vars_tail->next = var;  // g->vars => ... => tail => V => (null)
       // new tail is var
       g->vars_tail = var;
    }

    // Add variable to the hash table too
    g->hashtbl_vars->put2(g->hashtbl_vars, var->path, var->name, var);
}

// return is whether or not the name is unique
/*
enum ADIOS_FLAG adios_append_var (struct adios_var_struct ** root
        ,struct adios_var_struct * var
        ,uint16_t id
        )
{
    enum ADIOS_FLAG unique_names = adios_flag_yes;

    while (root)
    {
        if (   unique_names == adios_flag_yes
                && *root
                && !strcasecmp ((*root)->name, var->name)
           )
        {
            unique_names = adios_flag_no;
        }
        if (!*root)
        {
            var->id = id;
            *root = var;
            root = 0;
        }
        else
        {
            root = &(*root)->next;
        }
    }

    return unique_names;
}
*/

void adios_append_dimension (struct adios_dimension_struct ** root
        ,struct adios_dimension_struct * dimension
        )
{
    while (root)
    {
        if (!*root)
        {
            *root = dimension;
            root = 0;
        }
        else
        {
            root = &(*root)->next;
        }
    }
}

void adios_append_attribute (struct adios_attribute_struct ** root
        ,struct adios_attribute_struct * attribute
        ,uint32_t id
        )
{
    while (root)
    {
        if (!*root)
        {
            attribute->id = id;
            *root = attribute;
            root = 0;
        }
        else
        {
            root = &(*root)->next;
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
// functions to support C & Fortran interface
///////////////////////////////////////////////////////////////////////////////
int adios_common_declare_group (int64_t * id, const char * name
        ,enum ADIOS_FLAG host_language_fortran
        ,const char * coordination_comm
        ,const char * coordination_var
        ,const char * time_index_name
        ,enum ADIOS_FLAG stats
        )
{
    struct adios_group_struct * g = (struct adios_group_struct *)
        malloc (sizeof (struct adios_group_struct));

    g->name = strdup (name);
    g->adios_host_language_fortran = host_language_fortran;
    g->all_unique_var_names = adios_flag_no;
    // ADIOS Schema: adding similar var for meshes
    g->all_unique_mesh_names = adios_flag_yes;
    g->id = 0; // will be set in adios_append_group
    g->member_count = 0; // will be set in adios_append_group
    g->vars = NULL;
    g->vars_tail = NULL;
    g->hashtbl_vars = qhashtbl(500);
    g->vars_written = NULL;
    g->vars_written_tail = NULL;
    g->attributes = NULL;
    g->group_by = (coordination_var ? strdup (coordination_var) : 0L);
    g->group_comm = (coordination_comm ? strdup (coordination_comm) : 0L);
    g->time_index_name = (time_index_name ? strdup (time_index_name) : 0L);
    g->time_index = 0;
    g->stats_on = stats;
    g->process_id = 0;
    g->methods = NULL;
    // ADIOS Schema
    g->meshs = NULL;
    g->mesh_count = 0;

#if defined ADIOS_TIMERS || defined ADIOS_TIMER_EVENTS
    g->timing_obj = 0;
    g->prev_timing_obj = 0;
#endif

    *id = (int64_t) g;

    adios_append_group (g);

    return 1;
}

// Delete all attribute (definitions) from a group
int adios_common_delete_attrdefs (struct adios_group_struct * g)
{
    while (g->attributes)
    {
        struct adios_attribute_struct * attr = g->attributes;
        g->attributes = g->attributes->next;
        free (attr->value);
        free (attr->name);
        free (attr->path);
        free (attr);
    }
}

// Delete all variable (definitions) from a group
int adios_common_delete_vardefs (struct adios_group_struct * g)
{
    // remove variables from the hashtable at once
    g->hashtbl_vars->clear(g->hashtbl_vars);

    while (g->vars)
    {
        struct adios_var_struct * var = g->vars;
        g->vars = g->vars->next;

        if (var->name)
            free (var->name);
        if (var->path)
            free (var->path);

        while (var->dimensions)
        {
            struct adios_dimension_struct * dimensions
                = var->dimensions->next;

            free (var->dimensions);
            var->dimensions = dimensions;
        }

        // NCSU - Clear Stat
        if (var->stats)
        {
            uint8_t j = 0, idx = 0;
            enum ADIOS_DATATYPES original_var_type = adios_transform_get_var_original_type_var(var);
            uint8_t c = 0, count = adios_get_stat_set_count(original_var_type);

            for (c = 0; c < count; c ++)
            {
                while (var->bitmap >> j)
                {
                    if ((var->bitmap >> j) & 1)
                    {
                        if (j == adios_statistic_hist)
                        {
                            struct adios_hist_struct * hist = (struct adios_hist_struct *) (var->stats[c][idx].data);
                            free (hist->breaks);
                            free (hist->frequencies);
                            free (hist);
                        }
                        else
                            free (var->stats[c][idx].data);

                        idx ++;
                    }
                    j ++;
                }
                free (var->stats[c]);
            }
            free (var->stats);
        }

        // NCSU ALACRITY-ADIOS - Clean transform metadata
        adios_transform_clear_transform_var(var);

        if (var->data)
            free (var->data);

        free (var);
    }

    return 0;
}

int adios_common_free_group (int64_t id)
{
    struct adios_group_list_struct * root = adios_groups;
    struct adios_group_list_struct * old_root = adios_groups;
    struct adios_group_struct * g = (struct adios_group_struct *) id;

    if (!root)
    {
        adios_error (err_unspecified, "Err in adios_common_free_group(): no groups left\n");
        return -1;
    }
    while (root && root->group->id != g->id)
    {
        old_root = root;
        root = root->next;
    };

    if (!root)
    {
        // Didn't find the group
        adios_error (err_unspecified, "Err in adios_common_free_group(): did not find requested group\n");
        return -1;
    }

    // old_root->root->root next
    if (root == adios_groups)
    {
        adios_groups =  root->next;
    }
    else
    {
        old_root->next = root->next;
    }

    if (g->name)             free (g->name);
    if (g->group_by)         free (g->group_by);
    if (g->group_comm)       free (g->group_comm);
    if (g->time_index_name)  free (g->time_index_name);

    adios_common_delete_vardefs (g);
    adios_common_delete_attrdefs (g);
    g->hashtbl_vars->free(g->hashtbl_vars);

    free (root);
    free (g);

    return 0;
}

void trim_spaces (char * str)
{
    char * t = str, * p = NULL;
    while (*t != '\0')
    {
        if (*t == ' ')
        {
            p = t + 1;
            strcpy (t, p);
        }
        else
            t++;
    }

}

static void tokenize_dimensions (const char * str, char *** tokens, int * count)
{
    if (!str)
    {
        *tokens = 0;
        *count = 0;

        return;
    }

    char * save_str = strdup (str);
    char * t = save_str;
    int i;

    trim_spaces (save_str);

    if (strlen (save_str) > 0)
        *count = 1;
    else
    {
        *tokens = 0;
        *count = 0;
        free (save_str);

        return;
    }

    while (*t)
    {
        if (*t == ',')
            (*count)++;
        t++;
    }

    *tokens = (char **) malloc (sizeof (char **) * *count);
    (*tokens) [0] = strdup (strtok (save_str, ","));
    for (i = 1; i < *count; i++)
    {
        (*tokens) [i] = strdup (strtok (NULL, ","));
    }

    free (save_str);
}

static void cleanup_dimensions (char *** tokens, int * count)
{
    int i;
    for (i = 0; i < *count; i++)
    {
        free ((*tokens) [i]);
    }
    free (*tokens);
    *tokens = 0;
    *count = 0;
}

int adios_common_define_var_characteristics (struct adios_group_struct * g
        , const char * var_name
        , const char * bin_intervals
        , const char * bin_min
        , const char * bin_max
        , const char * bin_count
        )
{
    struct adios_var_struct * var;

    var = adios_find_var_by_name (g, var_name);

    struct adios_hist_struct * hist;

    if (var->type == adios_complex || var->type == adios_double_complex)
        return 0;

    int i = 0, j = 0;
    while ((var->bitmap >> j) && (j < adios_statistic_hist))
    {
        if ((var->bitmap >> j) & 1)
            i ++;
        j ++;
    }

    hist = var->stats[0][i].data = (struct adios_hist_struct *) malloc (sizeof(struct adios_hist_struct));

    if (!var)
    {
        adios_error (err_invalid_varname,
                "config.xml: Didn't find the variable %s for analysis\n",
                var_name);
        return 0;
    }
    else
    {
        int i;
        if (bin_intervals)
        {
            int count;
            char ** bin_tokens = 0;

            tokenize_dimensions (bin_intervals, &bin_tokens, &count);

            if (!count)
            {
                adios_error (err_histogram_error,
                        "config.xml: unable to tokenize break points\n");
                return 0;
            }

            hist->breaks = calloc(count, sizeof(double));

            if(!hist || !hist->breaks)
            {
                adios_error (err_histogram_error,
                        "config.xml: unable to allocate memory for histogram break points in "
                        "adios_common_define_var_characteristics\n");
                return 0;
            }

            for(i = 0; i < count; i++)
            {
                hist->breaks[i] = atof(bin_tokens[i]);
                if(i > 0 && (hist->breaks[i] <= hist->breaks[i-1]))
                {
                    adios_error (err_histogram_error,
                            "config.xml: break points should be in increasing order in "
                            "adios_common_define_var_characteristics\n");
                    return 0;
                }
            }

            hist->num_breaks = count;
            hist->min = hist->breaks[0];

            if(count > 0)
                hist->max = hist->breaks[count - 1];
            else
                hist->max = hist->min;

            var->bitmap = var->bitmap | (1 << adios_statistic_hist);
        }
        else
        {
            if(!bin_max || !bin_min || !bin_count)
            {
                adios_error (err_histogram_error,
                        "config.xml: unable to generate break points\n");
                return 0;
            }

            int count = atoi(bin_count);

            if (!count)
            {
                adios_error (err_histogram_error,
                        "config.xml: bin count is undefined\n");
                return 0;
            }

            hist->num_breaks = count + 1;
            hist->min = atof(bin_min);
            hist->max = atof(bin_max);
            hist->breaks = calloc(hist->num_breaks, sizeof(double));

            if(!hist || !hist->breaks)
            {
                adios_error (err_no_memory,
                        "config.xml: unable to allocate memory for histogram break points in "
                        "adios_common_define_var_characteristics\n");
                return 0;
            }

            if (hist->min >= hist->max)
            {
                adios_error (err_histogram_error,
                        "config.xml: minimum boundary value greater than maximum\n");
                return 0;
            }

            for(i = 0; i < hist->num_breaks; i ++)
                hist->breaks[i] = hist->min + i * (hist->max - hist->min) / count;

            var->bitmap = var->bitmap | (1 << adios_statistic_hist);
        }
    }


    return 1;
}

/* copy path but remove trailing / characters, and also
   NULL path becomes "", so that we don't need to check for NULL everywhere
*/
static char * dup_path (const char *path)
{
    char * p = NULL;
    int len;
    if (!path)
        return strdup("");
    len = strlen (path);
    /* remove trailing / characters */
    while (len > 1 && path[len-1] == '/') {
        /* ends with '/' and it is not a single '/' */
        len--;
    }
    p = malloc (len+1);
    if (!p)
        return NULL;
    strncpy (p, path, len);
    p[len] = '\0';
    return p;
}

int64_t adios_common_define_var (int64_t group_id, const char * name
        ,const char * path, enum ADIOS_DATATYPES type
        ,const char * dimensions
        ,const char * global_dimensions
        ,const char * local_offsets
        )
{
    struct adios_group_struct * t = (struct adios_group_struct *) group_id;
    struct adios_var_struct * v = (struct adios_var_struct *)
        malloc (sizeof (struct adios_var_struct));
    char * dim_temp;
    char * g_dim_temp;
    char * lo_dim_temp;
    uint8_t i;
    if (dimensions)
        dim_temp = strdup (dimensions);
    else
        dim_temp = 0;
    if (global_dimensions)
        g_dim_temp = strdup (global_dimensions);
    else
        g_dim_temp = 0;
    if (local_offsets)
        lo_dim_temp = strdup (local_offsets);
    else
        lo_dim_temp = 0;

    v->name = strdup (name);
    v->path = dup_path (path);  // copy but remove trailing / characters, and NULL path becomes ""
    //log_error ("define_var: name=%s, path=[%s], dup=[%s]\n", name, path, v->path);
    v->type = type;
    v->dimensions = 0;
    v->is_dim = adios_flag_no;
    v->got_buffer = adios_flag_no;
    v->free_data = adios_flag_no;
    v->parent_var = NULL;

    v->data = 0;
    v->write_offset = 0;

    v->data_size = 0;
    v->write_count = 0;

    v->next = 0;

    // NCSU - Initializing stat related info
    v->stats = 0;
    v->bitmap = 0;

    // NCSU ALACRITY-ADIOS - Initialize transform metadata (set to 'none')
    adios_transform_init_transform_var(v);

    // Q.L. - Check whether stats are disabled or not
    if (t->stats_on == adios_flag_yes)
    {
        // '1' at the bit location of stat id in adios_bp_v1.h, enables calculation of statistic.
        for (i = 0; i < ADIOS_STAT_LENGTH; i++)
            v->bitmap |= (1 << i);

        // Default values for histogram not yet implemented. Disabling it.
        v->bitmap ^= (1 << adios_statistic_hist);

        // For complex numbers, the set of statistics occur thrice: stat[0] - magnitude, stat[1] - real, stat[2] - imaginary
        if (v->type == adios_complex || v->type == adios_double_complex)
        {
            uint8_t c;
            v->stats = malloc (3 * sizeof(struct adios_stat_struct *));

            for (c = 0; c < 3; c ++)
                v->stats[c] = calloc (ADIOS_STAT_LENGTH, sizeof(struct adios_stat_struct));
        }
        else
        {
            v->stats = malloc (sizeof(struct adios_stat_struct *));
            v->stats[0] = calloc (ADIOS_STAT_LENGTH, sizeof(struct adios_stat_struct));
        }
    }

    // NCSU - End of initializing stat related info

    if (dim_temp && strcmp (dim_temp, ""))
    {
        int dim_count;
        char ** dim_tokens = 0;

        int g_dim_count;
        char ** g_dim_tokens = 0;

        int lo_dim_count;
        char ** lo_dim_tokens = 0;

        int i = 0;

        tokenize_dimensions (dim_temp, &dim_tokens, &dim_count);
        tokenize_dimensions (g_dim_temp, &g_dim_tokens, &g_dim_count);
        tokenize_dimensions (lo_dim_temp, &lo_dim_tokens, &lo_dim_count);

        while (i < dim_count)
        {
            int ret;
            struct adios_dimension_struct * d =
                (struct adios_dimension_struct *)
                calloc (1, sizeof (struct adios_dimension_struct));

            if (!d)
            {
                adios_error (err_no_memory,
                        "config.xml: out of memory in adios_common_define_var\n");

                return 0;
            }
            char * dim = 0;
            char * g_dim = "0";
            char * lo_dim = "0";

            if (i < dim_count)
                dim = dim_tokens [i];
            if (i < g_dim_count)
                g_dim = g_dim_tokens [i];
            if (i < lo_dim_count)
                lo_dim = lo_dim_tokens [i];

            if (!(ret = adios_parse_dimension (dim, g_dim, lo_dim, t, d)))
            {
                free (dim_temp);
                free (g_dim_temp);
                free (lo_dim_temp);
                free (v->name);
                free (v->path);
                free (v);
                cleanup_dimensions (&dim_tokens, &dim_count);
                cleanup_dimensions (&g_dim_tokens, &g_dim_count);
                cleanup_dimensions (&lo_dim_tokens, &lo_dim_count);

                return 0;
            }

            adios_append_dimension (&v->dimensions, d);

            i++;
        }
        cleanup_dimensions (&dim_tokens, &dim_count);
        cleanup_dimensions (&g_dim_tokens, &g_dim_count);
        cleanup_dimensions (&lo_dim_tokens, &lo_dim_count);
    }

    if (dim_temp)
        free (dim_temp);
    if (g_dim_temp)
        free (g_dim_temp);
    if (lo_dim_temp)
        free (lo_dim_temp);

    v->id = ++t->member_count;
    adios_append_var (t, v);

    return (int64_t)v;
}

/* Set the transformation method for a variable. Only one transformation will work for each variable */
int adios_common_set_transform (int64_t var_id, const char *transform_type_str)
{
    struct adios_var_struct * v = (struct adios_var_struct *)var_id;
    assert (v);
    // NCSU ALACRITY-ADIOS - parse transform type string, and call the transform layer to
    //   set up the variable as needed
    adios_transform_parse_spec(transform_type_str, v->transform_spec);
    if (v->transform_spec->transform_type == adios_transform_unknown) {
        adios_error(err_invalid_transform_type, 
                  "Unknown transform type \"%s\" specified for variable \"%s\", ignoring it...\n",
                  v->transform_spec->transform_type_str ? v->transform_spec->transform_type_str : "<null>", v->name);
        v->transform_spec->transform_type = adios_transform_none;
    }

    // This function sets the transform_type field. It does nothing if transform_type is none.
    // Note: ownership of the transform_spec struct is given to this function
    v = adios_transform_define_var(v);
    return adios_errno;
}


void adios_common_get_group (int64_t * group_id, const char * name)
{
    struct adios_group_list_struct * g = adios_get_groups ();

    *group_id = 0;

    while (g)
    {
        if (!strcasecmp (g->group->name, name))
        {
            *group_id = (int64_t) g->group;

            return;
        }

        g = g->next;
    }

    adios_error (err_invalid_group,
            "adios-group '%s' not found in configuration file\n",
            name);
}

// *****************************************************************************
static void buffer_write (char ** buffer, uint64_t * buffer_size
        ,uint64_t * buffer_offset
        ,const void * data, uint64_t size
        )
{
    if (*buffer_offset + size > *buffer_size || *buffer == 0)
    {
        char * b = realloc (*buffer, *buffer_offset + size + 1000000);
        if (b)
        {
            *buffer = b;
            *buffer_size = (*buffer_offset + size + 1000000);
        }
        else
        {
            adios_error (err_no_memory, "Cannot allocate memory in buffer_write.  "
                    "Requested: %llu\n", *buffer_offset + size + 1000000);
            return;
        }
    }

    memcpy (*buffer + *buffer_offset, data, size);
    *buffer_offset += size;
}

// NCSU ALACRITY-ADIOS - Genericized this to take a dimension struct, rather
//                       than the entire variable, so it can be used on the
//                       pre-transform dimension struct as well.
uint16_t adios_calc_var_characteristics_dims_overhead
                                                  (struct adios_dimension_struct * d)
{
    uint16_t overhead = 0;
    //struct adios_dimension_struct * d = v->dimensions;

    overhead += 1; // count
    overhead += 2; // length

    while (d)
    {
        overhead += 8 + 8 + 8; // the dims

        d = d->next;
    }

    return overhead;
}

// NCSU -This function precomputes the amount of overhead consumed by statistics
uint16_t adios_calc_var_characteristics_stat_overhead (struct adios_var_struct * var)
{
    uint16_t i, j, overhead;

    enum ADIOS_DATATYPES original_var_type = adios_transform_get_var_original_type_var (var);
    overhead = j = i = 0;

    while (var->bitmap >> j)
    {
        // NCSU - This characteristic is present. It adds to the overhead
        if ((var->bitmap >> j) & 1)
            overhead += adios_get_stat_size(var->stats[0][i ++].data, original_var_type, j);
        j ++;
    }

    return overhead;
}

static uint16_t adios_calc_var_characteristics_overhead(struct adios_var_struct * v)
{
    uint16_t overhead = 0;

    overhead += 1 + 4; // count + length

    enum ADIOS_DATATYPES original_var_type = adios_transform_get_var_original_type_var (v);
    // struct adios_dimension_struct *original_dimensions = adios_transform_get_characteristic_original_dims_from_var (v);

    switch (original_var_type)
    {
        case adios_string:   // nothing for strings
            //overhead += 1; // id
            //overhead += 2; // size
            break;

        default:   // the 12 numeric types
            if (v->dimensions)
            {
                overhead += 1; // id for bitmap
                overhead += 4; // value for bitmap

                overhead += 1;  // id for statistics
                // For complex numbers - min, max, avg repeated thrice
                overhead += adios_get_stat_set_count(original_var_type) * adios_calc_var_characteristics_stat_overhead (v);

                // NCSU ALACRITY-ADIOS - Adding transform type field overhead calc
                overhead += adios_transform_calc_transform_characteristic_overhead(v);

                overhead += 1;  // id
                overhead += adios_calc_var_characteristics_dims_overhead (v->dimensions);
            }
            break;
    }

    return overhead;
}

uint16_t adios_calc_var_overhead_v1 (struct adios_var_struct * v)
{
    uint16_t overhead = 0;

    struct adios_dimension_struct * d = v->dimensions;

    overhead += 8; // length of var entry
    overhead += 4; // member id
    overhead += 2; // length of name
    overhead += strlen (v->name); // name
    overhead += 2; // length of path
    overhead += strlen (v->path); // path
    overhead += 1; // datatype
    overhead += 1; // used as a dimension flag

    overhead += 1; // ranks
    overhead += 2; // dimensions length
    while (d)
    {
        overhead += 1; // var flag
        if (    d->dimension.var == NULL
             && d->dimension.attr == NULL
             && d->dimension.time_index == adios_flag_no
           )
        {
            overhead += 8; // value
        }
        else
        {
            overhead += 4; // member id
        }

        overhead += 1; // var flag
        if (    d->global_dimension.var == NULL
             && d->global_dimension.attr == NULL
             && d->global_dimension.time_index == adios_flag_no
           )
        {
            overhead += 8; // value
        }
        else
        {
            overhead += 4; // member id
        }

        overhead += 1; // var flag
        if (    d->local_offset.var == NULL
             && d->local_offset.attr == NULL
             && d->local_offset.time_index == adios_flag_no
           )
        {
            overhead += 8; // value
        }
        else
        {
            overhead += 4; // member id
        }

        d = d->next;
    }
    overhead += adios_calc_var_characteristics_overhead (v);

    return overhead;
}

uint32_t adios_calc_attribute_overhead_v1 (struct adios_attribute_struct * a)
{
    uint32_t overhead = 0;

    overhead += 4; // attribute length
    overhead += 4; // member id
    overhead += 2; // length of name
    overhead += strlen (a->name); // name
    overhead += 2; // length of path
    overhead += strlen (a->path); // path
    overhead += 1; // var flag
    if (a->var)
        overhead += 4; // var member id
    else
    {
        overhead += 1; // datatype
        overhead += 4; // length of value
        overhead += adios_get_type_size (a->type, a->value); // value
    }

    return overhead;
}

uint64_t adios_calc_overhead_v1 (struct adios_file_struct * fd)
{
    uint64_t overhead = 0;
    struct adios_var_struct * v = fd->group->vars;
    struct adios_attribute_struct * a = fd->group->attributes;
    struct adios_method_list_struct * m = fd->group->methods;

    overhead += 8; // process group length
    overhead += 1; // host language flag
    overhead += 2; // length of group name
    overhead += strlen (fd->group->name); // group name
    overhead += 4; // coordination var id
    overhead += 2; // length of time index name
    overhead += ((fd->group->time_index_name)
            ? strlen (fd->group->time_index_name)
            : 0
            );  // time index name
    overhead += 4; // time index

    overhead += 1; // count of methods employed
    overhead += 2; // length of methods section

    while (m)
    {
        overhead += 1; // method ID
        overhead += 2; // method params length
        overhead += strlen (m->method->parameters);
        m = m->next;
    }

    overhead += 4; // count of vars
    overhead += 8; // length of vars section

    while (v)
    {
        overhead += adios_calc_var_overhead_v1 (v);

        v = v->next;
    }

    overhead += 4; // attributes count
    overhead += 8; // attributes length

    while (a)
    {
        overhead += adios_calc_attribute_overhead_v1 (a);

        a = a->next;
    }

    return overhead;
}

int adios_write_process_group_header_v1 (struct adios_file_struct * fd
        ,uint64_t total_size
        )
{
    struct adios_group_struct * g = fd->group;

    uint8_t flag;
    struct adios_var_struct * var;
    uint16_t len;

    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &total_size, 8);

    flag = (g->adios_host_language_fortran == adios_flag_yes ? 'y' : 'n');
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &flag, 1);

    len = strlen (g->name);
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &len, 2);

    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, g->name, len);

    var = adios_find_var_by_name (g, g->group_by);
    if (var)
    {
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &var->id, 4);
    }
    else
    {
        uint32_t i = 0;
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &i, 4);
    }

    len = ((g->time_index_name) ? strlen (g->time_index_name) : 0);
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &len, 2);

    if (g->time_index_name)
    {
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                ,g->time_index_name, len
                );
    }
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
            ,&g->time_index, 4
            );

    struct adios_method_list_struct * m = fd->group->methods;
    uint8_t methods_count = 0;
    uint16_t methods_length = 0;
    while (m)
    {
        methods_count++;
        methods_length += 1 + 2 + strlen (m->method->parameters);

        m = m->next;
    }
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
            ,&methods_count, 1
            );
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
            ,&methods_length, 2
            );

    m = fd->group->methods;
    while (m)
    {
        uint16_t len = strlen (m->method->parameters);

        flag = (uint8_t) m->method->m;
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &flag, 1);

        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &len, 2);

        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                ,m->method->parameters, len
                );

        m = m->next;
    }

    if (fd->bytes_written < fd->offset)
        fd->bytes_written = fd->offset;

    return 0;
}

static void index_append_process_group_v1 (
        struct adios_index_process_group_struct_v1 ** root
        ,struct adios_index_process_group_struct_v1 * item
        )
{
    while (root)
    {
        if (!*root)
        {
            *root = item;
            root = 0;
        }
        else
        {
            root = &(*root)->next;
        }
    }
}

static void index_append_var_v1 (
        struct adios_index_struct_v1 *index
        ,struct adios_index_var_struct_v1 * item
        )
{
    struct adios_index_var_struct_v1 * olditem;

    olditem = (struct adios_index_var_struct_v1 *)
            index->hashtbl_vars->get2 (index->hashtbl_vars,
                                       item->var_path, item->var_name);

    log_debug ("Hashtable size=%d\n", index->hashtbl_vars->size (index->hashtbl_vars));
    log_debug ("var tail = %p, name=%s\n", index->vars_tail,
                (index->vars_tail ? index->vars_tail->var_name : ""));
    if (!olditem) {
        // new variable, insert into var list
        if (!index->vars_root) {
            log_debug ("   Very first variable\n");
            // first variable: g->vars_written     : V => (null)
            //                 g->vars_written_tail: V => (null)
            item->next = NULL;
            index->vars_root = item;      // V => (null)
            index->vars_tail = item;      // V => (null)
        } else {
            log_debug ("   Append as new variable\n");
            item->next = NULL;
            // append var to tail
            index->vars_tail->next = item;  // index->vars_root => ... => tail => V => (null)
            // new tail is var
            index->vars_tail = item;
        }
        // Add variable to the hash table too
        index->hashtbl_vars->put2(index->hashtbl_vars,
                item->var_path, item->var_name, item);

    } else {
        // existing variable, add this item to its characteristics
        log_debug ("   Append to existing variable\n");

        /* NOTE: old append made sure the variable mathes
         *  name + path + group name + type
         *  Here we just match name + path. We do not support same path
         *  in two groups, not to mention with two types
         */
        if (strcmp (olditem->group_name, item->group_name))
        {

            adios_error (err_unspecified, "Error when merging variable index lists. "
                    "Variable in two different groups have the same path+name. "
                    "Groups: %s and %s, variable: path=%s, name=%s. "
                    "Index aborted\n",
                    olditem->group_name, item->group_name,
                    item->var_path, item->var_name);
            return;
        }

        if (  olditem->characteristics_count
              + item->characteristics_count
              > olditem->characteristics_allocated
           )
        {
            int new_items = (item->characteristics_count == 1)
                ? 100 : item->characteristics_count;
            olditem->characteristics_allocated =
                olditem->characteristics_count + new_items;
            void * ptr = realloc (
                    olditem->characteristics,
                    olditem->characteristics_allocated *
                        sizeof (struct adios_index_characteristic_struct_v1)
                    );

            if (ptr)
            {
                olditem->characteristics = ptr;
            }
            else
            {
                adios_error (err_no_memory, "error allocating memory to build "
                        "var index.  Index aborted\n");
                return;
            }
        }
        memcpy (&olditem->characteristics [olditem->characteristics_count],
                item->characteristics,
                item->characteristics_count *
                    sizeof (struct adios_index_characteristic_struct_v1)
               );

        olditem->characteristics_count += item->characteristics_count;

        free (item->characteristics);
        free (item->group_name);
        free (item->var_name);
        free (item->var_path);
        free (item);
    }
}

static void index_append_attribute_v1
(struct adios_index_attribute_struct_v1 ** root
 ,struct adios_index_attribute_struct_v1 * item
 )
{
    while (root)
    {
        if (!*root)
        {
            *root = item;
            root = 0;
        }
        else
        {
            if (   !strcasecmp (item->group_name, (*root)->group_name)
                    && !strcasecmp (item->attr_name, (*root)->attr_name)
                    && !strcasecmp (item->attr_path, (*root)->attr_path)
               )
            {
                if (    (*root)->characteristics_count
                        + item->characteristics_count
                        > (*root)->characteristics_allocated
                   )
                {
                    int new_items = (item->characteristics_count == 1)
                        ? 100 : item->characteristics_count;
                    (*root)->characteristics_allocated =
                        (*root)->characteristics_count + new_items;
                    void * ptr;
                    ptr = realloc ((*root)->characteristics
                            ,  (*root)->characteristics_allocated
                            * sizeof (struct adios_index_characteristic_struct_v1)
                            );

                    if (ptr)
                    {
                        (*root)->characteristics = ptr;
                    }
                    else
                    {
                        adios_error (err_no_memory, "error allocating memory to build "
                                "attribute index.  Index aborted\n");
                        return;
                    }
                }
                memcpy (&(*root)->characteristics
                        [(*root)->characteristics_count]
                        ,item->characteristics
                        ,  item->characteristics_count
                        * sizeof (struct adios_index_characteristic_struct_v1)
                       );

                (*root)->characteristics_count += item->characteristics_count;

                free (item->characteristics);
                free (item->group_name);
                free (item->attr_name);
                free (item->attr_path);
                free (item);

                root = 0;  // exit the loop
            }
            else
            {
                root = &(*root)->next;
            }
        }
    }
}

// lists in new_index will be destroyed as part of the merge operation...
void adios_merge_index_v1 (
                   struct adios_index_struct_v1 * main_index
                  ,struct adios_index_process_group_struct_v1 * new_pg_root
                  ,struct adios_index_var_struct_v1 * new_vars_root
                  ,struct adios_index_attribute_struct_v1 * new_attrs_root
                  )
{
    // this will just add it on to the end and all should work fine
    index_append_process_group_v1 (&main_index->pg_root, new_pg_root);

    // need to do vars attrs one at a time to merge them properly
    struct adios_index_var_struct_v1 * v = new_vars_root;
    struct adios_index_var_struct_v1 * v_temp;
    struct adios_index_attribute_struct_v1 * a = new_attrs_root;
    struct adios_index_attribute_struct_v1 * a_temp;

    while (v)
    {
        v_temp = v->next;
        v->next = 0;
        log_debug ("merge index var %s/%s\n", v->var_path, v->var_name);
        index_append_var_v1 (main_index, v);
        v = v_temp;
    }

    while (a)
    {
        a_temp = a->next;
        a->next = 0;
        index_append_attribute_v1 (&main_index->attrs_root, a);
        a = a_temp;
    }
}

// sort pg/var indexes by time index
void adios_sort_index_v1 (struct adios_index_process_group_struct_v1 ** p1
        ,struct adios_index_var_struct_v1 ** v1
        ,struct adios_index_attribute_struct_v1 ** a1
        )
{
    struct adios_index_process_group_struct_v1 * p2 = 0, * p1_temp, * p2_temp, * p2_temp_prev;
    struct adios_index_var_struct_v1 * v1_temp;
    int i, j;

    while (*p1)
    {
        // if new index list is empty
        if (!p2)
        {
            p2 = *p1;
            *p1 = (*p1)->next;
            p2->next = 0;
        }
        else
        {
            p2_temp = p2;
            p2_temp_prev = p2;

            while (p2_temp && (*p1)->time_index >= p2_temp->time_index)
            {
                p2_temp_prev = p2_temp;
                p2_temp = p2_temp->next;
            }

            if (!p2_temp)
            {
                p2_temp_prev->next = *p1;
                *p1 = (*p1)->next;
                p2_temp_prev->next->next = 0;
            }
            else
            {
                p1_temp = (*p1)->next;
                (*p1)->next = p2_temp;
                p2_temp_prev->next = *p1;

                *p1 = p1_temp;
            }

        }
    }

    *p1 = p2;

    v1_temp = *v1;

    while (v1_temp)
    {
        for (i = 0; i < v1_temp->characteristics_count; i++)
        {
            for (j = 0; j < v1_temp->characteristics_count - i - 1; j++)
            {
                if (v1_temp->characteristics[j].time_index > v1_temp->characteristics[j + 1].time_index)
                {
                    uint64_t t_offset;  // beginning of the var or attr entry
                    struct adios_index_characteristic_dims_struct_v1 t_dims;
                    uint16_t t_var_id;
                    void * t_value;
                    uint64_t t_payload_offset;   // beginning of the var or attr payload
                    uint32_t t_file_index;
                    uint32_t t_time_index;

                    // NCSU - Statistics
                    uint32_t t_bitmap;
                    struct adios_index_characteristics_stat_struct ** t_stats;

                    t_offset = v1_temp->characteristics[j].offset;
                    t_dims.count = v1_temp->characteristics[j].dims.count;
                    t_dims.dims = v1_temp->characteristics[j].dims.dims;
                    t_var_id = v1_temp->characteristics[j].var_id;
                    t_value = v1_temp->characteristics[j].value;
                    t_payload_offset = v1_temp->characteristics[j].payload_offset;
                    t_file_index = v1_temp->characteristics[j].file_index;
                    t_time_index = v1_temp->characteristics[j].time_index;
                    t_bitmap = v1_temp->characteristics[j].bitmap;
                    t_stats = v1_temp->characteristics[j].stats;

                    v1_temp->characteristics[j].offset = v1_temp->characteristics[j + 1].offset;
                    v1_temp->characteristics[j].dims.count = v1_temp->characteristics[j + 1].dims.count;
                    v1_temp->characteristics[j].dims.dims = v1_temp->characteristics[j + 1].dims.dims;
                    v1_temp->characteristics[j].var_id = v1_temp->characteristics[j + 1].var_id;
                    v1_temp->characteristics[j].value = v1_temp->characteristics[j + 1].value;
                    v1_temp->characteristics[j].payload_offset = v1_temp->characteristics[j + 1].payload_offset;
                    v1_temp->characteristics[j].file_index = v1_temp->characteristics[j + 1].file_index;
                    v1_temp->characteristics[j].time_index = v1_temp->characteristics[j + 1].time_index;
                    v1_temp->characteristics[j].bitmap = v1_temp->characteristics[j + 1].bitmap;
                    v1_temp->characteristics[j].stats = v1_temp->characteristics[j + 1].stats;

                    v1_temp->characteristics[j + 1].offset = t_offset;
                    v1_temp->characteristics[j + 1].dims.count = t_dims.count;
                    v1_temp->characteristics[j + 1].dims.dims = t_dims.dims;
                    v1_temp->characteristics[j + 1].var_id = t_var_id;
                    v1_temp->characteristics[j + 1].value = t_value;
                    v1_temp->characteristics[j + 1].payload_offset = t_payload_offset;
                    v1_temp->characteristics[j + 1].file_index = t_file_index;
                    v1_temp->characteristics[j + 1].time_index = t_time_index;
                    v1_temp->characteristics[j + 1].bitmap = t_bitmap;
                    v1_temp->characteristics[j + 1].stats = t_stats;

                    // NCSU ALACRITY-ADIOS - Swap transform metadata
                    adios_transform_swap_transform_characteristics(&v1_temp->characteristics[j].transform, &v1_temp->characteristics[j + 1].transform);
                }
            }
        }

        v1_temp = v1_temp->next;
    }

    // no need to sort attributes
}

static void adios_clear_process_groups_index_v1 (
        struct adios_index_process_group_struct_v1 * root
        )
{
    while (root)
    {
        struct adios_index_process_group_struct_v1 * temp = root->next;
        if (root->group_name)
            free (root->group_name);
        if (root->time_index_name)
            free (root->time_index_name);
        free (root);
        root = temp;
    }
}

// NCSU - Clears up the statistical data from variable index table
static void adios_clear_vars_index_v1 (struct adios_index_var_struct_v1 * root)
{
    while (root)
    {
        int i;
        struct adios_index_var_struct_v1 * temp = root->next;
        enum ADIOS_DATATYPES original_var_type = adios_transform_get_var_original_type_index (root);

        if (root->group_name)
            free (root->group_name);
        if (root->var_name)
            free (root->var_name);
        if (root->var_path)
            free (root->var_path);

        for (i = 0; i < root->characteristics_count; i++)
        {
            if (root->characteristics [i].dims.count != 0)
                free (root->characteristics [i].dims.dims);
            if (root->characteristics [i].value)
                free (root->characteristics [i].value);

            // NCSU - Clears up the statistical data, based on bitmap
            if (root->characteristics [i].stats != 0)
            {
                uint8_t j = 0, idx = 0;
                uint8_t c = 0, count = adios_get_stat_set_count(original_var_type);

                for (c = 0; c < count; c ++)
                {
                    while (root->characteristics [i].bitmap >> j)
                    {
                        if ((root->characteristics [i].bitmap >> j) & 1)
                        {
                            if (j == adios_statistic_hist)
                            {
                                struct adios_index_characteristics_hist_struct * hist = (struct adios_index_characteristics_hist_struct    *) root->characteristics [i].stats[c][idx].data;
                                free (hist->breaks);
                                free (hist->frequencies);
                            }
                            else
                                free (root->characteristics [i].stats[c][idx].data);
                            idx ++;
                        }
                        j ++;
                    }
                    free (root->characteristics [i].stats [c]);
                }

                free (root->characteristics [i].stats);
            }

            // NCSU ALACRITY-ADIOS - Clear the transform metadata
            adios_transform_clear_transform_characteristic(&root->characteristics[i].transform);
        }
        if (root->characteristics)
            free (root->characteristics);

        free (root);
        root = temp;
    }
}

// NCSU - Clears up the statistical data, based on bitmap
    static void adios_clear_attributes_index_v1
(struct adios_index_attribute_struct_v1 * root)
{
    while (root)
    {
        int i;
        struct adios_index_attribute_struct_v1 * temp = root->next;
        enum ADIOS_DATATYPES var_type = root->type;

        if (root->group_name)
            free (root->group_name);
        if (root->attr_name)
            free (root->attr_name);
        if (root->attr_path)
            free (root->attr_path);
        for (i = 0; i < root->characteristics_count; i++)
        {
            if (root->characteristics [i].dims.count != 0)
                free (root->characteristics [i].dims.dims);

            // NCSU - Clears up the statistical data, based on bitmap
            if (root->characteristics [i].stats != 0)
            {
                uint8_t j = 0, idx = 0;
                uint8_t c = 0, count = adios_get_stat_set_count(var_type);
                for (c = 0; c < count; c ++)
                {
                    while (root->characteristics [i].bitmap >> j)
                    {
                        if ((root->characteristics [i].bitmap >> j) & 1)
                        {
                            if (j == adios_statistic_hist)
                            {
                                struct adios_index_characteristics_hist_struct * hist = (struct adios_index_characteristics_hist_struct *) root->characteristics [i].stats[c][idx].data;
                                free (hist->breaks);
                                free (hist->frequencies);
                                free (hist);
                            }
                            else
                                free (root->characteristics [i].stats[c][idx].data);

                            idx ++;
                        }
                        j ++;
                    }
                    free (root->characteristics [i].stats [c]);
                }
                free (root->characteristics [i].stats);
            }

            // NCSU ALACRITY-ADIOS - Clear the transform metadata
            adios_transform_clear_transform_characteristic(&root->characteristics[i].transform);

            if (root->characteristics [i].value)
                free (root->characteristics [i].value);
        }

        if (root->characteristics)
            free (root->characteristics);

        free (root);
        root = temp;
    }
}


struct adios_index_struct_v1 * adios_alloc_index_v1 (int alloc_hashtables)
{
    struct adios_index_struct_v1 * index = (struct adios_index_struct_v1 *)
                malloc (sizeof(struct adios_index_struct_v1));
    assert (index);
    index->pg_root = NULL;
    index->vars_root = NULL;
    index->vars_tail = NULL;
    index->attrs_root = NULL;
    index->attrs_tail = NULL;
    if (alloc_hashtables) {
        index->hashtbl_vars  = qhashtbl(500);
        //index->hashtbl_attrs = qhashtbl(100);
        index->hashtbl_attrs = NULL; // not used yet
    } else {
        index->hashtbl_vars = NULL;
        index->hashtbl_attrs = NULL;
    }
    return index;
}



void adios_free_index_v1 (struct adios_index_struct_v1 * index)
{
    if (!index)
        return;

    if (index->hashtbl_vars)
        index->hashtbl_vars->free  (index->hashtbl_vars);
    if (index->hashtbl_attrs)
        index->hashtbl_attrs->free (index->hashtbl_attrs);
    free(index);
}

void adios_clear_index_v1 (struct adios_index_struct_v1 * index)
{
    if (!index)
        return;

    adios_clear_process_groups_index_v1 (index->pg_root);
    adios_clear_vars_index_v1 (index->vars_root);
    adios_clear_attributes_index_v1 (index->attrs_root);
    index->pg_root = NULL;
    index->vars_root = NULL;
    index->vars_tail = NULL;
    index->attrs_root = NULL;
    index->attrs_tail = NULL;
    if (index->hashtbl_vars)
        index->hashtbl_vars->clear  (index->hashtbl_vars);
    if (index->hashtbl_attrs)
        index->hashtbl_attrs->clear (index->hashtbl_attrs);
}

uint8_t count_dimensions (const struct adios_dimension_struct * dimensions)
{
    uint8_t count = 0;

    while (dimensions)
    {
        count++;
        dimensions = dimensions->next;
    }

    return count;
}

static uint64_t cast_var_data_as_uint64 (const char * parent_name
        ,enum ADIOS_DATATYPES type
        ,void * data
        )
{
    if (!data)
    {
        adios_error (err_unspecified,
                "cannot write var since dim %s not provided\n",
                parent_name);
        return 0;
    }

    switch (type)
    {
        case adios_byte:
            return (uint64_t) *(int8_t *) data;

        case adios_short:
            return (uint64_t) *(int16_t *) data;

        case adios_integer:
            return (uint64_t) *(int32_t *) data;

        case adios_long:
            return (uint64_t) *(int64_t *) data;

        case adios_unsigned_byte:
            return (uint64_t) *(uint8_t *) data;

        case adios_unsigned_short:
            return (uint64_t) *(uint16_t *) data;

        case adios_unsigned_integer:
            return (uint64_t) *(uint32_t *) data;

        case adios_unsigned_long:
            return (uint64_t) *(uint64_t *) data;

        case adios_real:
            return (uint64_t) *(float *) data;

        case adios_double:
            return (uint64_t) *(double *) data;

        case adios_long_double:
            return (uint64_t) *(long double *) data;

        case adios_string:
        case adios_complex:
        case adios_double_complex:
        default:
            adios_error (err_unspecified,
                    "Cannot convert type %s to integer for var %s\n",
                    adios_type_to_string_int (type), parent_name);
            return 0;
    }
    return 0;
}

uint64_t adios_get_dim_value (struct adios_dimension_item_struct * dimension)
{
    uint64_t dim = 0;

    if (dimension->var != 0)
    {
        struct adios_var_struct * var = dimension->var;
        if (var->data)
        {
            dim = cast_var_data_as_uint64 (var->name, var->type, var->data);
        }
        else
        {
            adios_error (err_dimension_required, "array dimension data missing\n");
        }
    }
    else if (dimension->attr != 0)
    {
        struct adios_attribute_struct * attr = dimension->attr;
        if (attr->var)
        {
            if (attr->var->data)
            {
                dim = cast_var_data_as_uint64 (attr->var->name,attr->var->type,attr->var->data);
            }
            else
            {
                adios_error (err_dimension_required, "array dimension data missing\n");
            }
        }
        else
        {
            dim = cast_var_data_as_uint64 (attr->name, attr->type ,attr->value);
        }
    }
    else
    {
        if (dimension->time_index == adios_flag_yes)
            dim = 1;
        else
            dim = dimension->rank;
    }

    return dim;
}

void adios_copy_var_written (struct adios_group_struct * g, struct adios_var_struct * var)
{
    assert(g);
    struct adios_var_struct * var_new;

    var_new = (struct adios_var_struct *) malloc
        (sizeof (struct adios_var_struct));
    var_new->id = var->id;
    var_new->parent_var = var;
    var_new->name = strdup (var->name);
    var_new->path = strdup (var->path);
    var_new->type = var->type;
    var_new->dimensions = 0;
    var_new->got_buffer = var->got_buffer;
    var_new->is_dim = var->is_dim;
    var_new->write_offset = var->write_offset;
    var_new->stats = 0;
    var_new->free_data = var->free_data;
    var_new->data = 0;
    var_new->data_size = var->data_size;
            var_new->write_count = var->write_count;
    var_new->next = 0;

    uint64_t size = adios_get_type_size (var->type, var->data);
    switch (var->type)
    {
        case adios_byte:
        case adios_unsigned_byte:
        case adios_short:
        case adios_unsigned_short:
        case adios_integer:
        case adios_unsigned_integer:
        case adios_long:
        case adios_unsigned_long:
        case adios_real:
        case adios_double:
        case adios_long_double:
        case adios_complex:
        case adios_double_complex:
            if (var->dimensions)
            {
                uint8_t c;
                uint8_t j;
                struct adios_dimension_struct * d = var->dimensions;
                /*
                 *
                 * NOT ALL METHODS TRACK MIN/MAX.  CHECK BEFORE TRYING TO COPY.
                 *
                 */
                // NCSU Statistics - copy stat to new var struct
                enum ADIOS_DATATYPES original_var_type = adios_transform_get_var_original_type_var (var);
                uint8_t count = adios_get_stat_set_count(original_var_type);
                uint8_t idx = 0;
                uint64_t characteristic_size;

                var_new->bitmap = var->bitmap;
                var_new->stats = malloc (count * sizeof(struct adios_stat_struct *));

                // Set of characteristics will be repeated thrice for complex numbers
                for (c = 0; c < count; c ++)
                {
                    var_new->stats[c] = calloc(ADIOS_STAT_LENGTH, sizeof (struct adios_stat_struct));

                    j = idx = 0;
                    while (var->bitmap >> j)
                    {
                        if ((var->bitmap >> j) & 1)
                        {
                            if (var->stats[c][idx].data != NULL)
                            {
                                if (j == adios_statistic_hist)
                                {
                                    var_new->stats[c][idx].data = (struct adios_hist_struct *) malloc (sizeof(struct adios_hist_struct));

                                    struct adios_hist_struct * var_hist = var->stats[c][idx].data;
                                    struct adios_hist_struct * var_new_hist = var_new->stats[c][idx].data;

                                    var_new_hist->min = var_hist->min;
                                    var_new_hist->max = var_hist->max;
                                    var_new_hist->num_breaks = var_hist->num_breaks;

                                    var_new_hist->frequencies = malloc ((var_hist->num_breaks + 1) * adios_get_type_size(adios_unsigned_integer, ""));
                                    memcpy (var_new_hist->frequencies, var_hist->frequencies, (var_hist->num_breaks + 1) * adios_get_type_size(adios_unsigned_integer, ""));
                                    var_new_hist->breaks = malloc ((var_hist->num_breaks) * adios_get_type_size(adios_double, ""));
                                    memcpy (var_new_hist->breaks, var_hist->breaks, (var_hist->num_breaks) * adios_get_type_size(adios_double, ""));
                                }
                                else
                                {
                                    characteristic_size = adios_get_stat_size(var->stats[c][idx].data, original_var_type, j);
                                    var_new->stats[c][idx].data = malloc (characteristic_size);
                                    memcpy (var_new->stats[c][idx].data, var->stats[c][idx].data, characteristic_size);
                                }

                                idx ++;
                            }
                        }
                        j ++;
                    }
                }

                // NCSU ALACRITY-ADIOS - Copy transform metadata
                adios_transform_copy_var_transform(var_new, var);

                c = count_dimensions (var->dimensions);

                for (j = 0; j < c; j++)
                {
                    struct adios_dimension_struct * d_new = (struct adios_dimension_struct *)
                        malloc (sizeof (struct adios_dimension_struct));
                    // de-reference dimension id
                    d_new->dimension.var = NULL;
                    d_new->dimension.attr = NULL;
                    d_new->dimension.rank = adios_get_dim_value (&d->dimension);
                    d_new->dimension.time_index = d->dimension.time_index;
                    d_new->global_dimension.var = NULL;
                    d_new->global_dimension.attr = NULL;
                    d_new->global_dimension.rank = adios_get_dim_value (&d->global_dimension);
                    d_new->global_dimension.time_index = d->global_dimension.time_index;
                    d_new->local_offset.var = NULL;
                    d_new->local_offset.attr = NULL;
                    d_new->local_offset.rank = adios_get_dim_value (&d->local_offset);
                    d_new->local_offset.time_index = d->local_offset.time_index;
                    d_new->next = 0;

                    adios_append_dimension (&var_new->dimensions, d_new);

                    d = d->next;
                }
            }
            else
            {
                adios_transform_init_transform_var(var_new);
                var_new->stats = 0;
                var_new->data = malloc (size);
                memcpy (var_new->data, var->data, size);
            }

            break;

        case adios_string:
            {
                adios_transform_init_transform_var(var_new);
                var_new->data = malloc (size + 1);
                memcpy (var_new->data, var->data, size);
                ((char *) (var_new->data)) [size] = 0;

                break;
            }
        default:
            {
                adios_error (err_unspecified, "Reached unexpected branch in %s:%s:%d\n",
                        __FILE__,__func__, __LINE__);
            }
    }

    /* Insert new variable into the copy list */

    /* Note: many routines parse the list of variables through the ->next pointer with
     * "while (v) { ...; v=v->next }
     * So we don't make a double linked circular list, just a simple list, with
     * having an extra pointer to the tail
     */
    if (!g->vars_written) {
        // first variable: g->vars_written     : V => (null)
        //                 g->vars_written_tail: V => (null)
        var_new->next = NULL;
        g->vars_written = var_new;      // V => (null)
        g->vars_written_tail = var_new; // V => (null)
    } else {
       var_new->next = NULL;
       // append var to tail
       g->vars_written_tail->next = var_new;  // g->vars => ... => tail => V => (null)
       // new tail is var
       g->vars_written_tail = var_new;
    }

}

#if 0
void adios_copy_var_written (struct adios_var_struct ** root
        ,struct adios_var_struct * var
        ,struct adios_file_struct * fd
        )
{
    struct adios_var_struct * var_new;

    while (root)
    {
        if (!*root)
        {
            var_new = (struct adios_var_struct *) malloc
                (sizeof (struct adios_var_struct));
            //var_new->id = ++fd->group->member_count;
            var_new->id = var->id;
            var_new->parent_var = var;
            var_new->name = strdup (var->name);
            var_new->path = strdup (var->path);
            var_new->type = var->type;
            var_new->dimensions = 0;
            var_new->got_buffer = var->got_buffer;
            var_new->is_dim = var->is_dim;
            var_new->write_offset = var->write_offset;
            var_new->stats = 0;
            var_new->free_data = var->free_data;
            var_new->data = 0;
            var_new->data_size = var->data_size;
            var_new->next = 0;

            uint64_t size = adios_get_type_size (var->type, var->data);
            switch (var->type)
            {
                case adios_byte:
                case adios_unsigned_byte:
                case adios_short:
                case adios_unsigned_short:
                case adios_integer:
                case adios_unsigned_integer:
                case adios_long:
                case adios_unsigned_long:
                case adios_real:
                case adios_double:
                case adios_long_double:
                case adios_complex:
                case adios_double_complex:
                    if (var->dimensions)
                    {
                        uint8_t c;
                        uint8_t j;
                        struct adios_dimension_struct * d = var->dimensions;

                        // NCSU ALACRITY-ADIOS - Copy transform metadata
                        adios_transform_copy_var_transform(fd, var_new, var);

                        /*
                         *
                         * NOT ALL METHODS TRACK MIN/MAX.  CHECK BEFORE TRYING TO COPY.
                         *
                         */

                        // NCSU Statistics - copy stat to new var struct
                        enum ADIOS_DATATYPES original_var_type = adios_transform_get_var_original_type_var (var);
                        uint8_t count = adios_get_stat_set_count(original_var_type);
                        uint8_t idx = 0;
                        uint64_t characteristic_size;

                        var_new->bitmap = var->bitmap;
                        var_new->stats = malloc (count * sizeof(struct adios_stat_struct *));

                        // Set of characteristics will be repeated thrice for complex numbers
                        for (c = 0; c < count; c ++)
                        {
                            var_new->stats[c] = calloc(ADIOS_STAT_LENGTH, sizeof (struct adios_stat_struct));

                            j = idx = 0;
                            while (var->bitmap >> j)
                            {
                                if ((var->bitmap >> j) & 1)
                                {
                                    if (var->stats[c][idx].data != NULL)
                                    {
                                        if (j == adios_statistic_hist)
                                        {
                                            var_new->stats[c][idx].data = (struct adios_hist_struct *) malloc (sizeof(struct adios_hist_struct));

                                            struct adios_hist_struct * var_hist = var->stats[c][idx].data;
                                            struct adios_hist_struct * var_new_hist = var_new->stats[c][idx].data;

                                            var_new_hist->min = var_hist->min;
                                            var_new_hist->max = var_hist->max;
                                            var_new_hist->num_breaks = var_hist->num_breaks;

                                            var_new_hist->frequencies = malloc ((var_hist->num_breaks + 1) * adios_get_type_size(adios_unsigned_integer, ""));
                                            memcpy (var_new_hist->frequencies, var_hist->frequencies, (var_hist->num_breaks + 1) * adios_get_type_size(adios_unsigned_integer, ""));
                                            var_new_hist->breaks = malloc ((var_hist->num_breaks) * adios_get_type_size(adios_double, ""));
                                            memcpy (var_new_hist->breaks, var_hist->breaks, (var_hist->num_breaks) * adios_get_type_size(adios_double, ""));
                                        }
                                        else
                                        {
                                            characteristic_size = adios_get_stat_size(var->stats[c][idx].data, original_var_type, j);
                                            var_new->stats[c][idx].data = malloc (characteristic_size);
                                            memcpy (var_new->stats[c][idx].data, var->stats[c][idx].data, characteristic_size);
                                        }

                                        idx ++;
                                    }
                                }
                                j ++;
                            }
                        }

                        c = count_dimensions (var->dimensions);

                        for (j = 0; j < c; j++)
                        {
                            struct adios_dimension_struct * d_new = (struct adios_dimension_struct *)
                                malloc (sizeof (struct adios_dimension_struct));
                            // de-reference dimension id
                            d_new->dimension.var = NULL;
                            d_new->dimension.attr = NULL;
                            d_new->dimension.rank = adios_get_dim_value (&d->dimension);
                            d_new->dimension.time_index = d->dimension.time_index;
                            d_new->global_dimension.var = NULL;
                            d_new->global_dimension.attr = NULL;
                            d_new->global_dimension.rank = adios_get_dim_value (&d->global_dimension);
                            d_new->global_dimension.time_index = d->global_dimension.time_index;
                            d_new->local_offset.var = NULL;
                            d_new->local_offset.attr = NULL;
                            d_new->local_offset.rank = adios_get_dim_value (&d->local_offset);
                            d_new->local_offset.time_index = d->local_offset.time_index;
                            d_new->next = 0;

                            adios_append_dimension (&var_new->dimensions, d_new);

                            d = d->next;
                        }
                    }
                    else
                    {
                        var_new->stats = 0;
                        var_new->data = malloc (size);
                        memcpy (var_new->data, var->data, size);
                    }

                    break;

                case adios_string:
                    {
                        var_new->data = malloc (size + 1);
                        memcpy (var_new->data, var->data, size);
                        ((char *) (var_new->data)) [size] = 0;

                        break;
                    }
            }

            *root = var_new;
            root = 0;
        }
        else
        {
            root = &(*root)->next;
        }
    }
}
#endif

void adios_build_index_v1 (struct adios_file_struct * fd,
                           struct adios_index_struct_v1 * index)
{
    struct adios_group_struct * g = fd->group;
    struct adios_var_struct * v = g->vars_written;
    struct adios_attribute_struct * a = g->attributes;
    struct adios_index_process_group_struct_v1 * g_item;

    g_item = (struct adios_index_process_group_struct_v1 *)
        malloc (sizeof (struct adios_index_process_group_struct_v1));
    g_item->group_name = (g->name ? strdup (g->name) : 0L);
    g_item->adios_host_language_fortran = g->adios_host_language_fortran;
    g_item->process_id = g->process_id;
    g_item->time_index_name = (g->time_index_name ? strdup (g->time_index_name) : 0L);
    g_item->time_index = g->time_index;
    g_item->offset_in_file = fd->pg_start_in_file;
    g_item->next = 0;

    // build the groups and vars index
    index_append_process_group_v1 (&index->pg_root, g_item);

    while (v)
    {
        // only add items that were written to the index
        if (v->write_offset != 0)
        {
            struct adios_index_var_struct_v1 * v_index;
            v_index = malloc (sizeof (struct adios_index_var_struct_v1));
            v_index->characteristics = malloc (
                    sizeof (struct adios_index_characteristic_struct_v1)
                    );

            v_index->id = v->id;
            v_index->group_name = (g->name ? strdup (g->name) : 0L);
            v_index->var_name = (v->name ? strdup (v->name) : 0L);
            v_index->var_path = (v->path ? strdup (v->path) : 0L);
            v_index->type = v->type;
            v_index->characteristics_count = 1;
            v_index->characteristics_allocated = 1;
            v_index->characteristics [0].offset = v->write_offset;
            // Find the old var in g->vars.
            // We need this to calculate the correct payload_offset, because that
            // holds the variable references in the dimensions, while v-> contains
            // only numerical values
            struct adios_var_struct * old_var = v->parent_var;
            v_index->characteristics [0].payload_offset = v->write_offset
                + adios_calc_var_overhead_v1 (old_var)
                - strlen (old_var->path)  // take out the length of path defined in XML
                + strlen (v->path); // add length of the actual, current path of this var
            v_index->characteristics [0].file_index = fd->subfile_index;
            v_index->characteristics [0].time_index = g_item->time_index;

            v_index->characteristics [0].value = 0;
            v_index->characteristics [0].dims.count = 0;

            // NCSU - Initializing stat related info in index
            v_index->characteristics [0].bitmap = 0;
            v_index->characteristics [0].stats = 0;
            // NCSU ALACRITY-ADIOS - Initialize the transform metadata
            adios_transform_init_transform_characteristic(&v_index->characteristics[0].transform);
            //v_index->characteristics [0].transform_type = adios_transform_none;

            uint64_t size = adios_get_type_size (v->type, v->data);
            switch (v->type)
            {
                case adios_byte:
                case adios_unsigned_byte:
                case adios_short:
                case adios_unsigned_short:
                case adios_integer:
                case adios_unsigned_integer:
                case adios_long:
                case adios_unsigned_long:
                case adios_real:
                case adios_double:
                case adios_long_double:
                case adios_complex:
                case adios_double_complex:
                    if (v->dimensions)
                    {
                        uint8_t c;
                        uint8_t j;
                        struct adios_dimension_struct * d = v->dimensions;

                        // NCSU - Copy statistics from var struct to index
                        enum ADIOS_DATATYPES original_var_type = adios_transform_get_var_original_type_var (v);

                        uint8_t count = adios_get_stat_set_count(original_var_type);
                        uint8_t idx = 0;
                        uint64_t characteristic_size;

                        v_index->characteristics [0].bitmap = v->bitmap;
                        v_index->characteristics [0].stats = malloc (count * sizeof(struct adios_index_characteristics_stat_struct *));

                        // Set of characteristics will be repeated thrice for complex numbers
                        for (c = 0; c < count; c ++)
                        {
                            v_index->characteristics [0].stats[c] = calloc(ADIOS_STAT_LENGTH, sizeof (struct adios_index_characteristics_stat_struct));

                            j = idx = 0;
                            while (v_index->characteristics [0].bitmap >> j)
                            {
                                if ((v_index->characteristics [0].bitmap >> j) & 1)
                                {
                                    if (v->stats[c][idx].data != NULL)
                                    {
                                        if (j == adios_statistic_hist)
                                        {
                                            v_index->characteristics [0].stats[c][idx].data = (struct adios_index_characteristics_hist_struct *) malloc (sizeof(struct adios_index_characteristics_hist_struct));

                                            struct adios_hist_struct * v_hist = v->stats[c][idx].data;
                                            struct adios_hist_struct * v_index_hist = v_index->characteristics [0].stats[c][idx].data;

                                            v_index_hist->min = v_hist->min;
                                            v_index_hist->max = v_hist->max;
                                            v_index_hist->num_breaks = v_hist->num_breaks;

                                            v_index_hist->frequencies = malloc ((v_hist->num_breaks + 1) * adios_get_type_size(adios_unsigned_integer, ""));
                                            memcpy (v_index_hist->frequencies, v_hist->frequencies, (v_hist->num_breaks + 1) * adios_get_type_size(adios_unsigned_integer, ""));
                                            v_index_hist->breaks = malloc ((v_hist->num_breaks) * adios_get_type_size(adios_double, ""));
                                            memcpy (v_index_hist->breaks, v_hist->breaks, (v_hist->num_breaks) * adios_get_type_size(adios_double, ""));
                                        }
                                        else
                                        {
                                            characteristic_size = adios_get_stat_size(v->stats[c][idx].data, original_var_type, j);
                                            v_index->characteristics [0].stats[c][idx].data = malloc (characteristic_size);
                                            memcpy (v_index->characteristics [0].stats[c][idx].data, v->stats[c][idx].data, characteristic_size);
                                        }

                                        idx ++;
                                    }
                                }
                                j ++;
                            }
                        }
                        // NCSU - End of copy, for statistics

                        // NCSU ALACRITY-ADIOS - copy transform type field
                        adios_transform_copy_transform_characteristic(&v_index->characteristics[0].transform, v);

                        c = count_dimensions (v->dimensions);
                        v_index->characteristics [0].dims.count = c;
                        // (local, global, local offset)
                        v_index->characteristics [0].dims.dims = malloc
                            (3 * 8 * v_index->characteristics [0].dims.count);
                        for (j = 0; j < c; j++)
                        {
                            v_index->characteristics [0].dims.dims [j * 3 + 0] =
                                adios_get_dim_value (&d->dimension);
                            v_index->characteristics [0].dims.dims [j * 3 + 1] =
                                adios_get_dim_value (&d->global_dimension);
                            v_index->characteristics [0].dims.dims [j * 3 + 2] =
                                adios_get_dim_value (&d->local_offset);

                            d = d->next;
                        }
                        v_index->characteristics [0].value = 0;
                    }

                    if (v->data)
                    {
                        // NCSU - Copy statistics from var struct to index
                        v_index->characteristics [0].bitmap = 0;
                        v_index->characteristics [0].stats = 0;
                        // NCSU ALACRITY-ADIOS - Clear the transform metadata
                        // This is probably redundant with above code, but do it anyway to be safe
                        adios_transform_clear_transform_characteristic(&v_index->characteristics[0].transform);

                        v_index->characteristics [0].value = malloc (size);
                        memcpy (v_index->characteristics [0].value, v->data
                                ,size
                               );
                        v_index->characteristics [0].dims.count = 0;
                        v_index->characteristics [0].dims.dims = 0;
                    }

                    break;

                case adios_string:
                    {
                        v_index->characteristics [0].value = malloc (size + 1);
                        memcpy (v_index->characteristics [0].value, v->data, size);
                        ((char *) (v_index->characteristics [0].value)) [size] = 0;

                        break;
                    }
                default:
                    {
                        adios_error (err_unspecified, "Reached unexpected branch in %s:%s:%d\n",
                                __FILE__,__func__, __LINE__);
                    }
            }
            v_index->next = 0;

            // this fn will either take ownership for free
            log_debug ("build index var %s/%s\n", v_index->var_path, v_index->var_name);
            index_append_var_v1 (index, v_index);
        }

        v = v->next;
    }

    while (a)
    {
        // only add items that were written to the index
        if (a->write_offset != 0)
        {
            struct adios_index_attribute_struct_v1 * a_index;
            a_index = malloc (sizeof (struct adios_index_attribute_struct_v1));
            a_index->characteristics = malloc (
                    sizeof (struct adios_index_characteristic_struct_v1)
                    );

            a_index->id = a->id;
            a_index->group_name = (g->name ? strdup (g->name) : 0L);
            a_index->attr_name = (a->name ? strdup (a->name) : 0L);
            a_index->attr_path = (a->path ? strdup (a->path) : 0L);
            a_index->type = a->type;
            a_index->characteristics_count = 1;
            a_index->characteristics_allocated = 1;
            uint64_t size = adios_get_type_size (a->type, a->value);

            a_index->characteristics [0].offset = a->write_offset;
            a_index->characteristics [0].payload_offset = a->write_offset + adios_calc_attribute_overhead_v1 (a);
            a_index->characteristics [0].file_index = fd->subfile_index;
            a_index->characteristics [0].time_index = 0;

            // NCSU - Initializing stat related info in attribute index
            a_index->characteristics [0].bitmap = 0;
            a_index->characteristics [0].stats = 0;
            // NCSU ALACRITY-ADIOS - Initialize transform metadata
            adios_transform_init_transform_characteristic(&a_index->characteristics[0].transform);
            //a_index->characteristics[0].transform_type = adios_transform_none;

            if (a->value)
            {
                a_index->characteristics [0].value = malloc (size + 1);
                ((char *) (a_index->characteristics [0].value)) [size] = 0;
                memcpy (a_index->characteristics [0].value, a->value, size);
            }
            else
            {
                a_index->characteristics [0].value = 0;
            }
            a_index->characteristics [0].dims.count = 0;
            a_index->characteristics [0].dims.dims = 0;
            if (a->var)
                a_index->characteristics [0].var_id = a->var->id;
            else
                a_index->characteristics [0].var_id = 0;

            a_index->next = 0;

            // this fn will either take ownership for free
            index_append_attribute_v1 (&index->attrs_root, a_index);
        }

        a = a->next;
    }
}

int adios_write_index_v1 (char ** buffer
        ,uint64_t * buffer_size
        ,uint64_t * buffer_offset
        ,uint64_t index_start
        ,struct adios_index_struct_v1 * index
        )
{
    uint64_t groups_count = 0;
    uint32_t vars_count = 0;
    uint32_t attrs_count = 0;

    uint64_t index_size = 0;
    uint64_t pg_index_start = index_start;
    uint64_t vars_index_start = 0;
    uint64_t attrs_index_start = 0;

    // we need to save the offset we will write the count and size
    uint64_t buffer_offset_start = 0; // since we realloc, we can't save a ptr

    struct adios_index_process_group_struct_v1 * pg_root = index->pg_root;
    struct adios_index_var_struct_v1 * vars_root = index->vars_root;
    struct adios_index_attribute_struct_v1 * attrs_root = index->attrs_root;

    // save for the process group index
    buffer_offset_start = *buffer_offset;

    *buffer_offset += (8 + 8); // save space for groups count and index size

    while (pg_root)
    {
        uint8_t flag;
        uint16_t len;
        uint16_t group_size = 0;
        uint64_t group_start = *buffer_offset;

        groups_count++;

        *buffer_offset += 2; // save space for the size

        len = strlen (pg_root->group_name);
        buffer_write (buffer, buffer_size, buffer_offset, &len, 2);
        index_size += 2;
        group_size += 2;
        buffer_write (buffer, buffer_size, buffer_offset
                ,pg_root->group_name, len
                );
        index_size += len;
        group_size += len;

        flag = (pg_root->adios_host_language_fortran == adios_flag_yes ? 'y'
                : 'n'
               );
        buffer_write (buffer, buffer_size, buffer_offset, &flag, 1);
        index_size += 1;
        group_size += 1;

        buffer_write (buffer, buffer_size, buffer_offset
                ,&pg_root->process_id, 4
                );
        index_size += 4;
        group_size += 4;

        if (pg_root->time_index_name)
        {
            len = strlen (pg_root->time_index_name);
        }
        else
        {
            len = 0;
        }
        buffer_write (buffer, buffer_size, buffer_offset, &len, 2);
        index_size += 2;
        group_size += 2;
        if (len)
        {
            buffer_write (buffer, buffer_size, buffer_offset
                    ,pg_root->time_index_name, len
                    );
        }
        index_size += len;
        group_size += len;

        buffer_write (buffer, buffer_size, buffer_offset
                ,&pg_root->time_index, 4
                );
        index_size += 4;
        group_size += 4;
        buffer_write (buffer, buffer_size, buffer_offset
                ,&pg_root->offset_in_file, 8
                );
        index_size += 8;
        group_size += 8;

        buffer_write (buffer, buffer_size, &group_start, &group_size, 2);

        pg_root = pg_root->next;
    }

    buffer_write (buffer, buffer_size, &buffer_offset_start, &groups_count, 8);
    buffer_write (buffer, buffer_size, &buffer_offset_start, &index_size, 8);

    buffer_offset_start = *buffer_offset; // save to write the vars_count/size
    vars_index_start = buffer_offset_start + index_start;
    index_size = 0;

    *buffer_offset += (4 + 8); // save space for count and size

    while (vars_root)
    {
        uint8_t flag;
        uint16_t len;
        uint32_t var_size = 0;
        uint64_t var_start = *buffer_offset;
        int i;

        vars_count++;

        *buffer_offset += 4; // save space for var length

        buffer_write (buffer, buffer_size, buffer_offset, &vars_root->id, 4);
        index_size += 4;
        var_size += 4;

        len = strlen (vars_root->group_name);
        buffer_write (buffer, buffer_size, buffer_offset, &len, 2);
        index_size += 2;
        var_size += 2;
        buffer_write (buffer, buffer_size, buffer_offset
                ,vars_root->group_name, len
                );
        index_size += len;
        var_size += len;

        len = strlen (vars_root->var_name);
        buffer_write (buffer, buffer_size, buffer_offset, &len, 2);
        index_size += 2;
        var_size += 2;
        buffer_write (buffer, buffer_size, buffer_offset
                ,vars_root->var_name, len
                );
        index_size += len;
        var_size += len;

        len = strlen (vars_root->var_path);
        buffer_write (buffer, buffer_size, buffer_offset, &len, 2);
        index_size += 2;
        var_size += 2;
        buffer_write (buffer, buffer_size, buffer_offset
                ,vars_root->var_path, len
                );
        index_size += len;
        var_size += len;

        flag = vars_root->type;
        buffer_write (buffer, buffer_size, buffer_offset, &flag, 1);
        index_size += 1;
        var_size += 1;

        buffer_write (buffer, buffer_size, buffer_offset
                ,&vars_root->characteristics_count, 8
                );
        index_size += 8;
        var_size += 8;

        for (i = 0; i < vars_root->characteristics_count; i++)
        {
            uint64_t size;
            uint8_t characteristic_set_count = 0;
            uint32_t characteristic_set_length = 0;
            // NCSU ALACRITY-ADIOS - Temp vars to store bytes/num
            //   characteristics written by a delegate write function.
            uint64_t characteristic_write_length;
            uint8_t characteristic_write_count;

            uint64_t characteristic_set_start = *buffer_offset;
            *buffer_offset += 1 + 4; // save space for characteristic count/len
            index_size += 1 + 4;
            var_size += 1 + 4;

            // add an offset characteristic for all vars
            characteristic_set_count++;
            flag = (uint8_t) adios_characteristic_offset;
            buffer_write (buffer, buffer_size, buffer_offset, &flag, 1);
            index_size += 1;
            var_size += 1;
            characteristic_set_length += 1;

            buffer_write (buffer, buffer_size, buffer_offset
                    ,&vars_root->characteristics [i].offset, 8
                    );
            index_size += 8;
            var_size += 8;
            characteristic_set_length += 8;

            // add a payload offset characteristic for all vars
            characteristic_set_count++;
            flag = (uint8_t) adios_characteristic_payload_offset;
            buffer_write (buffer, buffer_size, buffer_offset, &flag, 1);
            index_size += 1;
            var_size += 1;
            characteristic_set_length += 1;

            buffer_write (buffer, buffer_size, buffer_offset
                    ,&vars_root->characteristics [i].payload_offset, 8
                    );
            index_size += 8;
            var_size += 8;
            characteristic_set_length += 8;

            // add a file index characteristic for all vars
            characteristic_set_count++;
            flag = (uint8_t) adios_characteristic_file_index;
            buffer_write (buffer, buffer_size, buffer_offset, &flag, 1);
            index_size += 1;
            var_size += 1;
            characteristic_set_length += 1;

            buffer_write (buffer, buffer_size, buffer_offset
                    ,&vars_root->characteristics [i].file_index, 4
                    );
            index_size += 4;
            var_size += 4;
            characteristic_set_length += 4;

            // add a time index characteristic for all vars
            characteristic_set_count++;
            flag = (uint8_t) adios_characteristic_time_index;
            buffer_write (buffer, buffer_size, buffer_offset, &flag, 1);
            index_size += 1;
            var_size += 1;
            characteristic_set_length += 1;

            buffer_write (buffer, buffer_size, buffer_offset
                    ,&vars_root->characteristics [i].time_index, 4
                    );
            index_size += 4;
            var_size += 4;
            characteristic_set_length += 4;

            // depending on if it is an array or not, generate a different
            // additional set of characteristics
            size = adios_get_type_size (vars_root->type
                    ,vars_root->characteristics [i].value
                    );

            switch (vars_root->type)
            {
                case adios_byte:
                case adios_unsigned_byte:
                case adios_short:
                case adios_unsigned_short:
                case adios_integer:
                case adios_unsigned_integer:
                case adios_long:
                case adios_unsigned_long:
                case adios_real:
                case adios_double:
                case adios_long_double:
                case adios_complex:
                case adios_double_complex:
                    if (vars_root->characteristics [i].dims.count)
                    {
                        // add a dimensions characteristic
                        characteristic_set_count++;
                        flag = (uint8_t) adios_characteristic_dimensions;
                        buffer_write (buffer, buffer_size, buffer_offset
                                ,&flag, 1
                                );
                        index_size += 1;
                        var_size += 1;
                        characteristic_set_length += 1;

                        buffer_write (buffer, buffer_size, buffer_offset
                                ,&vars_root->characteristics [i].dims.count
                                ,1
                                );
                        index_size += 1;
                        var_size += 1;
                        characteristic_set_length += 1;

                        len = 3 * 8 * vars_root->characteristics [i].dims.count;
                        buffer_write (buffer, buffer_size, buffer_offset
                                ,&len, 2
                                );
                        index_size += 2;
                        var_size += 2;
                        characteristic_set_length += 2;
                        buffer_write (buffer, buffer_size, buffer_offset
                                ,vars_root->characteristics [i].dims.dims
                                ,len
                                );
                        index_size += len;
                        var_size += len;
                        characteristic_set_length += len;

                        // NCSU ALACRITY-ADIOS - Adding transform type field
                        characteristic_write_length = 0;
                        characteristic_write_count =
                                adios_transform_serialize_transform_characteristic(
                                    &vars_root->characteristics[i].transform,
                                    &characteristic_write_length,
                                    buffer, buffer_size, buffer_offset
                                );

                        characteristic_set_count += characteristic_write_count;
                        index_size += characteristic_write_length;
                        var_size += characteristic_write_length;
                        characteristic_set_length += characteristic_write_length;

                        // NCSU - Adding bitmap
                        characteristic_set_count++;
                        flag = (uint8_t) adios_characteristic_bitmap;
                        buffer_write (buffer, buffer_size, buffer_offset
                                ,&flag, 1
                                );
                        index_size += 1;
                        var_size += 1;
                        characteristic_set_length += 1;

                        buffer_write (buffer, buffer_size, buffer_offset
                                ,&vars_root->characteristics [i].bitmap, 4
                                );
                        index_size += 4;
                        var_size += 4;
                        characteristic_set_length += 4;

                        // NCSU - Adding statistics
                        characteristic_set_count++;
                        flag = (uint8_t) adios_characteristic_stat;
                        buffer_write (buffer, buffer_size, buffer_offset
                                ,&flag, 1
                                );
                        index_size += 1;
                        var_size += 1;
                        characteristic_set_length += 1;

                        enum ADIOS_DATATYPES original_var_type = adios_transform_get_var_original_type_index (vars_root);
                        uint8_t count = adios_get_stat_set_count(original_var_type);

                        uint8_t idx = 0, c, j;
                        uint64_t characteristic_size;

                        for (c = 0; c < count; c ++)
                        {
                            j = idx = 0;

                            while (vars_root->characteristics [i].bitmap >> j)
                            {
                                if ((vars_root->characteristics [i].bitmap >> j) & 1)
                                {
                                    if (j == adios_statistic_hist)
                                    {
                                        struct adios_hist_struct * hist = vars_root->characteristics [i].stats[c][idx].data;

                                        buffer_write (  buffer, buffer_size, buffer_offset
                                                , &hist->num_breaks, 4
                                                );
                                        characteristic_size = 4;

                                        buffer_write (  buffer, buffer_size, buffer_offset
                                                , &hist->min, 8
                                                );
                                        characteristic_size += 8;

                                        buffer_write (  buffer, buffer_size, buffer_offset
                                                , &hist->max, 8
                                                );
                                        characteristic_size += 8;

                                        buffer_write (  buffer, buffer_size, buffer_offset
                                                , hist->frequencies, (hist->num_breaks + 1) * 4
                                                );
                                        characteristic_size += (hist->num_breaks + 1) * 4;

                                        buffer_write (  buffer, buffer_size, buffer_offset
                                                , hist->breaks, hist->num_breaks * 8
                                                );
                                        characteristic_size += (hist->num_breaks) * 8;
                                    }
                                    else
                                    {
                                        characteristic_size = adios_get_stat_size(vars_root->characteristics [i].stats[c][idx].data, original_var_type, j);

                                        buffer_write (     buffer, buffer_size, buffer_offset
                                                ,vars_root->characteristics [i].stats[c][idx].data, characteristic_size
                                                );

                                    }

                                    index_size += characteristic_size;
                                    var_size += characteristic_size;
                                    characteristic_set_length += characteristic_size;

                                    idx ++;
                                }
                                j ++;
                            }
                        }
                        // NCSU - End of addition statistic to buffer

                        /*
                        characteristic_set_count++;
                        flag = (uint8_t) adios_characteristic_transform_type;
                        buffer_write (buffer, buffer_size, buffer_offset, &flag, 1);
                        index_size += 1;
                        var_size += 1;
                        characteristic_set_length += 1;

                        buffer_write (buffer, buffer_size, buffer_offset, &vars_root->characteristics[i].transform_type, 1);
                        index_size += 1;
                        var_size += 1;
                        characteristic_set_length += 1;
                        */
                    }

                    if (vars_root->characteristics [i].value)
                    {
                        // add a value characteristic
                        characteristic_set_count++;
                        flag = (uint8_t) adios_characteristic_value;
                        buffer_write (buffer, buffer_size, buffer_offset
                                ,&flag, 1
                                );
                        index_size += 1;
                        var_size += 1;
                        characteristic_set_length += 1;
                        buffer_write (buffer, buffer_size, buffer_offset
                                ,vars_root->characteristics [i].value, size
                                );
                        index_size += size;
                        var_size += size;
                        characteristic_set_length += size;
                    }
                    break;

                case adios_string:
                    {
                        // add a value characteristic
                        characteristic_set_count++;
                        flag = (uint8_t) adios_characteristic_value;
                        buffer_write (buffer, buffer_size, buffer_offset
                                ,&flag, 1
                                );
                        index_size += 1;
                        var_size += 1;
                        characteristic_set_length += 1;
                        if (vars_root->type == adios_string)
                        {
                            uint16_t len = (uint16_t) size;
                            buffer_write (buffer, buffer_size, buffer_offset
                                    ,&len, 2
                                    );
                            index_size += 2;
                            var_size += 2;
                            characteristic_set_length += 2;
                        }
                        buffer_write (buffer, buffer_size, buffer_offset
                                ,vars_root->characteristics [i].value, size
                                );
                        index_size += size;
                        var_size += size;
                        characteristic_set_length += size;
                    }
                    break;
                default:
                    {
                        adios_error (err_unspecified, "Reached unexpected branch in %s:%s:%d\n",
                                __FILE__,__func__, __LINE__);
                    }
            }
            // characteristics count/size prefix
            buffer_write (buffer, buffer_size, &characteristic_set_start
                    ,&characteristic_set_count, 1
                    );
            buffer_write (buffer, buffer_size, &characteristic_set_start
                    ,&characteristic_set_length, 4
                    );
        }

        buffer_write (buffer, buffer_size, &var_start, &var_size, 4);

        vars_root = vars_root->next;
    }

    log_debug ("%s: wrote %d variables into the var-index buffer\n", __func__, vars_count);

    // vars index count/size prefix
    buffer_write (buffer, buffer_size, &buffer_offset_start, &vars_count, 4);
    buffer_write (buffer, buffer_size, &buffer_offset_start, &index_size, 8);

    buffer_offset_start = *buffer_offset; // save to write the attrs_count/size
    attrs_index_start = buffer_offset_start + index_start;
    index_size = 0;

    *buffer_offset += (4 + 8); // save space for count and size

    while (attrs_root)
    {
        uint8_t flag;
        uint16_t len;
        uint32_t attr_size = 0;
        uint64_t attr_start = *buffer_offset;
        int i;

        attrs_count++;

        *buffer_offset += 4; // save space for attr length

        buffer_write (buffer, buffer_size, buffer_offset, &attrs_root->id, 4);
        index_size += 4;
        attr_size += 4;

        len = strlen (attrs_root->group_name);
        buffer_write (buffer, buffer_size, buffer_offset, &len, 2);
        index_size += 2;
        attr_size += 2;
        buffer_write (buffer, buffer_size, buffer_offset
                ,attrs_root->group_name, len
                );
        index_size += len;
        attr_size += len;

        len = strlen (attrs_root->attr_name);
        buffer_write (buffer, buffer_size, buffer_offset, &len, 2);
        index_size += 2;
        attr_size += 2;
        buffer_write (buffer, buffer_size, buffer_offset
                ,attrs_root->attr_name, len
                );
        index_size += len;
        attr_size += len;

        len = strlen (attrs_root->attr_path);
        buffer_write (buffer, buffer_size, buffer_offset, &len, 2);
        index_size += 2;
        attr_size += 2;
        buffer_write (buffer, buffer_size, buffer_offset
                ,attrs_root->attr_path, len
                );
        index_size += len;
        attr_size += len;

        flag = attrs_root->type;
        buffer_write (buffer, buffer_size, buffer_offset, &flag, 1);
        index_size += 1;
        attr_size += 1;

        buffer_write (buffer, buffer_size, buffer_offset
                ,&attrs_root->characteristics_count, 8
                );
        index_size += 8;
        attr_size += 8;

        for (i = 0; i < attrs_root->characteristics_count; i++)
        {
            uint64_t size;
            uint8_t characteristic_set_count = 0;
            uint32_t characteristic_set_length = 0;

            uint64_t characteristic_set_start = *buffer_offset;
            *buffer_offset += 1 + 4; // save space for characteristic count/len
            index_size += 1 + 4;
            attr_size += 1 + 4;

            // add an offset characteristic for all attrs
            characteristic_set_count++;
            flag = (uint8_t) adios_characteristic_offset;
            buffer_write (buffer, buffer_size, buffer_offset, &flag, 1);
            index_size += 1;
            attr_size += 1;
            characteristic_set_length += 1;

            buffer_write (buffer, buffer_size, buffer_offset
                    ,&attrs_root->characteristics [i].offset, 8
                    );
            index_size += 8;
            attr_size += 8;
            characteristic_set_length += 8;

            // add a payload offset characteristic for all attrs
            characteristic_set_count++;
            flag = (uint8_t) adios_characteristic_payload_offset;
            buffer_write (buffer, buffer_size, buffer_offset, &flag, 1);
            index_size += 1;
            attr_size += 1;
            characteristic_set_length += 1;

            buffer_write (buffer, buffer_size, buffer_offset
                    ,&attrs_root->characteristics [i].payload_offset, 8
                    );
            index_size += 8;
            attr_size += 8;
            characteristic_set_length += 8;

            // add a file index characteristic for all attrs
            characteristic_set_count++;
            flag = (uint8_t) adios_characteristic_file_index;
            buffer_write (buffer, buffer_size, buffer_offset, &flag, 1);
            index_size += 1;
            attr_size += 1;
            characteristic_set_length += 1;

            buffer_write (buffer, buffer_size, buffer_offset
                    ,&attrs_root->characteristics [i].file_index, 4
                    );
            index_size += 4;
            attr_size += 4;
            characteristic_set_length += 4;

            // add a time index characteristic for all attrs
            characteristic_set_count++;
            flag = (uint8_t) adios_characteristic_time_index;
            buffer_write (buffer, buffer_size, buffer_offset, &flag, 1);
            index_size += 1;
            attr_size += 1;
            characteristic_set_length += 1;

            buffer_write (buffer, buffer_size, buffer_offset
                    ,&attrs_root->characteristics [i].time_index, 4
                    );
            index_size += 4;
            attr_size += 4;
            characteristic_set_length += 4;

            size = adios_get_type_size (attrs_root->type
                    ,attrs_root->characteristics [i].value
                    );

            if (attrs_root->characteristics [i].value != 0)
            {
                // add a value characteristic
                characteristic_set_count++;
                flag = (uint8_t) adios_characteristic_value;
                buffer_write (buffer, buffer_size, buffer_offset
                        ,&flag, 1
                        );
                index_size += 1;
                attr_size += 1;
                characteristic_set_length += 1;
                if (attrs_root->type == adios_string)
                {
                    uint16_t len = (uint16_t) size;
                    buffer_write (buffer, buffer_size, buffer_offset
                            ,&len, 2
                            );
                    index_size += 2;
                    attr_size += 2;
                    characteristic_set_length += 2;
                }
                buffer_write (buffer, buffer_size, buffer_offset
                        ,attrs_root->characteristics [i].value, size
                        );
                index_size += size;
                attr_size += size;
                characteristic_set_length += size;
            }
            if (attrs_root->characteristics [i].var_id != 0)
            {
                // add a var id characteristic
                characteristic_set_count++;
                flag = (uint8_t) adios_characteristic_var_id;
                buffer_write (buffer, buffer_size, buffer_offset
                        ,&flag, 1
                        );
                index_size += 1;
                attr_size += 1;
                characteristic_set_length += 1;
                buffer_write (buffer, buffer_size, buffer_offset
                        ,&attrs_root->characteristics [i].var_id, 4
                        );
                index_size += 4;
                attr_size += 4;
                characteristic_set_length += 4;
            }

            // characteristics count/size prefix
            buffer_write (buffer, buffer_size, &characteristic_set_start
                    ,&characteristic_set_count, 1
                    );
            buffer_write (buffer, buffer_size, &characteristic_set_start
                    ,&characteristic_set_length, 4
                    );
        }

        buffer_write (buffer, buffer_size, &attr_start, &attr_size, 4);

        attrs_root = attrs_root->next;
    }

    // attrs index count/size prefix
    buffer_write (buffer, buffer_size, &buffer_offset_start, &attrs_count, 4);
    buffer_write (buffer, buffer_size, &buffer_offset_start, &index_size, 8);


    /* Since ADIOS 1.4 Write new information before the last 24+4 bytes into the footer
       New information's format
       24 characters: ADIOS-BP v<version>, padded with spaces up to 24
       1 byte: major version
       1 byte: minor version
       1 byte: micro version
       1 byte: 0
       */
    {
        char verstr[25] = "                    ";
        unsigned char ver;
        snprintf (verstr, 25, "ADIOS-BP v%-14.14s", VERSION);
        buffer_write (buffer, buffer_size, buffer_offset, verstr, 24);
        ver = VERSION_MAJOR;
        buffer_write (buffer, buffer_size, buffer_offset, &ver, 1);
        ver = VERSION_MINOR;
        buffer_write (buffer, buffer_size, buffer_offset, &ver, 1);
        ver = VERSION_MICRO;
        buffer_write (buffer, buffer_size, buffer_offset, &ver, 1);
        ver = 0;
        buffer_write (buffer, buffer_size, buffer_offset, &ver, 1);
    }

    // location of the beginning of the indexes (first proc groups then vars)
    buffer_write (buffer, buffer_size, buffer_offset, &pg_index_start, 8);
    buffer_write (buffer, buffer_size, buffer_offset, &vars_index_start, 8);
    buffer_write (buffer, buffer_size, buffer_offset, &attrs_index_start, 8);

    return 0;
}

int adios_write_version_v1 (char ** buffer
        ,uint64_t * buffer_size
        ,uint64_t * buffer_offset
        )
{
    uint32_t test = 1;

    if (!*(char *) &test)
        test = 0x80000000;
    else
        test = 0;

    // version number 1 byte, endiness 1 byte,
    // the rest is user-defined options 2 bytes
    test += ADIOS_VERSION_BP_FORMAT;   // file format version
    // For the new read API to be able to read back older file format,
    // set this flag
    test |= ADIOS_VERSION_HAVE_TIME_INDEX_CHARACTERISTIC;

    test = htonl (test);

    buffer_write (buffer, buffer_size, buffer_offset, &test, 4);

    return 0;
}

int adios_write_version_flag_v1 (char ** buffer
        ,uint64_t * buffer_size
        ,uint64_t * buffer_offset
        ,uint32_t flag
        )
{
    uint32_t test = 1;

    if (!*(char *) &test)
        test = 0x80000000;
    else
        test = 0;

    // version number 1 byte, endiness 1 byte,
    // the rest is user-defined options 2 bytes
    test += ADIOS_VERSION_BP_FORMAT;   // file format version
    // For the new read API to be able to read back older file format,
    // set this flag
    test |= ADIOS_VERSION_HAVE_TIME_INDEX_CHARACTERISTIC | flag;

    test = htonl (test);

    buffer_write (buffer, buffer_size, buffer_offset, &test, 4);

    return 0;
}

static uint16_t calc_dimension_size (struct adios_dimension_struct * dimension)
{
    uint16_t size = 0;

    size += 1; // var (y or n)

    if (    dimension->dimension.var == NULL
         && dimension->dimension.attr == NULL
         && dimension->dimension.time_index == adios_flag_no
       )  // it is a number
    {
        size += 8;  // size of value
    }
    else   // it is a var
    {
        size += 4;  // size of var ID
    }

    size += 1; // var (y or n)

    if (    dimension->global_dimension.var == NULL
         && dimension->global_dimension.attr == NULL
         && dimension->global_dimension.time_index == adios_flag_no
       )  // it is a number
    {
        size += 8; // default to a rank
    }
    else
    {
        size += 4;  // size of var ID
    }

    size += 1; // var (y or n)

    if (    dimension->local_offset.var == NULL
         && dimension->local_offset.var == NULL
         && dimension->local_offset.time_index == adios_flag_no
       )  // it is a number
    {
        size += 8;  // default to a rank
    }
    else
    {
        size += 4;  // size of var ID
    }

    return size;
}

static uint16_t calc_dimensions_size (struct adios_dimension_struct * dimension)
{
    uint16_t size = 0;

    while (dimension)
    {
        size += calc_dimension_size (dimension);

        dimension = dimension->next;
    }

    return size;
}

static
uint64_t adios_write_dimension_v1 (struct adios_file_struct * fd
        ,struct adios_dimension_struct * dimension
        )
{
    uint64_t size = 0;
    uint32_t id;
    uint8_t var;

    if (    dimension->dimension.var == NULL
         && dimension->dimension.attr == NULL
         && dimension->dimension.time_index == adios_flag_no
       )
    {
        var = 'n';
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &var, 1);
        size += 1;
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                ,&dimension->dimension.rank, 8
                );
        size += 8;
    }
    else
    {
        if (dimension->dimension.var != NULL)
            id = dimension->dimension.var->id;
        else if (dimension->dimension.attr != NULL)
            id = dimension->dimension.attr->id;
        else
            id = 0; // just write this garbage
        var = 'y';
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &var, 1);
        size += 1;
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &id, 4);
        size += 4;
    }

    if (    dimension->global_dimension.var == NULL
         && dimension->global_dimension.attr == NULL
         && dimension->global_dimension.time_index == adios_flag_no
       )
    {
        var = 'n';
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &var, 1);
        size += 1;
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                ,&dimension->global_dimension.rank, 8
                );
        size += 8;
    }
    else
    {
        if (dimension->global_dimension.var != NULL)
            id = dimension->global_dimension.var->id;
        else if (dimension->global_dimension.attr != NULL)
            id = dimension->global_dimension.attr->id;
        else
            id = 0; // just write this garbage
        var = 'y';
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &var, 1);
        size += 1;
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &id, 4);
        size += 4;
    }

    if (    dimension->local_offset.var == NULL
         && dimension->local_offset.attr == NULL
         && dimension->local_offset.time_index == adios_flag_no
       )
    {
        var = 'n';
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &var, 1);
        size += 1;
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                ,&dimension->local_offset.rank, 8
                );
        size += 8;
    }
    else
    {
        if (dimension->local_offset.var != NULL)
            id = dimension->local_offset.var->id;
        else if (dimension->local_offset.attr != NULL)
            id = dimension->local_offset.attr->id;
        else
            id = 0; // just write this garbage
        var = 'y';
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &var, 1);
        size += 1;
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &id, 4);
        size += 4;
    }

    return size;
}

static
uint16_t adios_write_dimensions_v1 (struct adios_file_struct * fd
        ,struct adios_dimension_struct * dimensions
        )
{
    uint16_t size = 0;
    uint16_t dimensions_size = calc_dimensions_size (dimensions);
    uint8_t ranks = count_dimensions (dimensions);

    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &ranks, 1);
    size += 1;
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &dimensions_size, 2);
    size += 2;

    while (dimensions)
    {
        size += adios_write_dimension_v1 (fd, dimensions);

        dimensions = dimensions->next;
    }

    return size;
}

uint16_t adios_write_var_characteristics_dims_v1 (struct adios_file_struct * fd
        ,struct adios_var_struct * v
        )
{
    uint16_t total_size = 0;
    uint8_t dims_count = 0;
    uint16_t dims_length = 0;
    struct adios_dimension_struct * d = v->dimensions;
    uint64_t count_offset = fd->offset;

    fd->offset += 1;
    total_size += 1; // count

    fd->offset += 2;
    total_size += 2; // length

    while (d)
    {
        uint64_t dim = 0;

        dims_count++;

        dim = adios_get_dim_value (&d->dimension);
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &dim, 8);
        total_size += 8;
        dims_length += 8;

        dim = adios_get_dim_value (&d->global_dimension);
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &dim, 8);
        total_size += 8;
        dims_length += 8;

        dim = adios_get_dim_value (&d->local_offset);
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &dim, 8);
        total_size += 8;
        dims_length += 8;

        d = d->next;
    }

    buffer_write (&fd->buffer, &fd->buffer_size, &count_offset, &dims_count, 1);
    buffer_write (&fd->buffer, &fd->buffer_size, &count_offset, &dims_length, 2);

    return total_size;
}

uint16_t adios_write_var_characteristics_v1 (struct adios_file_struct * fd
        ,struct adios_var_struct * v
        )
{
    uint8_t flag;
    uint16_t len;
    uint8_t characteristic_set_count = 0;
    uint32_t characteristic_set_length = 0;
    uint64_t index_size = 0;

    uint64_t characteristic_set_start = fd->offset;
    fd->offset += 1 + 4; // save space for characteristic count/len
    index_size += 1 + 4;

    // depending on if it is an array or not, generate a different
    // additional set of characteristics

    switch (v->type)
    {
        case adios_byte:
        case adios_unsigned_byte:
        case adios_short:
        case adios_unsigned_short:
        case adios_integer:
        case adios_unsigned_integer:
        case adios_long:
        case adios_unsigned_long:
        case adios_real:
        case adios_double:
        case adios_long_double:
        case adios_complex:
        case adios_double_complex:
            if (v->dimensions)
            {
                // add a dimensions characteristic
                characteristic_set_count++;
                flag = (uint8_t) adios_characteristic_dimensions;
                buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                        ,&flag, 1
                        );
                index_size += 1;
                characteristic_set_length += 1;

                len = adios_write_var_characteristics_dims_v1 (fd, v);
                index_size += len;
                characteristic_set_length += len;

                // NCSU ALACRITY-ADIOS - Write transform metadata
                // Transform has to be written before stats, because the
                // original datatype is needed to determine the amount of data
                // that needs to be read. This is not a problem during writes,
                // but during reads.
                uint64_t char_write_length = 0;
                uint8_t char_write_count = 0;

                char_write_count = adios_transform_serialize_transform_var(
                    v, &char_write_length, &fd->buffer, &fd->buffer_size, &fd->offset);

                characteristic_set_count += char_write_count;
                index_size += char_write_length;
                characteristic_set_length += char_write_length;

                // NCSU - add the bitmap of characteristics
                characteristic_set_count++;
                flag = (uint8_t) adios_characteristic_bitmap;
                buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                        ,&flag, 1
                        );
                index_size += 1;
                characteristic_set_length += 1;

                buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                        ,&v->bitmap, 4
                        );
                index_size += 4;
                characteristic_set_length += 4;

                // NCSU - add a stat value characteristic
                characteristic_set_count++;
                flag = (uint8_t) adios_characteristic_stat;
                buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                        ,&flag, 1
                        );
                index_size += 1;
                characteristic_set_length += 1;

                uint8_t j, c;
                enum ADIOS_DATATYPES original_var_type = adios_transform_get_var_original_type_var(v);
                uint8_t count = adios_get_stat_set_count(original_var_type);
                uint8_t idx = 0;
                uint64_t characteristic_size;

                for (c = 0; c < count; c ++)
                {
                    j = idx = 0;
                    while (v->bitmap >> j)
                    {
                        if ((v->bitmap >> j) & 1)
                        {
                            if (j == adios_statistic_hist)
                            {
                                struct adios_hist_struct * hist = v->stats[c][idx].data;
                                int32_t num_breaks = hist->num_breaks;
                                // Adding number of bins
                                buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                                        ,&hist->num_breaks, 4
                                        );
                                characteristic_size = 4;

                                // Adding min bin
                                buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                                        ,&hist->min, 8
                                        );
                                characteristic_size += 8;

                                // Adding max bin
                                buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                                        ,&hist->max, 8
                                        );
                                characteristic_size += 8;

                                // add a frequencies value characteristic
                                buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                                        ,hist->frequencies, 4 * (num_breaks + 1)
                                        );
                                characteristic_size += 4 * (num_breaks + 1);

                                // add the breaks value characteristic
                                buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                                        ,hist->breaks, 8 * num_breaks
                                        );
                                characteristic_size += 8 * num_breaks;
                            }
                            else
                            {
                                characteristic_size = adios_get_stat_size(v->stats[c][idx].data, original_var_type, j);

                                buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                                        ,v->stats[c][idx].data, characteristic_size
                                        );
                            }

                            index_size += characteristic_size;
                            characteristic_set_length += characteristic_size;
                            idx ++;
                        }
                        j ++;
                    }
                }


                /*
                characteristic_set_count++;
                flag = (uint8_t) adios_characteristic_transform_type;
                buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                             ,&flag, 1
                             );
                index_size += 1;
                characteristic_set_length += 1;

                buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                             ,&v->transform_type, 1
                             );
                index_size += 1;
                characteristic_set_length += 1;
                */
            }
            break;

        case adios_string:
            break;
        default:
            break;
    }
    // characteristics count/size prefix
    buffer_write (&fd->buffer, &fd->buffer_size, &characteristic_set_start
            ,&characteristic_set_count, 1
            );
    buffer_write (&fd->buffer, &fd->buffer_size, &characteristic_set_start
            ,&characteristic_set_length, 4
            );


    return index_size;
}

int adios_generate_var_characteristics_v1 (struct adios_file_struct * fd, struct adios_var_struct * var)
{
    uint64_t total_size = 0;
    uint64_t size = 0;
    enum ADIOS_DATATYPES original_var_type = adios_transform_get_var_original_type_var(var);

    if (var->transform_type != adios_transform_none) {
        total_size = adios_transform_get_pre_transform_var_size (var);
    } else {
        total_size = adios_get_var_size (var, var->data);
    }

    if (var->bitmap == 0)
        return 0;

    int32_t map[32];
    memset (map, -1, sizeof(map));

#if 1
#define HIST(a) \
    { \
        int low, high, mid; \
        low=0; \
        high=hist->num_breaks - 1; \
        if (hist->breaks[low] > a) \
        hist->frequencies[0] += 1; \
        else if (a >= hist->breaks[high]) \
        hist->frequencies[high + 1] += 1; \
        else if (hist->breaks[low] <= a && a < hist->breaks[high]) \
        { \
            while(high-low>=2) \
            { \
                mid=(high+low)/2; \
                if(a >= hist->breaks[mid]) \
                { \
                    low=mid; \
                } \
                else \
                { \
                    high=mid; \
                } \
            } \
            hist->frequencies[low + 1] += 1; \
        } \
    }
#endif

#if 1
#define ADIOS_STATISTICS(a,b) \
{\
    a * data = (a *) var->data; \
    int i, j; \
    struct adios_stat_struct * stats = var->stats[0]; \
    a * min, * max; \
    double * sum, * sum_square; \
    uint32_t * cnt; \
    struct adios_hist_struct * hist = 0; \
    i = j = 0; \
    while (var->bitmap >> j) { \
        if ((var->bitmap >> j) & 1)    {\
            map [j] = i; \
            if (j == adios_statistic_hist) \
            ;\
            else \
            stats[i].data = malloc(adios_get_stat_size(NULL, original_var_type, j)); \
            i ++; \
        } \
        j ++; \
    } \
        min = (a *) stats[map[adios_statistic_min]].data; \
        max = (a *) stats[map[adios_statistic_max]].data; \
        sum = (double *) stats[map[adios_statistic_sum]].data; \
        sum_square = (double *) stats[map[adios_statistic_sum_square]].data; \
        cnt = (uint32_t *) stats[map[adios_statistic_cnt]].data; \
        *cnt = 0;\
        if (map[adios_statistic_hist] != -1) {\
            hist = (struct adios_hist_struct *) stats[map[adios_statistic_hist]].data; \
            hist->frequencies = calloc ((hist->num_breaks + 1), adios_get_type_size(adios_unsigned_integer, "")); \
        } \
        int finite = 0; \
        size = 0; \
        while ((size * b) < total_size) \
        { \
            if (isnan (data [size]) || !isfinite (data [size])) {\
                size ++; \
                continue; \
            }\
            if (!finite) { \
                *min = data [size]; \
                *max = data [size]; \
                *sum = data [size]; \
                *sum_square = (data [size] * data [size]) ; \
                *cnt = *cnt + 1; \
                if (map[adios_statistic_hist] != -1) \
                HIST(data [size]); \
                finite = 1; \
                size ++; \
                continue; \
            } \
            if (data [size] < *min) \
            *min = data [size]; \
            if (data [size] > *max) \
            *max = data [size]; \
            *sum += data [size]; \
            *sum_square += (data [size] * data [size]) ; \
            *cnt = *cnt + 1; \
            if (map[adios_statistic_hist] != -1) \
            HIST(data [size]); \
            size++; \
        } \
        if (map[adios_statistic_finite] != -1) \
        * ((uint8_t * ) stats[map[adios_statistic_finite]].data) = finite; \
        return 0; \
    }
#else
#define MIN_MAX(a,b)\
    {\
        a * data = (a *) var->data; \
        var->min = malloc (b); \
        var->max = malloc (b); \
        a * min = (a *) var->min; \
        a * max = (a *) var->max; \
        *min = data [0]; \
        *max = data [0]; \
        return 0; \
    }
#endif

    switch (original_var_type)
    {
        case adios_byte:
            ADIOS_STATISTICS(int8_t,1)

        case adios_unsigned_byte:
                ADIOS_STATISTICS(uint8_t,1)

        case adios_short:
                    ADIOS_STATISTICS(int16_t,2)

        case adios_unsigned_short:
                        ADIOS_STATISTICS(uint16_t,2)

        case adios_integer:
                            ADIOS_STATISTICS(int32_t,4)

        case adios_unsigned_integer:
                                ADIOS_STATISTICS(uint32_t,4)

        case adios_long:
                                    ADIOS_STATISTICS(int64_t,8)

        case adios_unsigned_long:
                                        ADIOS_STATISTICS(uint64_t,8)

        case adios_real:
                                            ADIOS_STATISTICS(float,4)

        case adios_double:
                                                ADIOS_STATISTICS(double,8)

        case adios_long_double:
                                                    ADIOS_STATISTICS(long double,16)

        case adios_complex:
        {
            int i, j, c, count = 3;
            struct adios_stat_struct ** stats = var->stats;
            float * data = var->data;
            i = j = 0;

            while (var->bitmap >> j) {
                if ((var->bitmap >> j) & 1)    {
                    map [j] = i;
                    for (c = 0; c < count; c ++)
                        if (j != adios_statistic_hist)
                            stats[c][i].data = malloc(adios_get_stat_size(NULL, original_var_type, j));
                    i ++;
                }
                j ++;
            }

            double *min_i, *min_r, *min_m;
            double *max_i, *max_r, *max_m;
            double *sum_i, *sum_r, *sum_m;
            double *sum_square_i, *sum_square_r, *sum_square_m;
            //struct adios_hist_struct *hist, *hist_i, *hist_r, *hist_m;
            uint32_t *cnt_i, *cnt_r, *cnt_m;
            double magnitude;
            uint8_t finite = 0;

            min_m = (double *) stats[0][map[adios_statistic_min]].data;
            min_r = (double *) stats[1][map[adios_statistic_min]].data;
            min_i = (double *) stats[2][map[adios_statistic_min]].data;

            max_m = (double *) stats[0][map[adios_statistic_max]].data;
            max_r = (double *) stats[1][map[adios_statistic_max]].data;
            max_i = (double *) stats[2][map[adios_statistic_max]].data;

            sum_m = (double *) stats[0][map[adios_statistic_sum]].data;
            sum_r = (double *) stats[1][map[adios_statistic_sum]].data;
            sum_i = (double *) stats[2][map[adios_statistic_sum]].data;

            sum_square_m = (double *) stats[0][map[adios_statistic_sum_square]].data;
            sum_square_r = (double *) stats[1][map[adios_statistic_sum_square]].data;
            sum_square_i = (double *) stats[2][map[adios_statistic_sum_square]].data;

            cnt_m = (uint32_t *) stats[0][map[adios_statistic_cnt]].data;
            cnt_r = (uint32_t *) stats[1][map[adios_statistic_cnt]].data;
            cnt_i = (uint32_t *) stats[2][map[adios_statistic_cnt]].data;

            // Histogram is not available for complex numbers, yet.
            /*
            if (map[adios_statistic_hist] != -1) {
                hist_r = (struct adios_hist_struct *) stat[0][map[adios_statistic_hist]].data;
                hist_i = (struct adios_hist_struct *) stat[1][map[adios_statistic_hist]].data;
                hist_m = (struct adios_hist_struct *) stat[2][map[adios_statistic_hist]].data;

                hist_r->frequencies = calloc ((hist->num_breaks + 1), adios_get_type_size(adios_unsigned_integer, ""));
                hist_i->frequencies = calloc ((hist->num_breaks + 1), adios_get_type_size(adios_unsigned_integer, ""));
                hist_m->frequencies = calloc ((hist->num_breaks + 1), adios_get_type_size(adios_unsigned_integer, ""));
            }
            */

            *cnt_r = *cnt_i = *cnt_m = 0;
            *min_r = *min_i = *min_m = HUGE_VAL;
            *max_r = *max_i = *max_m = -HUGE_VAL;
            *sum_r = *sum_i = *sum_m = 0;
            *sum_square_r = *sum_square_i = *sum_square_m = 0;

            while ((size * sizeof(float)) < total_size) {

                magnitude = sqrt((double) data [size] * data [size] + (double) data[size + 1] * data[size + 1]);

                // Both real and imaginary parts have to be finite, else skip calculating the characteristic
                if ( isnan(data [size]) || !isfinite(data [size]) || isnan(data[size + 1]) || !isfinite(data[size + 1]) ) {
                    size += 2;
                    continue;
                }

                finite = 1;

                // Updating the characteristic values
                if (data [size] < *min_r)
                    *min_r = data [size];
                if (data [size + 1] < *min_i)
                    *min_i = data [size + 1];
                if (magnitude < *min_m)
                    *min_m = magnitude;

                if (data [size] > *max_r)
                    *max_r = data [size];
                if (data [size + 1] > *max_i)
                    *max_i = data [size + 1];
                if (magnitude > *max_m)
                    *max_m = magnitude;

                *sum_r += data [size];
                *sum_i += data [size + 1];
                *sum_m += magnitude;

                *sum_square_r += (double) data [size] * data [size];
                *sum_square_i += (double) data [size + 1] * data [size + 1];
                *sum_square_m += magnitude * magnitude;

                *cnt_r = *cnt_r + 1;
                *cnt_i = *cnt_i + 1;
                *cnt_m = *cnt_m + 1;
                // Histogram not available yet
                /*
                if (map[adios_statistic_hist] != -1)
                {
                    hist = hist_r;
                    HIST (data[size]);

                    hist = hist_i;
                    HIST (data[size + 1]);

                    hist = hist_m;
                    HIST (magnitude);
                }
                */

                   size += 2;
            }

            if (map[adios_statistic_finite] != -1)
                for (c = 0; c < count; c ++)
                    * ((uint8_t * ) stats[c][map[adios_statistic_finite]].data) = finite;

            return 0;
        }

        case adios_double_complex:
        {
            int i, j, c, count = 3;
            struct adios_stat_struct ** stats = var->stats;
            double * data = var->data;
            i = j = 0;

            while (var->bitmap >> j) {
                if ((var->bitmap >> j) & 1)    {
                    map [j] = i;
                    for (c = 0; c < count; c ++)
                        if (j != adios_statistic_hist)
                            stats[c][i].data = malloc(adios_get_stat_size(NULL, original_var_type, j));
                    i ++;
                }
                j ++;
            }

            long double *min_i, *min_r, *min_m;
            long double *max_i, *max_r, *max_m;
            long double *sum_i, *sum_r, *sum_m;
            long double *sum_square_i, *sum_square_r, *sum_square_m;
            //struct adios_hist_struct *hist, *hist_i, *hist_r, *hist_m;
            uint32_t *cnt_i, *cnt_r, *cnt_m;
            long double magnitude;
            uint8_t finite = 0;

            min_m = (long double *) stats[0][map[adios_statistic_min]].data;
            min_r = (long double *) stats[1][map[adios_statistic_min]].data;
            min_i = (long double *) stats[2][map[adios_statistic_min]].data;

            max_m = (long double *) stats[0][map[adios_statistic_max]].data;
            max_r = (long double *) stats[1][map[adios_statistic_max]].data;
            max_i = (long double *) stats[2][map[adios_statistic_max]].data;

            sum_m = (long double *) stats[0][map[adios_statistic_sum]].data;
            sum_r = (long double *) stats[1][map[adios_statistic_sum]].data;
            sum_i = (long double *) stats[2][map[adios_statistic_sum]].data;

            sum_square_m = (long double *) stats[0][map[adios_statistic_sum_square]].data;
            sum_square_r = (long double *) stats[1][map[adios_statistic_sum_square]].data;
            sum_square_i = (long double *) stats[2][map[adios_statistic_sum_square]].data;


            // Histogram not available for complex numbers yet
            /*
            if (map[adios_statistic_hist] != -1) {
                hist_r = (struct adios_hist_struct *) stat[0][map[adios_statistic_hist]].data;
                hist_i = (struct adios_hist_struct *) stat[1][map[adios_statistic_hist]].data;
                hist_m = (struct adios_hist_struct *) stat[2][map[adios_statistic_hist]].data;

                hist_r->frequencies = calloc ((hist->num_breaks + 1), adios_get_type_size(adios_unsigned_integer, ""));
                hist_i->frequencies = calloc ((hist->num_breaks + 1), adios_get_type_size(adios_unsigned_integer, ""));
                hist_m->frequencies = calloc ((hist->num_breaks + 1), adios_get_type_size(adios_unsigned_integer, ""));
            }
            */

            cnt_m = (uint32_t *) stats[0][map[adios_statistic_cnt]].data;
            cnt_r = (uint32_t *) stats[1][map[adios_statistic_cnt]].data;
            cnt_i = (uint32_t *) stats[2][map[adios_statistic_cnt]].data;

            *cnt_r = *cnt_i = *cnt_m = 0;
            *min_r = *min_i = *min_m = HUGE_VAL;
            *max_r = *max_i = *max_m = -HUGE_VAL;
            *sum_r = *sum_i = *sum_m = 0;
            *sum_square_r = *sum_square_i = *sum_square_m = 0;

            while ((size * sizeof(double)) < total_size)
            {
                magnitude = sqrt((long double) data [size] * data [size] + 
                                 (long double) data[size + 1] * data[size + 1]);

                // Both real and imaginary parts have to be finite, else skip calculating the characteristic
                if ( isnan(data [size]) || !isfinite(data [size]) || isnan(data[size + 1]) || !isfinite(data[size + 1]) ) {
                    size += 2;
                    continue;
                }

                finite = 1;

                if (data [size] < *min_r)
                    *min_r = data [size];
                if (data [size + 1] < *min_i)
                    *min_i = data [size + 1];
                if (magnitude < *min_m)
                    *min_m = magnitude;

                if (data [size] > *max_r)
                    *max_r = data [size];
                if (data [size + 1] > *max_i)
                    *max_i = data [size + 1];
                if (magnitude > *max_m)
                    *max_m = magnitude;

                *sum_r += data [size];
                *sum_i += data [size + 1];
                *sum_m += magnitude;

                *sum_square_r += (long double) data [size] * data [size];
                *sum_square_i += (long double) data [size + 1] * data [size + 1];
                *sum_square_m += magnitude * magnitude;

                // Histgram has not available for complex yet
                /*
                if (map[adios_statistic_hist] != -1)
                {
                    hist = hist_r;
                    HIST (data[size]);

                    hist = hist_i;
                    HIST (data[size + 1]);

                    hist = hist_m;
                    HIST (magnitude);
                }
                */

                *cnt_r = *cnt_r + 1;
                *cnt_i = *cnt_i + 1;
                *cnt_m = *cnt_m + 1;

                   size += 2;
            }

            if (map[adios_statistic_finite] != -1)
                for (c = 0; c < count; c ++)
                    * ((uint8_t * ) stats[c][map[adios_statistic_finite]].data) = finite;

            return 0;
        }
        case adios_string:
        {
            var->stats = 0;

            return 0;
        }

        default:
        {
            var->stats = 0;

            return 0;
        }
    }
}

// data is only there for sizing
uint64_t adios_write_var_header_v1 (struct adios_file_struct * fd
        ,struct adios_var_struct * v
        )
{
    uint64_t total_size = 0;
    uint8_t flag;
    uint16_t len;

    uint64_t start = fd->offset;  // save to write the size
    v->write_offset = fd->offset + fd->base_offset; // save offset in file
    fd->offset += 8;              // save space for the size
    total_size += 8;              // makes final parsing easier

    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &v->id, 4);
    total_size += 4;

    len = strlen (v->name);
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &len, 2);
    total_size += 2;

    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, v->name, len);
    total_size += len;

    len = strlen (v->path);
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &len, 2);
    total_size += 2;

    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, v->path, len);
    total_size += len;

    flag = v->type;
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &flag, 1);
    total_size += 1;

    flag = (v->is_dim == adios_flag_yes ? 'y' : 'n');
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &flag, 1);
    total_size += 1;

    total_size += adios_write_dimensions_v1 (fd, v->dimensions);

    // Generate characteristics has been moved up, before transforms are applied
    // adios_generate_var_characteristics_v1 (fd, v);
    total_size += adios_write_var_characteristics_v1 (fd, v);

    total_size += adios_get_var_size (v, v->data); // payload

    buffer_write (&fd->buffer, &fd->buffer_size, &start, &total_size, 8);

    fd->vars_written++;

    if (fd->bytes_written < fd->offset)
        fd->bytes_written = fd->offset;

    return total_size;
}

int adios_write_var_payload_v1 (struct adios_file_struct * fd
        ,struct adios_var_struct * var
        )
{
    uint64_t size;

    // write payload
    size = adios_get_var_size (var, var->data);
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, var->data, size);

    if (fd->bytes_written < fd->offset)
        fd->bytes_written = fd->offset;

    return 0;
}

int adios_write_attribute_v1 (struct adios_file_struct * fd
        ,struct adios_attribute_struct * a
        )
{
    uint64_t start;        // save the start to write the size
    uint32_t size = 0;
    uint16_t len = 0;
    uint8_t flag = 0;

    // save space for attr length
    start = fd->offset;
    a->write_offset = fd->offset + fd->base_offset; // save offset in file
    fd->offset += 4;

    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &a->id, 4);
    size += 4;

    len = strlen (a->name);
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &len, 2);
    size += 2;

    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, a->name, len);
    size += len;

    len = strlen (a->path);
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &len, 2);
    size += 2;

    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, a->path, len);
    size += len;

    flag = (a->var ? 'y' : 'n');
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &flag, 1);
    size += 1;

    if (a->var)
    {
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                ,&a->var->id, 4
                );
        size += 4;
    }
    else
    {
        flag = a->type;
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &flag, 1);
        size += 1;

        uint32_t t = adios_get_type_size (a->type, a->value);
        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset, &t, 4);
        size += 4;

        buffer_write (&fd->buffer, &fd->buffer_size, &fd->offset
                ,a->value, t
                );
        size += t;
    }

    // put in the size we have put in for this attribute
    buffer_write (&fd->buffer, &fd->buffer_size, &start, &size, 4);

    fd->vars_written++;

    if (fd->bytes_written < fd->offset)
        fd->bytes_written = fd->offset;

    return 0;
}

int adios_write_open_vars_v1 (struct adios_file_struct * fd)
{
    fd->vars_written = 0;

    // it is now setup to write the vars and then the attrs on close
    fd->vars_start = fd->offset;

    fd->offset += (4 + 8); // (count + size)

    if (fd->bytes_written < fd->offset)
        fd->bytes_written = fd->offset;

    return 0;
}

int adios_write_close_vars_v1 (struct adios_file_struct * fd)
{
    // close the var area (count and total size) and write the attributes
    uint64_t size = fd->offset - fd->vars_start;
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->vars_start, &fd->vars_written, 4);

    buffer_write (&fd->buffer, &fd->buffer_size, &fd->vars_start, &size, 8);

    return 0;
}

int adios_write_open_attributes_v1 (struct adios_file_struct * fd)
{
    fd->vars_start = fd->offset;   // save the start of attr area for size
    fd->offset += (4 + 8);         // space to write the count and size
    fd->vars_written = 0;

    if (fd->bytes_written < fd->offset)
        fd->bytes_written = fd->offset;

    return 0;
}

int adios_write_close_attributes_v1 (struct adios_file_struct * fd)
{
    // write attribute count and total size
    uint64_t size = fd->offset - fd->vars_start;
    buffer_write (&fd->buffer, &fd->buffer_size, &fd->vars_start, &fd->vars_written, 4);

    buffer_write (&fd->buffer, &fd->buffer_size, &fd->vars_start, &size, 8);

    return 0;
}

// *****************************************************************************

int adios_multiply_dimensions (uint64_t * size
        ,struct adios_var_struct * var
        ,enum ADIOS_DATATYPES type
        ,void * data
        )
{
    switch (type)
    {
        case adios_unsigned_byte:
            *size *= (*(uint8_t *) data);
            return 1;

        case adios_byte:
            *size *= (*(int8_t *) data);
            return 1;

        case adios_unsigned_short:
            *size *= (*(uint16_t *) data);
            return 1;

        case adios_short:
            *size *= (*(int16_t *) data);
            return 1;

        case adios_unsigned_integer:
            *size *= (*(uint32_t *) data);
            return 1;

        case adios_integer:
            *size *= (*(int32_t *) data);
            return 1;

        case adios_unsigned_long:
            *size *= (*(uint64_t *) data);
            return 1;

        case adios_long:
            *size *= (*(int64_t *) data);
            return 1;

        default:
            adios_error (err_invalid_var_as_dimension,
                    "Invalid datatype for array dimension on var %s: %s\n",
                    var->name,
                    adios_type_to_string_int (type));
            return 0;
    }
}

// NCSU ALACRITY-ADIOS - generalizes the dimension multiplication in adios_get_var_size
//                       to work on a dimension struct, rather than a var struct, so it
//                       can be reused to compute the size from pre_transform_dimensions
// TODO: Factor out "var", since needed because of adios_multiply_dimensions, which needs
//       it only for debugging output.
uint64_t adios_get_dimension_space_size (struct adios_var_struct *var
                                        ,struct adios_dimension_struct * d) {
    uint64_t size = 1;
    while (d)
    {
        // calculate the size for this dimension element
        if (d->dimension.var != 0)
        {
            struct adios_var_struct * dim_var = d->dimension.var;
            if (!dim_var->data)
            {
                adios_error (err_invalid_var_as_dimension,
                        "adios_get_var_size: "
                        "sizing of %s failed because "
                        "dimension component %s was "
                        "not provided\n",
                        var->name, dim_var->name);
                return 0;
            }
            else
            {
                if (!adios_multiply_dimensions (&size, var
                            ,dim_var->type
                            ,dim_var->data
                            )
                   )
                {
                    return 0;
                }
            }
        }
        else if (d->dimension.attr != NULL)
        {
            struct adios_attribute_struct * attr = d->dimension.attr;
            if (attr->var)
            {
                if (!attr->var->data)
                {
                    adios_error (err_invalid_var_as_dimension,
                            "adios_get_var_size: "
                            "sizing of %s failed because "
                            "dimension component %s was "
                            "not provided\n",
                            var->name, attr->var->name);
                    return 0;
                }
                else
                {
                    if (!adios_multiply_dimensions (&size, var
                                ,attr->var->type
                                ,attr->var->data
                                )
                       )
                    {
                        return 0;
                    }
                }
            }
            else
            {
                if (!adios_multiply_dimensions (&size, var
                            ,attr->type
                            ,attr->value
                            )
                   )
                {
                    return 0;
                }
            }
        }
        else
        {
            if (d->dimension.time_index == adios_flag_no)
            {
                size *= d->dimension.rank;
            }
            // the time index doesn't take up space...
        }

        d = d->next;
    }

    return size;
}

// NCSU ALACRITY-ADIOS: Refactored to call the above dimension space compute code
uint64_t adios_get_var_size (struct adios_var_struct * var, void * data)
{
    uint64_t size = adios_get_type_size (var->type, data);
    if (var->dimensions)
        size *= adios_get_dimension_space_size(var, var->dimensions);

    return size;
}

const char * adios_type_to_string_int (int type)
{
    switch (type)
    {
        case adios_unsigned_byte:    return "unsigned byte";
        case adios_unsigned_short:   return "unsigned short";
        case adios_unsigned_integer: return "unsigned integer";
        case adios_unsigned_long:    return "unsigned long long";

        case adios_byte:             return "byte";
        case adios_short:            return "short";
        case adios_integer:          return "integer";
        case adios_long:             return "long long";

        case adios_real:             return "real";
        case adios_double:           return "double";
        case adios_long_double:      return "long double";

        case adios_string:           return "string";
        case adios_complex:          return "complex";
        case adios_double_complex:   return "double complex";

        default:
                                     {
                                         static char buf [50];
                                         sprintf (buf, "(unknown: %d)", type);
                                         return buf;
                                     }
    }
}

const char * adios_file_mode_to_string (int mode)
{
    static char buf [50];

    switch (mode)
    {
        case adios_mode_write:  return "write";
        case adios_mode_read:   return "read";
        case adios_mode_update: return "update";
        case adios_mode_append: return "append";

        default:
                                sprintf (buf, "(unknown: %d)", mode);
    }

    return buf;
}

//////////////////////////////////////////////////////////////////////////////
// Queue management code intended for adaptive API use
//////////////////////////////////////////////////////////////////////////////
void list_init (List * list, void (* destroy) (void * data))
{
    list->size = 0;
    list->destroy = destroy;
    list->head = NULL;
    list->tail = NULL;

    return;
}

void list_destroy (List * list)
{
    void * data;

    while (list_size (list) > 0)
    {
        if (list_rem_next (list, NULL, (void **) &data) == 0 && list->destroy != NULL)
        {
            list->destroy (data);
        }
    }

    memset (list, 0, sizeof (List));

    return;
}

int list_ins_next (List * list, ListElmt * element, const void * data)
{
    ListElmt * new_element;

    if ((new_element = (ListElmt *) malloc (sizeof (ListElmt))) == NULL)
        return -1;

    new_element->data = (void *) data;

    if (element == NULL)
    {
        if  (list_size (list) == 0)
            list->tail = new_element;

        new_element->next = list->head;
        list->head = new_element;
    }
    else
    {
        if (element->next == NULL)
            list->tail = new_element;

        new_element->next = element->next;
        element->next = new_element;
    }

    list->size++;

    return 0;
}

int list_rem_next (List * list, ListElmt * element, void ** data)
{
    ListElmt * old_element;

    if (list_size (list) == 0)
        return -1;

    if (element == NULL)
    {
        *data = list->head->data;
        old_element = list->head;
        list->head = list->head->next;

        if (list_size (list) == 1)
            list->tail = NULL;
    }
    else
    {
        if (element->next == NULL)
            return -1;

        *data = element->next->data;
        old_element = element->next;
        element->next = element->next->next;

        if (element->next == NULL)
            list->tail = element;
    }

    free (old_element);

    list->size--;

    return 0;
}

int queue_enqueue (Queue * queue, const void * data)
{
    return list_ins_next (queue, list_tail (queue), data);
}

int queue_dequeue (Queue * queue, void ** data)
{
    return list_rem_next (queue, NULL, data);
}


// Functions for non-XML API fo ADIOS Schema some of which are also called from functions in adios_internals_mxml.c
int adios_common_define_schema_version (struct adios_group_struct * new_group, char * schema_version)
{
    int64_t p_new_group = (int64_t) new_group;

    if (strcasecmp (schema_version,"")){
        char * ver;// copy version
        char * d;  // dot location
        char * ptr_end;
        ver = strdup (schema_version);
        char * schema_version_major_att_nam;
        char * schema_version_minor_att_nam;
        d = strtok (ver, ".");
        int counter = 0; // counter

        while (d)
        {
            int slength = 0;
            if (!strtod (d,&ptr_end)){
                printf("Schema version invalid.\n");
                counter = 0;
                break;
            }else{
                slength = strlen("adios_schema/");
                if (counter == 0 ){
                    slength = slength + strlen("version_major") + 1;
                    schema_version_major_att_nam = malloc (slength);
                    strcpy(schema_version_major_att_nam,"adios_schema/version_major");
                    adios_common_define_attribute (p_new_group,schema_version_major_att_nam,"/",adios_string,d,"");
                }else if (counter == 1){
                    slength = slength + strlen("version_minor") + 1;
                    schema_version_minor_att_nam = malloc (slength);
                    strcpy(schema_version_minor_att_nam,"adios_schema/version_minor");
                    adios_common_define_attribute (p_new_group,schema_version_minor_att_nam,"/",adios_string,d,"");
                }
            }
            counter++;
            d = strtok (NULL, ".");
        }
        if (counter == 0){
            printf("Error: Could not detect valid schema version.\n");
        }
        free(ver);
    }
    return 0;
}


// Parse mesh time series (single file for multiple time steps or
// multiple files for time steps, basename + timeformat + extension)
int adios_common_define_mesh_timeSeriesFormat (const char * timeseries,
                                               struct adios_group_struct * new_group,
                                               const char * name
                                              )
{
    char * d1;                     // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * format_att_nam = 0;     // extension format .xxxx att name
    char * format_att_val = 0;     // extension format att value

    // We expect a number from 1-10 (max 10?)
    // The number indicates how to write the time steps. Ex: 4 ==>
    // varname.XXXX.png where XXXX is the time step padded with 0s
    // We do not fail if this is not given as variables all have nsteps
    if (!timeseries || !strcmp(timeseries,"")){
        return 1;
    }

    char * ptr_end;
    d1 = strdup (timeseries);

    double tmp_d = strtod (d1, &ptr_end);
    if (!(ptr_end && ptr_end[0]==0))
    {
        adios_conca_mesh_att_nam(&format_att_nam, name, "time-series-format");
        adios_common_define_attribute (p_new_group,format_att_nam,"/",adios_double,d1,"");
        free(format_att_val);
    }
    free (d1);
    return 1;
}

// Parse mesh time scale (real time tracking, not integers)
int adios_common_define_mesh_timeScale (const char * timescale,
                                        struct adios_group_struct * new_group,
                                        const char * name
                                       )
{
    char * c;                      // comma location
    char * d1;                     // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * gettscalefrom0 = 0;     // scale attribute xml value
    char * gettscalefrom1 = 0;     // scale attribute xml value
    char * gettscalefrom2 = 0;     // scale attribute xml value
    char * time_var_att_nam = 0;   // scale attribute name for var or num
    char * time_start_att_nam = 0; // scale attribute name for start
    char * time_stride_att_nam = 0;// scale attribute name for stride
    char * time_count_att_nam = 0; // scale attribute name for count
    char * time_max_att_nam = 0;   // scale attribute name for max
    char * time_min_att_nam = 0;   // scale attribute name for min
    char * time_var_att_val = 0;   // scale attribute value for var or num
    char * time_start_att_val = 0; // scale attribute value for start
    char * time_stride_att_val = 0;// scale attribute value for stride
    char * time_count_att_val = 0; // scale attribute value for count
    char * time_max_att_val = 0;   // scale attribute value for max
    char * time_min_att_val = 0;   // scale attribute value for min
    int counter = 0;               // used to get type of time scale bounds

    // We are going to allow
    // 1. a number =  just the number of time scale default start = 0
    // 2. start/stride/count 3 components
    // 3. min/max range where this mesh is used
    // 4. An ADIOS var = time could be a list of int stored by user

    /* We do not fail if this is not given as variables all have nsteps
       in ADIOS_inq_var = # of times the var was written
       */
    if (!timescale || !strcmp(timescale,"")){
//        printf("time-scale attribute for mesh: %s not provided.\n", name);
        return 1;
    }

    d1 = strdup (timescale);
    char * ptr_end;
    c = strtok (d1, ",");

    while (c)
    {
        struct adios_var_struct * var = 0;
        //if (adios_int_is_num (c))
        double tmp_d1;
        tmp_d1 = strtod (c,&ptr_end);
        if (!(ptr_end && ptr_end[0]==0))
        {
            var = adios_find_var_by_name (new_group, c);
            if (!var)
            {
                log_warn ("config.xml: invalid variable %s\n"
                          "for time scale of mesh: %s\n",
                          c, name);
                free (d1);

                return 0;

            }else{
                // Found variable ==> create a dims attribute for it.
                if (counter == 0){
                    gettscalefrom0 = 0;
                    gettscalefrom0 = strdup(c);
                }else if (counter == 1){
                    gettscalefrom1 = 0;
                    gettscalefrom1 = strdup(c);
                }else if (counter == 2){
                    gettscalefrom2 = 0;
                    gettscalefrom2 = strdup(c);
                }
                counter++;
            }
        }
        else
        {
            if (counter == 0){
                gettscalefrom0 = 0;
                gettscalefrom0 = strdup(c);
            }else if (counter == 1){
                gettscalefrom1 = 0;
                gettscalefrom1 = strdup(c);
            }else if (counter == 2){
                gettscalefrom2 = 0;
                gettscalefrom2 = strdup(c);
            }
            counter++;
        }

        c = strtok (NULL, ",");
    }
    if (counter == 3){
        double tmp_d2;
        time_start_att_val = strdup(gettscalefrom0);
        adios_conca_mesh_att_nam(&time_start_att_nam, name, "time-scale-start");
        // if this is string
        tmp_d2 = strtod (time_start_att_val, &ptr_end);
        if (!(ptr_end && ptr_end[0]==0))
//        if (!strtod (time_start_att_val, &ptr_end))
            adios_common_define_attribute (p_new_group,time_start_att_nam,"/",adios_string,time_start_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_start_att_nam,"/",adios_double,time_start_att_val,"");
        time_stride_att_val = strdup(gettscalefrom1);
        adios_conca_mesh_att_nam(&time_stride_att_nam, name, "time-scale-stride");
        // if this is string
        tmp_d2 = strtod (time_stride_att_val, &ptr_end);
//        if (!strtod (time_stride_att_val, &ptr_end))
        if (!(ptr_end && ptr_end[0]==0))
            adios_common_define_attribute (p_new_group,time_stride_att_nam,"/",adios_string,time_stride_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_stride_att_nam,"/",adios_double,time_stride_att_val,"");
        time_count_att_val = strdup(gettscalefrom2);
        adios_conca_mesh_att_nam(&time_count_att_nam, name, "time-scale-count");
        // if this is string
        tmp_d2 = strtod (time_count_att_val, &ptr_end);
//        if (!strtod (time_count_att_val, &ptr_end))
        if (!(ptr_end && ptr_end[0]==0))
            adios_common_define_attribute (p_new_group,time_count_att_nam,"/",adios_string,time_count_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_count_att_nam,"/",adios_double,time_count_att_val,"");
        free(time_start_att_val);
        free(time_stride_att_val);
        free(time_count_att_val);
        free(gettscalefrom2);
        free(gettscalefrom1);
        free(gettscalefrom0);
    }else if (counter == 2) {
        double tmp_d2;
        adios_conca_mesh_att_nam(&time_min_att_nam, name, "time-scale-min");
        // if this is string
        tmp_d2 = strtod (time_min_att_nam, &ptr_end);
        if (!(ptr_end && ptr_end[0]==0))
//        if (!strtod (time_min_att_val, &ptr_end))
            adios_common_define_attribute (p_new_group,time_min_att_nam,"/",adios_string,time_min_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_min_att_nam,"/",adios_double,time_min_att_val,"");
        time_max_att_val = strdup(gettscalefrom1);
        adios_conca_mesh_att_nam(&time_max_att_nam, name, "time-scale-max");
        // if this is string
        tmp_d2 = strtod (time_max_att_nam, &ptr_end);
        if (!(ptr_end && ptr_end[0]==0))
//        if (!strtod (time_max_att_val, &ptr_end))
            adios_common_define_attribute (p_new_group,time_max_att_nam,"/",adios_string,time_max_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_max_att_nam,"/",adios_double,time_max_att_val,"");
        free(time_min_att_val);
        free(time_max_att_val);
        free(gettscalefrom1);
        free(gettscalefrom0);
    } else if (counter == 1){
        double tmp_d2;
        time_var_att_val = strdup(gettscalefrom0);
        tmp_d2 = strtod (time_var_att_val, &ptr_end);
        if (!(ptr_end && ptr_end[0]==0))
//        if (!strtod (time_var_att_val, &ptr_end))
        {
            adios_conca_mesh_att_nam(&time_var_att_nam, name, "time-scale-var");
            adios_common_define_attribute (p_new_group,time_var_att_nam,"/",adios_string,time_var_att_val,"");
        }else{
            adios_conca_mesh_att_nam(&time_var_att_nam, name, "time-scale-count");
            adios_common_define_attribute (p_new_group,time_var_att_nam,"/",adios_double,time_var_att_val,"");
        }
        free(gettscalefrom0);
        free(time_var_att_val);
    }else{
        printf("Error: time format not recognized.\nPlease check documentation for time formatting.\n");
        free(d1);
        return 0;
    }

    free (d1);

    return 1;
}

int adios_common_define_mesh_timeVarying (const char * timevarying, int64_t group_id, const char * name)
{
    char * mpath = 0;
    mpath = malloc(strlen("/adios_schema/")+strlen(name)+strlen("/time-varying")+1);
    strcpy (mpath, "/adios_schema/");
    strcat (mpath, name);
    strcat (mpath, "/time-varying");
    adios_common_define_attribute (group_id, mpath, "", adios_string, timevarying, "");
    free (mpath);
    return 0; 
}

// Parse mesh time steps (integers = number of times vars are written)
int adios_common_define_mesh_timeSteps (const char * timesteps,
                                        struct adios_group_struct * new_group,
                                        const char * name
                                       )
{
    char * c;                      // comma location
    char * d1;                     // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * gettstepsfrom0 = 0;     // tstep attribute xml value
    char * gettstepsfrom1 = 0;     // tstep attribute xml value
    char * gettstepsfrom2 = 0;     // tstep attribute xml value
    char * time_var_att_nam = 0;   // tstep attribute name for var or num
    char * time_start_att_nam = 0; // tstep attribute name for start
    char * time_stride_att_nam = 0;// tstep attribute name for stride
    char * time_count_att_nam = 0; // tstep attribute name for count
    char * time_max_att_nam = 0;   // tstep attribute name for max
    char * time_min_att_nam = 0;   // tstep attribute name for min
    char * time_var_att_val = 0;   // tstep attribute value for var or num
    char * time_start_att_val = 0; // tstep attribute value for start
    char * time_stride_att_val = 0;// tstep attribute value for stride
    char * time_count_att_val = 0; // tstep attribute value for count
    char * time_max_att_val = 0;   // tstep attribute value for max
    char * time_min_att_val = 0;   // tstep attribute value for min
    int counter = 0;               // used to get type of time steps bounds

    // We are going to allow
    // 1. a number =  just the number of time steps default start = 0
    // 2. start/stride/count 3 components
    // 3. min/max range where this mesh is used
    // 4. An ADIOS var = time could be a list of int stored by user

    /* We do not fail if this is not given as variables all have nsteps
       in ADIOS_inq_var = # of times the var was written
       */
    if (!timesteps || !strcmp(timesteps,"")){
//        printf("time-steps for mesh %s attribute not provided.\n", name);
        return 1;
    }

    d1 = strdup (timesteps);

    c = strtok (d1, ",");

    while (c)
    {
        struct adios_var_struct * var = 0;
        if (adios_int_is_var (c))
        {
            var = adios_find_var_by_name (new_group, c);
            if (!var)
            {
                log_warn ("config.xml: invalid variable %s\n"
                          "for dimensions of mesh: %s\n",
                          c, name);
                free (d1);

                return 0;

            }else{
                // Found variable ==> create a dims attribute for it.
                if (counter == 0){
                    gettstepsfrom0 = 0;
                    gettstepsfrom0 = strdup(c);
                }else if (counter == 1){
                    gettstepsfrom1 = 0;
                    gettstepsfrom1 = strdup(c);
                }else if (counter == 2){
                    gettstepsfrom2 = 0;
                    gettstepsfrom2 = strdup(c);
                }
                counter++;
            }
        }
        else
        {
            if (counter == 0){
                gettstepsfrom0 = 0;
                gettstepsfrom0 = strdup(c);
            }else if (counter == 1){
                gettstepsfrom1 = 0;
                gettstepsfrom1 = strdup(c);
            }else if (counter == 2){
                gettstepsfrom2 = 0;
                gettstepsfrom2 = strdup(c);
            }
            counter++;
        }

        c = strtok (NULL, ",");
    }

    if (counter == 3){
        time_start_att_val = strdup(gettstepsfrom0);
        adios_conca_mesh_att_nam(&time_start_att_nam, name, "time-steps-start");
        // if this is string
        if (adios_int_is_var (time_start_att_val))
            adios_common_define_attribute (p_new_group,time_start_att_nam,"/",adios_string,time_start_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_start_att_nam,"/",adios_double,time_start_att_val,"");
        time_stride_att_val = strdup(gettstepsfrom1);
        adios_conca_mesh_att_nam(&time_stride_att_nam, name, "time-steps-stride");
        // if this is string
        if (adios_int_is_var (time_stride_att_val))
            adios_common_define_attribute (p_new_group,time_stride_att_nam,"/",adios_string,time_stride_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_stride_att_nam,"/",adios_double,time_stride_att_val,"");
        time_count_att_val = strdup(gettstepsfrom2);
        adios_conca_mesh_att_nam(&time_count_att_nam, name, "time-steps-count");
        // if this is string
        if (adios_int_is_var (time_count_att_val))
            adios_common_define_attribute (p_new_group,time_count_att_nam,"/",adios_string,time_count_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_count_att_nam,"/",adios_double,time_count_att_val,"");
        free(time_start_att_val);
        free(time_stride_att_val);
        free(time_count_att_val);
        free(gettstepsfrom2);
        free(gettstepsfrom1);
        free(gettstepsfrom0);
    }else if (counter == 2) {
        time_min_att_val = strdup(gettstepsfrom0);
        adios_conca_mesh_att_nam(&time_min_att_nam, name, "time-steps-min");
        // if this is string
        if (adios_int_is_var (time_min_att_val))
            adios_common_define_attribute (p_new_group,time_min_att_nam,"/",adios_string,time_min_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_min_att_nam,"/",adios_double,time_min_att_val,"");
        time_max_att_val = strdup(gettstepsfrom1);
        adios_conca_mesh_att_nam(&time_max_att_nam, name, "time-steps-max");
        // if this is string
        if (adios_int_is_var (time_max_att_val))
            adios_common_define_attribute (p_new_group,time_max_att_nam,"/",adios_string,time_max_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_max_att_nam,"/",adios_double,time_max_att_val,"");
        free(time_min_att_val);
        free(time_max_att_val);
        free(gettstepsfrom1);
        free(gettstepsfrom0);
    } else if (counter == 1){
        time_var_att_val = strdup(gettstepsfrom0);
        if (adios_int_is_var (time_var_att_val)){
            adios_conca_mesh_att_nam(&time_var_att_nam, name, "time-steps-var");
            adios_common_define_attribute (p_new_group,time_var_att_nam,"/",adios_string,time_var_att_val,"");
        }else{
            adios_conca_mesh_att_nam(&time_var_att_nam, name, "time-steps-count");
            adios_common_define_attribute (p_new_group,time_var_att_nam,"/",adios_double,time_var_att_val,"");
        }
        free(time_var_att_val);
        free(gettstepsfrom0);
    }else{
        printf("Error: time format not recognized.\nPlease check documentation for time formatting.\n");
        free(d1);
        return 0;
    }

    free (d1);

    return 1;
}

// defining a uniform mesh
int adios_common_define_mesh_uniform (char * dimensions, 
                                      char * origin, 
                                      char * spacing, 
                                      char * maximum, 
                                      char * nspace,
//                                      struct adios_group_struct * new_group,
                                      const char * name,
                                      int64_t group_id
                                     )
{   
    struct adios_group_struct * new_group = (struct adios_group_struct *) group_id;
    char * mpath = 0;
    mpath = malloc(strlen("/adios_schema/")+strlen(name)+strlen("/type")+1);
    strcpy (mpath, "/adios_schema/");
    strcat (mpath, name);
    strcat (mpath, "/type");
    adios_common_define_attribute (group_id, mpath, "", adios_string, "uniform", "");

    if (!adios_define_mesh_uniform_dimensions (dimensions, new_group, name))
        return 1;

    adios_define_mesh_uniform_origins (origin, new_group, name);
    
    adios_define_mesh_uniform_spacings (spacing, new_group, name);

    adios_define_mesh_uniform_maximums (maximum, new_group, name);

    adios_define_mesh_nspace (nspace, new_group, name);

    free (mpath);   
    return 0;
}

// defining a rectilinear mesh
int adios_common_define_mesh_rectilinear (char * dimensions, 
                                          char * coordinates,
//                                          struct adios_group_struct * new_group,
                                          char * nspace,
                                          const char * name,
                                          int64_t group_id
                                         )
{
    struct adios_group_struct * new_group = (struct adios_group_struct *) group_id;
    char * mpath = 0;
    mpath = malloc(strlen("/adios_schema/")+strlen(name)+strlen("/type")+1);
    strcpy (mpath, "/adios_schema/");
    strcat (mpath, name);
    strcat (mpath, "/type");
    adios_common_define_attribute (group_id, mpath, "", adios_string, "rectilinear", "");

    if (!adios_define_mesh_rectilinear_dimensions (dimensions, new_group, name))
        return 1;

    // Determine if it is the multi-var or single-var case
    char *p;
    // If we do not find "," in the coordinates
    if (!(p = strstr(coordinates, ",")))
    {
        if (!adios_define_mesh_rectilinear_coordinatesSingleVar (coordinates, new_group, name))
            return 1;
    }
    else
    {
        if (!adios_define_mesh_rectilinear_coordinatesMultiVar (coordinates, new_group, name))
            return 1;
    }

    adios_define_mesh_nspace (nspace, new_group, name);

    free (mpath);
    return 0;
}

// defining a structured mesh
int adios_common_define_mesh_structured (char * dimensions,
                                         char * nspace,
                                         char * points,
//                                         struct adios_group_struct * new_group,
                                         const char * name,
                                         int64_t group_id
                                        )
{
    struct adios_group_struct * new_group = (struct adios_group_struct *) group_id;
    char * mpath = 0;
    mpath = malloc(strlen("/adios_schema/")+strlen(name)+strlen("/type")+1);
    strcpy (mpath, "/adios_schema/");
    strcat (mpath, name);
    strcat (mpath, "/type");
    adios_common_define_attribute (group_id, mpath, "", adios_string, "structured", "");

    if (dimensions){
        if (!adios_define_mesh_structured_dimensions (dimensions, new_group, name))
            return 0;
    }else{
        log_warn ("config.xml: value attribute on "
                  "dimensions required (%s)\n", name);

        return 0;
    }

    if (nspace){
//        if (!adios_define_mesh_structured_nspace (nspace, new_group, name))
          if (!adios_define_mesh_nspace (nspace, new_group, name))
            return 0;
    }
    if (points){
        char *p;
        // If we do find "," in points (single-var case)
        if (!(p = strstr(points, ","))){
            if (!adios_define_mesh_structured_pointsSingleVar (points, new_group, name))
                return 0;
        }else{
            if (!adios_define_mesh_structured_pointsMultiVar (points, new_group, name))
                return 0;
        }
    }else{
        log_warn ("config.xml: value on "
                  "points required for mesh type=structured (%s)\n", name);
        return 0;
    }
    free (mpath);
    return 1;
}

// defining a unstructured mesh
int adios_common_define_mesh_unstructured (char * points,
                                           char * data, 
                                           char * count, 
                                           char * type,
                                           char * nspace,
                                           char * npoints,
                                           const char * name,
                                           int64_t group_id)
{
    struct adios_group_struct * new_group = (struct adios_group_struct *) group_id;
    char * mpath = 0;
    mpath = malloc(strlen("/adios_schema/")+strlen(name)+strlen("/type")+1);
    strcpy (mpath, "/adios_schema/");
    strcat (mpath, name);
    strcat (mpath, "/type");
    adios_common_define_attribute (group_id, mpath, "", adios_string, "unstructured", "");
    if (nspace && *nspace != 0)
    {
//        if (!adios_define_mesh_unstructured_nspace (nspace, new_group, name))
            if (!adios_define_mesh_nspace (nspace, new_group, name))
            return 0;
    }
    if (npoints && *npoints != 0)
    {
        if (!adios_define_mesh_unstructured_npoints (npoints, new_group, name))
            return 0;

    }
    if (points && *points != 0){
        char *p;
        // If we do find "," in points (single-var case)
        if (!(p = strstr(points, ","))){
            if (!adios_define_mesh_unstructured_pointsSingleVar (points, new_group, name))
                return 0;
        }else{
            if (!adios_define_mesh_unstructured_pointsMultiVar (points, new_group, name))
                return 0;
        }
    }else{
        log_warn ("config.xml: value on "
                  "points required for mesh type=structured (%s)\n", name);
        return 0;
    }
    if (!data){
        log_warn ("config.xml: data attribute on "
                  "uniform-cells required (%s)\n", name);
        return 0;
    }
    if (!count) 
    {
        log_warn ("config.xml: count attribute on "
                  "uniform-cells required (%s)\n", name);
        return 0;
    }
    if (!type)
    {
        log_warn ("config.xml: type attribute on "
                  "uniform-cells required (%s)\n", name);
        return 0;
    }
    char *pt;
    // If we do find "," in data (uniform cell case)
    if (!(pt = strstr(data, ","))){
        if ( (pt = strstr(count,",")) ){
            log_warn ("count value on uniform-cells (check data value)"
                      " should not contain ',' (%s)\n",name);
            return 0;
        }
        if ( (pt = strstr(type,",")) ){
            log_warn ("type value on uniform-cells (check data value)"
                      " should not contain ',' (%s)\n", name);
            return 0;
        }
        if (!adios_define_mesh_unstructured_uniformCells (count, data, type
                    , new_group
                    ,name
                    )
           )
            return 0;
        // Mixed cells calse
    }else{
        if (!(pt = strstr(count,","))){
            log_warn ("count value on mixed-cells (check data value)"
                      " should contain ',' (%s)\n", name);
            return 0;
        }
        if (!(pt = strstr(type,","))){
            log_warn ("type value on mixed-cells (check data value)"
                      " should contain ',' (%s)\n", name);
            return 0;
        }
        if (!adios_define_mesh_unstructured_mixedCells (count, data, type
                    , new_group, name))
            return 0;
    }
    return 1;
}

// Compose schema attributes for mesh and var
// concat numbered mesh attribute name strings
void conca_mesh_numb_att_nam(char ** returnstr, const char * meshname, char * att_nam, char counterstr[5]) {
    *returnstr = malloc (strlen("adios_schema/") + strlen(meshname) + strlen(att_nam) + strlen(counterstr) + 3);
    strcpy(*returnstr,"adios_schema");
    strcat(*returnstr,"/");
    strcat(*returnstr,meshname);
    strcat(*returnstr,"/");
    strcat(*returnstr,att_nam);
    strcat(*returnstr,counterstr);
}

// concat mesh attribute name strings
void adios_conca_mesh_att_nam(char ** returnstr, const char * meshname, char * att_nam) {
    int slength = 0;
    slength = strlen("adios_schema/");
    slength = slength + strlen(meshname);
    slength = slength + 1;
    slength = slength + 1;
    slength = slength + strlen(att_nam);

    *returnstr = malloc (slength);

    strcpy(*returnstr,"adios_schema/");
    strcat(*returnstr,meshname);
    strcat(*returnstr,"/");
    strcat(*returnstr,att_nam);
}

// concat link attribute name strings
void adios_conca_link_att_nam(char ** returnstr, const char * name, char * att_nam) {
    int slength = 0;
    slength = strlen("adios_link/")+1;
    slength = slength + strlen(name);
    slength = slength + 1;
    slength = slength + strlen(att_nam);

    *returnstr = malloc (slength);

    strcpy(*returnstr,"adios_link/");
    strcat(*returnstr,name);
    strcat(*returnstr,"/");
    strcat(*returnstr,att_nam);
}

// concat var attribute name strings
void conca_var_att_nam(char ** returnstr, const char * varname, char * att_nam) {
    int slength = 0;
    slength = strlen("adios_schema/");
    slength = slength + strlen(varname);
    slength = slength + 1;
    slength = slength + 1;
    slength = slength + strlen(att_nam);

    *returnstr = malloc (slength);

    strcpy(*returnstr,varname);
    strcat(*returnstr,"/adios_schema/");
    strcat(*returnstr,att_nam);
}

// At this point mesh structures are not really being used,
// but this function still makes sure that we don't add a
// mesh twice (same name)
// Append a mesh to a group
enum ADIOS_FLAG adios_append_mesh (struct adios_mesh_struct ** root
        ,struct adios_mesh_struct * mesh
        ,uint16_t id
        )
{
    while (root)
    {
        if (*root && !strcasecmp ((*root)->name, mesh->name))
        {
            return adios_flag_no;
        }
        if (!*root)
        {
            *root = mesh;
            root = 0;
        }
        else
        {
            root = &(*root)->next;
        }
    }

    return adios_flag_yes;
}

// Define a new mesh
struct adios_mesh_struct * adios_common_define_mesh (
        int64_t group_id, const char * name,
        enum ADIOS_FLAG time_varying,
        enum ADIOS_MESH_TYPE type)
{
    struct adios_group_struct * t = (struct adios_group_struct *) group_id;
    struct adios_mesh_struct * m = (struct adios_mesh_struct *)
        malloc (sizeof (struct adios_mesh_struct));
    enum ADIOS_FLAG flag;

    m->name = strdup (name);
    m->type = type;
    m->time_varying = time_varying;
    m->next = 0;

    flag = adios_append_mesh (&t->meshs, m, t->mesh_count);
    if (flag == adios_flag_no)
    {
        log_warn ("config.xml: unique mesh names required; "
                  "second mesh: %s will be ignored.\n", name);
        free(m);
        m = 0;
    } else {
        t->mesh_count++;
    }

    return m;
}

// Define time steps, scale and formatting
// Parse var time steps (integers = number of times vars are written)
int adios_common_define_var_timesteps (const char * timesteps,
                                       struct adios_group_struct * new_group,
                                       const char * name,
                                       const char * path
                                      )
{
    char * c;                      // comma location
    char * d1;                     // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * gettstepsfrom0 = 0;     // tstep attribute xml value
    char * gettstepsfrom1 = 0;     // tstep attribute xml value
    char * gettstepsfrom2 = 0;     // tstep attribute xml value
    char * time_var_att_nam = 0;   // tstep attribute name for var or num
    char * time_start_att_nam = 0; // tstep attribute name for start
    char * time_stride_att_nam = 0;// tstep attribute name for stride
    char * time_count_att_nam = 0; // tstep attribute name for count
    char * time_max_att_nam = 0;   // tstep attribute name for max
    char * time_min_att_nam = 0;   // tstep attribute name for min
    char * time_var_att_val = 0;   // tstep attribute value for var or num
    char * time_start_att_val = 0; // tstep attribute value for start
    char * time_stride_att_val = 0;// tstep attribute value for stride
    char * time_count_att_val = 0; // tstep attribute value for count
    char * time_max_att_val = 0;   // tstep attribute value for max
    char * time_min_att_val = 0;   // tstep attribute value for min
    int counter = 0;               // used to get type of time steps bounds

    // We are going to allow
    // 1. a number =  just the number - a multiple of mesh time step
    // 2. start/stride/count 3 components - indices of the mesh time steps
    // 3. min/max range of the mesh time step
    // 4. An ADIOS var = time could be a list of int stored by user

    /* We do not fail if this is not given as variables all have nsteps
       in ADIOS_inq_var = # of times the var was written
       */
    if (!timesteps || !strcmp(timesteps,"")){
        return 1;
    }

    d1 = strdup (timesteps);

    c = strtok (d1, ",");

    while (c)
    {
        struct adios_var_struct * var = 0;
        if (adios_int_is_var (c))
        {
            var = adios_find_var_by_name (new_group, c);
            if (!var)
            {
                log_warn ("config.xml: invalid variable %s\n"
                          "for time-steps of var: %s\n",
                          c, name);
                free (d1);

                return 0;

            }else{
                // Found variable ==> create a dims attribute for it.
                if (counter == 0){
                    gettstepsfrom0 = 0;
                    gettstepsfrom0 = strdup(c);
                }else if (counter == 1){
                    gettstepsfrom1 = 0;
                    gettstepsfrom1 = strdup(c);
                }else if (counter == 2){
                    gettstepsfrom2 = 0;
                    gettstepsfrom2 = strdup(c);
                }
                counter++;
            }
        }
        else
        {
            if (counter == 0){
                gettstepsfrom0 = 0;
                gettstepsfrom0 = strdup(c);
            }else if (counter == 1){
                gettstepsfrom1 = 0;
                gettstepsfrom1 = strdup(c);
            }else if (counter == 2){
                gettstepsfrom2 = 0;
                gettstepsfrom2 = strdup(c);
            }
            counter++;
        }

        c = strtok (NULL, ",");
    }

    if (counter == 3){
        time_start_att_val = strdup(gettstepsfrom0);
        conca_var_att_nam(&time_start_att_nam, name, "time-steps-start");
        // if this is string
        if (adios_int_is_var (time_start_att_val))
            adios_common_define_attribute (p_new_group,time_start_att_nam,path,adios_string,time_start_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_start_att_nam,path,adios_double,time_start_att_val,"");
        time_stride_att_val = strdup(gettstepsfrom1);
        conca_var_att_nam(&time_stride_att_nam, name, "time-steps-stride");
        // if this is string
        if (adios_int_is_var (time_stride_att_val))
            adios_common_define_attribute (p_new_group,time_stride_att_nam,path,adios_string,time_stride_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_stride_att_nam,path,adios_double,time_stride_att_val,"");
        time_count_att_val = strdup(gettstepsfrom2);
        conca_var_att_nam(&time_count_att_nam, name, "time-steps-count");
        // if this is string
        if (adios_int_is_var (time_count_att_val))
            adios_common_define_attribute (p_new_group,time_count_att_nam,path,adios_string,time_count_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_count_att_nam,path,adios_double,time_count_att_val,"");
        free(time_start_att_val);
        free(time_stride_att_val);
        free(time_count_att_val);
        free(gettstepsfrom2);
        free(gettstepsfrom1);
        free(gettstepsfrom0);
    }else if (counter == 2) {
        time_min_att_val = strdup(gettstepsfrom0);
        conca_var_att_nam(&time_min_att_nam, name, "time-steps-min");
        // if this is string
        if (adios_int_is_var (time_min_att_val))
            adios_common_define_attribute (p_new_group,time_min_att_nam,path,adios_string,time_min_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_min_att_nam,path,adios_double,time_min_att_val,"");
        time_max_att_val = strdup(gettstepsfrom1);
        conca_var_att_nam(&time_max_att_nam, name, "time-steps-max");
        // if this is string
        if (adios_int_is_var (time_max_att_val))
            adios_common_define_attribute (p_new_group,time_max_att_nam,"/",adios_string,time_max_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_max_att_nam,path,adios_double,time_max_att_val,"");
        free(time_min_att_val);
        free(time_max_att_val);
        free(gettstepsfrom1);
        free(gettstepsfrom0);
    } else if (counter == 1){
        time_var_att_val = strdup(gettstepsfrom0);
        if (adios_int_is_var (time_var_att_val)){
            conca_var_att_nam(&time_var_att_nam, name, "time-steps-var");
            adios_common_define_attribute (p_new_group,time_var_att_nam,path,adios_string,time_var_att_val,"");
        }else{
            conca_var_att_nam(&time_var_att_nam, name, "time-steps-count");
            adios_common_define_attribute (p_new_group,time_var_att_nam,path,adios_double,time_var_att_val,"");
        }
        free(time_var_att_val);
        free(gettstepsfrom0);
    }else{
        printf("Error: time format not recognized.\nPlease check documentation for time formatting.\n");
        free(d1);
        return 0;
    }

    free (d1);

    return 1;
}

// Parse var time series format (real time tracking, not integers)
int adios_common_define_var_timeseriesformat (const char * timeseries,
                                              struct adios_group_struct * new_group,
                                              const char * name,
                                              const char * path
                                             )
{
    char * d1;                     // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * format_att_nam = 0;     // extension format .xxxx att name
    char * format_att_val = 0;     // extension format att value

    // We expect a number from 1-10 (max 10?)
    // The number indicates how to write the time steps. Ex: 4 ==>
    // varname.XXXX.png where XXXX is the time step padded with 0s
    // We do not fail if this is not given as variables all have nsteps
    if (!timeseries || !strcmp(timeseries,"")){
        return 1;
    }

    char * ptr_end;
    d1 = strdup (timeseries);
    double tmp_d2;
    tmp_d2 = strtod (d1, &ptr_end);
//    if (strtod(d1, &ptr_end))
    if ( !(ptr_end && ptr_end[0]==0))
    {
        adios_conca_mesh_att_nam(&format_att_nam, name, "time-series-format");
        adios_common_define_attribute (p_new_group,format_att_nam,path,adios_string,d1,"");
        free(format_att_val);
    }
    free (d1);
    return 1;
}

// Parse var time scale (real time tracking, not integers)
int adios_common_define_var_timescale (const char * timescale,
                                       struct adios_group_struct * new_group,
                                       const char * name,
                                       const char * path
                                      )
{
    char * c;                      // comma location
    char * d1;                     // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * gettscalefrom0 = 0;     // scale attribute xml value
    char * gettscalefrom1 = 0;     // scale attribute xml value
    char * gettscalefrom2 = 0;     // scale attribute xml value
    char * time_var_att_nam = 0;   // scale attribute name for var or num
    char * time_start_att_nam = 0; // scale attribute name for start
    char * time_stride_att_nam = 0;// scale attribute name for stride
    char * time_count_att_nam = 0; // scale attribute name for count
    char * time_max_att_nam = 0;   // scale attribute name for max
    char * time_min_att_nam = 0;   // scale attribute name for min
    char * time_var_att_val = 0;   // scale attribute value for var or num
    char * time_start_att_val = 0; // scale attribute value for start
    char * time_stride_att_val = 0;// scale attribute value for stride
    char * time_count_att_val = 0; // scale attribute value for count
    char * time_max_att_val = 0;   // scale attribute value for max
    char * time_min_att_val = 0;   // scale attribute value for min
    int counter = 0;               // used to get type of time scale bounds

    // We are going to allow
    // 1. a number =  just the real time scale - note: not the number of time steps (this is already in adios inq var) but the correpdance between timesteps and real time scale 1 step = 15ms for example
    // 2. start/stride/count 3 components - multiple of the mesh time steps
    // 3. min/max range where this var is used - a range of the mesh time steps
    // 4. An ADIOS var = time could be a list of int stored by user


    /* We do not fail if this is not given as variables all have nsteps
       in ADIOS_inq_var = # of times the var was written
       */
    if (!timescale || !strcmp(timescale,"")){
        return 1;
    }

    d1 = strdup (timescale);

    char * ptr_end;
    c = strtok (d1, ",");

    while (c)
    {
        struct adios_var_struct * var = 0;
        //if (adios_int_is_num (c))
        double tmp_d1;
        tmp_d1 = strtod (c,&ptr_end);
        if (!(ptr_end && ptr_end[0]==0))
        {
            var = adios_find_var_by_name (new_group, c);
            if (!var)
            {
                log_warn ("config.xml: invalid variable %s\n"
                          "for attribute of var: %s\n",
                          c, name);
                free (d1);

                return 0;

            }else{
                // Found variable ==> create a dims attribute for it.
                if (counter == 0){
                    gettscalefrom0 = 0;
                    gettscalefrom0 = strdup(c);
                }else if (counter == 1){
                    gettscalefrom1 = 0;
                    gettscalefrom1 = strdup(c);
                }else if (counter == 2){
                    gettscalefrom2 = 0;
                    gettscalefrom2 = strdup(c);
                }
                counter++;
            }
        }
        else
        {
            if (counter == 0){
                gettscalefrom0 = 0;
                gettscalefrom0 = strdup(c);
            }else if (counter == 1){
                gettscalefrom1 = 0;
                gettscalefrom1 = strdup(c);
            }else if (counter == 2){
                gettscalefrom2 = 0;
                gettscalefrom2 = strdup(c);
            }
            counter++;
        }

        c = strtok (NULL, ",");
    }

    if (counter == 3){
        double tmp_d2;
        time_start_att_val = strdup(gettscalefrom0);
        conca_var_att_nam(&time_start_att_nam, name, "time-scale-start");
        tmp_d2 = strtod (time_start_att_val, &ptr_end);
        // if this is string
        if ( !(ptr_end && ptr_end[0]==0))
//        if (!strtod (time_start_att_val, &ptr_end))
            adios_common_define_attribute (p_new_group,time_start_att_nam,path,adios_string,time_start_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_start_att_nam,path,adios_double,time_start_att_val,"");
        time_stride_att_val = strdup(gettscalefrom1);
        conca_var_att_nam(&time_stride_att_nam, name, "time-scale-stride");
        // if this is string
        tmp_d2 = strtod (time_stride_att_nam, &ptr_end);
        if ( !(ptr_end && ptr_end[0]==0))
//        if (!strtod (time_stride_att_val, &ptr_end))
            adios_common_define_attribute (p_new_group,time_stride_att_nam,path,adios_string,time_stride_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_stride_att_nam,path,adios_double,time_stride_att_val,"");
        time_count_att_val = strdup(gettscalefrom2);
        conca_var_att_nam(&time_count_att_nam, name, "time-scale-count");
        // if this is string
        tmp_d2 = strtod (time_count_att_nam, &ptr_end);
        if ( !(ptr_end && ptr_end[0]==0))
//        if (!strtod (time_count_att_val, &ptr_end))
            adios_common_define_attribute (p_new_group,time_count_att_nam,path,adios_string,time_count_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_count_att_nam,path,adios_double,time_count_att_val,"");
        free(time_start_att_val);
        free(time_stride_att_val);
        free(time_count_att_val);
        free(gettscalefrom2);
        free(gettscalefrom1);
        free(gettscalefrom0);
    }else if (counter == 2) {
        double tmp_d2;
        time_min_att_val = strdup(gettscalefrom0);
        conca_var_att_nam(&time_min_att_nam, name, "time-scale-min");
        // if this is string
        tmp_d2 = strtod (time_min_att_val, &ptr_end);
        if ( !(ptr_end && ptr_end[0]==0))
//        if (!strtod (time_min_att_val, &ptr_end))
            adios_common_define_attribute (p_new_group,time_min_att_nam,path,adios_string,time_min_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_min_att_nam,path,adios_double,time_min_att_val,"");
        time_max_att_val = strdup(gettscalefrom1);
        conca_var_att_nam(&time_max_att_nam, name, "time-scale-max");
        // if this is string
        tmp_d2 = strtod (time_max_att_nam, &ptr_end);
        if ( !(ptr_end && ptr_end[0]==0))
//        if (!strtod (time_max_att_val, &ptr_end))
            adios_common_define_attribute (p_new_group,time_max_att_nam,path,adios_string,time_max_att_val,"");
        else
            adios_common_define_attribute (p_new_group,time_max_att_nam,path,adios_double,time_max_att_val,"");
        free(time_min_att_val);
        free(time_max_att_val);
        free(gettscalefrom1);
        free(gettscalefrom0);
    } else if (counter == 1){
        double tmp_d2;
        time_var_att_val = strdup(gettscalefrom0);
        tmp_d2 = strtod (time_var_att_val, &ptr_end);
        if ( !(ptr_end && ptr_end[0]==0))
//        if (!strtod (time_var_att_val, &ptr_end))
        {
            conca_var_att_nam(&time_var_att_nam, name, "time-scale-var");
            adios_common_define_attribute (p_new_group,time_var_att_nam,path,adios_string,time_var_att_val,"");
        }else{
            conca_var_att_nam(&time_var_att_nam, name, "time-scale-count");
            adios_common_define_attribute (p_new_group,time_var_att_nam,path,adios_double,time_var_att_val,"");
        }
        free(gettscalefrom0);
        free(time_var_att_val);
    }else{
        printf("Error: time format not recognized.\nPlease check documentation for time formatting.\n");
        free(d1);
        return 0;
    }

    free (d1);

    return 1;
}

// Parse var hyper slab: lines or planes from a higher dimension mesh
int adios_common_define_var_hyperslab ( const char * hyperslab,
                                        struct adios_group_struct * new_group,
                                        const char * name,
                                        const char * path)
{
    char * c;                      // comma location
    char * d1;                     // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * gethslabfrom0 = 0;       // hslab attribute xml value
    char * gethslabfrom1 = 0;       // hslab attribute xml value
    char * gethslabfrom2 = 0;       // hslab attribute xml value
    char * hslab_start_att_nam = 0; // hslab attribute name for start
    char * hslab_stride_att_nam = 0;// hslab attribute name for stride
    char * hslab_count_att_nam = 0; // hslab attribute name for count
    char * hslab_max_att_nam = 0;   // hslab attribute name for max
    char * hslab_min_att_nam = 0;   // hslab attribute name for min
    char * hslab_single_att_nam = 0;// hslab attribute name for min
    char * hslab_start_att_val = 0; // hslab attribute value for start
    char * hslab_stride_att_val = 0;// hslab attribute value for stride
    char * hslab_count_att_val = 0; // hslab attribute value for count
    char * hslab_max_att_val = 0;   // hslab attribute value for max
    char * hslab_min_att_val = 0;   // hslab attribute value for min
    char * hslab_sngl_att_val = 0;  // hslab attribute value for single value  of an array ("25, 4, 50")
                                    // use the ":" symble to indicate that there is an extra dimension to process (process all lines, all planes etc.)
    int counter = 0;                // used to get type of time hslab bounds
    // We are going to allow
    // 1. start/stride/count 3 components - indices of the mesh dimensions
    // 2. min/max range of the mesh dimensions
    // 3. single value
    //    single value of ":" means there is an extra dimension to process

    /* We do not fail if this is not given as variables all have nsteps
       in ADIOS_inq_var = # of times the var was written
       */
    if (!hyperslab || !strcmp(hyperslab,"")){
        return 1;
    }

    d1 = strdup (hyperslab);

    c = strtok (d1, ",");

    while (c)
    {
        if (counter == 0){
            gethslabfrom0 = 0;
            gethslabfrom0 = strdup(c);
        }else if (counter == 1){
            gethslabfrom1 = 0;
            gethslabfrom1 = strdup(c);
        }else if (counter == 2){
            gethslabfrom2 = 0;
            gethslabfrom2 = strdup(c);
        }
        counter++;
        c = strtok (NULL, ",");
    }
    // TODO: these should be ints only, no decimal for start, stride count, not any number should work
    if (counter == 3){
        hslab_start_att_val = strdup(gethslabfrom0);
        conca_var_att_nam(&hslab_start_att_nam, name, "start");
        adios_common_define_attribute (p_new_group,hslab_start_att_nam,path,adios_string,hslab_start_att_val,"");
        hslab_stride_att_val = strdup(gethslabfrom1);
        conca_var_att_nam(&hslab_stride_att_nam, name, "stride");
        adios_common_define_attribute (p_new_group,hslab_stride_att_nam,path,adios_string,hslab_stride_att_val,"");
        hslab_count_att_val = strdup(gethslabfrom2);
        conca_var_att_nam(&hslab_count_att_nam, name, "count");
        adios_common_define_attribute (p_new_group,hslab_count_att_nam,path,adios_string,hslab_count_att_val,"");
        free(hslab_start_att_val);
        free(hslab_stride_att_val);
        free(hslab_count_att_val);
        free(gethslabfrom2);
        free(gethslabfrom1);
        free(gethslabfrom0);
    }else if (counter == 2) {
        hslab_min_att_val = strdup(gethslabfrom0);
        conca_var_att_nam(&hslab_min_att_nam, name, "min");
        adios_common_define_attribute (p_new_group,hslab_min_att_nam,path,adios_string,hslab_min_att_val,"");
        hslab_max_att_val = strdup(gethslabfrom1);
        conca_var_att_nam(&hslab_max_att_nam, name, "max");
        adios_common_define_attribute (p_new_group,hslab_max_att_nam,path,adios_string,hslab_max_att_val,"");
        free(hslab_min_att_val);
        free(hslab_max_att_val);
        free(gethslabfrom1);
        free(gethslabfrom0);
    } else if (counter == 1){
        hslab_sngl_att_val = strdup(gethslabfrom0);
        conca_var_att_nam(&hslab_single_att_nam, name, "singleton");
        adios_common_define_attribute (p_new_group,hslab_single_att_nam,path,adios_string,hslab_sngl_att_val,"");
        free(hslab_sngl_att_val);
        free(gethslabfrom0);
    }else{
        printf("Error: hyperslab format not recognized.\nPlease check documentation for hyperslab formatting.\n");
        free(d1);
        return 0;
    }

    free (d1);

    return 1;

}

int adios_define_mesh_nspace (const char * nspace,
                              struct adios_group_struct * new_group,
                              const char * name
                             )
{
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * nsp_att_nam = 0; // nspace attribute name

    if (!nspace || !strcmp(nspace, ""))
    {
//        log_warn ("config.xml: nspace value (optional) is not provided"
//                  "for uniform mesh: %s\n", name);
        return 0;
    }
    d1 = strdup (nspace);

    adios_conca_mesh_att_nam(&nsp_att_nam, name, "nspace");
    adios_common_define_attribute (p_new_group,nsp_att_nam,"/",adios_string,nspace,"");
    free (nsp_att_nam);
    free (d1);

    return 1;
}

int adios_define_mesh_uniform_dimensions (const char * dimensions,
                                          struct adios_group_struct * new_group,
                                          const char * name
                                         )
{
    char * c;  // comma location
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * dim_att_nam = 0; // dimensions attribute name
    int counter = 0;        // used to create dimX attributes
    char counterstr[5] = {0,0,0,0,0}; // used to create dimX attributes

    if (!dimensions || !strcmp(dimensions,""))
    {
        log_warn ("config.xml: dimensions value required for "
                  "uniform mesh: %s\n", name);
        return 0;
    }

    d1 = strdup (dimensions);

    c = strtok (d1, ",");

    while (c)
    {
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        dim_att_nam = 0;
        conca_mesh_numb_att_nam(&dim_att_nam, name, "dimensions", counterstr);
        adios_common_define_attribute (p_new_group,dim_att_nam,"/",adios_string,c,"");
        free (dim_att_nam);
        counter++;
        c = strtok (NULL, ",");
    }

    char * dims = 0;
    counterstr[0] = '\0';
    snprintf(counterstr, 5, "%d", counter);
    dims = 0;
    adios_conca_mesh_att_nam(&dims, name, "dimensions-num");

    adios_common_define_attribute (p_new_group,dims,"/",adios_integer,counterstr,"");

    free (dims);

    free (d1);

    return 1;
}

int adios_define_mesh_uniform_maximums (const char * maximum,
                                        struct adios_group_struct * new_group,
                                        const char * name
                                       )
{
    char * c;  // comma location
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * max_att_nam = 0; // maxima attribute name
    int counter = 0;        // used to create maxX attributes
    char counterstr[5] = {0,0,0,0,0}; // used to create maxX attributes

    if (!maximum || !strcmp(maximum,""))
    {
//        log_warn ("config.xml: maximum value (optional) is not provided"
//                  "for uniform mesh: %s\n",
//                  name);
        return 0;
    }

    d1 = strdup (maximum);

    c = strtok (d1, ",");

    while (c)
    {
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        max_att_nam = 0;
        conca_mesh_numb_att_nam(&max_att_nam, name, "maximums", counterstr);
        adios_common_define_attribute (p_new_group,max_att_nam,"/",adios_string,c,"");
        free (max_att_nam);
        counter++;
        c = strtok (NULL, ",");
    }

    char * maxa = 0;
    counterstr[0] = '\0';
    snprintf(counterstr, 5, "%d", counter);
    maxa = 0;
    adios_conca_mesh_att_nam(&maxa, name, "maximums-num");
    adios_common_define_attribute (p_new_group,maxa,"/",adios_integer,counterstr,"");
    free (maxa);
    free (d1);

    return 1;
}

int adios_define_mesh_uniform_origins (const char * origin,
                                       struct adios_group_struct * new_group,
                                       const char * name
                                      )
{
    char * c;  // comma location
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * org_att_nam = 0; // origins attribute name
    int counter = 0;        // used to create orgX attributes
    char counterstr[5] = {0,0,0,0,0}; // used to create orgX attributes

    if (!origin || !strcmp(origin,""))
    {
//        log_warn ("config.xml: origin value (optional) not provided "
//                  "for uniform mesh: %s\n",
//                  name);
        return 0;
    }

    d1 = strdup (origin);

    c = strtok (d1, ",");

    while (c)
    {
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        org_att_nam = 0;
        conca_mesh_numb_att_nam(&org_att_nam, name, "origins", counterstr);
        adios_common_define_attribute (p_new_group,org_att_nam,"/",adios_string,c,"");
        free (org_att_nam);
        counter++;
        c = strtok (NULL, ",");
    }

    char * orgs = 0;
    counterstr[0] = '\0';
    snprintf(counterstr, 5, "%d", counter);
    orgs = 0;
    adios_conca_mesh_att_nam(&orgs, name, "origins-num");
    adios_common_define_attribute (p_new_group,orgs,"/",adios_integer,counterstr,"");

    free (orgs);
    free (d1);
    return 1;
}

int adios_define_mesh_uniform_spacings (const char * spacing,
                                        struct adios_group_struct * new_group,
                                        const char * name
                                       )
{
    char * c;  // comma location
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * spa_att_nam = 0; // spacings attribute name
    int counter = 0;        // used to create spaX attributes
    char counterstr[5] = {0,0,0,0,0}; // used to create spaX attributes if (!spacing)

    if (!spacing || !strcmp(spacing,""))
    {
//        log_warn ("config.xml: spacing value (optional) not provided "
//                  "for uniform mesh: %s\n",
//                  name);
        return 0;
    }

    d1 = strdup (spacing);

    c = strtok (d1, ",");

    while (c)
    {
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        spa_att_nam = 0;
        conca_mesh_numb_att_nam(&spa_att_nam, name, "spacings", counterstr);
        adios_common_define_attribute (p_new_group,spa_att_nam,"/",adios_string,c,"");
        free (spa_att_nam);
        counter++;
        c = strtok (NULL, ",");
    }

    char * spas = 0;
    counterstr[0] = '\0';
    snprintf(counterstr, 5, "%d", counter);
    spas = 0;
    adios_conca_mesh_att_nam(&spas, name, "spacings-num");
    adios_common_define_attribute (p_new_group,spas,"/",adios_integer,counterstr,"");
    free (spas);
    free (d1);

    return 1;
}

int adios_define_mesh_rectilinear_dimensions (const char * dimensions,
                                              struct adios_group_struct * new_group,
                                              const char * name
                                             )
{
    char * c;  // comma location
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * dim_att_nam = 0; // dimensions attribute name
    int counter = 0;        // used to create dimX attributes
    char counterstr[5] = {0,0,0,0,0}; // used to create dimX attributes

    if (!dimensions || !strcmp(dimensions,""))
    {
        log_warn ("config.xml: dimensions value required "
                  "for rectilinear mesh: %s\n", name);
        return 0;
    }

    d1 = strdup (dimensions);

    c = strtok (d1, ",");

    while (c)
    {
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        dim_att_nam = 0;
        conca_mesh_numb_att_nam(&dim_att_nam, name, "dimensions", counterstr);
        adios_common_define_attribute (p_new_group,dim_att_nam,"/",adios_string,c,"");
        free (dim_att_nam);
        counter++;
        c = strtok (NULL, ",");
    }

    char * dims = 0;
    counterstr[0] = '\0';
    snprintf(counterstr, 5, "%d", counter);
    dims = 0;
    adios_conca_mesh_att_nam(&dims, name, "dimensions-num");
    adios_common_define_attribute (p_new_group,dims,"/",adios_integer,counterstr,"");

    free (dims);
    free (d1);

    return 1;
}

int adios_define_mesh_rectilinear_coordinatesMultiVar (const char * coordinates,
                                                       struct adios_group_struct * new_group,
                                                       const char * name
                                                      )
{
    char * c;  // comma location
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * coo_att_nam = 0; // coordinates attribute name
    int counter = 0;        // used to create ptsX attributes
    char counterstr[5] = {0,0,0,0,0}; // used to create ptsX attributes

    if (!coordinates || !strcmp(coordinates,""))
    {
        log_warn ("config.xml: coordinates-multi-var value required "
                  "for rectilinear mesh: %s\n", name);

        return 0;
    }

    d1 = strdup (coordinates);

    c = strtok (d1, ",");

    while (c)
    {
        coo_att_nam = 0;
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        conca_mesh_numb_att_nam(&coo_att_nam, name, "coords-multi-var", counterstr);
        adios_common_define_attribute (p_new_group,coo_att_nam,"/",adios_string,c,"");
        free (coo_att_nam);
        counter++;
        c = strtok (NULL, ",");
    }

    // At this points, coordinates should point to at least 2 variables
    // otherwise let the user know to use the coordinates-single-var tag
    if (counter > 1) {
        char * coords = 0;
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        adios_conca_mesh_att_nam(&coords, name, "coords-multi-var-num");
        adios_common_define_attribute (p_new_group,coords,"/",adios_integer,counterstr,"");
        free (coords);
    } 
    else
    {
        log_warn ("config.xml: coordinates-multi-var expects "
                  "at least 2 variables (%s)\n", name);
        free (d1);
        return 0;
    }

    free (d1);

    return 1;
}

int adios_define_mesh_rectilinear_coordinatesSingleVar (const char * coordinates,
                                                        struct adios_group_struct * new_group,
                                                        const char * name
                                                       )
{
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * coo_att_nam = 0; // coordinates attribute name

    if (!coordinates || !strcmp(coordinates,""))
    {
        log_warn ("config.xml: coordinates-single-var value required "
                  "for rectilinear mesh: %s\n", name);

        return 0;
    }

    d1 = strdup (coordinates);
    
    adios_conca_mesh_att_nam(&coo_att_nam, name, "coords-single-var");
    adios_common_define_attribute (p_new_group,coo_att_nam,"/",adios_string,d1,"");
    free (coo_att_nam);
    free (d1);
    return 1;
}

/*
int adios_define_mesh_structured_nspace (const char * nspace
        ,struct adios_group_struct * new_group
        ,const char * name
        )
{
    char * d1; // save of strdup
    int64_t      p_new_group = (int64_t) new_group;
    char * nsp_att_nam = 0; // nspace attribute name

    if (!nspace)
    {
        log_warn ("config.xml: npsace value required "
                "for structured mesh: %s\n"
                ,name
                );

        return 0;
    }

    d1 = strdup (nspace);
    adios_conca_mesh_att_nam(&nsp_att_nam, name, "nspace");
    adios_common_define_attribute (p_new_group,nsp_att_nam,"/",adios_string,nspace,"");
    free (nsp_att_nam);
    free (d1);

    return 1;
}
*/

int adios_define_mesh_structured_dimensions (const char * dimensions,
                                             struct adios_group_struct * new_group,
                                             const char * name
                                            )
{
    char * c;  // comma location
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * dim_att_nam = 0; // dimensions attribute name
    int counter = 0;        // used to create dimX attributes
    char counterstr[5] = {0,0,0,0,0}; // used to create dimX attributes

    if (!dimensions || !strcmp(dimensions,""))
    {
        log_warn ("config.xml: dimensions value required "
                  "for structured mesh: %s\n", name);

        return 0;
    }

    d1 = strdup (dimensions);

    c = strtok (d1, ",");
    while (c)
    {
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        dim_att_nam = 0;
        conca_mesh_numb_att_nam(&dim_att_nam, name, "dimensions", counterstr);
        adios_common_define_attribute (p_new_group,dim_att_nam,"/",adios_string,c,"");
        free (dim_att_nam);
        counter++;
        c = strtok (NULL, ",");
    }

    char * dims = 0;
    counterstr[0] = '\0';
    snprintf(counterstr, 5, "%d", counter);
    dims = 0;
    adios_conca_mesh_att_nam(&dims, name, "dimensions-num");
    adios_common_define_attribute (p_new_group,dims,"/",adios_integer,counterstr,"");

    free (dims);
    free (d1);
    return 1;
}

int adios_define_mesh_structured_pointsSingleVar (const char * points,
                                                  struct adios_group_struct * new_group,
                                                  const char * name
                                                 )
{
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * pts_att_nam = 0; // points attribute name

    if (!points || !strcmp(points,""))
    {
        log_warn ("config.xml: points-single-var value required "
                  "for structured mesh: %s\n", name);

        return 0;
    }

    d1 = strdup (points);
    adios_conca_mesh_att_nam(&pts_att_nam, name, "points-single-var");
    adios_common_define_attribute (p_new_group,pts_att_nam,"/",adios_string,d1,"");
    free (pts_att_nam);
    free (d1);
    return 1;
}

int adios_define_mesh_structured_pointsMultiVar (const char * points,
                                                 struct adios_group_struct * new_group,
                                                 const char * name
                                                )
{
    char * c;  // comma location
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * pts_att_nam = 0; // pointss attribute name
    int counter = 0;        // used to create ptsX attributes
    char counterstr[5] = {0,0,0,0,0}; // used to create ptsX attributes

    if (!points || !strcmp(points,""))
    {
        log_warn ("config.xml: points-multi-var value required "
                  "for structured mesh: %s\n", name);

        return 0;
    }

    d1 = strdup (points);

    c = strtok (d1, ",");
    while (c)
    {
        pts_att_nam = 0;
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        conca_mesh_numb_att_nam(&pts_att_nam, name, "points-multi-var", counterstr);
        adios_common_define_attribute (p_new_group,pts_att_nam,"/",adios_string,c,"");
        free (pts_att_nam);
        counter++;
        c = strtok (NULL, ",");
    }

    // Define an attribute showing the number of mesh_vars
    // Should be more than one in this multi-var parsing
    if (counter > 1){
        char * pts = 0;
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        adios_conca_mesh_att_nam(&pts, name, "points-multi-var-num");
        adios_common_define_attribute (p_new_group,pts,"/",adios_integer,counterstr,"");
        free (pts);
    } else
    {
        log_warn ("config.xml: points-multi-var tag for mesh: %s "
                  " expects at least 2 variables\n", name);
        free (d1);

        return 0;
    }

    free (d1);

    return 1;
}

/*
int adios_define_mesh_unstructured_nspace (const char * nspace
        ,struct adios_group_struct * new_group
        ,const char * name
        )
{
    char * d1; // save of strdup
    int64_t      p_new_group = (int64_t) new_group;
    char * nsp_att_nam = 0; // nspace attribute name

    if (!nspace)
    {
        log_warn ("config.xml: nspace value required for unstructured mesh: %s\n",
                name);

        return 0;
    }

    d1 = strdup (nspace);
    adios_conca_mesh_att_nam(&nsp_att_nam, name, "nspace");
    adios_common_define_attribute (p_new_group,nsp_att_nam,"/",adios_string,nspace,"");
    free (nsp_att_nam);
    free (d1);

    return 1;
}
*/

int adios_define_mesh_unstructured_npoints (const char * npoints
        ,struct adios_group_struct * new_group
        ,const char * name
        )
{
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * npts_att_nam = 0; // npoints attribute name

    if (!npoints || !strcmp(npoints,""))
    {
//        log_warn ("config.xml: npoints value required for unstructured mesh %s:\n",
//                  name);

        return 0;
    }

    d1 = strdup (npoints);

    adios_conca_mesh_att_nam(&npts_att_nam, name, "npoints");
    adios_common_define_attribute (p_new_group,npts_att_nam,"/",adios_string,npoints,"");
    free (npts_att_nam);

    free (d1);

    return 1;
}

int adios_define_mesh_unstructured_pointsMultiVar (const char * points,
                                                   struct adios_group_struct * new_group,
                                                   const char * name
                                                  )
{
    char * c;  // comma location
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * pts_att_nam = 0; // pointss attribute name
    int counter = 0;        // used to create ptsX attributes
    char counterstr[5] = {0,0,0,0,0}; // used to create ptsX attributes

    if (!points || !strcmp(points,""))
    {
        log_warn ("config.xml: points-multi-var value required "
                  "for unstructured mesh: %s\n", name);

        return 0;
    }

    d1 = strdup (points);
    c = strtok (d1, ",");

    while (c)
    {
        pts_att_nam = 0;
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        conca_mesh_numb_att_nam(&pts_att_nam, name, "points-multi-var", counterstr);
        adios_common_define_attribute (p_new_group,pts_att_nam,"/",adios_string,c,"");
        free (pts_att_nam);
        counter++;
        c = strtok (NULL, ",");
    }

    // At this point we expect at least 2 "points-multi-var values
    if (counter > 1){
        char * pts = 0;
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        adios_conca_mesh_att_nam(&pts, name, "points-multi-var-num");
        adios_common_define_attribute (p_new_group,pts,"/",adios_integer,counterstr,"");
        free (pts);
        free (d1);
    } else
    {
        log_warn ("config.xml: points-multi-var tag expects "
                  "at least two variabels. (%s)\n" , name);
        free (d1);
        return 0;
    }

    return 1;
}

int adios_define_mesh_unstructured_pointsSingleVar (const char * points,
                                                    struct adios_group_struct * new_group,
                                                    const char * name
                                                   )
{
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * pts_att_nam = 0; // points attribute name

    if (!points || !strcmp(points,""))
    {
        log_warn ("config.xml: points-single-var value required "
                  "for unstructured mesh: %s\n", name);

        return 0;
    }

    d1 = strdup (points);
    adios_conca_mesh_att_nam(&pts_att_nam, name, "points-single-var");
    adios_common_define_attribute (p_new_group,pts_att_nam,"/",adios_string,d1,"");
    free (pts_att_nam);

    free (d1);

    return 1;
}

int adios_define_mesh_unstructured_uniformCells (const char * count,
                                                 const char * data,
                                                 const char * type,
                                                 struct adios_group_struct * new_group,
                                                 const char * name
                                                )
{
    char * d1; // save of strdup
    int64_t p_new_group = (int64_t) new_group;
    char * ncellset_att_nam = 0;  // ncellset attribute
    char * cellcount_att_nam = 0; // single cell count attribute
    char * celldata_att_nam = 0;  // single cell data  attribute
    char * celltype_att_nam = 0;  // single cell type attribute

    adios_conca_mesh_att_nam(&ncellset_att_nam,name,"ncsets");
    adios_common_define_attribute (p_new_group,ncellset_att_nam,"/",adios_integer,"1","");
    free (ncellset_att_nam);

    if (!count || !strcmp(count,""))
    {
        log_warn ("config.xml: uniform-cells count value required "
                  "for unstructured mesh: %s\n", name);

        return 0;
    }
    if (!data || !strcmp(data,""))
    {
        log_warn ("config.xml: uniform-cells data value required "
                  "for unstructured mesh: %s\n", name);

        return 0;
    }
    if (!type || !strcmp(type,""))
    {
        log_warn ("config.xml: uniform-cells type value required "
                  "for unstructured mesh: %s\n", name);

        return 0;
    }

    d1 = strdup (count);
    adios_conca_mesh_att_nam(&cellcount_att_nam, name, "ccount");
    adios_common_define_attribute (p_new_group,cellcount_att_nam,"/",adios_string,d1,"");
    free (cellcount_att_nam);
    free (d1);

    d1 = strdup (data);
    adios_conca_mesh_att_nam(&celldata_att_nam, name, "cdata");
    adios_common_define_attribute (p_new_group,celldata_att_nam,"/",adios_string,d1,"");
    free (celldata_att_nam);
    free (d1);

    d1 = strdup (type);
    adios_conca_mesh_att_nam(&celltype_att_nam, name, "ctype");
    adios_common_define_attribute (p_new_group,celltype_att_nam,"/",adios_string,d1,"");
    free(celltype_att_nam);
    free (d1);

    return 1;
}

int adios_define_mesh_unstructured_mixedCells (const char * count,
                                               const char * data,
                                               const char * types,
                                               struct adios_group_struct * new_group,
                                               const char * name
                                              )
{
    char * c;  // comma location
    char * d1; // save of strdup
    int counter = 0;        // used to create countX, typeX, dataX? attributes
    char counterstr[5] = {0,0,0,0,0}; // used to create countX, typeX, dataX? attributes
    int64_t p_new_group = (int64_t) new_group;
    char * ncellset_att_nam = 0;  // ncellset attribute
    char * ccounts_att_nam = 0;   // ccountX attributes
    char * cdata_att_nam = 0;     // cdataX attributes
    char * celltype_att_nam = 0;  // ctypeX attributes

    if (!count || !strcmp(count,""))
    {
        log_warn ("config.xml: mixed-cells count value required "
                  "for unstructured mesh: %s\n", name);

        return 0;
    }
    if (!data || !strcmp(data,""))
    {
        log_warn ("config.xml: mixed-cells data value required "
                  "for unstructured mesh: %s\n", name);

        return 0;
    }
    if (!types || !strcmp(types,""))
    {
        log_warn ("config.xml: mixed-cells type value required "
                  "for unstructured mesh: %s\n", name);

        return 0;
    }

    d1 = strdup (count);
    c = strtok (d1, ",");
    while (c)
    {
        //cell_list->cell_list.count.var = 0;
        //cell_list->cell_list.count.rank = strtod (c, 0);
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        ccounts_att_nam = 0;
        conca_mesh_numb_att_nam(&ccounts_att_nam, name, "ccount", counterstr);
        adios_common_define_attribute (p_new_group,ccounts_att_nam,"/",adios_string,c,"");
        free (ccounts_att_nam);
        counter++;
        c = strtok (NULL, ",");
    }
//    free (d1);

    // We should have at least 2 cell sets, otherwise the cells are uniform
    if (counter <= 1){
        log_warn ("config.xml: Please provide at least 2 cell counts of mesh: %s\n"
                  "or use the 'uniform-cells' tag.\n", name);
        return 0;
    }

    snprintf(counterstr, 5, "%d", counter);
    adios_conca_mesh_att_nam(&ncellset_att_nam, name, "ncsets");
    adios_common_define_attribute (p_new_group,ncellset_att_nam,"/",adios_integer,counterstr,"");
    free (ncellset_att_nam);

    // From the number of counts expect the same number of data and type items
    int cell_set_count = counter;
    // Reset counter
    counter = 0;

    d1 = strdup (data);
    c = strtok (d1, ",");
    while (c)
    {
        cdata_att_nam = 0;
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        conca_mesh_numb_att_nam(&cdata_att_nam, name, "cdata", counterstr);
        adios_common_define_attribute (p_new_group,cdata_att_nam,"/",adios_string,c,"");
        free (cdata_att_nam);
        counter++;
        c = strtok (NULL, ",");
    }
    free (d1);

    // If the number of data variables does not match the number of counts
    // Generate an error message
    if (counter != cell_set_count){
        log_warn ("config.xml: Please provide at least %d cell data of mesh: %s\n"
                  "or use the 'uniform-cells' tag\n", cell_set_count, name);
        return 0;
    }

    // Reset counter
    counter = 0;

    d1 = strdup (types);
    c = strtok (d1, ",");

    while (c)
    {
        celltype_att_nam = 0;
        counterstr[0] = '\0';
        snprintf(counterstr, 5, "%d", counter);
        conca_mesh_numb_att_nam(&celltype_att_nam, name, "ctype", counterstr);
        adios_common_define_attribute (p_new_group,celltype_att_nam,"/",adios_string,c,"");
        c = strtok (NULL, ",");
        counter++;
        free (celltype_att_nam);
    }
    free (d1);

    // If the number of data variables does not match the number of counts
    // Generate an error message
    if (counter != cell_set_count){
        log_warn ("config.xml: Please provide at least %d cell types of mesh: %s\n"
                  "or use the 'uniform-cells' tag\n", cell_set_count, name);
        return 0;
    }

    return 1;
}

// called by NO-XML API
int adios_common_define_var_mesh (int64_t group_id, const char * varname, const char * meshname, const char * path)
{
    char *mpath = 0;
    mpath = malloc(strlen("/adios_schema")+strlen(varname)+1);
    strcpy(mpath,varname);
    strcat(mpath,"/adios_schema");
    adios_common_define_attribute (group_id, mpath, path, adios_string, meshname, "");
    free (mpath);
    return 0;
}

int adios_common_define_var_centering (int64_t group_id, const char * varname, const char * centering, const char * path)
{
    char *mpath = 0;
    mpath = malloc(strlen("/adios_schema/centering")+strlen(varname)+1);
    strcpy(mpath,varname);
    strcat(mpath,"/adios_schema/centering");
    adios_common_define_attribute (group_id, mpath, path, adios_string, centering, "");
    free (mpath);
    return 0;
}

int adios_common_define_mesh_group (int64_t group_id, const char * name, const char * group)
{
    char * mpath = 0;
    mpath = malloc(strlen("/adios_schema/")+strlen(name)+strlen("/mesh-group")+1);
    strcpy (mpath, "/adios_schema/");
    strcat (mpath, name);
    strcat (mpath, "/mesh-group");
//    adios_conca_mesh_att_nam(&group, name, "mesh-group");
    adios_common_define_attribute (group_id, mpath, "", adios_string, group, "");
    free (mpath);
    return 0;
}

int adios_common_define_mesh_file (int64_t group_id, char * name, char * file){
    char * mpath = 0;
    mpath = malloc(strlen("/adios_schema/")+strlen(name)+strlen("/mesh-file")+1);
    strcpy (mpath, "/adios_schema/");
    strcat (mpath, name);
    strcat (mpath, "/mesh-file");
    adios_common_define_attribute (group_id, mpath, "", adios_string, file, "");
    free (mpath);
    return 0;
}

