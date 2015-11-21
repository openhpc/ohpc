#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include "util.h"
#include "core/transforms/adios_transforms_hooks_read.h"
#include "core/transforms/adios_transforms_reqgroup.h"

#ifdef SZIP

#include "adios_transform_szip.h"

int adios_transform_szip_is_implemented (void) {return 1;}

int decompress_szip_pre_allocated(const void* input_data, const uint64_t input_len,
                                    void* output_data, uint64_t* output_len,
                                    const uint32_t ndims, const uint64_t* dim)
{
    assert(input_data != NULL && input_len > 0 && output_data != NULL && output_len != NULL && *output_len > 0);

    SZ_com_t sz_param;

    int sz_rtn = init_szip_parameters(&sz_param, ndims, dim);
    if(sz_rtn != 0)
    {
        return -1;
    }

    size_t temp_len = *output_len;

    sz_rtn = SZ_BufftoBuffDecompress(output_data, &temp_len, input_data, input_len, &sz_param);
    if(sz_rtn != SZ_OK)
    {
        printf("SZ_BufftoBuffDecompress error %d\n", sz_rtn);
        return -1;
    }

    *output_len = (uint64_t)(temp_len);

    return 0;
}

int adios_transform_szip_generate_read_subrequests(adios_transform_read_request *reqgroup,
                                                adios_transform_pg_read_request *pg_reqgroup)
{
    void *buf = malloc(pg_reqgroup->raw_var_length);
    adios_transform_raw_read_request *subreq = adios_transform_raw_read_request_new_whole_pg(pg_reqgroup, buf);
    adios_transform_raw_read_request_append(pg_reqgroup, subreq);
    return 0;
}

// Do nothing for individual subrequest
adios_datablock * adios_transform_szip_subrequest_completed(adios_transform_read_request *reqgroup,
                                                            adios_transform_pg_read_request *pg_reqgroup,
                                                            adios_transform_raw_read_request *completed_subreq)
{
    return NULL;
}

adios_datablock * adios_transform_szip_pg_reqgroup_completed(adios_transform_read_request *reqgroup,
                                                                adios_transform_pg_read_request *completed_pg_reqgroup)
{
    uint64_t raw_size = (uint64_t)completed_pg_reqgroup->raw_var_length;
    void* raw_buff = completed_pg_reqgroup->subreqs->data;

    uint64_t orig_size = adios_get_type_size(reqgroup->transinfo->orig_type, "");
    int d = 0;
    for(d = 0; d < reqgroup->transinfo->orig_ndim; d++)
        orig_size *= (uint64_t)(completed_pg_reqgroup->orig_varblock->count[d]);

    void* orig_buff = malloc(orig_size);

    int ndims = 1;
    uint64_t dim[1] = {orig_size / sizeof(double)};
    int rtn = decompress_szip_pre_allocated(raw_buff, raw_size, orig_buff, &orig_size, ndims, dim);
    if(rtn != 0)
    {
        return NULL;
    }

    return adios_datablock_new_whole_pg(reqgroup, completed_pg_reqgroup, orig_buff);
}

adios_datablock * adios_transform_szip_reqgroup_completed(adios_transform_read_request *completed_reqgroup)
{
    return NULL;
}


#else

DECLARE_TRANSFORM_READ_METHOD_UNIMPL(szip);

#endif

