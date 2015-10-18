#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include "util.h"
#include "core/transforms/adios_transforms_hooks_read.h"
#include "core/transforms/adios_transforms_reqgroup.h"

#ifdef BZIP2

#include "bzlib.h"

int adios_transform_bzip2_is_implemented (void) {return 1;}

int decompress_bzip2_pre_allocated(const void* input_data, const uint64_t input_len,
                                    void* output_data, uint64_t* output_len)
{
    // bzip2 only support input size of 32 bit integer

    assert(input_data != NULL
            && input_len > 0 && input_len <= UINT_MAX
            && output_data != NULL
            && output_len != NULL && *output_len > 0 && *output_len < UINT_MAX);

    unsigned int input_len_32 = (unsigned int)input_len;
    unsigned int output_len_32 = (unsigned int)(*output_len);

    int bz_rtn = BZ2_bzBuffToBuffDecompress((char*)output_data, &output_len_32,
                                                (char*)input_data, input_len_32,
                                                0, 0 );

    if(bz_rtn != BZ_OK)
    {
        printf("BZ2_bzBuffToBuffDecompress error %d\n", bz_rtn);
        return -1;
    }

    *output_len = output_len_32;
    return 0;
}

int adios_transform_bzip2_generate_read_subrequests(adios_transform_read_request *reqgroup,
                                                       adios_transform_pg_read_request *pg_reqgroup)
{
    void *buf = malloc(pg_reqgroup->raw_var_length);
    assert(buf);
    adios_transform_raw_read_request *subreq = adios_transform_raw_read_request_new_whole_pg(pg_reqgroup, buf);
    adios_transform_raw_read_request_append(pg_reqgroup, subreq);
    return 0;
}

// Do nothing for individual subrequest
adios_datablock * adios_transform_bzip2_subrequest_completed(adios_transform_read_request *reqgroup,
                                                                adios_transform_pg_read_request *pg_reqgroup,
                                                                adios_transform_raw_read_request *completed_subreq)
{
    return NULL;
}

adios_datablock * adios_transform_bzip2_pg_reqgroup_completed(adios_transform_read_request *reqgroup,
                                                                adios_transform_pg_read_request *completed_pg_reqgroup)
{
    uint64_t compressed_size = (uint64_t)completed_pg_reqgroup->raw_var_length;
    void* compressed_data = completed_pg_reqgroup->subreqs->data;
    
    uint64_t uncompressed_size_meta = *((uint64_t*)completed_pg_reqgroup->transform_metadata);
    char compress_ok = *((char*)(completed_pg_reqgroup->transform_metadata + sizeof(uint64_t)));

    uint64_t uncompressed_size = adios_get_type_size(reqgroup->transinfo->orig_type, "");
    int d = 0;
    for(d = 0; d < reqgroup->transinfo->orig_ndim; d++)
    {
        uncompressed_size *= (uint64_t)(completed_pg_reqgroup->orig_varblock->count[d]);
    }
    
    if(uncompressed_size_meta != uncompressed_size)
    {
        printf("WARNING: possible wrong data size or corrupted metadata\n");
    }
    
    void* uncompressed_data = malloc(uncompressed_size);
    if(!uncompressed_data)
    {
        return NULL;
    }

    if(compress_ok == 1)    // compression is successful
    {
        
        int rtn = decompress_bzip2_pre_allocated(compressed_data, compressed_size, uncompressed_data, &uncompressed_size);
        if(rtn != 0)
        {
            return NULL;
        }
    }
    else    // just copy the buffer since data is not compressed
    {
        // printf("compression failed before, fall back to memory copy\n");
        memcpy(uncompressed_data, compressed_data, compressed_size);
    }

    return adios_datablock_new_whole_pg(reqgroup, completed_pg_reqgroup, uncompressed_data);
}

adios_datablock * adios_transform_bzip2_reqgroup_completed(adios_transform_read_request *completed_reqgroup)
{
    return NULL;
}


#else

DECLARE_TRANSFORM_READ_METHOD_UNIMPL(bzip2);

#endif

