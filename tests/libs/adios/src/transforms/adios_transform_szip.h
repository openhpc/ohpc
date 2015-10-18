
#include <stdio.h>
#include "szlib.h"

#define MIN(a,b)    (((a)<(b)) ? (a) : (b))

#define VERIFY_INPUT_PRE() assert(input_data != NULL && input_len > 0 && output_data != NULL && output_len != NULL && *output_len > 0)

enum COMPRESS_TYPE
{
     compress_type_unknown         = -1
    ,compress_type_none            = 0
    ,compress_type_zlib            = 1
    ,compress_type_bzlib2          = 2
    ,compress_type_szip            = 3
    ,num_compress_types            = 4 // Not counting unknown; KEEP THIS UPDATED
};


static const struct
{
    const char *alias;                // A possible name for a compres method
    enum COMPRESS_TYPE type;    // The corresponding COMPRESS_TYPE
} COMPRESS_TYPE_NAMES[] = {
     { "unknown"    , compress_type_unknown }

    ,{ "none"        , compress_type_none }
    ,{ "no"            , compress_type_none }
    ,{ "raw"        , compress_type_none }
    ,{ ""            , compress_type_none }

    ,{ "zlib"    , compress_type_zlib }
    ,{ "zip"    , compress_type_zlib }

    ,{ "bzlib2"    , compress_type_bzlib2 }
    ,{ "bzip2"    , compress_type_bzlib2 }

    ,{ "szlib"    , compress_type_szip }
    ,{ "szip"    , compress_type_szip }
};


// #define EXPAND_SIZE(s) (((double)(s)*1.01)+12)

#define EXPAND_SIZE(s) (s)

/////////////// initilization for szip ///////////////

enum
{
    O32_LITTLE_ENDIAN = 0x03020100ul,
    O32_BIG_ENDIAN = 0x00010203ul,
    O32_PDP_ENDIAN = 0x01000302ul
};

static const union { unsigned char bytes[4]; uint32_t value; } o32_host_order =
    { { 0, 1, 2, 3 } };

#define O32_HOST_ORDER (o32_host_order.value)        

// #define H5_SZIP_ALLOW_K13_OPTION_MASK   1
// #define H5_SZIP_CHIP_OPTION_MASK        2
// #define H5_SZIP_EC_OPTION_MASK          4
// #define H5_SZIP_NN_OPTION_MASK          32
// #define H5_SZIP_MAX_PIXELS_PER_BLOCK    32

static int init_szip_parameters(SZ_com_t* p_sz_param, const uint32_t ndims, const uint64_t* dim)
{
    assert(p_sz_param && ndims > 0 && dim);
    p_sz_param->options_mask = SZ_NN_OPTION_MASK;
    
    p_sz_param->options_mask &= (~SZ_CHIP_OPTION_MASK);
    p_sz_param->options_mask |= SZ_ALLOW_K13_OPTION_MASK;

    /* Always set "raw" (no szip header) flag for data */
    p_sz_param->options_mask |= SZ_RAW_OPTION_MASK;

    /* Mask off the LSB and MSB options, if they were given */
    /* (The HDF5 library sets them internally, as needed) */
    p_sz_param->options_mask &= ~(SZ_LSB_OPTION_MASK|SZ_MSB_OPTION_MASK);
    
    p_sz_param->options_mask &= ~(SZ_LSB_OPTION_MASK|SZ_MSB_OPTION_MASK);
    switch(O32_HOST_ORDER) 
    {
        case O32_LITTLE_ENDIAN:      /* Little-endian byte order */
            // printf("little endian\n");
            p_sz_param->options_mask |= SZ_LSB_OPTION_MASK;
            break;

        case O32_BIG_ENDIAN:      /* Big-endian byte order */
            p_sz_param->options_mask |= SZ_MSB_OPTION_MASK;
            break;

        default:
            p_sz_param->options_mask |= SZ_LSB_OPTION_MASK;
            break;
    } /* end switch */
    
    p_sz_param->bits_per_pixel = 64;
    p_sz_param->pixels_per_block = 32;
    
    uint32_t npoints = 1;
    uint32_t i = 0;
    for(i = 0; i < ndims; i++)
    {
        npoints *= dim[i];
    }
    
    uint32_t scanline = dim[ndims - 1];
    
    if(scanline < p_sz_param->pixels_per_block) 
    {

        if(npoints < 0 || npoints < p_sz_param->pixels_per_block)
        {
            printf("buffer too small for szip compression %d\n", npoints);
            return -1;
        }
        
        scanline = MIN((p_sz_param->pixels_per_block * SZ_MAX_BLOCKS_PER_SCANLINE), npoints);
    }
    else 
    {
        if(scanline <= SZ_MAX_PIXELS_PER_SCANLINE)
        {
            scanline = MIN((p_sz_param->pixels_per_block * SZ_MAX_BLOCKS_PER_SCANLINE), scanline);
        }
        else
        {
            scanline = p_sz_param->pixels_per_block * SZ_MAX_BLOCKS_PER_SCANLINE;
        }
    }
    
    p_sz_param->pixels_per_scanline = scanline;
    return 0;
}
////////////////////////////////////////////////////////////


