/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#include <stdint.h>
#include <stdio.h>
#include "adios_types.h"
#include "core/adios_logger.h"
#include "core/adios_internals.h"

void show_bytes(unsigned char * start, int len)
{
    int i;
    for (i = 0; i < len; i ++) {
        log_info(" %.2x", start[i]);
    }
    log_info("\n");
}


void swap_16_ptr(void *data) 
{
    uint16_t d = *(uint16_t *)data;
    *(uint16_t *)data = d>>8 | d<<8;
}


void swap_32_ptr(void *data)
{
    uint32_t d = *(uint32_t *)data;
    *(uint32_t *)data = ((d&0x000000FF)<<24) 
                      + ((d&0x0000FF00)<<8)
                      + ((d&0x00FF0000)>>8)
                      + ((d&0xFF000000)>>24);
}


void swap_64_ptr(void *data)
{
    uint64_t d = *(uint64_t *)data;
    *(uint64_t *)data = ((d&0x00000000000000FF)<<56) 
                          + ((d&0x000000000000FF00)<<40)
                          + ((d&0x0000000000FF0000)<<24)
                          + ((d&0x00000000FF000000)<<8)
                          + ((d&0x000000FF00000000LL)>>8)
                          + ((d&0x0000FF0000000000LL)>>24)
                          + ((d&0x00FF000000000000LL)>>40)
                          + ((d&0xFF00000000000000LL)>>56);
}


void swap_128_ptr(void *data)
{
    uint64_t d = *(uint64_t *)data;
    *(uint64_t *)data = ((d&0x00000000000000FF)<<56) 
                          + ((d&0x000000000000FF00)<<40)
                          + ((d&0x0000000000FF0000)<<24)
                          + ((d&0x00000000FF000000)<<8)
                          + ((d&0x000000FF00000000LL)>>8)
                          + ((d&0x0000FF0000000000LL)>>24)
                          + ((d&0x00FF000000000000LL)>>40)
                          + ((d&0xFF00000000000000LL)>>56);
    d = *((uint64_t *)data + 1);
    d = ((d&0x00000000000000FF)<<56) 
                          + ((d&0x000000000000FF00)<<40)
                          + ((d&0x0000000000FF0000)<<24)
                          + ((d&0x00000000FF000000)<<8)
                          + ((d&0x000000FF00000000LL)>>8)
                          + ((d&0x0000FF0000000000LL)>>24)
                          + ((d&0x00FF000000000000LL)>>40)
                          + ((d&0xFF00000000000000LL)>>56);
    *((uint64_t *)data + 1) = *(uint64_t *)data;
    *(uint64_t *)data = d; 
}


void swap_adios_type(void *data, enum ADIOS_DATATYPES type)
{
    if(type == adios_string) {
        return;
    }
    else {
        uint64_t size = adios_get_type_size (type, "");

        switch (size)
        {
            case 1:
                break;
            case 2:
                swap_16_ptr(data);
                break;
            case 4:
                swap_32_ptr(data);
                break;
            case 8:
                swap_64_ptr(data);
                break;
            case 16:
                swap_128_ptr(data);
                break;
            case adios_complex:
            case adios_double_complex:
                // TODO
                break;
        }
    }
}


void swap_adios_type_array(void *data, enum ADIOS_DATATYPES type, uint64_t payload_size)
{
    uint64_t size = adios_get_type_size (type, "");
    uint64_t num_elements = payload_size / size;

    uint64_t i = 0;
    for(i = 0; i < num_elements; i ++) {
        swap_adios_type((char *)data + i*size, type);
    }    
}

void swap_ptr(void * data, int size)
{
    switch (size)
    {
        case 16:
            swap_16_ptr(data);
            break;
        case 32:
            swap_32_ptr(data);
            break;
        case 64:
            swap_64_ptr(data);
            break;
        case 128:
            swap_128_ptr(data);
            break;
    }
}
