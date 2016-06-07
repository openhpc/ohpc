/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#ifndef ENDIANNESS_H 
#define ENDIANNESS_H 1

#include "public/adios_types.h"

void show_bytes(unsigned char * start, int len);

void swap_ptr(void * data, int size);

void swap_16_ptr(void *data);

void swap_32_ptr(void *data);

void swap_64_ptr(void *data);

void swap_128_ptr(void *data);

#define swap_16(data) swap_16_ptr(&(data))

#define swap_32(data) swap_32_ptr(&(data))

#define swap_64(data) swap_64_ptr(&(data))

#define swap_128(data) swap_128_ptr(&(data))

void swap_adios_type(void *data, enum ADIOS_DATATYPES type);

void swap_adios_type_array(void *payload, enum ADIOS_DATATYPES type, uint64_t payload_size);

#endif
