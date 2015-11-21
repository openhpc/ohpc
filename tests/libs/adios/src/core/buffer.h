/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#ifndef ADIOS_BUFFER_H
#define ADIOS_BUFFER_H

#include "public/adios_types.h"

void      adios_buffer_size_requested_set (uint64_t v);
uint64_t  adios_buffer_size_requested_get (void);
void      adios_buffer_size_max_set (uint64_t v);
void      adios_buffer_size_remaining_set (uint64_t v);
void      adios_buffer_alloc_percentage_set (int v);
void      adios_buffer_alloc_when_set (enum ADIOS_BUFFER_ALLOC_WHEN v);

enum ADIOS_BUFFER_ALLOC_WHEN adios_buffer_alloc_when_get (void);

int adios_set_buffer_size (void);
uint64_t adios_method_buffer_alloc (uint64_t size);
int adios_method_buffer_free (uint64_t size);

#endif
