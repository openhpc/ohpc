/*
 * adios_transforms_util.h
 *
 *  Created on: Jul 13, 2012
 *      Author: Drew
 */

#ifndef ADIOS_TRANSFORMS_UTIL_H_
#define ADIOS_TRANSFORMS_UTIL_H_

#include <stdint.h>
#include "public/adios_types.h"
#include "core/adios_internals.h"
//#include "adios_internals.h"


int shared_buffer_write(struct adios_file_struct *fd, const void * data, uint64_t size);
int shared_buffer_reserve(struct adios_file_struct *fd, uint64_t size);
int shared_buffer_mark_written(struct adios_file_struct *fd, uint64_t size);


#endif /* ADIOS_TRANSFORMS_UTIL_H_ */
