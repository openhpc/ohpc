#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#include "adios_transforms_common.h"
#include "adios_transforms_util.h"
#include "core/adios_internals.h"

static int buffer_reserve(char ** buffer, uint64_t * buffer_size
                         ,uint64_t * buffer_offset
                         ,uint64_t size, uint64_t max_size
                         )
{
    if (*buffer_offset + size > *buffer_size || *buffer == 0)
    {
        if (*buffer_offset + size + 1000 > max_size) {
            fprintf (stderr, "Cannot allocate memory in buffer_write.  "
                             "Requested: %llu, Maximum: %llu\n", *buffer_offset + size + 1000, max_size);
            return 0;
        }

        char * b = realloc (*buffer, *buffer_offset + size + 1000);
        if (b)
        {
            *buffer = b;
            *buffer_size = (*buffer_offset + size + 1000);
        }
        else
        {
            fprintf (stderr, "Cannot allocate memory in buffer_write.  "
                             "Requested: %llu\n", *buffer_offset + size + 1000);

            return 0;
        }
    }
    return 1;
}

static int buffer_write (char ** buffer, uint64_t * buffer_size
                         ,uint64_t * buffer_offset
                         ,const void * data, uint64_t size, uint64_t max_size
                         )
{
    if (!buffer_reserve(buffer, buffer_size, buffer_offset, size, max_size))
        return 0;

    memcpy (*buffer + *buffer_offset, data, size);
    *buffer_offset += size;
    return 1;
}


int shared_buffer_write(struct adios_file_struct *fd, const void * data, uint64_t size) {
    return buffer_write(&fd->buffer, &fd->buffer_size, &fd->offset, data, size, fd->write_size_bytes);
}

int shared_buffer_reserve(struct adios_file_struct *fd, uint64_t size) {
    return buffer_reserve(&fd->buffer, &fd->buffer_size, &fd->offset, size, fd->write_size_bytes);
}

int shared_buffer_mark_written(struct adios_file_struct *fd, uint64_t size) {
    if (fd->offset + size > fd->buffer_size)
        return 0;

    fd->offset += size;
    return 1;
}
