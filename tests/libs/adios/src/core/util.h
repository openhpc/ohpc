#ifndef UTIL_H_
#define UTIL_H_

#include <stdint.h>
#include "public/adios_mpi.h"
#include "public/adios_types.h"
#include "public/adios_selection.h"

typedef struct read_request
{
    ADIOS_SELECTION * sel;
    int varid;
    int from_steps;
    int nsteps;
    void * data;
    uint64_t datasize; // size of selection to hold data
// above is the common fields that all read method will use
    void * priv; // private structure for each read method
    struct read_request * next;
} read_request;

/* Reverse the order in an array in place.
   use swapping from Fortran/column-major order to ADIOS-read-api/C/row-major order and back
*/
void swap_order(int n, uint64_t *array, int *timedim);
void change_endianness( void *data, uint64_t slice_size, enum ADIOS_DATATYPES type);
void copy_data (void *dst, void *src,
                int idim,
                int ndim,
                uint64_t* size_in_dset,
                uint64_t* ldims,
                const uint64_t * readsize,
                uint64_t dst_stride,
                uint64_t src_stride,
                uint64_t dst_offset,
                uint64_t src_offset,
                uint64_t ele_num,
                int      size_of_type,
                enum ADIOS_FLAG change_endiness,
                enum ADIOS_DATATYPES type
                );
void alloc_namelist (char ***namelist, int length);
void free_namelist (char **namelist, int length);
void list_insert_read_request_tail (read_request ** h, read_request * q);
void list_insert_read_request_next (read_request ** h, read_request * q);
void list_append_read_request_list (read_request ** h, read_request * q);
void list_free_read_request (read_request * h);
int list_get_length (read_request * h);
ADIOS_SELECTION * copy_selection (const ADIOS_SELECTION * sel);
void free_selection (ADIOS_SELECTION * sel);
// This helper routine returns a vector of unique NID's
int get_nids (MPI_Comm comm, uint32_t * nids);

/*******************************************************
   Processing parameter lists
**********************************************************/
/*
   Process a ;-separated and possibly multi-line text and 
   create a list of name=value pairs from each 
   item which has a "name=value" pattern. Whitespaces are removed. 
   Input is not modified. Space is allocated;
   Also, simple "name" or "name=" patterns are processed and 
   returned with value=NULL. 
*/
struct PairStruct {
    char * name;
    char * value;
    struct PairStruct * next;
};
typedef struct PairStruct PairStruct;

PairStruct * text_to_name_value_pairs (const char * text);
void free_name_value_pairs (PairStruct * pairs);

/**********************************************************
   Timing
**********************************************************/
/* sleep for a bit */
void adios_nanosleep (int sec, int nanosec);
/* get current time as double (in seconds) */
double adios_gettime();

void * bufdup(const void *buf, uint64_t elem_size, uint64_t count);

#endif
