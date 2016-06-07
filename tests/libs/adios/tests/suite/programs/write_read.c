/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C test: write then read adios files 
 *
 * How to run: mpirun -np <N> write_read
 * Output: write_read.bp
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "adios.h"
#include "adios_read.h"

#ifdef DMALLOC
#include "dmalloc.h"
#endif

#define log(...) fprintf (stderr, "[rank=%3.3d, line %d]: ", rank, __LINE__); fprintf (stderr, __VA_ARGS__); fflush(stderr);
#define printE(...) fprintf (stderr, "[rank=%3.3d, line %d]: ERROR: ", rank, __LINE__); fprintf (stderr, __VA_ARGS__); fflush(stderr);

typedef struct complex {
    float r;
    float i;
} complex;

typedef struct double_complex {
    double r;
    double i;
} double_complex;

/* Variables to write */
int8_t   scalar_byte    = -100;
int16_t  scalar_short   = -10000;
int32_t  scalar_int     = -1000000000;
int64_t  scalar_long    = -1000000000000L;
uint8_t  scalar_ubyte   = 100;
uint16_t scalar_ushort  = 10000;
uint32_t scalar_uint    = 1000000000;
uint64_t scalar_ulong   = 1000000000000L;
float    scalar_float   = 3.14159265358979323846; 
double   scalar_double  = 2.7182818284590452353602874713526625L;
char *   scalar_string  = "ADIOS example string";
complex  scalar_complex;
double_complex scalar_double_complex;

int8_t   *a1_byte,    *a2_byte,    *a3_byte,    *a6_byte;
int16_t  *a1_short,   *a2_short,   *a3_short,   *a6_short;
int32_t  *a1_int,     *a2_int,     *a3_int,     *a6_int;
int64_t  *a1_long,    *a2_long,    *a3_long,    *a6_long;
uint8_t  *a1_ubyte,   *a2_ubyte,   *a3_ubyte,   *a6_ubyte;
uint16_t *a1_ushort,  *a2_ushort,  *a3_ushort,  *a6_ushort;
uint32_t *a1_uint,    *a2_uint,    *a3_uint,    *a6_uint;
uint64_t *a1_ulong,   *a2_ulong,   *a3_ulong,   *a6_ulong;
float    *a1_float,   *a2_float,   *a3_float,   *a6_float;
double   *a1_double,  *a2_double,  *a3_double,  *a6_double;
complex  *a1_complex, *a2_complex, *a3_complex, *a6_complex;
double_complex *a1_double_complex, *a2_double_complex, *a3_double_complex, *a6_double_complex;

/* Variables to read */
int8_t   *r1_byte,    *r2_byte,    *r3_byte,    *r6_byte;
int16_t  *r1_short,   *r2_short,   *r3_short,   *r6_short;
int32_t  *r1_int,     *r2_int,     *r3_int,     *r6_int;
int64_t  *r1_long,    *r2_long,    *r3_long,    *r6_long;
uint8_t  *r1_ubyte,   *r2_ubyte,   *r3_ubyte,   *r6_ubyte;
uint16_t *r1_ushort,  *r2_ushort,  *r3_ushort,  *r6_ushort;
uint32_t *r1_uint,    *r2_uint,    *r3_uint,    *r6_uint;
uint64_t *r1_ulong,   *r2_ulong,   *r3_ulong,   *r6_ulong;
float    *r1_float,   *r2_float,   *r3_float,   *r6_float;
double   *r1_double,  *r2_double,  *r3_double,  *r6_double;
complex  *r1_complex, *r2_complex, *r3_complex, *r6_complex;
double_complex *r1_double_complex, *r2_double_complex, *r3_double_complex, *r6_double_complex;

int ldim1 = 7;
int ldim2 = 5;
int ldim3 = 3;
int ldim4 = 2;
int ldim5 = 4;
int ldim6 = 9;
int gdim1, gdim2, gdim3, gdim4, gdim5, gdim6;
int offs1, offs2, offs3, offs4, offs5, offs6;

MPI_Comm    comm = MPI_COMM_WORLD;
int rank;
int size;

void init_vars()
{
    int n, i;
    int v = rank%256;
    scalar_complex.r = 8.0;
    scalar_complex.i = 9.0;
    scalar_double_complex.r = 10.0;
    scalar_double_complex.i = 11.0;

    gdim1 = size*ldim1;
    gdim2 = ldim2;
    gdim3 = ldim3;
    gdim4 = ldim4;
    gdim5 = ldim5;
    gdim6 = ldim6;

    offs1 = rank*ldim1;
    offs2 = 0;
    offs3 = 0;
    offs4 = 0;
    offs5 = 0;
    offs6 = 0;

    n = ldim1;
    a1_byte           = (int8_t*)   malloc (n * sizeof(int8_t));
    a1_short          = (int16_t*)  malloc (n * sizeof(int16_t));
    a1_int            = (int32_t*)  malloc (n * sizeof(int32_t));
    a1_long           = (int64_t*)  malloc (n * sizeof(int64_t));
    a1_ubyte          = (uint8_t*)  malloc (n * sizeof(uint8_t));
    a1_ushort         = (uint16_t*) malloc (n * sizeof(uint16_t));
    a1_uint           = (uint32_t*) malloc (n * sizeof(uint32_t));
    a1_ulong          = (uint64_t*) malloc (n * sizeof(uint64_t));
    a1_float          = (float*)    malloc (n * sizeof(float));
    a1_double         = (double*)   malloc (n * sizeof(double));
    a1_complex        = (complex*)  malloc (n * sizeof(complex));
    a1_double_complex = (double_complex*) malloc (n * sizeof(double_complex));

    r1_byte           = (int8_t*)   malloc (n * sizeof(int8_t));
    r1_short          = (int16_t*)  malloc (n * sizeof(int16_t));
    r1_int            = (int32_t*)  malloc (n * sizeof(int32_t));
    r1_long           = (int64_t*)  malloc (n * sizeof(int64_t));
    r1_ubyte          = (uint8_t*)  malloc (n * sizeof(uint8_t));
    r1_ushort         = (uint16_t*) malloc (n * sizeof(uint16_t));
    r1_uint           = (uint32_t*) malloc (n * sizeof(uint32_t));
    r1_ulong          = (uint64_t*) malloc (n * sizeof(uint64_t));
    r1_float          = (float*)    malloc (n * sizeof(float));
    r1_double         = (double*)   malloc (n * sizeof(double));
    r1_complex        = (complex*)  malloc (n * sizeof(complex));
    r1_double_complex = (double_complex*) malloc (n * sizeof(double_complex));

    for (i=0; i<n; i++)    a1_byte[i]   = rank % 256;
    for (i=0; i<n; i++)    a1_short[i]  = rank % 65536;
    for (i=0; i<n; i++)    a1_int[i]    = rank;
    for (i=0; i<n; i++)    a1_long[i]   = rank;
    for (i=0; i<n; i++)    a1_ubyte[i]  = rank % 256;
    for (i=0; i<n; i++)    a1_ushort[i] = rank % 65536;
    for (i=0; i<n; i++)    a1_uint[i]   = rank;
    for (i=0; i<n; i++)    a1_ulong[i]  = rank;
    for (i=0; i<n; i++)    a1_float[i]  = rank;
    for (i=0; i<n; i++)    a1_double[i] = rank;
    for (i=0; i<n; i++)    a1_complex[i].r = rank;
    for (i=0; i<n; i++)    a1_complex[i].i = rank;
    for (i=0; i<n; i++)    a1_double_complex[i].r = rank;
    for (i=0; i<n; i++)    a1_double_complex[i].i = rank;

    n = ldim1 * ldim2;
    a2_byte           = (int8_t*)   malloc (n * sizeof(int8_t));
    a2_short          = (int16_t*)  malloc (n * sizeof(int16_t));
    a2_int            = (int32_t*)  malloc (n * sizeof(int32_t));
    a2_long           = (int64_t*)  malloc (n * sizeof(int64_t));
    a2_ubyte          = (uint8_t*)  malloc (n * sizeof(uint8_t));
    a2_ushort         = (uint16_t*) malloc (n * sizeof(uint16_t));
    a2_uint           = (uint32_t*) malloc (n * sizeof(uint32_t));
    a2_ulong          = (uint64_t*) malloc (n * sizeof(uint64_t));
    a2_float          = (float*)    malloc (n * sizeof(float));
    a2_double         = (double*)   malloc (n * sizeof(double));
    a2_complex        = (complex*)  malloc (n * sizeof(complex));
    a2_double_complex = (double_complex*) malloc (n * sizeof(double_complex));

    r2_byte           = (int8_t*)   malloc (n * sizeof(int8_t));
    r2_short          = (int16_t*)  malloc (n * sizeof(int16_t));
    r2_int            = (int32_t*)  malloc (n * sizeof(int32_t));
    r2_long           = (int64_t*)  malloc (n * sizeof(int64_t));
    r2_ubyte          = (uint8_t*)  malloc (n * sizeof(uint8_t));
    r2_ushort         = (uint16_t*) malloc (n * sizeof(uint16_t));
    r2_uint           = (uint32_t*) malloc (n * sizeof(uint32_t));
    r2_ulong          = (uint64_t*) malloc (n * sizeof(uint64_t));
    r2_float          = (float*)    malloc (n * sizeof(float));
    r2_double         = (double*)   malloc (n * sizeof(double));
    r2_complex        = (complex*)  malloc (n * sizeof(complex));
    r2_double_complex = (double_complex*) malloc (n * sizeof(double_complex));

    for (i=0; i<n; i++)    a2_byte[i]   = rank % 256;
    for (i=0; i<n; i++)    a2_short[i]  = rank % 65536;
    for (i=0; i<n; i++)    a2_int[i]    = rank;
    for (i=0; i<n; i++)    a2_long[i]   = rank;
    for (i=0; i<n; i++)    a2_ubyte[i]  = rank % 256;
    for (i=0; i<n; i++)    a2_ushort[i] = rank % 65536;
    for (i=0; i<n; i++)    a2_uint[i]   = rank;
    for (i=0; i<n; i++)    a2_ulong[i]  = rank;
    for (i=0; i<n; i++)    a2_float[i]  = rank;
    for (i=0; i<n; i++)    a2_double[i] = rank;
    for (i=0; i<n; i++)    a2_complex[i].r = rank;
    for (i=0; i<n; i++)    a2_complex[i].i = rank;
    for (i=0; i<n; i++)    a2_double_complex[i].r = rank;
    for (i=0; i<n; i++)    a2_double_complex[i].i = rank;

    n = ldim1 * ldim2 * ldim3;
    a3_byte           = (int8_t*)   malloc (n * sizeof(int8_t));
    a3_short          = (int16_t*)  malloc (n * sizeof(int16_t));
    a3_int            = (int32_t*)  malloc (n * sizeof(int32_t));
    a3_long           = (int64_t*)  malloc (n * sizeof(int64_t));
    a3_ubyte          = (uint8_t*)  malloc (n * sizeof(uint8_t));
    a3_ushort         = (uint16_t*) malloc (n * sizeof(uint16_t));
    a3_uint           = (uint32_t*) malloc (n * sizeof(uint32_t));
    a3_ulong          = (uint64_t*) malloc (n * sizeof(uint64_t));
    a3_float          = (float*)    malloc (n * sizeof(float));
    a3_double         = (double*)   malloc (n * sizeof(double));
    a3_complex        = (complex*)  malloc (n * sizeof(complex));
    a3_double_complex = (double_complex*) malloc (n * sizeof(double_complex));

    r3_byte           = (int8_t*)   malloc (n * sizeof(int8_t));
    r3_short          = (int16_t*)  malloc (n * sizeof(int16_t));
    r3_int            = (int32_t*)  malloc (n * sizeof(int32_t));
    r3_long           = (int64_t*)  malloc (n * sizeof(int64_t));
    r3_ubyte          = (uint8_t*)  malloc (n * sizeof(uint8_t));
    r3_ushort         = (uint16_t*) malloc (n * sizeof(uint16_t));
    r3_uint           = (uint32_t*) malloc (n * sizeof(uint32_t));
    r3_ulong          = (uint64_t*) malloc (n * sizeof(uint64_t));
    r3_float          = (float*)    malloc (n * sizeof(float));
    r3_double         = (double*)   malloc (n * sizeof(double));
    r3_complex        = (complex*)  malloc (n * sizeof(complex));
    r3_double_complex = (double_complex*) malloc (n * sizeof(double_complex));

    for (i=0; i<n; i++)    a3_byte[i]   = rank % 256;
    for (i=0; i<n; i++)    a3_short[i]  = rank % 65536;
    for (i=0; i<n; i++)    a3_int[i]    = rank;
    for (i=0; i<n; i++)    a3_long[i]   = rank;
    for (i=0; i<n; i++)    a3_ubyte[i]  = rank % 256;
    for (i=0; i<n; i++)    a3_ushort[i] = rank % 65536;
    for (i=0; i<n; i++)    a3_uint[i]   = rank;
    for (i=0; i<n; i++)    a3_ulong[i]  = rank;
    for (i=0; i<n; i++)    a3_float[i]  = rank;
    for (i=0; i<n; i++)    a3_double[i] = rank;
    for (i=0; i<n; i++)    a3_complex[i].r = rank;
    for (i=0; i<n; i++)    a3_complex[i].i = rank;
    for (i=0; i<n; i++)    a3_double_complex[i].r = rank;
    for (i=0; i<n; i++)    a3_double_complex[i].i = rank;

    n = ldim1 * ldim2 * ldim3 * ldim4 * ldim5 * ldim6;
    a6_byte           = (int8_t*)   malloc (n * sizeof(int8_t));
    a6_short          = (int16_t*)  malloc (n * sizeof(int16_t));
    a6_int            = (int32_t*)  malloc (n * sizeof(int32_t));
    a6_long           = (int64_t*)  malloc (n * sizeof(int64_t));
    a6_ubyte          = (uint8_t*)  malloc (n * sizeof(uint8_t));
    a6_ushort         = (uint16_t*) malloc (n * sizeof(uint16_t));
    a6_uint           = (uint32_t*) malloc (n * sizeof(uint32_t));
    a6_ulong          = (uint64_t*) malloc (n * sizeof(uint64_t));
    a6_float          = (float*)    malloc (n * sizeof(float));
    a6_double         = (double*)   malloc (n * sizeof(double));
    a6_complex        = (complex*)  malloc (n * sizeof(complex));
    a6_double_complex = (double_complex*) malloc (n * sizeof(double_complex));

    r6_byte           = (int8_t*)   malloc (n * sizeof(int8_t));
    r6_short          = (int16_t*)  malloc (n * sizeof(int16_t));
    r6_int            = (int32_t*)  malloc (n * sizeof(int32_t));
    r6_long           = (int64_t*)  malloc (n * sizeof(int64_t));
    r6_ubyte          = (uint8_t*)  malloc (n * sizeof(uint8_t));
    r6_ushort         = (uint16_t*) malloc (n * sizeof(uint16_t));
    r6_uint           = (uint32_t*) malloc (n * sizeof(uint32_t));
    r6_ulong          = (uint64_t*) malloc (n * sizeof(uint64_t));
    r6_float          = (float*)    malloc (n * sizeof(float));
    r6_double         = (double*)   malloc (n * sizeof(double));
    r6_complex        = (complex*)  malloc (n * sizeof(complex));
    r6_double_complex = (double_complex*) malloc (n * sizeof(double_complex));

    for (i=0; i<n; i++)    a6_byte[i]   = rank % 256;
    for (i=0; i<n; i++)    a6_short[i]  = rank % 65536;
    for (i=0; i<n; i++)    a6_int[i]    = rank;
    for (i=0; i<n; i++)    a6_long[i]   = rank;
    for (i=0; i<n; i++)    a6_ubyte[i]  = rank % 256;
    for (i=0; i<n; i++)    a6_ushort[i] = rank % 65536;
    for (i=0; i<n; i++)    a6_uint[i]   = rank;
    for (i=0; i<n; i++)    a6_ulong[i]  = rank;
    for (i=0; i<n; i++)    a6_float[i]  = rank;
    for (i=0; i<n; i++)    a6_double[i] = rank;
    for (i=0; i<n; i++)    a6_complex[i].r = rank;
    for (i=0; i<n; i++)    a6_complex[i].i = rank;
    for (i=0; i<n; i++)    a6_double_complex[i].r = rank;
    for (i=0; i<n; i++)    a6_double_complex[i].i = rank;

}

void fini_vars()
{
    free (a1_byte);
    free (a1_short);
    free (a1_int);
    free (a1_long);
    free (a1_ubyte);
    free (a1_ushort);
    free (a1_uint);
    free (a1_ulong);
    free (a1_float);
    free (a1_double);
    free (a1_complex);
    free (a1_double_complex);

    free (a2_byte);
    free (a2_short);
    free (a2_int);
    free (a2_long);
    free (a2_ubyte);
    free (a2_ushort);
    free (a2_uint);
    free (a2_ulong);
    free (a2_float);
    free (a2_double);
    free (a2_complex);
    free (a2_double_complex);

    free (a3_byte);
    free (a3_short);
    free (a3_int);
    free (a3_long);
    free (a3_ubyte);
    free (a3_ushort);
    free (a3_uint);
    free (a3_ulong);
    free (a3_float);
    free (a3_double);
    free (a3_complex);
    free (a3_double_complex);

    free (a6_byte);
    free (a6_short);
    free (a6_int);
    free (a6_long);
    free (a6_ubyte);
    free (a6_ushort);
    free (a6_uint);
    free (a6_ulong);
    free (a6_float);
    free (a6_double);
    free (a6_complex);
    free (a6_double_complex);

    free (r1_byte);
    free (r1_short);
    free (r1_int);
    free (r1_long);
    free (r1_ubyte);
    free (r1_ushort);
    free (r1_uint);
    free (r1_ulong);
    free (r1_float);
    free (r1_double);
    free (r1_complex);
    free (r1_double_complex);

    free (r2_byte);
    free (r2_short);
    free (r2_int);
    free (r2_long);
    free (r2_ubyte);
    free (r2_ushort);
    free (r2_uint);
    free (r2_ulong);
    free (r2_float);
    free (r2_double);
    free (r2_complex);
    free (r2_double_complex);

    free (r3_byte);
    free (r3_short);
    free (r3_int);
    free (r3_long);
    free (r3_ubyte);
    free (r3_ushort);
    free (r3_uint);
    free (r3_ulong);
    free (r3_float);
    free (r3_double);
    free (r3_complex);
    free (r3_double_complex);

    free (r6_byte);
    free (r6_short);
    free (r6_int);
    free (r6_long);
    free (r6_ubyte);
    free (r6_ushort);
    free (r6_uint);
    free (r6_ulong);
    free (r6_float);
    free (r6_double);
    free (r6_complex);
    free (r6_double_complex);
}


int write_file (char *fname);
int read_file (char *fname);

int main (int argc, char ** argv) 
{
    int err; 

    MPI_Init (&argc, &argv);
    MPI_Comm_rank (comm, &rank);
    MPI_Comm_size (comm, &size);

    init_vars();
    adios_init ("write_read.xml", comm);
    err = adios_read_init_method(ADIOS_READ_METHOD_BP, comm, "verbose=2");
    if (err) {
        printE ("%s\n", adios_errmsg());
    }

    if (!err)
        err = write_file ("write_read_1.bp"); 
    if (!err)
        err = read_file ("write_read_1.bp"); 
    if (!err)
        err = write_file ("write_read_2.bp"); 
    if (!err)
        err = read_file ("write_read_2.bp"); 
    adios_finalize (rank);
    fini_vars();
    MPI_Finalize ();
    return err;
}


int write_file (char *fname) 
{
    int64_t       fh;
    uint64_t       groupsize=0, totalsize;
    int64_t       s;

    log ("Write data to %s\n", fname);
    adios_open (&fh, "alltypes", fname, "w", comm);
    
    s = 1+2+4+8 + 1+2+4+8 + 4+8 + 8+16; // sizeof different types
    groupsize  = 18*4;                       // dimensions 
    groupsize += s + strlen(scalar_string);  // scalars 
    groupsize += s * ldim1;                  // 1D 
    groupsize += s * ldim1 * ldim2;          // 2D 
    groupsize += s * ldim1 * ldim2 * ldim3;  // 3D
    groupsize += s * ldim1 * ldim2 * ldim3 * ldim4 * ldim5 * ldim6; // 6D
    adios_group_size (fh, groupsize, &totalsize);
    adios_write (fh, "gdim1", &gdim1);
    adios_write (fh, "gdim2", &gdim2);
    adios_write (fh, "gdim3", &gdim3);
    adios_write (fh, "gdim4", &gdim4);
    adios_write (fh, "gdim5", &gdim5);
    adios_write (fh, "gdim6", &gdim6);
    adios_write (fh, "ldim1", &ldim1);
    adios_write (fh, "ldim2", &ldim2);
    adios_write (fh, "ldim3", &ldim3);
    adios_write (fh, "ldim4", &ldim4);
    adios_write (fh, "ldim5", &ldim5);
    adios_write (fh, "ldim6", &ldim6);
    adios_write (fh, "offs1", &offs1);
    adios_write (fh, "offs2", &offs2);
    adios_write (fh, "offs3", &offs3);
    adios_write (fh, "offs4", &offs4);
    adios_write (fh, "offs5", &offs5);
    adios_write (fh, "offs6", &offs6);
    adios_write (fh, "/scalars/scalar_byte", &scalar_byte);
    adios_write (fh, "/scalars/scalar_short", &scalar_short);
    adios_write (fh, "/scalars/scalar_int", &scalar_int);
    adios_write (fh, "/scalars/scalar_long", &scalar_long);
    adios_write (fh, "/scalars/scalar_ubyte", &scalar_ubyte);
    adios_write (fh, "/scalars/scalar_ushort", &scalar_ushort);
    adios_write (fh, "/scalars/scalar_uint", &scalar_uint);
    adios_write (fh, "/scalars/scalar_ulong", &scalar_ulong);
    adios_write (fh, "/scalars/scalar_float", &scalar_float);
    adios_write (fh, "/scalars/scalar_double", &scalar_double);
    adios_write (fh, "/scalars/scalar_string", scalar_string);
    adios_write (fh, "/scalars/scalar_complex", &scalar_complex);
    adios_write (fh, "/scalars/scalar_double_complex", &scalar_double_complex);
    adios_write (fh, "/1D/a1_byte", a1_byte);
    adios_write (fh, "/1D/a1_short", a1_short);
    adios_write (fh, "/1D/a1_int", a1_int);
    adios_write (fh, "/1D/a1_long", a1_long);
    adios_write (fh, "/1D/a1_ubyte", a1_ubyte);
    adios_write (fh, "/1D/a1_ushort", a1_ushort);
    adios_write (fh, "/1D/a1_uint", a1_uint);
    adios_write (fh, "/1D/a1_ulong", a1_ulong);
    adios_write (fh, "/1D/a1_float", a1_float);
    adios_write (fh, "/1D/a1_double", a1_double);
    adios_write (fh, "/1D/a1_complex", a1_complex);
    adios_write (fh, "/1D/a1_double_complex", a1_double_complex);
    adios_write (fh, "/2D/a2_byte", a2_byte);
    adios_write (fh, "/2D/a2_short", a2_short);
    adios_write (fh, "/2D/a2_int", a2_int);
    adios_write (fh, "/2D/a2_long", a2_long);
    adios_write (fh, "/2D/a2_ubyte", a2_ubyte);
    adios_write (fh, "/2D/a2_ushort", a2_ushort);
    adios_write (fh, "/2D/a2_uint", a2_uint);
    adios_write (fh, "/2D/a2_ulong", a2_ulong);
    adios_write (fh, "/2D/a2_float", a2_float);
    adios_write (fh, "/2D/a2_double", a2_double);
    adios_write (fh, "/2D/a2_complex", a2_complex);
    adios_write (fh, "/2D/a2_double_complex", a2_double_complex);
    adios_write (fh, "/3D/a3_byte", a3_byte);
    adios_write (fh, "/3D/a3_short", a3_short);
    adios_write (fh, "/3D/a3_int", a3_int);
    adios_write (fh, "/3D/a3_long", a3_long);
    adios_write (fh, "/3D/a3_ubyte", a3_ubyte);
    adios_write (fh, "/3D/a3_ushort", a3_ushort);
    adios_write (fh, "/3D/a3_uint", a3_uint);
    adios_write (fh, "/3D/a3_ulong", a3_ulong);
    adios_write (fh, "/3D/a3_float", a3_float);
    adios_write (fh, "/3D/a3_double", a3_double);
    adios_write (fh, "/3D/a3_complex", a3_complex);
    adios_write (fh, "/3D/a3_double_complex", a3_double_complex);
    adios_write (fh, "/6D/a6_byte", a6_byte);
    adios_write (fh, "/6D/a6_short", a6_short);
    adios_write (fh, "/6D/a6_int", a6_int);
    adios_write (fh, "/6D/a6_long", a6_long);
    adios_write (fh, "/6D/a6_ubyte", a6_ubyte);
    adios_write (fh, "/6D/a6_ushort", a6_ushort);
    adios_write (fh, "/6D/a6_uint", a6_uint);
    adios_write (fh, "/6D/a6_ulong", a6_ulong);
    adios_write (fh, "/6D/a6_float", a6_float);
    adios_write (fh, "/6D/a6_double", a6_double);
    adios_write (fh, "/6D/a6_complex", a6_complex);
    adios_write (fh, "/6D/a6_double_complex", a6_double_complex);

    adios_close (fh);
    MPI_Barrier (comm);
    return 0;
}


#define GET_SCALAR(VARNAME) \
    vi = adios_inq_var (f, VARNAME); \
    if (vi == NULL) { \
        printE ("No such variable %s\n", VARNAME); \
        err = 101; \
        goto endread; \
    } \

#define CHECK_SCALAR(VARNAME,VAR,TYPE) \
    GET_SCALAR(VARNAME) \
    if (*(TYPE*)vi->value != VAR) { \
        printE ("Scalar %s as read from file != as written\n", VARNAME); \
        err = 102; \
        goto endread; \
    } \
    adios_free_varinfo (vi);

#define CHECK_ARRAY(A,R) \
    for (i=0;i<n;i++) \
        if (A[i] != R[i]) { \
            printE ("Variable " #A " does not equal in written and read values at position %d\n", i);\
            err = 103; \
            goto endread;\
        }

void reset_rarrays()
{
    int n;
    
    n = ldim1;
    memset (r1_byte,    -1, n*sizeof(int8_t));
    memset (r1_short,   -1, n*sizeof(int16_t));
    memset (r1_int,     -1, n*sizeof(int32_t));
    memset (r1_long,    -1, n*sizeof(int64_t));
    memset (r1_ubyte,   -1, n*sizeof(uint8_t));
    memset (r1_ushort,  -1, n*sizeof(uint16_t));
    memset (r1_uint,    -1, n*sizeof(uint32_t));
    memset (r1_ulong,   -1, n*sizeof(uint64_t));
    memset (r1_float,   -1, n*sizeof(float));
    memset (r1_double,  -1, n*sizeof(double));
    memset (r1_complex, -1, n*sizeof(complex));
    memset (r1_double_complex, -1, n*sizeof(double_complex));

    n = ldim1 * ldim2;
    memset (r2_byte,    -1, n*sizeof(int8_t));
    memset (r2_short,   -1, n*sizeof(int16_t));
    memset (r2_int,     -1, n*sizeof(int32_t));
    memset (r2_long,    -1, n*sizeof(int64_t));
    memset (r2_ubyte,   -1, n*sizeof(uint8_t));
    memset (r2_ushort,  -1, n*sizeof(uint16_t));
    memset (r2_uint,    -1, n*sizeof(uint32_t));
    memset (r2_ulong,   -1, n*sizeof(uint64_t));
    memset (r2_float,   -1, n*sizeof(float));
    memset (r2_double,  -1, n*sizeof(double));
    memset (r2_complex, -1, n*sizeof(complex));
    memset (r2_double_complex, -1, n*sizeof(double_complex));

    n = ldim1 * ldim2 * ldim3;
    memset (r3_byte,    -1, n*sizeof(int8_t));
    memset (r3_short,   -1, n*sizeof(int16_t));
    memset (r3_int,     -1, n*sizeof(int32_t));
    memset (r3_long,    -1, n*sizeof(int64_t));
    memset (r3_ubyte,   -1, n*sizeof(uint8_t));
    memset (r3_ushort,  -1, n*sizeof(uint16_t));
    memset (r3_uint,    -1, n*sizeof(uint32_t));
    memset (r3_ulong,   -1, n*sizeof(uint64_t));
    memset (r3_float,   -1, n*sizeof(float));
    memset (r3_double,  -1, n*sizeof(double));
    memset (r3_complex, -1, n*sizeof(complex));
    memset (r3_double_complex, -1, n*sizeof(double_complex));

    n = ldim1 * ldim2 * ldim3 * ldim4 * ldim5 * ldim6;
    memset (r6_byte,    -1, n*sizeof(int8_t));
    memset (r6_short,   -1, n*sizeof(int16_t));
    memset (r6_int,     -1, n*sizeof(int32_t));
    memset (r6_long,    -1, n*sizeof(int64_t));
    memset (r6_ubyte,   -1, n*sizeof(uint8_t));
    memset (r6_ushort,  -1, n*sizeof(uint16_t));
    memset (r6_uint,    -1, n*sizeof(uint32_t));
    memset (r6_ulong,   -1, n*sizeof(uint64_t));
    memset (r6_float,   -1, n*sizeof(float));
    memset (r6_double,  -1, n*sizeof(double));
    memset (r6_complex, -1, n*sizeof(complex));
    memset (r6_double_complex, -1, n*sizeof(double_complex));
}

int read_file (char *fname)
{
    ADIOS_SELECTION *sel;
    ADIOS_FILE * f;
    ADIOS_VARINFO * vi;
    int err=0,i,n;

    uint64_t start[6] = {offs1,offs2,offs3,offs4,offs5,offs6};
    uint64_t count[6] = {ldim1,ldim2,ldim3,ldim4,ldim5,ldim6};
    uint64_t ndim;
    
    reset_rarrays();

    log ("Read and check data in %s\n", fname);
    f = adios_read_open_file (fname, ADIOS_READ_METHOD_BP, comm);
    if (f == NULL) {
        printE ("Error at opening file: %s\n", adios_errmsg());
        return 1;
    }

    log ("  Scalars... %s\n", fname);
    CHECK_SCALAR ("/scalars/scalar_byte",   scalar_byte,  int8_t)
    CHECK_SCALAR ("/scalars/scalar_short",  scalar_short, int16_t)
    CHECK_SCALAR ("/scalars/scalar_int",    scalar_int,   int32_t)
    CHECK_SCALAR ("/scalars/scalar_long",   scalar_long,  int64_t)
    CHECK_SCALAR ("/scalars/scalar_ubyte",  scalar_ubyte,  uint8_t)
    CHECK_SCALAR ("/scalars/scalar_ushort", scalar_ushort, uint16_t)
    CHECK_SCALAR ("/scalars/scalar_uint",   scalar_uint,   uint32_t)
    CHECK_SCALAR ("/scalars/scalar_ulong",  scalar_ulong,  uint64_t)
    CHECK_SCALAR ("/scalars/scalar_float",  scalar_float,   float)
    CHECK_SCALAR ("/scalars/scalar_double", scalar_double,  double)
    GET_SCALAR ("/scalars/scalar_complex")
    if (((complex*)vi->value)->r != scalar_complex.r ||
        ((complex*)vi->value)->i != scalar_complex.i) {
        printE ("Scalar scalar_complex as read from file != as written\n"); 
        err = 102; 
        goto endread;
    }
    adios_free_varinfo (vi);
    GET_SCALAR ("/scalars/scalar_double_complex")
    if (((double_complex*)vi->value)->r != scalar_double_complex.r ||
        ((double_complex*)vi->value)->i != scalar_double_complex.i) {
        printE ("Scalar scalar_double_complex as read from file != as written\n"); 
        err = 102; 
        goto endread;
    }
    adios_free_varinfo (vi);
    GET_SCALAR ("/scalars/scalar_string")
    if (strcmp((char*)vi->value, scalar_string)) {
        printE ("Scalar scalar_string as read from file != as written: [%s] and [%s]\n",
            (char*)vi->value, scalar_string); 
        err = 102; 
        goto endread;
    }
    adios_free_varinfo (vi);

    log ("  1D arrays... %s\n", fname);
    n = ldim1;
    sel = adios_selection_boundingbox (1, start, count); 
    adios_schedule_read (f, sel, "/1D/a1_byte", 0, 1, r1_byte);
    adios_schedule_read (f, sel, "/1D/a1_short", 0, 1, r1_short);
    adios_schedule_read (f, sel, "/1D/a1_int", 0, 1, r1_int);
    adios_schedule_read (f, sel, "/1D/a1_long", 0, 1, r1_long);
    adios_schedule_read (f, sel, "/1D/a1_ubyte", 0, 1, r1_ubyte);
    adios_schedule_read (f, sel, "/1D/a1_ushort", 0, 1, r1_ushort);
    adios_schedule_read (f, sel, "/1D/a1_uint", 0, 1, r1_uint);
    adios_schedule_read (f, sel, "/1D/a1_ulong", 0, 1, r1_ulong);
    adios_schedule_read (f, sel, "/1D/a1_float", 0, 1, r1_float);
    adios_schedule_read (f, sel, "/1D/a1_double", 0, 1, r1_double);
    adios_schedule_read (f, sel, "/1D/a1_complex", 0, 1, r1_complex);
    adios_schedule_read (f, sel, "/1D/a1_double_complex", 0, 1, r1_double_complex);
    adios_perform_reads (f, 1);
    adios_selection_delete (sel);

    CHECK_ARRAY (a1_byte,   r1_byte)
    CHECK_ARRAY (a1_short,  r1_short)
    CHECK_ARRAY (a1_int,    r1_int)
    CHECK_ARRAY (a1_long,   r1_long)
    CHECK_ARRAY (a1_ubyte,  r1_ubyte)
    CHECK_ARRAY (a1_ushort, r1_ushort)
    CHECK_ARRAY (a1_uint,   r1_uint)
    CHECK_ARRAY (a1_ulong,  r1_ulong)
    CHECK_ARRAY (a1_float,  r1_float)
    CHECK_ARRAY (a1_double, r1_double)
    for (i=0;i<n;i++)
        if (r1_complex[i].r != a1_complex[i].r ||
            r1_complex[i].i != a1_complex[i].i) {
              printE ("Variable a1_complex does not equal in written and read values at position %d\n",i);
              err = 103; 
              goto endread;
        }
    for (i=0;i<n;i++)
        if (r1_double_complex[i].r != a1_double_complex[i].r ||
            r1_double_complex[i].i != a1_double_complex[i].i) {
              printE ("Variable a1_double_complex does not equal in written and read values at position %d\n",i);
              err = 103; 
              goto endread;
        }

    log ("  2D arrays... %s\n", fname);
    n = ldim1 * ldim2;
    sel = adios_selection_boundingbox (2, start, count); 
    adios_schedule_read (f, sel, "/2D/a2_byte", 0, 1, r2_byte);
    adios_schedule_read (f, sel, "/2D/a2_short", 0, 1, r2_short);
    adios_schedule_read (f, sel, "/2D/a2_int", 0, 1, r2_int);
    adios_schedule_read (f, sel, "/2D/a2_long", 0, 1, r2_long);
    adios_schedule_read (f, sel, "/2D/a2_ubyte", 0, 1, r2_ubyte);
    adios_schedule_read (f, sel, "/2D/a2_ushort", 0, 1, r2_ushort);
    adios_schedule_read (f, sel, "/2D/a2_uint", 0, 1, r2_uint);
    adios_schedule_read (f, sel, "/2D/a2_ulong", 0, 1, r2_ulong);
    adios_schedule_read (f, sel, "/2D/a2_float", 0, 1, r2_float);
    adios_schedule_read (f, sel, "/2D/a2_double", 0, 1, r2_double);
    adios_schedule_read (f, sel, "/2D/a2_complex", 0, 1, r2_complex);
    adios_schedule_read (f, sel, "/2D/a2_double_complex", 0, 1, r2_double_complex);
    adios_perform_reads (f, 1);
    adios_selection_delete (sel);

    CHECK_ARRAY (a2_byte,   r2_byte)
    CHECK_ARRAY (a2_short,  r2_short)
    CHECK_ARRAY (a2_int,    r2_int)
    CHECK_ARRAY (a2_long,   r2_long)
    CHECK_ARRAY (a2_ubyte,  r2_ubyte)
    CHECK_ARRAY (a2_ushort, r2_ushort)
    CHECK_ARRAY (a2_uint,   r2_uint)
    CHECK_ARRAY (a2_ulong,  r2_ulong)
    CHECK_ARRAY (a2_float,  r2_float)
    CHECK_ARRAY (a2_double, r2_double)
    for (i=0;i<n;i++)
        if (r2_complex[i].r != a2_complex[i].r ||
            r2_complex[i].i != a2_complex[i].i) {
              printE ("Variable a2_complex does not equal in written and read values at position %d\n",i);
              err = 103; 
              goto endread;
        }
    for (i=0;i<n;i++)
        if (r2_double_complex[i].r != a2_double_complex[i].r ||
            r2_double_complex[i].i != a2_double_complex[i].i) {
              printE ("Variable a2_double_complex does not equal in written and read values at position %d\n",i);
              err = 103; 
              goto endread;
        }


    log ("  3D arrays... %s\n", fname);
    n = ldim1 * ldim2 * ldim3;
    sel = adios_selection_boundingbox (3, start, count); 
    adios_schedule_read (f, sel, "/3D/a3_byte", 0, 1, r3_byte);
    adios_schedule_read (f, sel, "/3D/a3_short", 0, 1, r3_short);
    adios_schedule_read (f, sel, "/3D/a3_int", 0, 1, r3_int);
    adios_schedule_read (f, sel, "/3D/a3_long", 0, 1, r3_long);
    adios_schedule_read (f, sel, "/3D/a3_ubyte", 0, 1, r3_ubyte);
    adios_schedule_read (f, sel, "/3D/a3_ushort", 0, 1, r3_ushort);
    adios_schedule_read (f, sel, "/3D/a3_uint", 0, 1, r3_uint);
    adios_schedule_read (f, sel, "/3D/a3_ulong", 0, 1, r3_ulong);
    adios_schedule_read (f, sel, "/3D/a3_float", 0, 1, r3_float);
    adios_schedule_read (f, sel, "/3D/a3_double", 0, 1, r3_double);
    adios_schedule_read (f, sel, "/3D/a3_complex", 0, 1, r3_complex);
    adios_schedule_read (f, sel, "/3D/a3_double_complex", 0, 1, r3_double_complex);
    adios_perform_reads (f, 1);
    adios_selection_delete (sel);

    CHECK_ARRAY (a3_byte,   r3_byte)
    CHECK_ARRAY (a3_short,  r3_short)
    CHECK_ARRAY (a3_int,    r3_int)
    CHECK_ARRAY (a3_long,   r3_long)
    CHECK_ARRAY (a3_ubyte,  r3_ubyte)
    CHECK_ARRAY (a3_ushort, r3_ushort)
    CHECK_ARRAY (a3_uint,   r3_uint)
    CHECK_ARRAY (a3_ulong,  r3_ulong)
    CHECK_ARRAY (a3_float,  r3_float)
    CHECK_ARRAY (a3_double, r3_double)
    for (i=0;i<n;i++)
        if (r3_complex[i].r != a3_complex[i].r ||
            r3_complex[i].i != a3_complex[i].i) {
              printE ("Variable a3_complex does not equal in written and read values at position %d\n",i);
              err = 103; 
              goto endread;
        }
    for (i=0;i<n;i++)
        if (r3_double_complex[i].r != a3_double_complex[i].r ||
            r3_double_complex[i].i != a3_double_complex[i].i) {
              printE ("Variable a3_double_complex does not equal in written and read values at position %d\n",i);
              err = 103; 
              goto endread;
        }


    log ("  6D arrays... %s\n", fname);
    n = ldim1 * ldim2 * ldim3 * ldim4 * ldim5 * ldim6;
    sel = adios_selection_boundingbox (6, start, count); 
    adios_schedule_read (f, sel, "/6D/a6_byte", 0, 1, r6_byte);
    adios_schedule_read (f, sel, "/6D/a6_short", 0, 1, r6_short);
    adios_schedule_read (f, sel, "/6D/a6_int", 0, 1, r6_int);
    adios_schedule_read (f, sel, "/6D/a6_long", 0, 1, r6_long);
    adios_schedule_read (f, sel, "/6D/a6_ubyte", 0, 1, r6_ubyte);
    adios_schedule_read (f, sel, "/6D/a6_ushort", 0, 1, r6_ushort);
    adios_schedule_read (f, sel, "/6D/a6_uint", 0, 1, r6_uint);
    adios_schedule_read (f, sel, "/6D/a6_ulong", 0, 1, r6_ulong);
    adios_schedule_read (f, sel, "/6D/a6_float", 0, 1, r6_float);
    adios_schedule_read (f, sel, "/6D/a6_double", 0, 1, r6_double);
    adios_schedule_read (f, sel, "/6D/a6_complex", 0, 1, r6_complex);
    adios_schedule_read (f, sel, "/6D/a6_double_complex", 0, 1, r6_double_complex);
    adios_perform_reads (f, 1);
    adios_selection_delete (sel);

    CHECK_ARRAY (a6_byte,   r6_byte)
    CHECK_ARRAY (a6_short,  r6_short)
    CHECK_ARRAY (a6_int,    r6_int)
    CHECK_ARRAY (a6_long,   r6_long)
    CHECK_ARRAY (a6_ubyte,  r6_ubyte)
    CHECK_ARRAY (a6_ushort, r6_ushort)
    CHECK_ARRAY (a6_uint,   r6_uint)
    CHECK_ARRAY (a6_ulong,  r6_ulong)
    CHECK_ARRAY (a6_float,  r6_float)
    CHECK_ARRAY (a6_double, r6_double)
    for (i=0;i<n;i++)
        if (r6_complex[i].r != a6_complex[i].r ||
            r6_complex[i].i != a6_complex[i].i) {
              printE ("Variable a6_complex does not equal in written and read values at position %d\n",i);
              err = 103; 
              goto endread;
        }
    for (i=0;i<n;i++)
        if (r6_double_complex[i].r != a6_double_complex[i].r ||
            r6_double_complex[i].i != a6_double_complex[i].i) {
              printE ("Variable a6_double_complex does not equal in written and read values at position %d\n",i);
              err = 103; 
              goto endread;
        }


endread:
    adios_read_close(f);
    MPI_Barrier (comm);
    return err;
}
