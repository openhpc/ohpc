/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS C Example: write a global array from N processors with gwrite
 *
 * How to run: mpirun -np <N> adios_global
 * Output: adios_global.bp
 * ADIOS config file: adios_global.xml
 *
*/
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/ioctl.h>
//#include <lustre/lustre_user.h>
//#endif
// from /usr/include/lustre/lustre_user.h
#define LUSTRE_SUPER_MAGIC 0x0BD00BD0
#  define LOV_USER_MAGIC 0x0BD10BD0
#  define LL_IOC_LOV_SETSTRIPE  _IOW ('f', 154, long)
#  define LL_IOC_LOV_GETSTRIPE  _IOW ('f', 155, long)
#define O_LOV_DELAY_CREATE 0100000000

struct lov_user_ost_data {           // per-stripe data structure
        uint64_t l_object_id;        // OST object ID
        uint64_t l_object_gr;        // OST object group (creating MDS number)
        uint32_t l_ost_gen;          // generation of this OST index
        uint32_t l_ost_idx;          // OST index in LOV
} __attribute__((packed));
struct lov_user_md {                 // LOV EA user data (host-endian)
        uint32_t lmm_magic;          // magic number = LOV_USER_MAGIC_V1
        uint32_t lmm_pattern;        // LOV_PATTERN_RAID0, LOV_PATTERN_RAID1
        uint64_t lmm_object_id;      // LOV object ID
        uint64_t lmm_object_gr;      // LOV object group
        uint32_t lmm_stripe_size;    // size of stripe in bytes
        uint16_t lmm_stripe_count;   // num stripes in use for this object
        uint16_t lmm_stripe_offset;  // starting stripe offset in lmm_objects
        struct lov_user_ost_data  lmm_objects[0]; // per-stripe data
} __attribute__((packed));
struct obd_uuid {
        char uuid[40];
};

#include "mpi.h"
#ifdef HAVE_FGR
#include "fgr.h"
#endif

int main (int argc, char ** argv) 
{
	char        filename [256];
	int         rank, size, i;
	int         NX = 262144*2*2*2*2*2*2*2*2*2;
	double      t[NX];
	MPI_Comm    comm = MPI_COMM_WORLD;
        struct lov_user_md lum;
        int f;

	MPI_Init (&argc, &argv);
	MPI_Comm_rank (comm, &rank);
	MPI_Comm_size (comm, &size);

#ifdef HAVE_FGR
        if (fgr_init (0) == false)
        {
            fprintf (stderr, "fgr_init() error\n");
        }
#endif
	for (i = 0; i < NX; i++)
		t[i] = rank*NX + i;

        sprintf (filename, "%s.%d", "posix_fgr.bin", rank);

        MPI_Barrier (MPI_COMM_WORLD);
        double start_time = MPI_Wtime();

        f = open(filename, O_CREAT | O_RDWR | O_LOV_DELAY_CREATE, 0644);
        if (f == -1)
        {
            fprintf (stderr,"open() failed: %s\n", filename);
            return -1;
        }

        lum.lmm_magic = LOV_USER_MAGIC;
        lum.lmm_pattern = 0;
        lum.lmm_stripe_size = NX * 8;
        lum.lmm_stripe_count = 1;
#ifdef HAVE_FGR
        lum.lmm_stripe_offset = find_myost(comm);
//        lum.lmm_stripe_offset = rank;
#else
        lum.lmm_stripe_offset = -1;
#endif

        ioctl (f, LL_IOC_LOV_SETSTRIPE ,(void *) &lum);

        for (i = 0; i < 10; i++)
        {
	    if (write(f, t, NX*8) == -1)
            {
                fprintf (stderr, "write() error.\n");
            }
        }

        close (f);

        double io_time = MPI_Wtime() - start_time;
        double max;

        MPI_Reduce(&io_time, &max, 1,
                   MPI_DOUBLE, MPI_MAX, 0,
                   MPI_COMM_WORLD);

        if (rank == 0)
            printf ("time = %4.2f\n", max);
#ifdef HAVE_FGR
        fgr_finalize();
#endif
	MPI_Finalize ();

	return 0;
}
