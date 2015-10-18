/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

/* ADIOS test: declare multiple groups and free them. 
 * Test issue that was fixed in 084d601de8fca676361b0ac4b96735e04295fa0f
 *
 * How to run: group_free_test
 * Output: None
 * ADIOS config file: None
 *
 * This is a sequential test.
*/

/* This example will create 3 adios groups for writing, then frees them.
*/
#ifndef _NOMPI
#define _NOMPI
#endif

#include <stdio.h>
#include <string.h>
#include "adios.h"
#include "adios_types.h"

const int  NBLOCKS = 3;
const int  NGROUPS = 3;

int main (int argc, char ** argv) 
{
	int         rank, size, i, j;
	int         NX = 100, gb, offset;  //local/global/offset
	MPI_Comm    comm = MPI_COMM_WORLD;
        char g_str[100], o_str[100], l_str[100];
        char groupname[256];

	MPI_Init (&argc, &argv);
	MPI_Comm_rank (comm, &rank);
	MPI_Comm_size (comm, &size);

        gb = NBLOCKS * NX * size;
        sprintf (g_str, "%d", gb);
        sprintf (l_str, "%d", NX);


	adios_init_noxml (comm);
        adios_allocate_buffer (ADIOS_BUFFER_ALLOC_NOW, 10);

        int64_t   groupid[NGROUPS];
        int64_t   var_ids[NGROUPS][NBLOCKS];

        for (j = 0; j < NGROUPS; j++) 
        {
            sprintf (groupname, "group%1.1d", j);
            adios_declare_group (&groupid[j], groupname, "", adios_flag_yes);
            adios_select_method (groupid[j], "MPI", "", "");

            for (i = 0; i < NBLOCKS; i++) 
            {
                offset = rank * NBLOCKS * NX + i * NX;
                sprintf (o_str, "%d", offset);
                var_ids[j][i] = adios_define_var (groupid[j], "temperature"
                        ,"", adios_double
                        ,l_str, g_str, o_str
                        );
                adios_set_transform (var_ids[j][i], "identity");
            }
        }

        MPI_Barrier (comm);

        // free groups in an illogical order 1, 0, 2 [3 4 ...]
        if (NGROUPS > 1) adios_free_group (groupid[1]);
        adios_free_group (groupid[0]);
        for (j = 2; j < NGROUPS; j++) 
        {
            adios_free_group (groupid[j]);
        }

	adios_finalize (rank);
	MPI_Finalize ();
	return 0;
}
