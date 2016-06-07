
/*  Write a lot of text
*/

#include <stdio.h>
#include <string.h>
#include "adios.h"
#include "adios_types.h"

#ifdef DMALLOC
#include "dmalloc.h"
#endif

int main (int argc, char ** argv) 
{
	char        filename [256];
	int         rank, size, i, j, step, block;
        int         Offset; 

	int         NX = 2; // number of records written per step per process
        int         Width=20;
        int         sub_blocks = 2; // number of record-blocks written per process in one step
        int         steps = 3;

	char        t[NX][Width];
	MPI_Comm    comm = MPI_COMM_WORLD;

	/* ADIOS variables declarations for matching gwrite_temperature.ch */
	int         adios_err;
	uint64_t    adios_groupsize, adios_totalsize;
	int64_t     adios_handle;

	MPI_Init (&argc, &argv);
	MPI_Comm_rank (comm, &rank);
	MPI_Comm_size (comm, &size);

        //Global_bounds = sub_blocks * NX * size;

	strcpy (filename, "steps.bp");

	adios_init_noxml (comm);
        adios_allocate_buffer (ADIOS_BUFFER_ALLOC_NOW, 1);

        int64_t       m_adios_group;
        int64_t       m_adios_file;

        adios_declare_group (&m_adios_group, "steps", "", adios_flag_yes);
        adios_select_method (m_adios_group, "MPI", "", "");


        adios_define_var (m_adios_group, "NX" ,"", adios_integer ,0, 0, 0);
        adios_define_var (m_adios_group, "Width" ,"", adios_integer ,0, 0, 0);
        adios_define_var (m_adios_group, "nproc" ,"", adios_integer ,0, 0, 0);
   
        for (i=0;i<sub_blocks;i++) {
   
           adios_define_var (m_adios_group, "record" ,"", adios_byte ,"NX,Width", "", "");
        }


        for (step=0; step<steps; step++) {

            adios_open (&m_adios_file, "steps", filename, "a", comm);

            adios_groupsize = sub_blocks * (4 + 4 + 4 + (uint64_t) NX * (uint64_t)Width);

            adios_group_size (m_adios_file, adios_groupsize, &adios_totalsize);
            adios_write(m_adios_file, "nproc", (void *) &size);
            adios_write(m_adios_file, "NX", (void *) &NX);
            adios_write(m_adios_file, "Width", (void *) &Width);
            /* now we will write the data for each sub block */
            for (block=0;block<sub_blocks;block++) {

                for (i = 0; i < NX; i++)
                    //print 19 chars here + '\0'
                    sprintf (t[i], "r%2d  b%2d  s%2d  i%2d ", rank, block, step, i); 

                adios_write(m_adios_file, "record", t);
            }

            adios_close (m_adios_file);
        }

        MPI_Barrier (comm);

	adios_finalize (rank);

	MPI_Finalize ();
	return 0;
}
