#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(argc, argv, env)
     int argc;
     char *argv[];
     char *env[];

{
  int num_procs, num_local;
  char mach_name[MPI_MAX_PROCESSOR_NAME];
  int mach_len;

  MPI_Init (&argc,&argv);
  MPI_Comm_size (MPI_COMM_WORLD, &num_procs);
  MPI_Comm_rank (MPI_COMM_WORLD, &num_local);
  MPI_Get_processor_name(mach_name,&mach_len);

  MPI_Barrier(MPI_COMM_WORLD);

  if(num_local == 0)
    {
      printf("\n Hello, world (%i procs total)\n",num_procs);
      //printf("   -> Env. variable KOOMIE_TEST = %s\n",getenv("OMPI_MCA_ns_nds_vpid"));
      //      printf("   -> Env. variable KOOMIE_TEST = %s\n",getenv("LD_LIBRARY_PATH"));
    }

  MPI_Barrier(MPI_COMM_WORLD);

  printf("    --> Process # %3i of %3i is alive. ->%s\n",
	 num_local,num_procs,mach_name);

  MPI_Finalize();
  return 0;

}
