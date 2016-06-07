#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>

int main(int argc, char *argv[])
{
  int num_procs, num_local;
  char mach_name[MPI_MAX_PROCESSOR_NAME];
  int mach_len;
  int globalMax;

  MPI_Init (&argc,&argv);
  MPI_Comm_size (MPI_COMM_WORLD, &num_procs);
  MPI_Comm_rank (MPI_COMM_WORLD, &num_local);

  MPI_Allreduce(&num_local,&globalMax,1,MPI_INT,MPI_MAX,MPI_COMM_WORLD);
  assert(globalMax == (num_procs -1));

  MPI_Finalize();
  return 0;
}
