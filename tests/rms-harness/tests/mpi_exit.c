// Simple MPI code that can exit with user-supplied exit code. Used to
// verify correct test harness behavior.

#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
  int num_procs, num_local;
  char mach_name[MPI_MAX_PROCESSOR_NAME];
  int mach_len;

  int desiredExitCode;

  if(argc > 1)
    desiredExitCode = atoi(argv[1]);
  else
    desiredExitCode = 0;

  MPI_Init (&argc,&argv);
  MPI_Comm_size (MPI_COMM_WORLD, &num_procs);
  MPI_Comm_rank (MPI_COMM_WORLD, &num_local);

  MPI_Finalize();
  return desiredExitCode;
}
