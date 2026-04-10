#include <mpi.h>
#include <cassert>
#include <cstdlib>

int main(int argc, char *argv[])
{
  int num_procs, num_local;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &num_procs);
  MPI_Comm_rank(MPI_COMM_WORLD, &num_local);

  /* compare avail MPI tasks to requested. If not requested via argv,
     convention is to assume 1 task */

  if(argc > 1)
    assert(num_procs == atoi(argv[1]));
  else
    assert(num_procs == 1);

  /* Verify a quick collective */

  int local=1;
  int global;

  MPI_Allreduce(&local, &global, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

  assert(global == num_procs);

  MPI_Finalize();
  return 0;

}
