#include "mpi.h"
#include <cassert>
#include <cstdlib>

int main(int argc, char *argv[])
{
  int num_procs, num_local;

  MPI::Init (argc,argv);
  num_procs = MPI::COMM_WORLD.Get_size();
  num_local = MPI::COMM_WORLD.Get_rank();

  /* compare avail MPI tasks to requested. If not requested via argv,
     convention is to assume 1 task */

  if(argc > 1)
    assert(num_procs == atoi(argv[1]));
  else
    assert(num_procs == 1);

  /* Verify a quick collective */

  int local=1;
  int global;

  MPI::COMM_WORLD.Allreduce(&local,&global,1,MPI::INT,MPI::SUM);

  assert(global == num_procs);

  MPI::Finalize();
  return 0;

}
