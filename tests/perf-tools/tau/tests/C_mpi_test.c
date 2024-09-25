#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define SIZE 10

void func(int me, int proc) {
  int i;
  int field[SIZE];
  MPI_Status status;

  for (i=0; i<SIZE; i++)
    field[i] = i;

  MPI_Barrier(MPI_COMM_WORLD);

  for (i=0; i<3; ++i) {
    if (me==0) {
      MPI_Send(&field, SIZE, MPI_INT, 1, 4711, MPI_COMM_WORLD);
      MPI_Recv(&field, SIZE, MPI_INT, proc-1, 4711, MPI_COMM_WORLD, &status);
    }
    else {
      MPI_Recv(&field, SIZE, MPI_INT, me-1, 4711, MPI_COMM_WORLD, &status);
      MPI_Send(&field, SIZE, MPI_INT, (me+1)%proc, 4711, MPI_COMM_WORLD);
    }
  }
  MPI_Bcast (&field, SIZE, MPI_INT, 0, MPI_COMM_WORLD);
  printf("%d done.\n", me);
}

int main(int argc, char **argv) {
  int proc, me;

  sleep(2);
  MPI_Init (&argc, & argv);
  sleep(3);
  MPI_Comm_size (MPI_COMM_WORLD, &proc);
  MPI_Comm_rank (MPI_COMM_WORLD, &me);

  func(me, proc);
    
  MPI_Finalize ();
}
