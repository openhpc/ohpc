#include <mpi.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
	int rank, size, token;
	MPI_Status status;

	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	if (rank != 0) {
		MPI_Recv(&token, 1, MPI_INT, rank - 1, 0, MPI_COMM_WORLD,
			 &status);
		printf("Process %d received token %d from process %d\n", rank,
		       token, rank - 1);
	} else {
		token = 42;
	}

	MPI_Send(&token, 1, MPI_INT, (rank + 1) % size, 0, MPI_COMM_WORLD);

	if (rank == 0) {
		MPI_Recv(&token, 1, MPI_INT, size - 1, 0, MPI_COMM_WORLD,
			 &status);
		printf("Process %d received token %d from process %d\n", rank,
		       token, size - 1);
	}

	MPI_Finalize();
	return 0;
}
