/*  Approximate pi with the n-point rectangle quadrature rule */
/*  applied to the integral from 0 to 1 of 4 / (1+x**2).c */

#define MAXN    100000

#include <mpi.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
  double pi  = 0.0; /* The calculated result */
  double my_pi;     /* The partial pi calculated by one processor */
  int    n   = 0;   /* Number of points of integration */
  double h;         /* Reciprocal of n, interval size */
  double x;         /* Midpoint of each rectangle's interval */
  double f;         /* Function to integrate */
  double sum;       /* Area of rectangles */
  double st, ut, time;

  int    i; /* do loop index */

  int nr_procs; /* Number of processors */
  int my_rank;  /* Processor's id */

  /* Initialize MPI */
  MPI_Init(&argc, &argv);

  /* Number of processes */
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

  /* Get the current processor's id */
  MPI_Comm_size(MPI_COMM_WORLD, &nr_procs);

  for(n=10;n<MAXN;n*=10) {

    /* start time measurement */
    MPI_Barrier(MPI_COMM_WORLD);
    st = MPI_Wtime();

    /* Send n to all other processors */
    MPI_Bcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);

    h   = 1.0 / n; /* Calculate interval size */
    sum = 0.0;     /* Initialize sum */


    /* Calculate partial sums */
    for(i = my_rank + 1; i <= n; i = i + nr_procs) {

      x   = (i - 0.5) * h;
      f   = 4.0 / (1.0 + x * x);
      sum = sum + f;
    }

    my_pi = h * sum;

    /* Reduction - all subsums are added */
    MPI_Reduce(&my_pi, &pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

    ut = MPI_Wtime()-st;
    MPI_Reduce(&ut, &time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);

    /* Output - done by root processor */
    if(my_rank == 0) {
      printf("%02d of %02d: Value of pi is: %20.16lf, n=%9d, calculated in %10.8e s\n", my_rank, nr_procs, pi, n, time);
    }

  }

  /* Complete MPI */
  MPI_Finalize();

  return 0;
}
