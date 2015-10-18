/*
   Example 3

   Interface:      Structured interface (Struct)

   Compile with:   make ex3

   Sample run:     mpirun -np 16 ex3 -n 33 -solver 0 -v 1 1

   To see options: ex3 -help

   Description:    This code solves a system corresponding to a discretization
                   of the Laplace equation -Delta u = 1 with zero boundary
                   conditions on the unit square.  The domain is split into
                   an N x N processor grid.  Thus, the given number of processors
                   should be a perfect square.  Each processor's piece of the
                   grid has n x n cells with n x n nodes connected by the
                   standard 5-point stencil. Note that the struct interface
                   assumes a cell-centered grid, and, therefore, the nodes are
                   not shared.  This example demonstrates more features than the
                   previous two struct examples (Example 1 and Example 2).  Two
                   solvers are available.

                   To incorporate the boundary conditions, we do the following:
                   Let x_i and x_b be the interior and boundary parts of the
                   solution vector x. We can split the matrix A as
                                   A = [A_ii A_ib; A_bi A_bb].
                   Let u_0 be the Dirichlet B.C.  We can simply say that x_b = u_0.
                   If b_i is the right-hand side, then we just need to solve in
                   the interior:
                                    A_ii x_i = b_i - A_ib u_0.
                   For this partitcular example, u_0 = 0, so we are just solving
                   A_ii x_i = b_i.

                   We recommend viewing examples 1 and 2 before viewing this
                   example.
*/

#include <math.h>
#include "_hypre_utilities.h"
#include "HYPRE_struct_ls.h"

#include "vis.c"

int main (int argc, char *argv[])
{
   int i, j, k;

   int myid, num_procs;

   int n, N, pi, pj;
   double h, h2;
   int ilower[2], iupper[2];

   int solver_id;
   int n_pre, n_post;

   HYPRE_StructGrid     grid;
   HYPRE_StructStencil  stencil;
   HYPRE_StructMatrix   A;
   HYPRE_StructVector   b;
   HYPRE_StructVector   x;
   HYPRE_StructSolver   solver;
   HYPRE_StructSolver   precond;

   int num_iterations;
   double final_res_norm;

   int vis;

   /* Initialize MPI */
   MPI_Init(&argc, &argv);
   MPI_Comm_rank(MPI_COMM_WORLD, &myid);
   MPI_Comm_size(MPI_COMM_WORLD, &num_procs);

   /* Set defaults */
   n = 33;
   solver_id = 0;
   n_pre  = 1;
   n_post = 1;
   vis = 0;

   /* Parse command line */
   {
      int arg_index = 0;
      int print_usage = 0;

      while (arg_index < argc)
      {
         if ( strcmp(argv[arg_index], "-n") == 0 )
         {
            arg_index++;
            n = atoi(argv[arg_index++]);
         }
         else if ( strcmp(argv[arg_index], "-solver") == 0 )
         {
            arg_index++;
            solver_id = atoi(argv[arg_index++]);
         }
         else if ( strcmp(argv[arg_index], "-v") == 0 )
         {
            arg_index++;
            n_pre = atoi(argv[arg_index++]);
            n_post = atoi(argv[arg_index++]);
         }
         else if ( strcmp(argv[arg_index], "-vis") == 0 )
         {
            arg_index++;
            vis = 1;
         }
         else if ( strcmp(argv[arg_index], "-help") == 0 )
         {
            print_usage = 1;
            break;
         }
         else
         {
            arg_index++;
         }
      }

      if ((print_usage) && (myid == 0))
      {
         printf("\n");
         printf("Usage: %s [<options>]\n", argv[0]);
         printf("\n");
         printf("  -n <n>              : problem size per processor (default: 33)\n");
         printf("  -solver <ID>        : solver ID\n");
         printf("                        0  - PCG with SMG precond (default)\n");
         printf("                        1  - SMG\n");
         printf("  -v <n_pre> <n_post> : number of pre and post relaxations (default: 1 1)\n");
         printf("  -vis                : save the solution for GLVis visualization\n");
         printf("\n");
      }

      if (print_usage)
      {
         MPI_Finalize();
         return (0);
      }
   }

   /* Figure out the processor grid (N x N).  The local problem
      size for the interior nodes is indicated by n (n x n).
      pi and pj indicate position in the processor grid. */
   N  = sqrt(num_procs);
   h  = 1.0 / (N*n+1); /* note that when calculating h we must
                          remember to count the boundary nodes */
   h2 = h*h;
   pj = myid / N;
   pi = myid - pj*N;

   /* Figure out the extents of each processor's piece of the grid. */
   ilower[0] = pi*n;
   ilower[1] = pj*n;

   iupper[0] = ilower[0] + n-1;
   iupper[1] = ilower[1] + n-1;

   /* 1. Set up a grid */
   {
      /* Create an empty 2D grid object */
      HYPRE_StructGridCreate(MPI_COMM_WORLD, 2, &grid);

      /* Add a new box to the grid */
      HYPRE_StructGridSetExtents(grid, ilower, iupper);

      /* This is a collective call finalizing the grid assembly.
         The grid is now ``ready to be used'' */
      HYPRE_StructGridAssemble(grid);
   }

   /* 2. Define the discretization stencil */
   {
      /* Create an empty 2D, 5-pt stencil object */
      HYPRE_StructStencilCreate(2, 5, &stencil);

      /* Define the geometry of the stencil */
      {
         int entry;
         int offsets[5][2] = {{0,0}, {-1,0}, {1,0}, {0,-1}, {0,1}};

         for (entry = 0; entry < 5; entry++)
            HYPRE_StructStencilSetElement(stencil, entry, offsets[entry]);
      }
   }

   /* 3. Set up a Struct Matrix */
   {
      int nentries = 5;
      int nvalues = nentries*n*n;
      double *values;
      int stencil_indices[5];

      /* Create an empty matrix object */
      HYPRE_StructMatrixCreate(MPI_COMM_WORLD, grid, stencil, &A);

      /* Indicate that the matrix coefficients are ready to be set */
      HYPRE_StructMatrixInitialize(A);

      values = calloc(nvalues, sizeof(double));

      for (j = 0; j < nentries; j++)
         stencil_indices[j] = j;

      /* Set the standard stencil at each grid point,
         we will fix the boundaries later */
      for (i = 0; i < nvalues; i += nentries)
      {
         values[i] = 4.0;
         for (j = 1; j < nentries; j++)
            values[i+j] = -1.0;
      }

      HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, nentries,
                                     stencil_indices, values);

      free(values);
   }

   /* 4. Incorporate the zero boundary conditions: go along each edge of
         the domain and set the stencil entry that reaches to the boundary to
         zero.*/
   {
      int bc_ilower[2];
      int bc_iupper[2];
      int nentries = 1;
      int nvalues  = nentries*n; /*  number of stencil entries times the length
                                     of one side of my grid box */
      double *values;
      int stencil_indices[1];

      values = calloc(nvalues, sizeof(double));
      for (j = 0; j < nvalues; j++)
         values[j] = 0.0;

      /* Recall: pi and pj describe position in the processor grid */
      if (pj == 0)
      {
         /* Bottom row of grid points */
         bc_ilower[0] = pi*n;
         bc_ilower[1] = pj*n;

         bc_iupper[0] = bc_ilower[0] + n-1;
         bc_iupper[1] = bc_ilower[1];

         stencil_indices[0] = 3;

         HYPRE_StructMatrixSetBoxValues(A, bc_ilower, bc_iupper, nentries,
                                        stencil_indices, values);
      }

      if (pj == N-1)
      {
         /* upper row of grid points */
         bc_ilower[0] = pi*n;
         bc_ilower[1] = pj*n + n-1;

         bc_iupper[0] = bc_ilower[0] + n-1;
         bc_iupper[1] = bc_ilower[1];

         stencil_indices[0] = 4;

         HYPRE_StructMatrixSetBoxValues(A, bc_ilower, bc_iupper, nentries,
                                        stencil_indices, values);
      }

      if (pi == 0)
      {
         /* Left row of grid points */
         bc_ilower[0] = pi*n;
         bc_ilower[1] = pj*n;

         bc_iupper[0] = bc_ilower[0];
         bc_iupper[1] = bc_ilower[1] + n-1;

         stencil_indices[0] = 1;

         HYPRE_StructMatrixSetBoxValues(A, bc_ilower, bc_iupper, nentries,
                                        stencil_indices, values);
      }

      if (pi == N-1)
      {
         /* Right row of grid points */
         bc_ilower[0] = pi*n + n-1;
         bc_ilower[1] = pj*n;

         bc_iupper[0] = bc_ilower[0];
         bc_iupper[1] = bc_ilower[1] + n-1;

         stencil_indices[0] = 2;

         HYPRE_StructMatrixSetBoxValues(A, bc_ilower, bc_iupper, nentries,
                                        stencil_indices, values);
      }

      free(values);
   }

   /* This is a collective call finalizing the matrix assembly.
      The matrix is now ``ready to be used'' */
   HYPRE_StructMatrixAssemble(A);

   /* 5. Set up Struct Vectors for b and x */
   {
      int    nvalues = n*n;
      double *values;

      values = calloc(nvalues, sizeof(double));

      /* Create an empty vector object */
      HYPRE_StructVectorCreate(MPI_COMM_WORLD, grid, &b);
      HYPRE_StructVectorCreate(MPI_COMM_WORLD, grid, &x);

      /* Indicate that the vector coefficients are ready to be set */
      HYPRE_StructVectorInitialize(b);
      HYPRE_StructVectorInitialize(x);

     /* Set the values */
      for (i = 0; i < nvalues; i ++)
         values[i] = h2;
      HYPRE_StructVectorSetBoxValues(b, ilower, iupper, values);

      for (i = 0; i < nvalues; i ++)
         values[i] = 0.0;
      HYPRE_StructVectorSetBoxValues(x, ilower, iupper, values);

      free(values);

      /* This is a collective call finalizing the vector assembly.
         The vector is now ``ready to be used'' */
      HYPRE_StructVectorAssemble(b);
      HYPRE_StructVectorAssemble(x);
   }

   /* 6. Set up and use a struct solver
      (Solver options can be found in the Reference Manual.) */
   if (solver_id == 0)
   {
      HYPRE_StructPCGCreate(MPI_COMM_WORLD, &solver);
      HYPRE_StructPCGSetMaxIter(solver, 50 );
      HYPRE_StructPCGSetTol(solver, 1.0e-06 );
      HYPRE_StructPCGSetTwoNorm(solver, 1 );
      HYPRE_StructPCGSetRelChange(solver, 0 );
      HYPRE_StructPCGSetPrintLevel(solver, 2 ); /* print each CG iteration */
      HYPRE_StructPCGSetLogging(solver, 1);

      /* Use symmetric SMG as preconditioner */
      HYPRE_StructSMGCreate(MPI_COMM_WORLD, &precond);
      HYPRE_StructSMGSetMemoryUse(precond, 0);
      HYPRE_StructSMGSetMaxIter(precond, 1);
      HYPRE_StructSMGSetTol(precond, 0.0);
      HYPRE_StructSMGSetZeroGuess(precond);
      HYPRE_StructSMGSetNumPreRelax(precond, 1);
      HYPRE_StructSMGSetNumPostRelax(precond, 1);

      /* Set the preconditioner and solve */
      HYPRE_StructPCGSetPrecond(solver, HYPRE_StructSMGSolve,
                                  HYPRE_StructSMGSetup, precond);
      HYPRE_StructPCGSetup(solver, A, b, x);
      HYPRE_StructPCGSolve(solver, A, b, x);

      /* Get some info on the run */
      HYPRE_StructPCGGetNumIterations(solver, &num_iterations);
      HYPRE_StructPCGGetFinalRelativeResidualNorm(solver, &final_res_norm);

      /* Clean up */
      HYPRE_StructPCGDestroy(solver);
   }

   if (solver_id == 1)
   {
      HYPRE_StructSMGCreate(MPI_COMM_WORLD, &solver);
      HYPRE_StructSMGSetMemoryUse(solver, 0);
      HYPRE_StructSMGSetMaxIter(solver, 50);
      HYPRE_StructSMGSetTol(solver, 1.0e-06);
      HYPRE_StructSMGSetRelChange(solver, 0);
      HYPRE_StructSMGSetNumPreRelax(solver, n_pre);
      HYPRE_StructSMGSetNumPostRelax(solver, n_post);
      /* Logging must be on to get iterations and residual norm info below */
      HYPRE_StructSMGSetLogging(solver, 1);

      /* Setup and solve */
      HYPRE_StructSMGSetup(solver, A, b, x);
      HYPRE_StructSMGSolve(solver, A, b, x);

      /* Get some info on the run */
      HYPRE_StructSMGGetNumIterations(solver, &num_iterations);
      HYPRE_StructSMGGetFinalRelativeResidualNorm(solver, &final_res_norm);

      /* Clean up */
      HYPRE_StructSMGDestroy(solver);
   }

   /* Save the solution for GLVis visualization, see vis/glvis-ex3.sh */
   if (vis)
   {
      FILE *file;
      char filename[255];

      int nvalues = n*n;
      double *values = calloc(nvalues, sizeof(double));

      /* get the local solution */
      HYPRE_StructVectorGetBoxValues(x, ilower, iupper, values);

      sprintf(filename, "%s.%06d", "vis/ex3.sol", myid);
      if ((file = fopen(filename, "w")) == NULL)
      {
         printf("Error: can't open output file %s\n", filename);
         MPI_Finalize();
         exit(1);
      }

      /* save solution with global unknown numbers */
      k = 0;
      for (j = 0; j < n; j++)
         for (i = 0; i < n; i++)
            fprintf(file, "%06d %.14e\n", pj*N*n*n+pi*n+j*N*n+i, values[k++]);

      fflush(file);
      fclose(file);
      free(values);

      /* save global finite element mesh */
      if (myid == 0)
         GLVis_PrintGlobalSquareMesh("vis/ex3.mesh", N*n-1);
   }

   if (myid == 0)
   {
      printf("\n");
      printf("Iterations = %d\n", num_iterations);
      printf("Final Relative Residual Norm = %g\n", final_res_norm);
      printf("\n");
   }

   /* Free memory */
   HYPRE_StructGridDestroy(grid);
   HYPRE_StructStencilDestroy(stencil);
   HYPRE_StructMatrixDestroy(A);
   HYPRE_StructVectorDestroy(b);
   HYPRE_StructVectorDestroy(x);

   /* Finalize MPI */
   MPI_Finalize();

   return (0);
}
