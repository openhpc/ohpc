/*
   Example 2

   Interface:    Structured interface (Struct)

   Compile with: make ex2

   Sample run:   mpirun -np 2 ex2

   Description:  This is a two processor example and is similar to the previous
                 structured interface example (Example 1). However, in
                 this case the grid boxes are exactly those in the example
                 diagram in the struct interface chapter of the User's Manual.
                 (Processor 0 owns two boxes and processor 1 owns one box.)
                 The solver is PCG with SMG preconditioner.

                 We recommend viewing example 1 before viewing this
                 example.
*/

#include <stdio.h>

/* Struct linear solvers header */
#include "HYPRE_struct_ls.h"

#include "vis.c"

int main (int argc, char *argv[])
{
   int i, j;

   int myid, num_procs;

   int vis = 0;

   HYPRE_StructGrid     grid;
   HYPRE_StructStencil  stencil;
   HYPRE_StructMatrix   A;
   HYPRE_StructVector   b;
   HYPRE_StructVector   x;
   HYPRE_StructSolver   solver;
   HYPRE_StructSolver   precond;

   /* Initialize MPI */
   MPI_Init(&argc, &argv);
   MPI_Comm_rank(MPI_COMM_WORLD, &myid);
   MPI_Comm_size(MPI_COMM_WORLD, &num_procs);

   if (num_procs != 2)
   {
      if (myid == 0) printf("Must run with 2 processors!\n");
      MPI_Finalize();

      return(0);
   }

   /* Parse command line */
   {
      int arg_index = 0;
      int print_usage = 0;

      while (arg_index < argc)
      {
         if ( strcmp(argv[arg_index], "-vis") == 0 )
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
         printf("  -vis : save the solution for GLVis visualization\n");
         printf("\n");
      }

      if (print_usage)
      {
         MPI_Finalize();
         return (0);
      }
   }

   /* 1. Set up a grid */
   {
      /* Create an empty 2D grid object */
      HYPRE_StructGridCreate(MPI_COMM_WORLD, 2, &grid);

      /* Processor 0 owns two boxes in the grid. */
      if (myid == 0)
      {
         /* Add a new box to the grid */
         {
            int ilower[2] = {-3, 1};
            int iupper[2] = {-1, 2};

            HYPRE_StructGridSetExtents(grid, ilower, iupper);
         }

         /* Add a new box to the grid */
         {
            int ilower[2] = {0, 1};
            int iupper[2] = {2, 4};

            HYPRE_StructGridSetExtents(grid, ilower, iupper);
         }
      }

      /* Processor 1 owns one box in the grid. */
      else if (myid == 1)
      {
         /* Add a new box to the grid */
         {
            int ilower[2] = {3, 1};
            int iupper[2] = {6, 4};

            HYPRE_StructGridSetExtents(grid, ilower, iupper);
         }
      }

      /* This is a collective call finalizing the grid assembly.
         The grid is now ``ready to be used'' */
      HYPRE_StructGridAssemble(grid);
   }

   /* 2. Define the discretization stencil */
   {
      /* Create an empty 2D, 5-pt stencil object */
      HYPRE_StructStencilCreate(2, 5, &stencil);

      /* Define the geometry of the stencil. Each represents a
         relative offset (in the index space). */
      {
         int entry;
         int offsets[5][2] = {{0,0}, {-1,0}, {1,0}, {0,-1}, {0,1}};

         /* Assign each of the 5 stencil entries */
         for (entry = 0; entry < 5; entry++)
            HYPRE_StructStencilSetElement(stencil, entry, offsets[entry]);
      }
   }

   /* 3. Set up a Struct Matrix */
   {
      /* Create an empty matrix object */
      HYPRE_StructMatrixCreate(MPI_COMM_WORLD, grid, stencil, &A);

      /* Indicate that the matrix coefficients are ready to be set */
      HYPRE_StructMatrixInitialize(A);

      if (myid == 0)
      {
         /* Set the matrix coefficients for some set of stencil entries
            over all the gridpoints in my first box (account for boundary
            grid points later) */
         {
            int ilower[2] = {-3, 1};
            int iupper[2] = {-1, 2};

            int nentries = 5;
            int nvalues  = 30; /* 6 grid points, each with 5 stencil entries */
            double values[30];

            int stencil_indices[5];
            for (j = 0; j < nentries; j++) /* label the stencil indices -
                                              these correspond to the offsets
                                              defined above */
               stencil_indices[j] = j;

            for (i = 0; i < nvalues; i += nentries)
            {
               values[i] = 4.0;
               for (j = 1; j < nentries; j++)
                  values[i+j] = -1.0;
            }

            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, nentries,
                                           stencil_indices, values);
         }

         /* Set the matrix coefficients for some set of stencil entries
            over the gridpoints in my second box */
         {
            int ilower[2] = {0, 1};
            int iupper[2] = {2, 4};

            int nentries = 5;
            int nvalues  = 60; /* 12 grid points, each with 5 stencil entries */
            double values[60];

            int stencil_indices[5];
            for (j = 0; j < nentries; j++)
               stencil_indices[j] = j;

            for (i = 0; i < nvalues; i += nentries)
            {
               values[i] = 4.0;
               for (j = 1; j < nentries; j++)
                  values[i+j] = -1.0;
            }

            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, nentries,
                                           stencil_indices, values);
         }
      }
      else if (myid == 1)
      {
         /* Set the matrix coefficients for some set of stencil entries
            over the gridpoints in my box */
         {
            int ilower[2] = {3, 1};
            int iupper[2] = {6, 4};

            int nentries = 5;
            int nvalues  = 80; /* 16 grid points, each with 5 stencil entries */
            double values[80];

            int stencil_indices[5];
            for (j = 0; j < nentries; j++)
               stencil_indices[j] = j;

            for (i = 0; i < nvalues; i += nentries)
            {
               values[i] = 4.0;
               for (j = 1; j < nentries; j++)
                  values[i+j] = -1.0;
            }

            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, nentries,
                                           stencil_indices, values);
         }
      }

      /* For each box, set any coefficients that reach ouside of the
         boundary to 0 */
      if (myid == 0)
      {
         int maxnvalues = 6;
         double values[6];

         for (i = 0; i < maxnvalues; i++)
            values[i] = 0.0;

         {
            /* Values below our first AND second box */
            int ilower[2] = {-3, 1};
            int iupper[2] = { 2, 1};

            int stencil_indices[1] = {3};

            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }

         {
            /* Values to the left of our first box */
            int ilower[2] = {-3, 1};
            int iupper[2] = {-3, 2};

            int stencil_indices[1] = {1};

            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }

         {
            /* Values above our first box */
            int ilower[2] = {-3, 2};
            int iupper[2] = {-1, 2};

            int stencil_indices[1] = {4};

            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }

         {
            /* Values to the left of our second box (that do not border the
               first box). */
            int ilower[2] = { 0, 3};
            int iupper[2] = { 0, 4};

            int stencil_indices[1] = {1};

            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }

         {
            /* Values above our second box */
            int ilower[2] = { 0, 4};
            int iupper[2] = { 2, 4};

            int stencil_indices[1] = {4};

            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }
      }
      else if (myid == 1)
      {
         int maxnvalues = 4;
         double values[4];
         for (i = 0; i < maxnvalues; i++)
            values[i] = 0.0;

         {
            /* Values below our box */
            int ilower[2] = { 3, 1};
            int iupper[2] = { 6, 1};

            int stencil_indices[1] = {3};

            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }

         {
            /* Values to the right of our box */
            int ilower[2] = { 6, 1};
            int iupper[2] = { 6, 4};

            int stencil_indices[1] = {2};

            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }

         {
            /* Values above our box */
            int ilower[2] = { 3, 4};
            int iupper[2] = { 6, 4};

            int stencil_indices[1] = {4};

            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }
      }

      /* This is a collective call finalizing the matrix assembly.
         The matrix is now ``ready to be used'' */
      HYPRE_StructMatrixAssemble(A);
   }

   /* 4. Set up Struct Vectors for b and x */
   {
      /* Create an empty vector object */
      HYPRE_StructVectorCreate(MPI_COMM_WORLD, grid, &b);
      HYPRE_StructVectorCreate(MPI_COMM_WORLD, grid, &x);

      /* Indicate that the vector coefficients are ready to be set */
      HYPRE_StructVectorInitialize(b);
      HYPRE_StructVectorInitialize(x);

      if (myid == 0)
      {
         /* Set the vector coefficients over the gridpoints in my first box */
         {
            int ilower[2] = {-3, 1};
            int iupper[2] = {-1, 2};

            int nvalues = 6;  /* 6 grid points */
            double values[6];

            for (i = 0; i < nvalues; i ++)
               values[i] = 1.0;
            HYPRE_StructVectorSetBoxValues(b, ilower, iupper, values);

            for (i = 0; i < nvalues; i ++)
               values[i] = 0.0;
            HYPRE_StructVectorSetBoxValues(x, ilower, iupper, values);
         }

         /* Set the vector coefficients over the gridpoints in my second box */
         {
            int ilower[2] = { 0, 1};
            int iupper[2] = { 2, 4};

            int nvalues = 12; /* 12 grid points */
            double values[12];

            for (i = 0; i < nvalues; i ++)
               values[i] = 1.0;
            HYPRE_StructVectorSetBoxValues(b, ilower, iupper, values);

            for (i = 0; i < nvalues; i ++)
               values[i] = 0.0;
            HYPRE_StructVectorSetBoxValues(x, ilower, iupper, values);
         }
      }
      else if (myid == 1)
      {
         /* Set the vector coefficients over the gridpoints in my box */
         {
            int ilower[2] = { 3, 1};
            int iupper[2] = { 6, 4};

            int nvalues = 16; /* 16 grid points */
            double values[16];

            for (i = 0; i < nvalues; i ++)
               values[i] = 1.0;
            HYPRE_StructVectorSetBoxValues(b, ilower, iupper, values);

            for (i = 0; i < nvalues; i ++)
               values[i] = 0.0;
            HYPRE_StructVectorSetBoxValues(x, ilower, iupper, values);
         }
      }

      /* This is a collective call finalizing the vector assembly.
         The vectors are now ``ready to be used'' */
      HYPRE_StructVectorAssemble(b);
      HYPRE_StructVectorAssemble(x);
   }


   /* 5. Set up and use a solver (See the Reference Manual for descriptions
      of all of the options.) */
   {
      /* Create an empty PCG Struct solver */
      HYPRE_StructPCGCreate(MPI_COMM_WORLD, &solver);

      /* Set PCG parameters */
      HYPRE_StructPCGSetTol(solver, 1.0e-06);
      HYPRE_StructPCGSetPrintLevel(solver, 2);
      HYPRE_StructPCGSetMaxIter(solver, 50);

      /* Use symmetric SMG as preconditioner */
      HYPRE_StructSMGCreate(MPI_COMM_WORLD, &precond);
      HYPRE_StructSMGSetMaxIter(precond, 1);
      HYPRE_StructSMGSetTol(precond, 0.0);
      HYPRE_StructSMGSetZeroGuess(precond);
      HYPRE_StructSMGSetNumPreRelax(precond, 1);
      HYPRE_StructSMGSetNumPostRelax(precond, 1);

      /* Set preconditioner and solve */
      HYPRE_StructPCGSetPrecond(solver, HYPRE_StructSMGSolve,
                                HYPRE_StructSMGSetup, precond);
      HYPRE_StructPCGSetup(solver, A, b, x);
      HYPRE_StructPCGSolve(solver, A, b, x);
   }

   /* Save the solution for GLVis visualization, see vis/glvis-ex2.sh */
   if (vis)
   {
      GLVis_PrintStructGrid(grid, "vis/ex2.mesh", myid, NULL, NULL);
      GLVis_PrintStructVector(x, "vis/ex2.sol", myid);
      GLVis_PrintData("vis/ex2.data", myid, num_procs);
   }

   /* Free memory */
   HYPRE_StructGridDestroy(grid);
   HYPRE_StructStencilDestroy(stencil);
   HYPRE_StructMatrixDestroy(A);
   HYPRE_StructVectorDestroy(b);
   HYPRE_StructVectorDestroy(x);
   HYPRE_StructPCGDestroy(solver);
   HYPRE_StructSMGDestroy(precond);

   /* Finalize MPI */
   MPI_Finalize();

   return (0);
}
