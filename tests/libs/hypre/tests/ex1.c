/*
   Example 1

   Interface:    Structured interface (Struct)

   Compile with: make ex1 (may need to edit HYPRE_DIR in Makefile)

   Sample run:   mpirun -np 2 ex1

   Description:  This is a two processor example.  Each processor owns one
                 box in the grid.  For reference, the two grid boxes are those
                 in the example diagram in the struct interface chapter
                 of the User's Manual. Note that in this example code, we have
                 used the two boxes shown in the diagram as belonging
                 to processor 0 (and given one box to each processor). The
                 solver is PCG with no preconditioner.

                 We recommend viewing examples 1-4 sequentially for
                 a nice overview/tutorial of the struct interface.
*/

#include <stdio.h>

/* Struct linear solvers header */
#include "HYPRE_struct_ls.h"

#include "vis.c"

int main (int argc, char *argv[])
{
   int i, j, myid, num_procs;

   int vis = 0;

   HYPRE_StructGrid     grid;
   HYPRE_StructStencil  stencil;
   HYPRE_StructMatrix   A;
   HYPRE_StructVector   b;
   HYPRE_StructVector   x;
   HYPRE_StructSolver   solver;

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

   /* 1. Set up a grid. Each processor describes the piece
      of the grid that it owns. */
   {
      /* Create an empty 2D grid object */
      HYPRE_StructGridCreate(MPI_COMM_WORLD, 2, &grid);

      /* Add boxes to the grid */
      if (myid == 0)
      {
         int ilower[2]={-3,1}, iupper[2]={-1,2};
         HYPRE_StructGridSetExtents(grid, ilower, iupper);
      }
      else if (myid == 1)
      {
         int ilower[2]={0,1}, iupper[2]={2,4};
         HYPRE_StructGridSetExtents(grid, ilower, iupper);
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

      /* Set the matrix coefficients.  Each processor assigns coefficients
         for the boxes in the grid that it owns. Note that the coefficients
         associated with each stencil entry may vary from grid point to grid
         point if desired.  Here, we first set the same stencil entries for
         each grid point.  Then we make modifications to grid points near
         the boundary. */
      if (myid == 0)
      {
         int ilower[2]={-3,1}, iupper[2]={-1,2};
         int stencil_indices[5] = {0,1,2,3,4}; /* labels for the stencil entries -
                                                  these correspond to the offsets
                                                  defined above */
         int nentries = 5;
         int nvalues  = 30; /* 6 grid points, each with 5 stencil entries */
         double values[30];

         /* We have 6 grid points, each with 5 stencil entries */
         for (i = 0; i < nvalues; i += nentries)
         {
            values[i] = 4.0;
            for (j = 1; j < nentries; j++)
               values[i+j] = -1.0;
         }

         HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, nentries,
                                        stencil_indices, values);
      }
      else if (myid == 1)
      {
         int ilower[2]={0,1}, iupper[2]={2,4};
         int stencil_indices[5] = {0,1,2,3,4};
         int nentries = 5;
         int nvalues  = 60; /* 12 grid points, each with 5 stencil entries */
         double values[60];

         for (i = 0; i < nvalues; i += nentries)
         {
            values[i] = 4.0;
            for (j = 1; j < nentries; j++)
               values[i+j] = -1.0;
         }

         HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, nentries,
                                        stencil_indices, values);
      }

      /* Set the coefficients reaching outside of the boundary to 0 */
      if (myid == 0)
      {
         double values[3];
         for (i = 0; i < 3; i++)
            values[i] = 0.0;
         {
            /* values below our box */
            int ilower[2]={-3,1}, iupper[2]={-1,1};
            int stencil_indices[1] = {3};
            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }
         {
            /* values to the left of our box */
            int ilower[2]={-3,1}, iupper[2]={-3,2};
            int stencil_indices[1] = {1};
            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }
         {
            /* values above our box */
            int ilower[2]={-3,2}, iupper[2]={-1,2};
            int stencil_indices[1] = {4};
            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }
      }
      else if (myid == 1)
      {
         double values[4];
         for (i = 0; i < 4; i++)
            values[i] = 0.0;
         {
            /* values below our box */
            int ilower[2]={0,1}, iupper[2]={2,1};
            int stencil_indices[1] = {3};
            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }
         {
            /* values to the right of our box */
            int ilower[2]={2,1}, iupper[2]={2,4};
            int stencil_indices[1] = {2};
            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }
         {
            /* values above our box */
            int ilower[2]={0,4}, iupper[2]={2,4};
            int stencil_indices[1] = {4};
            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }
         {
            /* values to the left of our box
               (that do not border the other box on proc. 0) */
            int ilower[2]={0,3}, iupper[2]={0,4};
            int stencil_indices[1] = {1};
            HYPRE_StructMatrixSetBoxValues(A, ilower, iupper, 1,
                                           stencil_indices, values);
         }
      }

      /* This is a collective call finalizing the matrix assembly.
         The matrix is now ``ready to be used'' */
      HYPRE_StructMatrixAssemble(A);
   }

   /* 4. Set up Struct Vectors for b and x.  Each processor sets the vectors
      corresponding to its boxes. */
   {
      /* Create an empty vector object */
      HYPRE_StructVectorCreate(MPI_COMM_WORLD, grid, &b);
      HYPRE_StructVectorCreate(MPI_COMM_WORLD, grid, &x);

      /* Indicate that the vector coefficients are ready to be set */
      HYPRE_StructVectorInitialize(b);
      HYPRE_StructVectorInitialize(x);

      /* Set the vector coefficients */
      if (myid == 0)
      {
         int ilower[2]={-3,1}, iupper[2]={-1,2};
         double values[6]; /* 6 grid points */

         for (i = 0; i < 6; i ++)
            values[i] = 1.0;
         HYPRE_StructVectorSetBoxValues(b, ilower, iupper, values);

         for (i = 0; i < 6; i ++)
            values[i] = 0.0;
         HYPRE_StructVectorSetBoxValues(x, ilower, iupper, values);
      }
      else if (myid == 1)
      {
         int ilower[2]={0,1}, iupper[2]={2,4};
         double values[12]; /* 12 grid points */

         for (i = 0; i < 12; i ++)
            values[i] = 1.0;
         HYPRE_StructVectorSetBoxValues(b, ilower, iupper, values);

         for (i = 0; i < 12; i ++)
            values[i] = 0.0;
         HYPRE_StructVectorSetBoxValues(x, ilower, iupper, values);
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

      /* Set some parameters */
      HYPRE_StructPCGSetTol(solver, 1.0e-06); /* convergence tolerance */
      HYPRE_StructPCGSetPrintLevel(solver, 2); /* amount of info. printed */

      /* Setup and solve */
      HYPRE_StructPCGSetup(solver, A, b, x);
      HYPRE_StructPCGSolve(solver, A, b, x);
   }

   /* Save the solution for GLVis visualization, see vis/glvis-ex1.sh */
   if (vis)
   {
      GLVis_PrintStructGrid(grid, "vis/ex1.mesh", myid, NULL, NULL);
      GLVis_PrintStructVector(x, "vis/ex1.sol", myid);
      GLVis_PrintData("vis/ex1.data", myid, num_procs);
   }

   /* Free memory */
   HYPRE_StructGridDestroy(grid);
   HYPRE_StructStencilDestroy(stencil);
   HYPRE_StructMatrixDestroy(A);
   HYPRE_StructVectorDestroy(b);
   HYPRE_StructVectorDestroy(x);
   HYPRE_StructPCGDestroy(solver);

   /* Finalize MPI */
   MPI_Finalize();

   return (0);
}
