/*
   Example 8

   Interface:    Semi-Structured interface (SStruct)

   Compile with: make ex8

   Sample run:   mpirun -np 2 ex8

   Description:  This is a two processor example which solves a similar
                 problem to the one in Example 2, and Example 6 (The grid
                 boxes are exactly those in the example diagram in the
                 struct interface chapter of the User's Manual.)

                 The difference with the previous examples is that we use
                 three parts, two with a 5-point and one with a 9-point
                 discretization stencil. The solver is PCG with split-SMG
                 preconditioner.
*/

#include <stdio.h>

/* SStruct linear solvers headers */
#include "HYPRE_sstruct_ls.h"

#include "vis.c"

int main (int argc, char *argv[])
{
   int myid, num_procs;

   int vis = 0;

   HYPRE_SStructGrid     grid;
   HYPRE_SStructGraph    graph;
   HYPRE_SStructStencil  stencil_5pt;
   HYPRE_SStructStencil  stencil_9pt;
   HYPRE_SStructMatrix   A;
   HYPRE_SStructVector   b;
   HYPRE_SStructVector   x;
   HYPRE_SStructSolver   solver;
   HYPRE_SStructSolver   precond;

   int object_type;

   /* Initialize MPI */
   MPI_Init(&argc, &argv);
   MPI_Comm_rank(MPI_COMM_WORLD, &myid);
   MPI_Comm_size(MPI_COMM_WORLD, &num_procs);

   if (num_procs != 2)
   {
      if (myid ==0) printf("Must run with 2 processors!\n");
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

   /* 1. Set up the 2D grid.  This gives the index space in each part.
      We have one variable in each part. */
   {
      int ndim = 2;
      int nparts = 3;
      int part;

      /* Create an empty 2D grid object */
      HYPRE_SStructGridCreate(MPI_COMM_WORLD, ndim, nparts, &grid);

      /* Set the extents of the grid - each processor sets its grid
         boxes.  Each part has its own relative index space numbering. */

      /* Processor 0 owns two boxes - one in part 0 and one in part 1. */
      if (myid == 0)
      {
         /* Add the first box to the grid in part 0 */
         {
            int ilower[2] = {-3, 1};
            int iupper[2] = {-1, 2};

            part = 0;
            HYPRE_SStructGridSetExtents(grid, part, ilower, iupper);
         }

         /* Add the second box to the grid in part 1 */
         {
            /* For convenience we use the same index space across all
               parts, but this is not a requirement. For example, on this
               part we could have used ilower=[23,24] and iupper=[25,27]. */
            int ilower[2] = {0, 1};
            int iupper[2] = {2, 4};

            part = 1;
            HYPRE_SStructGridSetExtents(grid, part, ilower, iupper);
         }
      }

      /* Processor 1 owns one box in part 2. */
      else if (myid == 1)
      {
         /* Add a new box to the grid in part 2 */
         {
            int ilower[2] = {3, 1};
            int iupper[2] = {6, 4};

            part = 2;
            HYPRE_SStructGridSetExtents(grid, part, ilower, iupper);
         }
      }

      /* Set the variable type and number of variables on each part. */
      {
         int i;
         int nvars = 1;
         HYPRE_SStructVariable vartypes[1] = {HYPRE_SSTRUCT_VARIABLE_CELL};

         for (i = 0; i< nparts; i++)
            HYPRE_SStructGridSetVariables(grid, i, nvars, vartypes);
      }

      /* Now we need to set the spatial relation between each of the parts.
         Since we have the same types of variables on both parts, we can
         use HYPRE_GridSetNeighborPart().  Each processor calls this function
         for each part on which it owns boxes that border a different part. */

      if (myid == 0)
      {
         /* Relation between part 0 and part 1 on processor 0 */
         {
            int part = 0;
            int nbor_part = 1;
            /* Cells just outside of the boundary of part 0 in
               its coordinates */
            int b_ilower[2] = {0,1}, b_iupper[2] = {0,2};
            /* The same cells in part 1's coordinates.  Since we use the same
               index space across all parts, the coordinates coincide. */
            int nbor_ilower[2] = {0,1}, nbor_iupper[2] = {0,2};
            /* These parts have the same orientation, so no
               rotation is necessary */
            int index_map[2] = {0,1};
            /* These parts map increasing values to increasing values 
               for both variables (note: if decreasing maps to increasing, use -1)*/
            int index_dir[2] = {1,1};

            HYPRE_SStructGridSetNeighborPart(grid, part, b_ilower, b_iupper,
                                             nbor_part, nbor_ilower, nbor_iupper,
                                             index_map, index_dir);
         }

         /* Relation between part 1 and part 0 on processor 0 */
         {
            int part = 1;
            int nbor_part = 0;
            /* Cells just outside of the boundary of part 1 in
               its coordinates */
            int b_ilower[2] = {-1,1}, b_iupper[2] = {-1,2};
            /* The same cells in part 0's coordinates.  Since we use the same
               index space across all parts, the coordinates coincide. */
            int nbor_ilower[2] = {-1,1}, nbor_iupper[2] = {-1,2};
            /* These parts have the same orientation, so no
               rotation is necessary */
            int index_map[2] = {0,1};
            /* These parts map increasing values to increasing values 
               for both variables (note: if decreasing maps to increasing, use -1)*/
            int index_dir[2] = {1,1};

            HYPRE_SStructGridSetNeighborPart(grid, part, b_ilower, b_iupper,
                                             nbor_part, nbor_ilower, nbor_iupper,
                                             index_map, index_dir);
         }

         /* Relation between part 1 and part 2 on processor 0 */
         {
            int part = 1;
            int nbor_part = 2;
            /* Cells just outside of the boundary of part 1 in
               its coordinates */
            int b_ilower[2] = {3,1}, b_iupper[2] = {3,4};
            /* The same cells in part 2's coordinates.  Since we use the same
               index space across all parts, the coordinates coincide. */
            int nbor_ilower[2] = {3,1}, nbor_iupper[2] = {3,4};
            /* These parts have the same orientation, so no
               rotation is necessary */
            int index_map[2] = {0,1};
            /* These parts map increasing values to increasing values 
               for both variables (note: if decreasing maps to increasing, use -1)*/
            int index_dir[2] = {1,1};

            HYPRE_SStructGridSetNeighborPart(grid, part, b_ilower, b_iupper,
                                            nbor_part, nbor_ilower, nbor_iupper,
                                             index_map, index_dir);
         }
      }
      else if (myid == 1)
      {
         /* Relation between part 2 and part 1 on processor 1 */
         {
            int part = 2;
            int nbor_part = 1;
            /* Cells just outside of the boundary of part 2 in
               its coordinates */
            int b_ilower[2] = {2,1}, b_iupper[2] = {2,4};
            /* The same cells in part 1's coordinates.  Since we use the same
               index space across all parts, the coordinates coincide. */
            int nbor_ilower[2] = {2,1}, nbor_iupper[2] = {2,4};
            /* These parts have the same orientation, so no
               rotation is necessary */
            int index_map[2] = {0,1};
            /* These parts map increasing values to increasing values 
              for both variables (note: if decreasing maps to increasing, use -1)*/
            int index_dir[2] = {1,1};

            HYPRE_SStructGridSetNeighborPart(grid, part, b_ilower, b_iupper,
                                             nbor_part, nbor_ilower, nbor_iupper,
                                             index_map, index_dir); 
         }
      }

      /* Now the grid is ready to use */
      HYPRE_SStructGridAssemble(grid);
   }

   /* 2. Define the discretization stencils */
   {
      int ndim = 2;
      int var = 0;
      int entry;

      /* the 5-pt stencil in 2D */
      {
         int offsets[5][2] = {{0,0}, {-1,0}, {1,0}, {0,-1}, {0,1}};
         int stencil_size = 5;

         HYPRE_SStructStencilCreate(ndim, stencil_size, &stencil_5pt);

         for (entry = 0; entry < 5; entry++)
            HYPRE_SStructStencilSetEntry(stencil_5pt, entry, offsets[entry], var);
      }

      /* the 9-pt stencil in 2D */
      {
         int offsets[9][2] = {{0,0}, {-1,0}, {1,0}, {0,-1}, {0,1},
                              {-1,-1}, {1,-1}, {1,1}, {-1,1}};
         int stencil_size = 9;
         HYPRE_SStructStencilCreate(ndim, stencil_size, &stencil_9pt);

         for (entry = 0; entry < stencil_size; entry++)
            HYPRE_SStructStencilSetEntry(stencil_9pt, entry, offsets[entry], var);
      }
   }

   /* 3. Set up the Graph  - this determines the non-zero structure
      of the matrix and allows non-stencil relationships between the parts */
   {
      int var = 0;
      int part;

      /* Create the graph object */
      HYPRE_SStructGraphCreate(MPI_COMM_WORLD, grid, &graph);

      /* See MatrixSetObjectType below */
      object_type = HYPRE_SSTRUCT;
      HYPRE_SStructGraphSetObjectType(graph, object_type);

      /* Use the 5-pt stencil on part 0 */
      part = 0;
      HYPRE_SStructGraphSetStencil(graph, part, var, stencil_5pt);

      /* Use the 9-pt stencil on part 1 */
      part = 1;
      HYPRE_SStructGraphSetStencil(graph, part, var, stencil_9pt);

      /* Use the 5-pt stencil on part 2 */
      part = 2;
      HYPRE_SStructGraphSetStencil(graph, part, var, stencil_5pt);

      /*  Since we have only stencil connections between parts, we don't need to
          call HYPRE_SStructGraphAddEntries. */

      /* Assemble the graph */
      HYPRE_SStructGraphAssemble(graph);
   }

   /* 4. Set up a SStruct Matrix */
   {
      int i,j;
      int part;
      int var = 0;

      /* Create the empty matrix object */
      HYPRE_SStructMatrixCreate(MPI_COMM_WORLD, graph, &A);

      /* Set the object type (by default HYPRE_SSTRUCT). This determines the
         data structure used to store the matrix.  If you want to use unstructured
         solvers, e.g. BoomerAMG, the object type should be HYPRE_PARCSR.
         If the problem is purely structured (with one part), you may want to use
         HYPRE_STRUCT to access the structured solvers.  Since we have two parts
         with different stencils, we set the object type to HYPRE_SSTRUCT. */
      object_type = HYPRE_SSTRUCT;
      HYPRE_SStructMatrixSetObjectType(A, object_type);

      /* Get ready to set values */
      HYPRE_SStructMatrixInitialize(A);

      /* Each processor must set the stencil values for their boxes on each part.
         In this example, we only set stencil entries and therefore use
         HYPRE_SStructMatrixSetBoxValues.  If we need to set non-stencil entries,
         we have to use HYPRE_SStructMatrixSetValues. */

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

            part = 0;
            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, nentries,
                                            stencil_indices, values);
         }

         /* Set the matrix coefficients for some set of stencil entries
            over the gridpoints in my second box */
         {
            int ilower[2] = {0, 1};
            int iupper[2] = {2, 4};

            int nentries = 9;
            int nvalues  = 108; /* 12 grid points, each with 5 stencil entries */
            double values[108];

            int stencil_indices[9];
            for (j = 0; j < nentries; j++)
               stencil_indices[j] = j;

            for (i = 0; i < nvalues; i += nentries)
            {
               values[i] = 8./3.;
               for (j = 1; j < nentries; j++)
                  values[i+j] = -1./3.;
            }

            part = 1;
            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, nentries,
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

            part = 2;
            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, nentries,
                                            stencil_indices, values);
         }
      }

      /* Modify the 9-pt stencil on the boundary between parts to ensure
         symmetry and good global approximation. */
      if (myid == 0)
      {
         int nentries = 6;
         int nvalues  = 24; /* 4 grid points, each with 6 stencil entries */
         double values[24];

         part = 1;

         for (i = 0; i < nvalues; i += nentries)
         {
            values[i]   = 10./3.;
            values[i+1] = -1.;
            values[i+2] = -2./3.;
            values[i+3] = -2./3.;
            values[i+4] = 0.0;
            values[i+5] = 0.0;
         }

         {
            /* Values to the right of the second box */
            int ilower[2] = { 2, 1};
            int iupper[2] = { 2, 4};

            int stencil_indices[6] = {0,2,3,4,6,7};

            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, nentries,
                                            stencil_indices, values);
         }

         {
            /* Values to the left of the second box */
            int ilower[2] = { 0, 1};
            int iupper[2] = { 0, 4};

            int stencil_indices[6] = {0,1,3,4,5,8};

            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, nentries,
                                            stencil_indices, values);
         }
      }

      /* For each box, set any coefficients that reach ouside of the
         boundary to 0 */
      if (myid == 0)
      {
         int maxnvalues = 9;
         double values[9];

         for (i = 0; i < maxnvalues; i++)
            values[i] = 0.0;

         part = 0;

         {
            /* Values below our first box */
            int ilower[2] = {-3, 1};
            int iupper[2] = {-1, 1};

            int stencil_indices[1] = {3};

            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, 1,
                                            stencil_indices, values);
         }

         {
            /* Values to the left of our first box */
            int ilower[2] = {-3, 1};
            int iupper[2] = {-3, 2};

            int stencil_indices[1] = {1};

            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, 1,
                                            stencil_indices, values);
         }

         {
            /* Values above our first box */
            int ilower[2] = {-3, 2};
            int iupper[2] = {-1, 2};

            int stencil_indices[1] = {4};

            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, 1,
                                            stencil_indices, values);
         }

         part = 1;

         {
            /* Values below our second box */
            int ilower[2] = { 0, 1};
            int iupper[2] = { 2, 1};

            int stencil_indices[3] = {3,5,6};

            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, 3,
                                            stencil_indices, values);
         }

         {
            /* Values to the left of our second box (that do not border the
               first box). */
            int ilower[2] = { 0, 3};
            int iupper[2] = { 0, 4};

            int stencil_indices[3] = {1,5,8};

            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, 3,
                                            stencil_indices, values);
         }

         {
            /* Values above our second box */
            int ilower[2] = { 0, 4};
            int iupper[2] = { 2, 4};

            int stencil_indices[3] = {4,7,8};

            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, 3,
                                            stencil_indices, values);
         }
      }
      else if (myid == 1)
      {
         int maxnvalues = 4;
         double values[4];

         for (i = 0; i < maxnvalues; i++)
            values[i] = 0.0;

         part = 2;

         {
            /* Values below our box */
            int ilower[2] = { 3, 1};
            int iupper[2] = { 6, 1};

            int stencil_indices[1] = {3};

            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, 1,
                                            stencil_indices, values);
         }

         {
            /* Values to the right of our box */
            int ilower[2] = { 6, 1};
            int iupper[2] = { 6, 4};

            int stencil_indices[1] = {2};

            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, 1,
                                            stencil_indices, values);
         }

         {
            /* Values above our box */
            int ilower[2] = { 3, 4};
            int iupper[2] = { 6, 4};

            int stencil_indices[1] = {4};

            HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                            var, 1,
                                            stencil_indices, values);
         }
      }

      /* This is a collective call finalizing the matrix assembly.
         The matrix is now ``ready to be used'' */
      HYPRE_SStructMatrixAssemble(A);
   }

   /* 5. Set up SStruct Vectors for b and x */
   {
      int i;
      int part;
      int var = 0;

      /* Create an empty vector object */
      HYPRE_SStructVectorCreate(MPI_COMM_WORLD, grid, &b);
      HYPRE_SStructVectorCreate(MPI_COMM_WORLD, grid, &x);

      /* As with the matrix,  set the object type for the vectors
         to be the sstruct type */
      object_type = HYPRE_SSTRUCT;
      HYPRE_SStructVectorSetObjectType(b, object_type);
      HYPRE_SStructVectorSetObjectType(x, object_type);

      /* Indicate that the vector coefficients are ready to be set */
      HYPRE_SStructVectorInitialize(b);
      HYPRE_SStructVectorInitialize(x);

      if (myid == 0)
      {
         /* Set the vector coefficients over the gridpoints in my first box */
         {
            int ilower[2] = {-3, 1};
            int iupper[2] = {-1, 2};

            int nvalues = 6;  /* 6 grid points */
            double values[6];

            part = 0;

            for (i = 0; i < nvalues; i ++)
               values[i] = 1.0;
            HYPRE_SStructVectorSetBoxValues(b, part, ilower, iupper, var, values);

            for (i = 0; i < nvalues; i ++)
               values[i] = 0.0;
            HYPRE_SStructVectorSetBoxValues(x, part, ilower, iupper, var, values);
         }

         /* Set the vector coefficients over the gridpoints in my second box */
         {
            int ilower[2] = { 0, 1};
            int iupper[2] = { 2, 4};

            int nvalues = 12; /* 12 grid points */
            double values[12];

            part = 1;

            for (i = 0; i < nvalues; i ++)
               values[i] = 1.0;
            HYPRE_SStructVectorSetBoxValues(b, part, ilower, iupper, var, values);

            for (i = 0; i < nvalues; i ++)
               values[i] = 0.0;
            HYPRE_SStructVectorSetBoxValues(x, part, ilower, iupper, var, values);
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

            part = 2;

            for (i = 0; i < nvalues; i ++)
               values[i] = 1.0;
            HYPRE_SStructVectorSetBoxValues(b, part, ilower, iupper, var, values);

            for (i = 0; i < nvalues; i ++)
               values[i] = 0.0;
            HYPRE_SStructVectorSetBoxValues(x, part, ilower, iupper, var, values);
         }
      }

      /* This is a collective call finalizing the vector assembly.
         The vectors are now ``ready to be used'' */
      HYPRE_SStructVectorAssemble(b);
      HYPRE_SStructVectorAssemble(x);
   }

   /* 6. Set up and use a solver (See the Reference Manual for descriptions
      of all of the options.) */
   {
      /* Create an empty PCG Struct solver */
      HYPRE_SStructPCGCreate(MPI_COMM_WORLD, &solver);

      /* Set PCG parameters */
      HYPRE_SStructPCGSetTol(solver, 1.0e-6 );
      HYPRE_SStructPCGSetPrintLevel(solver, 2);
      HYPRE_SStructPCGSetMaxIter(solver, 50);

      /* Create a split SStruct solver for use as a preconditioner */
      HYPRE_SStructSplitCreate(MPI_COMM_WORLD, &precond);
      HYPRE_SStructSplitSetMaxIter(precond, 1);
      HYPRE_SStructSplitSetTol(precond, 0.0);
      HYPRE_SStructSplitSetZeroGuess(precond);

      /* Set the preconditioner type to split-SMG */
      HYPRE_SStructSplitSetStructSolver(precond, HYPRE_SMG);

      /* Set preconditioner and solve */
      HYPRE_SStructPCGSetPrecond(solver, HYPRE_SStructSplitSolve,
                                 HYPRE_SStructSplitSetup, precond);
      HYPRE_SStructPCGSetup(solver, A, b, x);
      HYPRE_SStructPCGSolve(solver, A, b, x);
   }

   /* Save the solution for GLVis visualization, see vis/glvis-ex8.sh */
   if (vis)
   {
      GLVis_PrintSStructGrid(grid, "vis/ex8.mesh", myid, NULL, NULL);
      GLVis_PrintSStructVector(x, 0, "vis/ex8.sol", myid);
      GLVis_PrintData("vis/ex8.data", myid, num_procs);
   }

   /* Free memory */
   HYPRE_SStructGridDestroy(grid);
   HYPRE_SStructStencilDestroy(stencil_5pt);
   HYPRE_SStructStencilDestroy(stencil_9pt);
   HYPRE_SStructGraphDestroy(graph);
   HYPRE_SStructMatrixDestroy(A);
   HYPRE_SStructVectorDestroy(b);
   HYPRE_SStructVectorDestroy(x);

   HYPRE_SStructPCGDestroy(solver);
   HYPRE_SStructSplitDestroy(precond);

   /* Finalize MPI */
   MPI_Finalize();

   return (0);
}
