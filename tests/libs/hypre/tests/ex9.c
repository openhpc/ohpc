/*
   Example 9

   Interface:      Semi-Structured interface (SStruct)

   Compile with:   make ex9

   Sample run:     mpirun -np 16 ex9 -n 33 -solver 0 -v 1 1

   To see options: ex9 -help

   Description:    This code solves a system corresponding to a discretization
                   of the biharmonic problem treated as a system of equations
                   on the unit square.  Specifically, instead of solving
                   Delta^2(u) = f with zero boundary conditions for u and
                   Delta(u), we solve the system A x = b, where

                   A = [ Delta -I ; 0 Delta], x = [ u ; v] and b = [ 0 ; f]

                   The corresponding boundary conditions are u = 0 and v = 0.

                   The domain is split into an N x N processor grid.  Thus, the
                   given number of processors should be a perfect square.
                   Each processor's piece of the grid has n x n cells with n x n
                   nodes. We use cell-centered variables, and, therefore, the
                   nodes are not shared. Note that we have two variables, u and
                   v, and need only one part to describe the domain. We use the
                   standard 5-point stencil to discretize the Laplace operators.
                   The boundary conditions are incorporated as in Example 3.

                   We recommend viewing Examples 3, 6 and 7 before this example.
*/

#include <math.h>
#include "_hypre_utilities.h"
#include "HYPRE_sstruct_ls.h"
#include "HYPRE_krylov.h"

#include "vis.c"

int main (int argc, char *argv[])
{
   int i, j;

   int myid, num_procs;

   int n, N, pi, pj;
   double h, h2;
   int ilower[2], iupper[2];

   int solver_id;
   int n_pre, n_post;

   int vis;
   int object_type;

   HYPRE_SStructGrid     grid;
   HYPRE_SStructGraph    graph;
   HYPRE_SStructStencil  stencil_v;
   HYPRE_SStructStencil  stencil_u;
   HYPRE_SStructMatrix   A;
   HYPRE_SStructVector   b;
   HYPRE_SStructVector   x;

   /* sstruct solvers */
   HYPRE_SStructSolver   solver;
   HYPRE_SStructSolver   precond;

   /* parcsr solvers */
   HYPRE_Solver          par_solver;
   HYPRE_Solver          par_precond;

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
         printf("                        0  - GMRES with sysPFMG precond (default)\n");
         printf("                        1  - sysPFMG\n");
         printf("                        2  - GMRES with AMG precond\n");
         printf("                        3  - AMG\n");
         printf("  -v <n_pre> <n_post> : number of pre and post relaxations for SysPFMG (default: 1 1)\n");
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

   /* 1. Set up a grid - we have one part and two variables */
   {
      int nparts = 1;
      int part = 0;
      int ndim = 2;

      /* Create an empty 2D grid object */
      HYPRE_SStructGridCreate(MPI_COMM_WORLD, ndim, nparts, &grid);

      /* Add a new box to the grid */
      HYPRE_SStructGridSetExtents(grid, part, ilower, iupper);

      /* Set the variable type and number of variables on each part.*/
      {
         int i;
         int nvars = 2;
         HYPRE_SStructVariable vartypes[2] = {HYPRE_SSTRUCT_VARIABLE_CELL,
                                              HYPRE_SSTRUCT_VARIABLE_CELL };

         for (i = 0; i< nparts; i++)
            HYPRE_SStructGridSetVariables(grid, i, nvars, vartypes);
      }

      /* This is a collective call finalizing the grid assembly.
         The grid is now ``ready to be used'' */
      HYPRE_SStructGridAssemble(grid);
   }

   /* 2. Define the discretization stencils */
   {
      int entry;
      int stencil_size;
      int var;
      int ndim = 2;

      /* Stencil object for variable u (labeled as variable 0) */
      {
         int offsets[6][2] = {{0,0}, {-1,0}, {1,0}, {0,-1}, {0,1}, {0,0}};
         stencil_size = 6;

         HYPRE_SStructStencilCreate(ndim, stencil_size, &stencil_u);

         /* The first 5 entries are for the u-u connections */
         var = 0; /* connect to variable 0 */
         for (entry = 0; entry < stencil_size-1 ; entry++)
            HYPRE_SStructStencilSetEntry(stencil_u, entry, offsets[entry], var);

         /* The last entry is for the u-v connection */
         var = 1;  /* connect to variable 1 */
         entry = 5;
         HYPRE_SStructStencilSetEntry(stencil_u, entry, offsets[entry], var);
      }

      /* Stencil object for variable v  (variable 1) */
      {
         int offsets[5][2] = {{0,0}, {-1,0}, {1,0}, {0,-1}, {0,1}};
         stencil_size = 5;

         HYPRE_SStructStencilCreate(ndim, stencil_size, &stencil_v);

         /* These are all v-v connections */
         var = 1; /* Connect to variable 1 */
         for (entry = 0; entry < stencil_size; entry++)
            HYPRE_SStructStencilSetEntry(stencil_v, entry, offsets[entry], var);
      }
   }

   /* 3. Set up the Graph  - this determines the non-zero structure
      of the matrix and allows non-stencil relationships between the parts. */
   {
      int var;
      int part = 0;

      /* Create the graph object */
      HYPRE_SStructGraphCreate(MPI_COMM_WORLD, grid, &graph);

      /* See MatrixSetObjectType below */
      if (solver_id > 1 && solver_id < 4)
      {
         object_type = HYPRE_PARCSR;
      }
      else
      {
         object_type = HYPRE_SSTRUCT;
      }
      HYPRE_SStructGraphSetObjectType(graph, object_type);

      /* Assign the u-stencil we created to variable u (variable 0) */
      var = 0;
      HYPRE_SStructGraphSetStencil(graph, part, var, stencil_u);

      /* Assign the v-stencil we created to variable v (variable 1) */
      var = 1;
      HYPRE_SStructGraphSetStencil(graph, part, var, stencil_v);

      /* Assemble the graph */
      HYPRE_SStructGraphAssemble(graph);
   }

   /* 4. Set up the SStruct Matrix */
   {
      int nentries;
      int nvalues;
      int var;
      int part = 0;

      /* Create an empty matrix object */
      HYPRE_SStructMatrixCreate(MPI_COMM_WORLD, graph, &A);

      /* Set the object type (by default HYPRE_SSTRUCT). This determines the
         data structure used to store the matrix.  If you want to use
         unstructured solvers, e.g. BoomerAMG, the object type should be
         HYPRE_PARCSR. If the problem is purely structured (with one part), you
         may want to use HYPRE_STRUCT to access the structured solvers.  */
      HYPRE_SStructMatrixSetObjectType(A, object_type);

      /* Indicate that the matrix coefficients are ready to be set */
      HYPRE_SStructMatrixInitialize(A);

      /* Each processor must set the stencil values for their boxes on each part.
         In this example, we only set stencil entries and therefore use
         HYPRE_SStructMatrixSetBoxValues.  If we need to set non-stencil entries,
         we have to use HYPRE_SStructMatrixSetValues. */

      /* First set the u-stencil entries.  Note that
         HYPRE_SStructMatrixSetBoxValues can only set values corresponding
         to stencil entries for the same variable. Therefore, we must set the
         entries for each variable within a stencil with separate function calls.
         For example, below the u-u connections and u-v connections are handled
         in separate calls.  */
      {
         int     i, j;
         double *u_values;
         int     u_v_indices[1] = {5};
         int     u_u_indices[5] = {0, 1, 2, 3, 4};

         var = 0; /* Set values for the u connections */

         /*  First the u-u connections */
         nentries = 5;
         nvalues = nentries*n*n;
         u_values = calloc(nvalues, sizeof(double));

         for (i = 0; i < nvalues; i += nentries)
         {
            u_values[i] = 4.0;
            for (j = 1; j < nentries; j++)
               u_values[i+j] = -1.0;
         }

         HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                         var, nentries,
                                         u_u_indices, u_values);
         free(u_values);

         /* Next the u-v connections */
         nentries = 1;
         nvalues = nentries*n*n;
         u_values = calloc(nvalues, sizeof(double));

         for (i = 0; i < nvalues; i++)
         {
            u_values[i] = -h2;
         }

         HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                         var, nentries,
                                         u_v_indices, u_values);

         free(u_values);
      }

      /*  Now set the v-stencil entries */
      {
         int     i, j;
         double *v_values;
         int     v_v_indices[5] = {0, 1, 2, 3, 4};

         var = 1; /* the v connections */

         /* the v-v connections */
         nentries = 5;
         nvalues = nentries*n*n;
         v_values = calloc(nvalues, sizeof(double));

         for (i = 0; i < nvalues; i += nentries)
         {
            v_values[i] = 4.0;
            for (j = 1; j < nentries; j++)
               v_values[i+j] = -1.0;
         }

         HYPRE_SStructMatrixSetBoxValues(A, part, ilower, iupper,
                                         var, nentries,
                                         v_v_indices, v_values);

         free(v_values);

         /* There are no v-u connections to set */
      }
   }

   /* 5. Incorporate the zero boundary conditions: go along each edge of
         the domain and set the stencil entry that reaches to the boundary
         to zero.*/
   {
      int bc_ilower[2];
      int bc_iupper[2];
      int nentries = 1;
      int nvalues  = nentries*n; /*  number of stencil entries times the length
                                     of one side of my grid box */
      int var;
      double *values;
      int stencil_indices[1];

      int part = 0;

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

         /* Need to do this for u and for v */
         var = 0;
         HYPRE_SStructMatrixSetBoxValues(A, part, bc_ilower, bc_iupper,
                                         var, nentries,
                                         stencil_indices, values);

         var = 1;
         HYPRE_SStructMatrixSetBoxValues(A, part, bc_ilower, bc_iupper,
                                         var, nentries,
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

         /* Need to do this for u and for v */
         var = 0;
         HYPRE_SStructMatrixSetBoxValues(A, part, bc_ilower, bc_iupper,
                                         var, nentries,
                                         stencil_indices, values);

         var = 1;
         HYPRE_SStructMatrixSetBoxValues(A, part, bc_ilower, bc_iupper,
                                         var, nentries,
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

         /* Need to do this for u and for v */
         var = 0;
         HYPRE_SStructMatrixSetBoxValues(A, part, bc_ilower, bc_iupper,
                                         var, nentries,
                                         stencil_indices, values);

         var = 1;
         HYPRE_SStructMatrixSetBoxValues(A, part, bc_ilower, bc_iupper,
                                         var, nentries,
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

         /* Need to do this for u and for v */
         var = 0;
         HYPRE_SStructMatrixSetBoxValues(A, part, bc_ilower, bc_iupper,
                                         var, nentries,
                                         stencil_indices, values);

         var = 1;
         HYPRE_SStructMatrixSetBoxValues(A, part, bc_ilower, bc_iupper,
                                         var, nentries,
                                         stencil_indices, values);
      }

      free(values);
   }

   /* This is a collective call finalizing the matrix assembly.
      The matrix is now ``ready to be used'' */
   HYPRE_SStructMatrixAssemble(A);

   /* 5. Set up SStruct Vectors for b and x */
   {
      int    nvalues = n*n;
      double *values;
      int part = 0;
      int var;

      values = calloc(nvalues, sizeof(double));

      /* Create an empty vector object */
      HYPRE_SStructVectorCreate(MPI_COMM_WORLD, grid, &b);
      HYPRE_SStructVectorCreate(MPI_COMM_WORLD, grid, &x);

      /* Set the object type for the vectors
         to be the same as was already set for the matrix */
      HYPRE_SStructVectorSetObjectType(b, object_type);
      HYPRE_SStructVectorSetObjectType(x, object_type);

      /* Indicate that the vector coefficients are ready to be set */
      HYPRE_SStructVectorInitialize(b);
      HYPRE_SStructVectorInitialize(x);

      /* Set the values for b */
      for (i = 0; i < nvalues; i ++)
         values[i] = h2;
      var = 1;
      HYPRE_SStructVectorSetBoxValues(b, part, ilower, iupper, var, values);

      for (i = 0; i < nvalues; i ++)
         values[i] = 0.0;
      var = 0;
      HYPRE_SStructVectorSetBoxValues(b, part, ilower, iupper, var, values);

      /* Set the values for the initial guess */
      var = 0;
      HYPRE_SStructVectorSetBoxValues(x, part, ilower, iupper, var, values);

      var = 1;
      HYPRE_SStructVectorSetBoxValues(x, part, ilower, iupper, var, values);

      free(values);

      /* This is a collective call finalizing the vector assembly.
         The vector is now ``ready to be used'' */
      HYPRE_SStructVectorAssemble(b);
      HYPRE_SStructVectorAssemble(x);
   }

   /* 6. Set up and use a solver
      (Solver options can be found in the Reference Manual.) */
   {
      double final_res_norm;
      int its;

      HYPRE_ParCSRMatrix    par_A;
      HYPRE_ParVector       par_b;
      HYPRE_ParVector       par_x;

      /* If we are using a parcsr solver, we need to get the object for the
         matrix and vectors. */
      if (object_type == HYPRE_PARCSR)
      {
         HYPRE_SStructMatrixGetObject(A, (void **) &par_A);
         HYPRE_SStructVectorGetObject(b, (void **) &par_b);
         HYPRE_SStructVectorGetObject(x, (void **) &par_x);
      }

      if (solver_id ==0 ) /* GMRES with SysPFMG - the default*/
      {
         HYPRE_SStructGMRESCreate(MPI_COMM_WORLD, &solver);

         /* GMRES parameters */
         HYPRE_SStructGMRESSetMaxIter(solver, 50 );
         HYPRE_SStructGMRESSetTol(solver, 1.0e-06 );
         HYPRE_SStructGMRESSetPrintLevel(solver, 2 ); /* print each GMRES
                                                         iteration */
         HYPRE_SStructGMRESSetLogging(solver, 1);

         /* use SysPFMG as precondititioner */
         HYPRE_SStructSysPFMGCreate(MPI_COMM_WORLD, &precond);

         /* Set sysPFMG parameters */
         HYPRE_SStructSysPFMGSetTol(precond, 0.0);
         HYPRE_SStructSysPFMGSetMaxIter(precond, 1);
         HYPRE_SStructSysPFMGSetNumPreRelax(precond, n_pre);
         HYPRE_SStructSysPFMGSetNumPostRelax(precond, n_post);
         HYPRE_SStructSysPFMGSetPrintLevel(precond, 0);
         HYPRE_SStructSysPFMGSetZeroGuess(precond);

         /* Set the preconditioner*/
         HYPRE_SStructGMRESSetPrecond(solver, HYPRE_SStructSysPFMGSolve,
                                      HYPRE_SStructSysPFMGSetup, precond);
         /* do the setup */
         HYPRE_SStructGMRESSetup(solver, A, b, x);

         /* do the solve */
         HYPRE_SStructGMRESSolve(solver, A, b, x);

         /* get some info */
         HYPRE_SStructGMRESGetFinalRelativeResidualNorm(solver,
                                                        &final_res_norm);
         HYPRE_SStructGMRESGetNumIterations(solver, &its);

         /* clean up */
         HYPRE_SStructGMRESDestroy(solver);
      }
      else if (solver_id == 1) /* SysPFMG */
      {
         HYPRE_SStructSysPFMGCreate(MPI_COMM_WORLD, &solver);

         /* Set sysPFMG parameters */
         HYPRE_SStructSysPFMGSetTol(solver, 1.0e-6);
         HYPRE_SStructSysPFMGSetMaxIter(solver, 50);
         HYPRE_SStructSysPFMGSetNumPreRelax(solver, n_pre);
         HYPRE_SStructSysPFMGSetNumPostRelax(solver, n_post);
         HYPRE_SStructSysPFMGSetPrintLevel(solver, 0);
         HYPRE_SStructSysPFMGSetLogging(solver, 1);

         /* do the setup */
         HYPRE_SStructSysPFMGSetup(solver, A, b, x);

         /* do the solve */
         HYPRE_SStructSysPFMGSolve(solver, A, b, x);

         /* get some info */
         HYPRE_SStructSysPFMGGetFinalRelativeResidualNorm(solver,
                                                          &final_res_norm);
         HYPRE_SStructSysPFMGGetNumIterations(solver, &its);

         /* clean up */
         HYPRE_SStructSysPFMGDestroy(solver);
      }
      else if (solver_id == 2) /* GMRES with AMG */
      {
         HYPRE_ParCSRGMRESCreate(MPI_COMM_WORLD, &par_solver);

         /* set the GMRES paramaters */
         HYPRE_GMRESSetKDim(par_solver, 5);
         HYPRE_GMRESSetMaxIter(par_solver, 100);
         HYPRE_GMRESSetTol(par_solver, 1.0e-06);
         HYPRE_GMRESSetPrintLevel(par_solver, 2);
         HYPRE_GMRESSetLogging(par_solver, 1);

         /* use BoomerAMG as preconditioner */
         HYPRE_BoomerAMGCreate(&par_precond);
         HYPRE_BoomerAMGSetCoarsenType(par_precond, 6);
         HYPRE_BoomerAMGSetStrongThreshold(par_precond, 0.25);
         HYPRE_BoomerAMGSetTol(par_precond, 0.0);
         HYPRE_BoomerAMGSetPrintLevel(par_precond, 1);
         HYPRE_BoomerAMGSetPrintFileName(par_precond, "ex9.out.log");
         HYPRE_BoomerAMGSetMaxIter(par_precond, 1);

         /* set the preconditioner */
         HYPRE_ParCSRGMRESSetPrecond(par_solver,
                                     HYPRE_BoomerAMGSolve,
                                     HYPRE_BoomerAMGSetup,
                                     par_precond);

         /* do the setup */
         HYPRE_ParCSRGMRESSetup(par_solver, par_A, par_b, par_x);

         /* do the solve */
         HYPRE_ParCSRGMRESSolve(par_solver, par_A, par_b, par_x);

         /* get some info */
         HYPRE_GMRESGetNumIterations(par_solver, &its);
         HYPRE_GMRESGetFinalRelativeResidualNorm(par_solver,
                                                 &final_res_norm);
         /* clean up */
         HYPRE_ParCSRGMRESDestroy(par_solver);
         HYPRE_BoomerAMGDestroy(par_precond);
      }
      else if (solver_id == 3) /* AMG */
      {
         HYPRE_BoomerAMGCreate(&par_solver);
         HYPRE_BoomerAMGSetCoarsenType(par_solver, 6);
         HYPRE_BoomerAMGSetStrongThreshold(par_solver, 0.25);
         HYPRE_BoomerAMGSetTol(par_solver, 1.9e-6);
         HYPRE_BoomerAMGSetPrintLevel(par_solver, 1);
         HYPRE_BoomerAMGSetPrintFileName(par_solver, "ex9.out.log");
         HYPRE_BoomerAMGSetMaxIter(par_solver, 50);

         /* do the setup */
         HYPRE_BoomerAMGSetup(par_solver, par_A, par_b, par_x);

         /* do the solve */
         HYPRE_BoomerAMGSolve(par_solver, par_A, par_b, par_x);

         /* get some info */
         HYPRE_BoomerAMGGetNumIterations(par_solver, &its);
         HYPRE_BoomerAMGGetFinalRelativeResidualNorm(par_solver,
                                                     &final_res_norm);
         /* clean up */
         HYPRE_BoomerAMGDestroy(par_solver);
      }
      else
      {
         if (myid ==0) printf("\n ERROR: Invalid solver id specified.\n");
      }

      /* Gather the solution vector.  This needs to be done if:
         (1) the  object  type is parcsr OR
         (2) any one of the variables is NOT cell-centered */
      if (object_type == HYPRE_PARCSR)
      {
         HYPRE_SStructVectorGather(x);
      }

      /* Save the solution for GLVis visualization, see vis/glvis-ex7.sh */
      if (vis)
      {
         FILE *file;
         char filename[255];

         int k, part = 0, var;
         int nvalues = n*n;
         double *values = calloc(nvalues, sizeof(double));

         /* save local solution for variable u */
         var = 0;
         HYPRE_SStructVectorGetBoxValues(x, part, ilower, iupper,
                                         var, values);

         sprintf(filename, "%s.%06d", "vis/ex9-u.sol", myid);
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

         /* save local solution for variable v */
         var = 1;
         HYPRE_SStructVectorGetBoxValues(x, part, ilower, iupper,
                                         var, values);

         sprintf(filename, "%s.%06d", "vis/ex9-v.sol", myid);
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
            GLVis_PrintGlobalSquareMesh("vis/ex9.mesh", N*n-1);
      }

      if (myid == 0)
      {
         printf("\n");
         printf("Iterations = %d\n", its);
         printf("Final Relative Residual Norm = %g\n", final_res_norm);
         printf("\n");
      }
   }

   /* Free memory */
   HYPRE_SStructGridDestroy(grid);
   HYPRE_SStructStencilDestroy(stencil_v);
   HYPRE_SStructStencilDestroy(stencil_u);
   HYPRE_SStructGraphDestroy(graph);
   HYPRE_SStructMatrixDestroy(A);
   HYPRE_SStructVectorDestroy(b);
   HYPRE_SStructVectorDestroy(x);

   /* Finalize MPI */
   MPI_Finalize();

   return (0);
}
