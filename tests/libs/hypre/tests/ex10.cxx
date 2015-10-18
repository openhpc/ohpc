/*
   Example 10

   Interface:      Finite Element Interface (FEI)

   Compile with:   make ex10

   Sample run:     mpirun -np 4 ex10 -n 120 -solver 2

   To see options: ex10 -help

   Description:    This code solves a system corresponding to a discretization
                   of the Laplace equation -Delta u = 1 with zero boundary
                   conditions on the unit square.  The domain is split into
                   a n x n grid of quadrilateral elements and each processors
                   owns a horizontal strip of size m x n, where m = n/nprocs. We
                   use bilinear finite element discretization, so there are
                   nodes (vertices) that are shared between neighboring
                   processors. The Finite Element Interface is used to assemble
                   the matrix and solve the problem. Nine different solvers are
                   available.
*/

#include <math.h>
#include <iostream>
#include <fstream>
#include "_hypre_utilities.h"
#include "LLNL_FEI_Impl.h"

using namespace std;

#include "vis.c"

int main(int argc, char *argv[])
{
   int i, j, k;

   int nprocs, mypid;

   int n, m, offset;
   double h;

   int solverID;
   int vis;

   // Initialize MPI
   MPI_Init(&argc, &argv);
   MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
   MPI_Comm_rank(MPI_COMM_WORLD, &mypid);

   // Set default parameters
   n = 4*nprocs;
   solverID = 2;
   vis = 0;

   // Parse command line
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
            solverID = atoi(argv[arg_index++]);
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

      if ((print_usage) && (mypid == 0))
      {
         printf("\n");
         printf("Usage: %s [<options>]\n", argv[0]);
         printf("\n");
         printf("  -n <n>              : problem size per processor (default: %d)\n", 4*nprocs);
         printf("  -solver <ID>        : solver ID\n");
         printf("                        0 - DS-PCG\n");
         printf("                        1 - ParaSails-PCG\n");
         printf("                        2 - AMG-PCG (default)\n");
         printf("                        3 - AMGSA-PCG\n");
         printf("                        4 - Euclid-PCG\n");
         printf("                        5 - DS-GMRES\n");
         printf("                        6 - AMG-GMRES\n");
         printf("                        7 - AMGSA-GMRES\n");
         printf("                        8 - Euclid-GMRES\n");
         printf("  -vis                : save the solution for GLVis visualization\n");
         printf("\n");
      }

      if (print_usage)
      {
         MPI_Finalize();
         return (0);
      }
   }

   // Each processor owns a m x n grid of quadrilateral finite elements.
   // The unknowns are located in the nodes (vertices of the mesh) and
   // are numbered globally starting from the lower left corner and moving
   // row-wise to the upper right corner.
   m = n / nprocs;
   offset = mypid*(m*(n+1));

   h = 1.0 / n; // mesh size

   // 1. FEI initialization phase

   // Instantiate the FEI object
   LLNL_FEI_Impl *feiPtr = new LLNL_FEI_Impl(MPI_COMM_WORLD);

   // Set the matrix storage type to HYPRE
   {
      char **paramStrings = new char*[1];
      paramStrings[0] = new char[100];
      strcpy(paramStrings[0], "externalSolver HYPRE");
      feiPtr->parameters(1, paramStrings);
      delete paramStrings[0];
      delete [] paramStrings;
   }

   // The unknowns in FEI are called fields. Each field has an
   // identifier (fieldID) and rank (fieldSize).
   int nFields = 1;
   int *fieldSizes = new int[nFields];  fieldSizes[0] = 1;
   int *fieldIDs = new int[nFields];    fieldIDs[0] = 0;

   // Pass the field information to the FEI
   feiPtr->initFields(nFields, fieldSizes, fieldIDs);

   // Elements are grouped into blocks (in this case one block), and we
   // have to describe the number of elements in the block (nElems) as
   // well as the fields (unknowns) per element.
   int elemBlkID = 0;
   int nElems = m*n;
   int elemNNodes = 4; // number of (shared) nodes per element
   int *nodeNFields = new int[elemNNodes]; // fields per node
   int **nodeFieldIDs = new int*[elemNNodes]; // node-fields IDs
   int elemNFields = 0; // number of (non-shared) fields per element
   int *elemFieldIDs = NULL; // element-fields IDs
   for (i = 0; i < elemNNodes; i++)
   {
      nodeNFields[i] = 1;
      nodeFieldIDs[i] = new int[nodeNFields[i]];
      nodeFieldIDs[i][0] = fieldIDs[0];
   }

   // Pass the block information to the FEI. The interleave parameter
   // controls how different fields are ordered in the element matrices.
   int interleave = 0;
   feiPtr->initElemBlock(elemBlkID, nElems, elemNNodes, nodeNFields,
                         nodeFieldIDs, elemNFields, elemFieldIDs, interleave);

   // List the global indexes (IDs) of the nodes in each element
   int **elemConn = new int*[nElems];
   for (i = 0; i < m; i++)
      for (j = 0; j < n; j++)
      {
         elemConn[i*n+j] = new int[elemNNodes]; // element with coordinates (i,j)
         elemConn[i*n+j][0] = offset + i*(n+1)+j;     // node in the lower left
         elemConn[i*n+j][1] = elemConn[i*n+j][0]+1;   // node in the lower right
         elemConn[i*n+j][2] = elemConn[i*n+j][1]+n+1; // node in the upper right
         elemConn[i*n+j][3] = elemConn[i*n+j][2]-1;   // node in the upper left
      }

   // Pass the element topology information to the FEI
   for (i = 0; i < nElems; i++)
      feiPtr->initElem(elemBlkID, i, elemConn[i]);

   // List the global indexes of nodes that are shared between processors
   int nShared, *SharedIDs, *SharedLengs, **SharedProcs;
   if (mypid == 0)
   {
      // Nodes in the top row are shared
      nShared = n+1;
      SharedIDs = new int[nShared];
      for (i = 0; i < nShared; i++)
         SharedIDs[i] = offset + m*(n+1) + i;
      SharedLengs = new int[nShared];
      for (i = 0; i < nShared; i++)
         SharedLengs[i] = 2;
      SharedProcs = new int*[nShared];
      for (i = 0; i < nShared; i++)
      {
         SharedProcs[i] = new int[SharedLengs[i]];
         SharedProcs[i][0] = mypid;
         SharedProcs[i][1] = mypid+1;
      }
   }
   else if (mypid == nprocs-1)
   {
      // Nodes in the bottom row are shared
      nShared = n+1;
      SharedIDs = new int[nShared];
      for (i = 0; i < nShared; i++)
         SharedIDs[i] = offset + i;
      SharedLengs = new int[nShared];
      for (i = 0; i < nShared; i++)
         SharedLengs[i] = 2;
      SharedProcs = new int*[nShared];
      for (i = 0; i < nShared; i++)
      {
         SharedProcs[i] = new int[SharedLengs[i]];
         SharedProcs[i][0] = mypid-1;
         SharedProcs[i][1] = mypid;
      }
   }
   else
   {
      // Nodes in the top and bottom rows are shared
      nShared = 2*(n+1);
      SharedIDs = new int[nShared];
      for (i = 0; i < n+1; i++)
      {
         SharedIDs[i] = offset + i;
         SharedIDs[n+1+i] = offset + m*(n+1) + i;
      }
      SharedLengs = new int[nShared];
      for (i = 0; i < nShared; i++)
         SharedLengs[i] = 2;
      SharedProcs = new int*[nShared];
      for (i = 0; i < n+1; i++)
      {
         SharedProcs[i] = new int[SharedLengs[i]];
         SharedProcs[i][0] = mypid-1;
         SharedProcs[i][1] = mypid;

         SharedProcs[n+1+i] = new int[SharedLengs[n+1+i]];
         SharedProcs[n+1+i][0] = mypid;
         SharedProcs[n+1+i][1] = mypid+1;
      }
   }

   // Pass the shared nodes information to the FEI
   if (nprocs != 1 && nShared > 0)
      feiPtr->initSharedNodes(nShared, SharedIDs, SharedLengs, SharedProcs);

   // Finish the FEI initialization phase
   feiPtr->initComplete();

   // 2. FEI load phase

   // Specify the boundary conditions
   int nBCs, *BCEqn;
   double **alpha, **beta, **gamma;
   if (mypid == 0)
   {
      // Nodes in the bottom row and left and right columns
      nBCs = n+1 + 2*m;
      BCEqn = new int[nBCs];
      for (i = 0; i < n+1; i++)
         BCEqn[i] = offset + i;
      for (i = 0; i < m; i++)
      {
         BCEqn[n+1+2*i] = offset + (i+1)*(n+1);
         BCEqn[n+2+2*i] = offset + (i+1)*(n+1)+n;
      }
   }
   else if (mypid == nprocs-1)
   {
      // Nodes in the top row and left and right columns
      nBCs = n+1 + 2*m;
      BCEqn = new int[nBCs];
      for (i = 0; i < n+1; i++)
         BCEqn[i] = offset + m*(n+1) + i;
      for (i = 0; i < m; i++)
      {
         BCEqn[n+1+2*i] = offset + i*(n+1);
         BCEqn[n+2+2*i] = offset + i*(n+1)+n;
      }
   }
   else
   {
      // Nodes in the left and right columns
      nBCs = 2*(m+1);
      BCEqn = new int[nBCs];
      for (i = 0; i < m+1; i++)
      {
         BCEqn[2*i]   = offset + i*(n+1);
         BCEqn[2*i+1] = offset + i*(n+1)+n;
      }
   }

   // The arrays alpha, beta and gamma specify the type of boundary
   // condition (essential, natural, mixed). The most general form
   // for Laplace problems is alpha U + beta dU/dn = gamma. In this
   // example we impose zero Dirichlet boundary conditions.
   alpha = new double*[nBCs];
   beta  = new double*[nBCs];
   gamma = new double*[nBCs];
   for (i = 0; i < nBCs; i++)
   {
      alpha[i] = new double[1];  alpha[i][0] = 1.0;
      beta[i]  = new double[1];  beta[i][0]  = 0.0;
      gamma[i] = new double[1];  gamma[i][0] = 0.0;
   }

   // Pass the boundary condition information to the FEI
   feiPtr->loadNodeBCs(nBCs, BCEqn, fieldIDs[0], alpha, beta, gamma);

   // Specify element stiffness matrices
   double ***elemStiff = new double**[nElems];
   for (i = 0; i < m; i++)
      for (j = 0; j < n; j++)
      {
         // Element with coordinates (i,j)
         elemStiff[i*n+j] = new double*[elemNNodes];
         for (k = 0; k < elemNNodes; k++)
            elemStiff[i*n+j][k] = new double[elemNNodes];

         // Stiffness matrix for the reference square
         //                3 +---+ 2
         //                  |   |
         //                0 +---+ 1

         double **A = elemStiff[i*n+j];

         for (k = 0; k < 4; k++)
            A[k][k] = 2/3.;

         A[0][1] = A[1][0] = -1/6.;
         A[0][2] = A[2][0] = -1/3.;
         A[0][3] = A[3][0] = -1/6.;
         A[1][2] = A[2][1] = -1/6.;
         A[1][3] = A[3][1] = -1/3.;
         A[2][3] = A[3][2] = -1/6.;
      }

   // Specify element load vectors
   double *elemLoad = new double[nElems*elemNNodes];
   for (i = 0; i < nElems*elemNNodes; i++)
      elemLoad[i] = h*h/4;

   // Assemble the matrix. The elemFormat parameter describes
   // the storage (symmetric/non-symmetric, row/column-wise)
   // of the element stiffness matrices.
   int elemFormat = 0;
   for (i = 0; i < nElems; i++)
      feiPtr->sumInElem(elemBlkID, i, elemConn[i], elemStiff[i],
                        &(elemLoad[i*elemNNodes]), elemFormat);

   // Finish the FEI load phase
   feiPtr->loadComplete();

   // Clean up
   for (i = 0; i < nElems; i++) delete [] elemConn[i];
   delete [] elemConn;
   for (i = 0; i < nElems; i++)
   {
      for (j = 0; j < elemNNodes; j++) delete [] elemStiff[i][j];
      delete [] elemStiff[i];
   }
   delete [] elemStiff;
   delete [] elemLoad;

   delete [] BCEqn;
   for (i = 0; i < nBCs; i++)
   {
      delete [] alpha[i];
      delete [] beta[i];
      delete [] gamma[i];
   }
   delete [] alpha;
   delete [] beta;
   delete [] gamma;

   if (nShared > 0)
   {
      delete [] SharedIDs;
      delete [] SharedLengs;
      for (i = 0; i < nShared; i++) delete [] SharedProcs[i];
      delete [] SharedProcs;
   }

   delete [] nodeNFields;
   for (i = 0; i < elemNNodes; i++) delete [] nodeFieldIDs[i];
   delete [] nodeFieldIDs;

   delete [] fieldSizes;
   delete [] fieldIDs;

   // 3. Set up problem parameters and pass them to the FEI
   {
      int nParams = 19;
      char **paramStrings = new char*[nParams];
      for (i = 0; i < nParams; i++)
         paramStrings[i] = new char[100];

      strcpy(paramStrings[0], "outputLevel 2");
      switch(solverID)
      {
         case 0:
            strcpy(paramStrings[1], "solver cg");
            strcpy(paramStrings[2], "preconditioner diagonal");
            break;
         case 1:
            strcpy(paramStrings[1], "solver cg");
            strcpy(paramStrings[2], "preconditioner parasails");
            break;
         default:
         case 2:
            strcpy(paramStrings[1], "solver cg");
            strcpy(paramStrings[2], "preconditioner boomeramg");
            break;
         case 3:
            strcpy(paramStrings[1], "solver cg");
            strcpy(paramStrings[2], "preconditioner mli");
            break;
         case 4:
            strcpy(paramStrings[1], "solver cg");
            strcpy(paramStrings[2], "preconditioner euclid");
            break;
         case 5:
            strcpy(paramStrings[1], "solver gmres");
            strcpy(paramStrings[2], "preconditioner diagonal");
            break;
         case 6:
            strcpy(paramStrings[1], "solver gmres");
            strcpy(paramStrings[2], "preconditioner boomeramg");
            break;
         case 7:
            strcpy(paramStrings[1], "solver gmres");
            strcpy(paramStrings[2], "preconditioner mli");
            break;
         case 8:
            strcpy(paramStrings[1], "solver gmres");
            strcpy(paramStrings[2], "preconditioner euclid");
            break;
      }
      strcpy(paramStrings[3], "maxIterations 100");
      strcpy(paramStrings[4], "tolerance 1e-6");
      strcpy(paramStrings[5], "gmresDim 30");
      strcpy(paramStrings[6], "amgNumSweeps 1");
      strcpy(paramStrings[7], "amgCoarsenType falgout");
      strcpy(paramStrings[8], "amgRelaxType hybridsym");
      strcpy(paramStrings[9], "amgSystemSize 1");
      strcpy(paramStrings[10], "amgStrongThreshold 0.25");
      strcpy(paramStrings[11], "MLI smoother HSGS");
      strcpy(paramStrings[12], "MLI numSweeps 1");
      strcpy(paramStrings[13], "MLI smootherWeight 1.0");
      strcpy(paramStrings[14], "MLI nodeDOF 1");
      strcpy(paramStrings[15], "MLI nullSpaceDim 1");
      strcpy(paramStrings[16], "MLI minCoarseSize 50");
      strcpy(paramStrings[17], "MLI outputLevel 0");
      strcpy(paramStrings[18], "parasailsSymmetric outputLevel 0");

      feiPtr->parameters(nParams, paramStrings);

      for (i = 0; i < nParams; i++)
         delete [] paramStrings[i];
      delete [] paramStrings;
   }

   // 4. Solve the system
   int status;
   feiPtr->solve(&status);

   // 5. Save the solution for GLVis visualization, see vis/glvis-ex10.sh
   if (vis)
   {
      int numNodes, *nodeIDList, *solnOffsets;
      double *solnValues;

      // Get the number of nodes in the element block
      feiPtr->getNumBlockActNodes(elemBlkID, &numNodes);

      // Get their global IDs
      nodeIDList = new int[numNodes];
      feiPtr->getBlockNodeIDList(elemBlkID, numNodes, nodeIDList);

      // Get the values corresponding to nodeIDList
      solnOffsets = new int[numNodes];
      solnValues = new double[numNodes];
      feiPtr->getBlockNodeSolution(elemBlkID, numNodes, nodeIDList,
                                   solnOffsets, solnValues);

      // Find the location of the ith local node
      for (i = 0; i < numNodes; i++)
         solnOffsets[nodeIDList[i]-offset] = i;

      // Save the ordered nodal values to a file
      char sol_out[20];
      sprintf(sol_out, "%s.%06d", "vis/ex10.sol", mypid);
      ofstream sol(sol_out);
      sol << "FiniteElementSpace\n"
          << "FiniteElementCollection: H1_2D_P1\n"
          << "VDim: 1\n"
          << "Ordering: 0\n\n";
      for (i = 0; i < numNodes; i++)
         sol << solnValues[solnOffsets[i]] << endl;

      // Save local finite element mesh
      GLVis_PrintLocalSquareMesh("vis/ex10.mesh", n, m, h, 0, mypid*h*m, mypid);

      // additional visualization data
      if (mypid == 0)
      {
         char data_out[20];
         sprintf(data_out, "%s", "vis/ex10.data");
         ofstream data(data_out);
         data << "np " << nprocs << endl;
      }

      // Clean up
      delete [] solnValues;
      delete [] solnOffsets;
      delete [] nodeIDList;
   }
   delete feiPtr;

   // Finalize MPI
   MPI_Finalize();

   return (0);
}
