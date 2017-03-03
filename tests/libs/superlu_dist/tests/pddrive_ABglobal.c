/*! \file
Copyright (c) 2003, The Regents of the University of California, through
Lawrence Berkeley National Laboratory (subject to receipt of any required 
approvals from U.S. Dept. of Energy) 

All rights reserved. 

The source code is distributed under BSD license, see the file License.txt
at the top-level directory.
*/


/*! @file 
 * \brief Driver program for pdgssvx_ABglobal example
 *
 * <pre>
 * -- Distributed SuperLU routine (version 1.0) --
 * Lawrence Berkeley National Lab, Univ. of California Berkeley.
 * September 1, 1999
 * </pre>
 */

#include <math.h>
#include "superlu_ddefs.h"

/*! \brief
 *
 * <pre>
 * Purpose
 * =======
 *
 * The driver program pddrive_ABglobal.
 *
 * This example illustrates how to use pdgssvx_ABglobal with the full
 * (default) options to solve a linear system.
 * 
 * Five basic steps are required:
 *   1. Initialize the MPI environment and the SuperLU process grid
 *   2. Set up the input matrix and the right-hand side
 *   3. Set the options argument
 *   4. Call pdgssvx_ABglobal
 *   5. Release the process grid and terminate the MPI environment
 *
 * On an IBM SP, the program may be run by typing
 *    poe pddrive_ABglobal -r <proc rows> -c <proc columns> <input_file> -procs <p>
 * </pre>
 */

int main(int argc, char *argv[])
{
    superlu_dist_options_t options;
    SuperLUStat_t stat;
    SuperMatrix A;
    ScalePermstruct_t ScalePermstruct;
    LUstruct_t LUstruct;
    gridinfo_t grid;
    double   *berr;
    double   *a, *b, *xtrue;
    int_t    *asub, *xa;
    int_t    m, n, nnz;
    int_t    nprow, npcol;
    int      iam, info, ldb, ldx, nrhs;
    char     trans[1];
    char     **cpp, c;
    FILE *fp, *fopen();
    extern int cpp_defs();

    /* prototypes */
    extern void LUstructInit(const int_t, LUstruct_t *);
    extern void LUstructFree(LUstruct_t *);
    extern void Destroy_LU(int_t, gridinfo_t *, LUstruct_t *);

    nprow = 1;  /* Default process rows.      */
    npcol = 1;  /* Default process columns.   */
    nrhs = 1;   /* Number of right-hand side. */

    /* ------------------------------------------------------------
       INITIALIZE MPI ENVIRONMENT. 
       ------------------------------------------------------------*/
    MPI_Init( &argc, &argv );

    /* Parse command line argv[]. */
    for (cpp = argv+1; *cpp; ++cpp) {
	if ( **cpp == '-' ) {
	    c = *(*cpp+1);
	    ++cpp;
	    switch (c) {
	      case 'h':
		  printf("Options:\n");
		  printf("\t-r <int>: process rows    (default " IFMT ")\n", nprow);
		  printf("\t-c <int>: process columns (default " IFMT ")\n", npcol);
		  exit(0);
		  break;
	      case 'r': nprow = atoi(*cpp);
		        break;
	      case 'c': npcol = atoi(*cpp);
		        break;
	    }
	} else { /* Last arg is considered a filename */
	    if ( !(fp = fopen(*cpp, "r")) ) {
                ABORT("File does not exist");
            }
	    break;
	}
    }

    /* ------------------------------------------------------------
       INITIALIZE THE SUPERLU PROCESS GRID. 
       ------------------------------------------------------------*/
    superlu_gridinit(MPI_COMM_WORLD, nprow, npcol, &grid);

    /* Bail out if I do not belong in the grid. */
    iam = grid.iam;
    if ( iam >= nprow * npcol )
	goto out;

#if ( VAMPIR>=1 )
    VT_traceoff();
#endif

#if ( DEBUGlevel>=1 )
    CHECK_MALLOC(iam, "Enter main()");
#endif
    
    /* ------------------------------------------------------------
       PROCESS 0 READS THE MATRIX A, AND THEN BROADCASTS IT TO ALL
       THE OTHER PROCESSES.
       ------------------------------------------------------------*/
    if ( !iam ) {
	/* Print the CPP definitions. */
	cpp_defs();
	
	/* Read the matrix stored on disk in Harwell-Boeing format. */
	dreadhb_dist(iam, fp, &m, &n, &nnz, &a, &asub, &xa);
	
	printf("Input matrix file: %s\n", *cpp);
	printf("\tDimension\t" IFMT "x" IFMT "\t # nonzeros " IFMT "\n", m, n, nnz);
	printf("\tProcess grid\t%d X %d\n", (int) grid.nprow, (int) grid.npcol);

	/* Broadcast matrix A to the other PEs. */
	MPI_Bcast( &m,   1,   mpi_int_t,  0, grid.comm );
	MPI_Bcast( &n,   1,   mpi_int_t,  0, grid.comm );
	MPI_Bcast( &nnz, 1,   mpi_int_t,  0, grid.comm );
	MPI_Bcast( a,    nnz, MPI_DOUBLE, 0, grid.comm );
	MPI_Bcast( asub, nnz, mpi_int_t,  0, grid.comm );
	MPI_Bcast( xa,   n+1, mpi_int_t,  0, grid.comm );
    } else {
	/* Receive matrix A from PE 0. */
	MPI_Bcast( &m,   1,   mpi_int_t,  0, grid.comm );
	MPI_Bcast( &n,   1,   mpi_int_t,  0, grid.comm );
	MPI_Bcast( &nnz, 1,   mpi_int_t,  0, grid.comm );

	/* Allocate storage for compressed column representation. */
	dallocateA_dist(n, nnz, &a, &asub, &xa);

	MPI_Bcast( a,    nnz, MPI_DOUBLE, 0, grid.comm );
	MPI_Bcast( asub, nnz, mpi_int_t,  0, grid.comm );
	MPI_Bcast( xa,   n+1, mpi_int_t,  0, grid.comm );
    }
	
    /* Create compressed column matrix for A. */
    dCreate_CompCol_Matrix_dist(&A, m, n, nnz, a, asub, xa,
				SLU_NC, SLU_D, SLU_GE);

    /* Generate the exact solution and compute the right-hand side. */
    if (!(b=doubleMalloc_dist(m*nrhs))) ABORT("Malloc fails for b[]");
    if (!(xtrue=doubleMalloc_dist(n*nrhs))) ABORT("Malloc fails for xtrue[]");
    *trans = 'N';
    ldx = n;
    ldb = m;
    dGenXtrue_dist(n, nrhs, xtrue, ldx);
    dFillRHS_dist(trans, nrhs, xtrue, ldx, &A, b, ldb);

    if ( !(berr = doubleMalloc_dist(nrhs)) )
	ABORT("Malloc fails for berr[].");

    /* ------------------------------------------------------------
       NOW WE SOLVE THE LINEAR SYSTEM.
       ------------------------------------------------------------*/

    /* Set the default input options:
        options.Fact = DOFACT;
        options.Equil = YES;
        options.ColPerm = METIS_AT_PLUS_A;
        options.RowPerm = LargeDiag;
        options.ReplaceTinyPivot = YES;
        options.Trans = NOTRANS;
        options.IterRefine = DOUBLE;
        options.SolveInitialized = NO;
        options.RefineInitialized = NO;
        options.PrintStat = YES;
     */
    set_default_options_dist(&options);

    if (!iam) {
	print_sp_ienv_dist(&options);
	print_options_dist(&options);
    }

    /* Initialize ScalePermstruct and LUstruct. */
    ScalePermstructInit(m, n, &ScalePermstruct);
    LUstructInit(n, &LUstruct);

    /* Initialize the statistics variables. */
    PStatInit(&stat);

    /* Call the linear equation solver. */
    pdgssvx_ABglobal(&options, &A, &ScalePermstruct, b, ldb, nrhs, &grid,
		     &LUstruct, berr, &stat, &info);

    /* Check the accuracy of the solution. */
    if ( !iam ) {
	dinf_norm_error_dist(n, nrhs, b, ldb, xtrue, ldx, &grid);
    }
    PStatPrint(&options, &stat, &grid);        /* Print the statistics. */

    /* ------------------------------------------------------------
       DEALLOCATE STORAGE.
       ------------------------------------------------------------*/
    PStatFree(&stat);
    Destroy_CompCol_Matrix_dist(&A);
    Destroy_LU(n, &grid, &LUstruct);
    ScalePermstructFree(&ScalePermstruct);
    LUstructFree(&LUstruct);
    SUPERLU_FREE(b);
    SUPERLU_FREE(xtrue);
    SUPERLU_FREE(berr);

    /* ------------------------------------------------------------
       RELEASE THE SUPERLU PROCESS GRID.
       ------------------------------------------------------------*/
out:
    superlu_gridexit(&grid);

    /* ------------------------------------------------------------
       TERMINATES THE MPI EXECUTION ENVIRONMENT.
       ------------------------------------------------------------*/
    MPI_Finalize();

#if ( DEBUGlevel>=1 )
    CHECK_MALLOC(iam, "Exit main()");
#endif

}


int cpp_defs()
{
    printf(".. CPP definitions:\n");
#if ( PRNTlevel>=1 )
    printf("\tPRNTlevel = %d\n", PRNTlevel);
#endif
#if ( DEBUGlevel>=1 )
    printf("\tDEBUGlevel = %d\n", DEBUGlevel);
#endif
#if ( PROFlevel>=1 )
    printf("\tPROFlevel = %d\n", PROFlevel);
#endif
#if ( StaticPivot>=1 )
    printf("\tStaticPivot = %d\n", StaticPivot);
#endif
    printf("....\n");
    return 0;
}
