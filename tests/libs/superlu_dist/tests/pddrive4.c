/*! \file
Copyright (c) 2003, The Regents of the University of California, through
Lawrence Berkeley National Laboratory (subject to receipt of any required 
approvals from U.S. Dept. of Energy) 

All rights reserved. 

The source code is distributed under BSD license, see the file License.txt
at the top-level directory.
*/


/*! @file 
 * \brief This example illustrates how to divide up the processes into subgroups
 *
 * <pre>
 * -- Distributed SuperLU routine (version 4.1) --
 * Lawrence Berkeley National Lab, Univ. of California Berkeley.
 * March 15, 2003
 * April 5, 2015
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
 * The driver program PDDRIVE4.
 *
 * This example illustrates how to divide up the processes into
 * subgroups (multiple grids) such that each subgroup solves a linear
 * system independently from the other.
 *
 * In this example, there are 2 subgroups:
 *  1. subgroup 1 consists of processes 0 to 5 arranged as
 *     a 2-by-3 process grid.
 *  2. subgroup 2 consists of processes 6 to 9 arranged as
 *     a 2-by-2 process grid.
 *
 * With MPICH,  program may be run by typing:
 *    mpiexec -n 10 pddrive4 big.rua
 * </pre>
 */

int main(int argc, char *argv[])
{
    superlu_dist_options_t options;
    SuperLUStat_t stat;
    SuperMatrix A;
    ScalePermstruct_t ScalePermstruct;
    LUstruct_t LUstruct;
    SOLVEstruct_t SOLVEstruct;
    gridinfo_t grid1, grid2;
    double   *berr;
    double   *a, *b, *xtrue;
    int_t    *asub, *xa;
    int_t    i, j, m, n;
    int      nprow, npcol, ldumap, p;
    int_t    usermap[6];
    int      iam, info, ldb, ldx, nprocs;
    int      nrhs = 1;   /* Number of right-hand side. */
    char     **cpp, c;
    FILE *fp, *fopen();
    int cpp_defs();


    /* ------------------------------------------------------------
       INITIALIZE MPI ENVIRONMENT. 
       ------------------------------------------------------------*/
    MPI_Init( &argc, &argv );
    MPI_Comm_size( MPI_COMM_WORLD, &nprocs );
    if ( nprocs < 10 ) {
	fprintf(stderr, "Requires at least 10 processes\n");
	exit(-1);
    }

    /* Parse command line argv[]. */
    for (cpp = argv+1; *cpp; ++cpp) {
	if ( **cpp == '-' ) {
	    c = *(*cpp+1);
	    ++cpp;
	    switch (c) {
	      case 'h':
		  printf("Options:\n");
		  printf("\t-r <int>: process rows    (default %d)\n", nprow);
		  printf("\t-c <int>: process columns (default %d)\n", npcol);
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
       INITIALIZE THE SUPERLU PROCESS GRID 1. 
       ------------------------------------------------------------*/
    nprow = 2;
    npcol = 3;
    ldumap = 2;
    p = 0;    /* Grid 1 starts from process 0. */
    for (i = 0; i < nprow; ++i)
	for (j = 0; j < npcol; ++j) usermap[i+j*ldumap] = p++;
    superlu_gridmap(MPI_COMM_WORLD, nprow, npcol, usermap, ldumap, &grid1);

    /* ------------------------------------------------------------
       INITIALIZE THE SUPERLU PROCESS GRID 2. 
       ------------------------------------------------------------*/
    nprow = 2;
    npcol = 2;
    ldumap = 2;
    p = 6;   /* Grid 2 starts from process 6. */
    for (i = 0; i < nprow; ++i)
	for (j = 0; j < npcol; ++j) usermap[i+j*ldumap] = p++;
    superlu_gridmap(MPI_COMM_WORLD, nprow, npcol, usermap, ldumap, &grid2);

    /* Bail out if I do not belong in any of the 2 grids. */
    MPI_Comm_rank( MPI_COMM_WORLD, &iam );
    if ( iam >= 10 ) goto out;
    
#if ( DEBUGlevel>=1 )
    CHECK_MALLOC(iam, "Enter main()");
#endif

    if ( iam >= 0 && iam < 6 ) { /* I am in grid 1. */
	iam = grid1.iam;  /* Get the logical number in the new grid. */

        /* ------------------------------------------------------------
           GET THE MATRIX FROM FILE AND SETUP THE RIGHT HAND SIDE. 
           ------------------------------------------------------------*/
        dcreate_matrix(&A, nrhs, &b, &ldb, &xtrue, &ldx, fp, &grid1);
	
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
            options.ReplaceTinyPivot = NO;
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

        m = A.nrow;
        n = A.ncol;

	/* Initialize ScalePermstruct and LUstruct. */
	ScalePermstructInit(m, n, &ScalePermstruct);
	LUstructInit(n, &LUstruct);

	/* Initialize the statistics variables. */
	PStatInit(&stat);
	
	/* Call the linear equation solver. */
	pdgssvx(&options, &A, &ScalePermstruct, b, ldb, nrhs, &grid1,
                &LUstruct, &SOLVEstruct, berr, &stat, &info);

        /* Check the accuracy of the solution. */
        pdinf_norm_error(iam, ((NRformat_loc *)A.Store)->m_loc,
                         nrhs, b, ldb, xtrue, ldx, &grid1);
    
	/* Print the statistics. */
	PStatPrint(&options, &stat, &grid1);

	/* ------------------------------------------------------------
	   DEALLOCATE STORAGE.
	   ------------------------------------------------------------*/
	PStatFree(&stat);
        Destroy_CompRowLoc_Matrix_dist(&A);
        ScalePermstructFree(&ScalePermstruct);
	Destroy_LU(n, &grid1, &LUstruct);
	LUstructFree(&LUstruct);
        if ( options.SolveInitialized ) {
            dSolveFinalize(&options, &SOLVEstruct);
        }
	SUPERLU_FREE(b);
	SUPERLU_FREE(xtrue);
	SUPERLU_FREE(berr);

    } else { /* I am in grid 2. */
	iam = grid2.iam;  /* Get the logical number in the new grid. */

        /* ------------------------------------------------------------
           GET THE MATRIX FROM FILE AND SETUP THE RIGHT HAND SIDE. 
           ------------------------------------------------------------*/
        dcreate_matrix(&A, nrhs, &b, &ldb, &xtrue, &ldx, fp, &grid2);

	if ( !(berr = doubleMalloc_dist(nrhs)) )
	    ABORT("Malloc fails for berr[].");

	/* ------------------------------------------------------------
	   NOW WE SOLVE THE LINEAR SYSTEM.
	   ------------------------------------------------------------*/
	
        /* Set the default input options:
            options.Fact = DOFACT;
            options.Equil = YES;
            options.ColPerm = MMD_AT_PLUS_A;
            options.RowPerm = LargeDiag;
            options.ReplaceTinyPivot = YES;
            options.Trans = NOTRANS;
            options.IterRefine = DOUBLE;
            options.SolveInitialized = NO;
            options.RefineInitialized = NO;
            options.PrintStat = YES;
         */
	set_default_options_dist(&options);
	
        m = A.nrow;
        n = A.ncol;

	/* Initialize ScalePermstruct and LUstruct. */
	ScalePermstructInit(m, n, &ScalePermstruct);
	LUstructInit(n, &LUstruct);

	/* Initialize the statistics variables. */
	PStatInit(&stat);
	
	/* Call the linear equation solver. */
	pdgssvx(&options, &A, &ScalePermstruct, b, ldb, nrhs, &grid2,
                &LUstruct, &SOLVEstruct, berr, &stat, &info);

        /* Check the accuracy of the solution. */
        pdinf_norm_error(iam, ((NRformat_loc *)A.Store)->m_loc,
                         nrhs, b, ldb, xtrue, ldx, &grid2);
    
	/* Print the statistics. */
	PStatPrint(&options, &stat, &grid2);

	/* ------------------------------------------------------------
	   DEALLOCATE STORAGE.
	   ------------------------------------------------------------*/
	PStatFree(&stat);
        Destroy_CompRowLoc_Matrix_dist(&A);
        ScalePermstructFree(&ScalePermstruct);
	Destroy_LU(n, &grid2, &LUstruct);
	LUstructFree(&LUstruct);
        if ( options.SolveInitialized ) {
            dSolveFinalize(&options, &SOLVEstruct);
        }
	SUPERLU_FREE(b);
	SUPERLU_FREE(xtrue);
	SUPERLU_FREE(berr);
    }

    /* ------------------------------------------------------------
       RELEASE THE SUPERLU PROCESS GRIDS.
       ------------------------------------------------------------*/
    superlu_gridexit(&grid1);
    superlu_gridexit(&grid2);

out:
    /* ------------------------------------------------------------
       TERMINATES THE MPI EXECUTION ENVIRONMENT.
       ------------------------------------------------------------*/
    MPI_Finalize();

#if ( DEBUGlevel>=1 )
    CHECK_MALLOC(iam, "Exit main()");
#endif

}
