/*! \file
Copyright (c) 2003, The Regents of the University of California, through
Lawrence Berkeley National Laboratory (subject to receipt of any required 
approvals from U.S. Dept. of Energy) 

All rights reserved. 

The source code is distributed under BSD license, see the file License.txt
at the top-level directory.
*/


/*! @file 
 * \brief Driver program for PDGSSVX example
 *
 * <pre>
 * -- Distributed SuperLU routine (version 6.1) --
 * Lawrence Berkeley National Lab, Univ. of California Berkeley.
 * March 15, 2003
 * April 5, 2015
 * December 31, 2016 version 5.1.3
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
 * The driver program PDDRIVE2.
 *
 * This example illustrates how to use  to solve
 * systems repeatedly with the same sparsity pattern of matrix A.
 * In this case, the column permutation vector ScalePermstruct->perm_c is
 * computed once. The following data structures will be reused in the
 * subsequent call to PDGSSVX:
 *        ScalePermstruct : perm_c
 *        LUstruct        : etree
 *
 * With MPICH,  program may be run by typing:
 *    mpiexec -n <np> pddrive2 -r <proc rows> -c <proc columns> g20.rua
 * </pre>
 */

int main(int argc, char *argv[])
{
    superlu_dist_options_t options;
    SuperLUStat_t stat;
    SuperMatrix A;
    NRformat_loc *Astore;
    dScalePermstruct_t ScalePermstruct;
    dLUstruct_t LUstruct;
    dSOLVEstruct_t SOLVEstruct;
    gridinfo_t grid;
    double   *berr;
    double   *b, *b1, *xtrue, *xtrue1;
    int_t    *colind, *colind1, *rowptr, *rowptr1;
    int_t    i, j, m, n, nnz_loc, m_loc;
    int      nprow, npcol;
    int      iam, info, ldb, ldx, nrhs;
    char     **cpp, c, *postfix;
    int ii, omp_mpi_level;
    FILE *fp, *fopen();
    int cpp_defs();

    /* prototypes */
    extern int dcreate_matrix_perturbed
        (SuperMatrix *, int, double **, int *, double **, int *,
         FILE *, gridinfo_t *);
    extern int dcreate_matrix_perturbed_postfix
        (SuperMatrix *, int, double **, int *, double **, int *,
         FILE *, char *, gridinfo_t *);

    nprow = 1;  /* Default process rows.      */
    npcol = 1;  /* Default process columns.   */
    nrhs = 1;   /* Number of right-hand side. */

    /* ------------------------------------------------------------
       INITIALIZE MPI ENVIRONMENT. 
       ------------------------------------------------------------*/
    MPI_Init_thread( &argc, &argv, MPI_THREAD_MULTIPLE, &omp_mpi_level); 

    /* Parse command line argv[]. */
    for (cpp = argv+1; *cpp; ++cpp) {
	if ( **cpp == '-' ) {
	    c = *(*cpp+1);
	    ++cpp;
	    switch (c) {
	      case 'h':
		  printf("Options:\n");
		  printf("\t-r <int>: process rows    (default %4d)\n", nprow);
		  printf("\t-c <int>: process columns (default %4d)\n", npcol);
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
    if ( iam >= nprow * npcol )	goto out;
    if ( !iam ) {
	int v_major, v_minor, v_bugfix;
#ifdef __INTEL_COMPILER
	printf("__INTEL_COMPILER is defined\n");
#endif
	printf("__STDC_VERSION__ %ld\n", __STDC_VERSION__);

	superlu_dist_GetVersionNumber(&v_major, &v_minor, &v_bugfix);
	printf("Library version:\t%d.%d.%d\n", v_major, v_minor, v_bugfix);

	printf("Input matrix file:\t%s\n", *cpp);
        printf("Process grid:\t\t%d X %d\n", (int)grid.nprow, (int)grid.npcol);
	fflush(stdout);
    }
    
#if ( DEBUGlevel>=1 )
    CHECK_MALLOC(iam, "Enter main()");
#endif

    for(ii = 0;ii<strlen(*cpp);ii++){
	if((*cpp)[ii]=='.'){
	    postfix = &((*cpp)[ii+1]);
	}
    }	
    // printf("%s\n", postfix);

    /* ------------------------------------------------------------
       GET THE MATRIX FROM FILE AND SETUP THE RIGHT-HAND SIDE. 
       ------------------------------------------------------------*/
    dcreate_matrix_postfix(&A, nrhs, &b, &ldb, &xtrue, &ldx, fp, postfix, &grid);

    if ( !(berr = doubleMalloc_dist(nrhs)) )
	ABORT("Malloc fails for berr[].");
    m = A.nrow;
    n = A.ncol;
    Astore = (NRformat_loc *) A.Store;
    m_loc = Astore->m_loc;

    /* ------------------------------------------------------------
       WE SOLVE THE LINEAR SYSTEM FOR THE FIRST TIME.
       ------------------------------------------------------------*/

    /* Set the default input options:
        options.Fact = DOFACT;
        options.Equil = YES;
        options.ColPerm = METIS_AT_PLUS_A;
        options.RowPerm = LargeDiag_MC64;
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
	fflush(stdout);
    }

    /* Initialize ScalePermstruct and LUstruct. */
    dScalePermstructInit(m, n, &ScalePermstruct);
    dLUstructInit(n, &LUstruct);

    /* Initialize the statistics variables. */
    PStatInit(&stat);

    /* Call the linear equation solver: factorize and solve. */
    pdgssvx(&options, &A, &ScalePermstruct, b, ldb, nrhs, &grid,
            &LUstruct, &SOLVEstruct, berr, &stat, &info);

    /* Check the accuracy of the solution. */
    pdinf_norm_error(iam, m_loc, nrhs, b, ldb, xtrue, ldx, &grid);
    
    PStatPrint(&options, &stat, &grid);        /* Print the statistics. */
    PStatFree(&stat);
    Destroy_CompRowLoc_Matrix_dist(&A); /* Deallocate storage of matrix A.  */ 
    dDestroy_LU(n, &grid, &LUstruct); /* Deallocate storage associated with 
					the L and U matrices.               */
    SUPERLU_FREE(b);                 /* Free storage of right-hand side.    */
    SUPERLU_FREE(xtrue);             /* Free storage of the exact solution. */

    /* ------------------------------------------------------------
       NOW WE SOLVE ANOTHER LINEAR SYSTEM.
       ONLY THE SPARSITY PATTERN OF MATRIX A IS THE SAME.
       ------------------------------------------------------------*/
    options.Fact = SamePattern;

    if (iam==0) {
	print_options_dist(&options);
#if ( PRNTlevel>=2 )
	PrintInt10("perm_r", m, ScalePermstruct.perm_r);
	PrintInt10("perm_c", n, ScalePermstruct.perm_c);
#endif
    }

    /* Get the matrix from file, perturbed some diagonal entries to force
       a different perm_r[]. Set up the right-hand side.   */
    if ( !(fp = fopen(*cpp, "r")) ) ABORT("File does not exist");
    dcreate_matrix_perturbed_postfix(&A, nrhs, &b1, &ldb, &xtrue1, &ldx, fp, postfix, &grid);

    PStatInit(&stat); /* Initialize the statistics variables. */

    /* Solve the linear system. */
    pdgssvx(&options, &A, &ScalePermstruct, b1, ldb, nrhs, &grid,
            &LUstruct, &SOLVEstruct, berr, &stat, &info);

    /* Check the accuracy of the solution. */
    if ( !iam ) printf("Solve the system with the same sparsity pattern.\n");
    pdinf_norm_error(iam, m_loc, nrhs, b1, ldb, xtrue1, ldx, &grid);

#if ( PRNTlevel>=2 )
    if (iam==0) {
	PrintInt10("new perm_r", m, ScalePermstruct.perm_r);
	PrintInt10("new perm_c", n, ScalePermstruct.perm_c);
    }
#endif
    /* Print the statistics. */
    PStatPrint(&options, &stat, &grid);

    /* ------------------------------------------------------------
       DEALLOCATE STORAGE.
       ------------------------------------------------------------*/
    PStatFree(&stat);
    Destroy_CompRowLoc_Matrix_dist(&A); /* Deallocate storage of matrix A.  */
    dDestroy_LU(n, &grid, &LUstruct); /* Deallocate storage associated with    
					the L and U matrices.               */
    dScalePermstructFree(&ScalePermstruct);
    dLUstructFree(&LUstruct);         /* Deallocate the structure of L and U.*/
    if ( options.SolveInitialized ) {
        dSolveFinalize(&options, &SOLVEstruct);
    }
    SUPERLU_FREE(b1);	             /* Free storage of right-hand side.    */
    SUPERLU_FREE(xtrue1);             /* Free storage of the exact solution. */
    SUPERLU_FREE(berr);
    fclose(fp);

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


