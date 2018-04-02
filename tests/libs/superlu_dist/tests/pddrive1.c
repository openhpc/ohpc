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
 * -- Distributed SuperLU routine (version 4.1) --
 * Lawrence Berkeley National Lab, Univ. of California Berkeley.
 * March 15, 2003
 * April 5, 2015
 * </pre>
 */

#include <math.h>
#include <superlu_dist_config.h>
#include "superlu_ddefs.h"

/*! \brief
 *
 * <pre>
 * Purpose
 * =======
 *
 * The driver program PDDRIVE1.
 *
 * This example illustrates how to use PDGSSVX to
 * solve systems with the same A but different right-hand side.
 * In this case, we factorize A only once in the first call to
 * PDGSSVX, and reuse the following data structures
 * in the subsequent call to PDGSSVX:
 *        ScalePermstruct  : DiagScale, R, C, perm_r, perm_c
 *        LUstruct         : Glu_persist, Llu
 * 
 * With MPICH,  program may be run by typing:
 *    mpiexec -n <np> pddrive1 -r <proc rows> -c <proc columns> big.rua
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
    gridinfo_t grid;
    double   *berr;
    double   *b, *xtrue, *b1;
    int    i, j, m, n;
    int    nprow, npcol;
    int    iam, info, ldb, ldx, nrhs;
    char     **cpp, c;
    FILE *fp, *fopen();
    int cpp_defs();

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
       INITIALIZE THE SUPERLU PROCESS GRID. 
       ------------------------------------------------------------*/
    superlu_gridinit(MPI_COMM_WORLD, nprow, npcol, &grid);

    /* Bail out if I do not belong in the grid. */
    iam = grid.iam;
    if ( iam >= nprow * npcol )	goto out;
    if ( !iam ) {
	int v_major, v_minor, v_bugfix;
	superlu_dist_GetVersionNumber(&v_major, &v_minor, &v_bugfix);
	printf("Library version:\t%d.%d.%d\n", v_major, v_minor, v_bugfix);

	printf("Input matrix file:\t%s\n", *cpp);
        printf("Process grid:\t\t%d X %d\n", (int)grid.nprow, (int)grid.npcol);
	fflush(stdout);
    }

#if ( VAMPIR>=1 )
    VT_traceoff();
#endif

#if ( DEBUGlevel>=1 )
    CHECK_MALLOC(iam, "Enter main()");
#endif

    /* ------------------------------------------------------------
       GET THE MATRIX FROM FILE AND SETUP THE RIGHT HAND SIDE. 
       ------------------------------------------------------------*/
    dcreate_matrix(&A, nrhs, &b, &ldb, &xtrue, &ldx, fp, &grid);
    if ( !(b1 = doubleMalloc_dist(ldb * nrhs)) )
        ABORT("Malloc fails for b1[]");
    for (j = 0; j < nrhs; ++j)
        for (i = 0; i < ldb; ++i) b1[i+j*ldb] = b[i+j*ldb];

    if ( !(berr = doubleMalloc_dist(nrhs)) )
	ABORT("Malloc fails for berr[].");

    /* ------------------------------------------------------------
       WE SOLVE THE LINEAR SYSTEM FOR THE FIRST TIME.
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
    printf("options.ColPerm = %d\n", options.ColPerm);

    if (!iam) {
	print_sp_ienv_dist(&options);
	print_options_dist(&options);
	fflush(stdout);
    }

    m = A.nrow;
    n = A.ncol;

    /* Initialize ScalePermstruct and LUstruct. */
    ScalePermstructInit(m, n, &ScalePermstruct);
    LUstructInit(n, &LUstruct);

    /* Initialize the statistics variables. */
    PStatInit(&stat);

    /* Call the linear equation solver. */
    pdgssvx(&options, &A, &ScalePermstruct, b, ldb, nrhs, &grid,
	    &LUstruct, &SOLVEstruct, berr, &stat, &info);


    /* Check the accuracy of the solution. */
    if ( !iam ) printf("\tSolve the first system:\n");
    pdinf_norm_error(iam, ((NRformat_loc *)A.Store)->m_loc,
		     nrhs, b, ldb, xtrue, ldx, &grid);

    PStatPrint(&options, &stat, &grid);        /* Print the statistics. */
    PStatFree(&stat);

    /* ------------------------------------------------------------
       NOW WE SOLVE ANOTHER SYSTEM WITH THE SAME A BUT DIFFERENT
       RIGHT-HAND SIDE,  WE WILL USE THE EXISTING L AND U FACTORS IN
       LUSTRUCT OBTAINED FROM A PREVIOUS FATORIZATION.
       ------------------------------------------------------------*/
    options.Fact = FACTORED; /* Indicate the factored form of A is supplied. */
    PStatInit(&stat); /* Initialize the statistics variables. */

    pdgssvx(&options, &A, &ScalePermstruct, b1, ldb, nrhs, &grid,
	    &LUstruct, &SOLVEstruct, berr, &stat, &info);

    /* Check the accuracy of the solution. */
    if ( !iam ) printf("\tSolve the system with a different B:\n");
    pdinf_norm_error(iam, ((NRformat_loc *)A.Store)->m_loc,
		     nrhs, b1, ldb, xtrue, ldx, &grid);

    PStatPrint(&options, &stat, &grid);        /* Print the statistics. */


    /* ------------------------------------------------------------
       DEALLOCATE STORAGE.
       ------------------------------------------------------------*/
    PStatFree(&stat);
    Destroy_CompRowLoc_Matrix_dist(&A);
    ScalePermstructFree(&ScalePermstruct);
    Destroy_LU(n, &grid, &LUstruct);
    LUstructFree(&LUstruct);
    if ( options.SolveInitialized ) {
        dSolveFinalize(&options, &SOLVEstruct);
    }
    SUPERLU_FREE(b);
    SUPERLU_FREE(b1);
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
