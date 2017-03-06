

/*! @file 
 * \brief Distribute the input matrix in a distributed compressed row format.
 *
 * <pre>
 * -- Distributed SuperLU routine (version 3.2) --
 * Lawrence Berkeley National Lab, Univ. of California Berkeley.
 * October 2012
 *
 *
 * Purpose
 * =======
 * 
 * DCREATE_DIST_MATRIX reads the global matrix from three input arrays
 * and distribute it to the processes in a distributed compressed row format.
 *
 * Arguments   
 * =========      
 *
 * A             (output) SuperMatrix*
 *               Local matrix A in NR_loc format. 
 *
 * M             (input) int_t
 *               The row number of the global matrix. 
 *
 * N             (input) int_t
 *               The col number of the global matrix. 
 *
 * NNZ           (input) int_t
 *               The number nonzeros in the global matrix. 
 *
 * NZVAL_G       (input) double*
 *               Nonzero values of the global matrix. 
 *
 * ROWIND_G      (input) int_t*
 *               Row indices of the global matrix. 
 *
 * COLPTR_G      (input) int_t*
 *               Columns pointers of the global matrix. 
 *
 * GRID          (input) gridinof_t*
 *               The 2D process mesh.
 *
 * </pre>
 */
#include <math.h>
#include "superlu_ddefs.h"

int dcreate_dist_matrix(SuperMatrix *A, int_t m, int_t n, int_t nnz,
			double *nzval_g, int_t *rowind_g, int_t *colptr_g,
			gridinfo_t *grid)
{
    SuperMatrix GA;              /* global A */
    int_t    *rowind, *colptr;	 /* global */
    double   *nzval;             /* global */
    double   *nzval_loc;         /* local */
    int_t    *colind, *rowptr;	 /* local */
    int_t    m_loc, fst_row, nnz_loc;
    int_t    m_loc_fst; /* Record m_loc of the first p-1 processors,
			   when mod(m, p) is not zero. */ 
    int_t    iam, row, col, i, j, relpos;
    char     trans[1];
    int_t    *marker;

    iam = grid->iam;

#if ( DEBUGlevel>=1 )
    CHECK_MALLOC(iam, "Enter dcreate_dist_matrix()");
#endif
 
    if ( !iam ) {

        /* Allocate storage for compressed column representation. */
        dallocateA_dist(n, nnz, &nzval, &rowind, &colptr);

	/* Copy the global matrix. */
#if 0
	/* and ADJUST to 0-based indexing 
           which is required by the C routines.*/
#endif
        for(i=0; i<nnz; i++){
	  nzval[i]=nzval_g[i];
	  rowind[i]=rowind_g[i]; /* - 1;*/
        }
        for(i=0; i<n+1; i++)
	  colptr[i]=colptr_g[i]; /* - 1;*/


	/* Broadcast matrix A to the other PEs. */
	MPI_Bcast( &m,     1,   mpi_int_t,  0, grid->comm );
	MPI_Bcast( &n,     1,   mpi_int_t,  0, grid->comm );
	MPI_Bcast( &nnz,   1,   mpi_int_t,  0, grid->comm );
	MPI_Bcast( nzval,  nnz, MPI_DOUBLE, 0, grid->comm );
	MPI_Bcast( rowind, nnz, mpi_int_t,  0, grid->comm );
	MPI_Bcast( colptr, n+1, mpi_int_t,  0, grid->comm );
    } else {
	/* Receive matrix A from PE 0. */
	MPI_Bcast( &m,   1,   mpi_int_t,  0, grid->comm );
	MPI_Bcast( &n,   1,   mpi_int_t,  0, grid->comm );
	MPI_Bcast( &nnz, 1,   mpi_int_t,  0, grid->comm );

	/* Allocate storage for compressed column representation. */
	dallocateA_dist(n, nnz, &nzval, &rowind, &colptr);

	MPI_Bcast( nzval,   nnz, MPI_DOUBLE, 0, grid->comm );
	MPI_Bcast( rowind,  nnz, mpi_int_t,  0, grid->comm );
	MPI_Bcast( colptr,  n+1, mpi_int_t,  0, grid->comm );
    }

#if 0
    nzval[0]=0.1;
#endif

    /* Compute the number of rows to be distributed to local process */
    m_loc = m / (grid->nprow * grid->npcol); 
    m_loc_fst = m_loc;
    /* When m / procs is not an integer */
    if ((m_loc * grid->nprow * grid->npcol) != m) {
      m_loc = m_loc+1;
      m_loc_fst = m_loc;
      if (iam == (grid->nprow * grid->npcol - 1)) 
	m_loc = m - m_loc_fst * (grid->nprow * grid->npcol - 1);
    }

    /* Create compressed column matrix for GA. */
    dCreate_CompCol_Matrix_dist(&GA, m, n, nnz, nzval, rowind, colptr,
				SLU_NC, SLU_D, SLU_GE);


    /*************************************************
     * Change GA to a local A with NR_loc format     *
     *************************************************/

    rowptr = (int_t *) intMalloc_dist(m_loc+1);
    marker = (int_t *) intCalloc_dist(n);

    /* Get counts of each row of GA */
    for (i = 0; i < n; ++i)
      for (j = colptr[i]; j < colptr[i+1]; ++j) ++marker[rowind[j]];
    /* Set up row pointers */
    rowptr[0] = 0;
    fst_row = iam * m_loc_fst;
    nnz_loc = 0;
    for (j = 0; j < m_loc; ++j) {
      row = fst_row + j;
      rowptr[j+1] = rowptr[j] + marker[row];
      marker[j] = rowptr[j];
    }
    nnz_loc = rowptr[m_loc];

    nzval_loc = (double *) doubleMalloc_dist(nnz_loc);
    colind = (int_t *) intMalloc_dist(nnz_loc);

    /* Transfer the matrix into the compressed row storage */
    for (i = 0; i < n; ++i) {
      for (j = colptr[i]; j < colptr[i+1]; ++j) {
	row = rowind[j];
	if ( (row>=fst_row) && (row<fst_row+m_loc) ) {
	  row = row - fst_row;
	  relpos = marker[row];
	  colind[relpos] = i;
	  nzval_loc[relpos] = nzval[j];
	  ++marker[row];
	}
      }
    }

#if ( DEBUGlevel>=1 )
    if ( !iam ) dPrint_CompCol_Matrix_dist(&GA);
#endif   


    /* Destroy GA */
    Destroy_CompCol_Matrix_dist(&GA);


    /******************************************************/
    /* Change GA to a local A with NR_loc format */
    /******************************************************/

    /* Set up the local A in NR_loc format */
    dCreate_CompRowLoc_Matrix_dist(A, m, n, nnz_loc, m_loc, fst_row,
				   nzval_loc, colind, rowptr,
				   SLU_NR_loc, SLU_D, SLU_GE);
    
    SUPERLU_FREE(marker);

#if ( DEBUGlevel>=1 )
    printf("sizeof(NRforamt_loc) %d\n", sizeof(NRformat_loc));
    CHECK_MALLOC(iam, "Exit dcreate_dist_matrix()");
#endif
    return 0;
}





