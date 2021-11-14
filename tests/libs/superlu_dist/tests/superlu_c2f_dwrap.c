

/*! @file 
 * \brief C interface functions for the Fortran90 wrapper.
 *
 * <pre>
 * -- Distributed SuperLU routine (version 4.1) --
 * Lawrence Berkeley National Lab, Univ. of California Berkeley.
 * October 2012
 * April 5, 2015
 */

#include "superlu_ddefs.h"
#include "superlu_FCnames.h"

/* kind of integer to hold a pointer.  Use int.
   This might need to be changed on systems with large memory.
   If changed, be sure to change it in superlupara.f90 too */

#if 0
typedef int fptr;  /* 32-bit */
#else
typedef long long int fptr;  /* 64-bit */
#endif


/* some MPI implementations may require conversion between a Fortran
   communicator and a C communicator.  This routine is used to perform the
   conversion.  It may need different forms for different MPI libraries. */

/* NO_MPI2 should be defined on the compiler command line if the MPI
   library does not provide MPI_Comm_f2c */

MPI_Comm f2c_comm(int *f_comm)
{
#ifndef NO_MPI2

/* MPI 2 provides a standard way of doing this */
   return MPI_Comm_f2c((MPI_Fint)(*f_comm));
#else

/* will probably need some special cases here */
/* when in doubt, just return the input */
   return (MPI_Comm)(*f_comm);
#endif
}


/* functions that create memory for a struct and return a handle */

void f_create_gridinfo_handle(fptr *handle)
{
   *handle = (fptr) SUPERLU_MALLOC(sizeof(gridinfo_t));
}

void f_create_options_handle(fptr *handle)
{
   *handle = (fptr) SUPERLU_MALLOC(sizeof(superlu_dist_options_t));
}

void f_create_ScalePerm_handle(fptr *handle)
{
   *handle = (fptr) SUPERLU_MALLOC(sizeof(dScalePermstruct_t));
}

void f_create_LUstruct_handle(fptr *handle)
{
   *handle = (fptr) SUPERLU_MALLOC(sizeof(dLUstruct_t));
}

void f_create_SOLVEstruct_handle(fptr *handle)
{
   *handle = (fptr) SUPERLU_MALLOC(sizeof(dSOLVEstruct_t));
}

void f_create_SuperMatrix_handle(fptr *handle)
{
   *handle = (fptr) SUPERLU_MALLOC(sizeof(SuperMatrix));
}

void f_create_SuperLUStat_handle(fptr *handle)
{
   *handle = (fptr) SUPERLU_MALLOC(sizeof(SuperLUStat_t));
}

/* functions that free the memory allocated by the above functions */

void f_destroy_gridinfo_handle(fptr *handle)
{
   SUPERLU_FREE((void *)*handle);
}

void f_destroy_options_handle(fptr *handle)
{
   SUPERLU_FREE((void *)*handle);
}

void f_destroy_ScalePerm_handle(fptr *handle)
{
   SUPERLU_FREE((void *)*handle);
}

void f_destroy_LUstruct_handle(fptr *handle)
{
   SUPERLU_FREE((void *)*handle);
}

void f_destroy_SOLVEstruct_handle(fptr *handle)
{
   SUPERLU_FREE((void *)*handle);
}

void f_destroy_SuperMatrix_handle(fptr *handle)
{
   SUPERLU_FREE((void *)*handle);
}

void f_destroy_SuperLUStat_handle(fptr *handle)
{
   SUPERLU_FREE((void *)*handle);
}

/* functions that get or set values in a C struct.
   This is not the complete set of structs for which a user might want
   to get/set a component, and there may be missing components. */

void f_get_gridinfo(fptr *grid, int *iam, int_t *nprow, int_t *npcol)
{
  *iam=((gridinfo_t *) *grid)->iam;
  *npcol=((gridinfo_t *) *grid)->npcol;
  *nprow=((gridinfo_t *) *grid)->nprow;
}

void f_get_SuperMatrix(fptr *A, int_t *nrow, int_t *ncol)
{
   *nrow = ((SuperMatrix *) *A)->nrow;
   *ncol = ((SuperMatrix *) *A)->ncol;
}

void f_set_SuperMatrix(fptr *A, int_t *nrow, int_t *ncol)
{
   ((SuperMatrix *) *A)->nrow = *nrow;
   ((SuperMatrix *) *A)->ncol = *ncol;
}

void f_get_CompRowLoc_Matrix(fptr *A, int_t *m, int_t *n, int_t *nnz_loc,
			     int_t *m_loc, int_t *fst_row)
{
  *m=((SuperMatrix *) *A)->nrow;
  *n=((SuperMatrix *) *A)->ncol;
  *m_loc=((NRformat_loc *) ((SuperMatrix *) *A)->Store)->m_loc;
  *nnz_loc=((NRformat_loc *) ((SuperMatrix *) *A)->Store)->nnz_loc;
  *fst_row=((NRformat_loc *) ((SuperMatrix *) *A)->Store)->fst_row;
}

void f_set_CompRowLoc_Matrix(fptr *A, int_t *m, int_t *n, int_t *nnz_loc,
			     int_t *m_loc, int_t *fst_row)
{
  ((SuperMatrix *) *A)->nrow = *m;
  ((SuperMatrix *) *A)->ncol = *n;
  ((NRformat_loc *) ((SuperMatrix *) *A)->Store)->m_loc = *m_loc;
  ((NRformat_loc *) ((SuperMatrix *) *A)->Store)->nnz_loc = *nnz_loc;
  ((NRformat_loc *) ((SuperMatrix *) *A)->Store)->fst_row = *fst_row;
}

void f_get_superlu_options(fptr *opt, int *Fact, int *Equil, int *ParSymbFact,
                           int *ColPerm, int *RowPerm, int *IterRefine,
			   int *Trans, int *ReplaceTinyPivot,
			   int *SolveInitialized, int *RefineInitialized,
			   int *PrintStat)
{
   *Fact = (int) ((superlu_dist_options_t *) *opt)->Fact;
   *Equil = (int) ((superlu_dist_options_t *) *opt)->Equil;
   *ParSymbFact = (int) ((superlu_dist_options_t *) *opt)->ParSymbFact;
   *ColPerm = (int) ((superlu_dist_options_t *) *opt)->ColPerm;
   *RowPerm = (int) ((superlu_dist_options_t *) *opt)->RowPerm;
   *IterRefine = (int) ((superlu_dist_options_t *) *opt)->IterRefine;
   *Trans = (int) ((superlu_dist_options_t *) *opt)->Trans;
   *ReplaceTinyPivot = (int) ((superlu_dist_options_t *) *opt)->ReplaceTinyPivot;
   *SolveInitialized = (int) ((superlu_dist_options_t *) *opt)->SolveInitialized;
   *RefineInitialized = (int) ((superlu_dist_options_t *) *opt)->RefineInitialized;
   *PrintStat = (int) ((superlu_dist_options_t *) *opt)->PrintStat;
}

void f_set_superlu_options(fptr *opt, int *Fact, int *Equil, int *ParSymbFact,
                           int *ColPerm, int *RowPerm, int *IterRefine,
			   int *Trans, int *ReplaceTinyPivot,
			   int *SolveInitialized, int *RefineInitialized,
			   int *PrintStat)
{
    superlu_dist_options_t *l_options = (superlu_dist_options_t*) *opt;
    l_options->Fact = (fact_t) *Fact;
   ((superlu_dist_options_t *) *opt)->Equil = (yes_no_t) *Equil;
   ((superlu_dist_options_t *) *opt)->ParSymbFact = (yes_no_t) *ParSymbFact;
   ((superlu_dist_options_t *) *opt)->ColPerm = (colperm_t) *ColPerm;
   ((superlu_dist_options_t *) *opt)->RowPerm = (rowperm_t) *RowPerm;
   ((superlu_dist_options_t *) *opt)->IterRefine = (IterRefine_t) *IterRefine;
   ((superlu_dist_options_t *) *opt)->Trans = (trans_t) *Trans;
   ((superlu_dist_options_t *) *opt)->ReplaceTinyPivot = (yes_no_t) *ReplaceTinyPivot;
   ((superlu_dist_options_t *) *opt)->SolveInitialized = (yes_no_t) *SolveInitialized;
   ((superlu_dist_options_t *) *opt)->RefineInitialized = (yes_no_t) *RefineInitialized;
   ((superlu_dist_options_t *) *opt)->PrintStat = (yes_no_t) *PrintStat;
}

/* wrappers for SuperLU functions */

void f_set_default_options(fptr *options)
{
   set_default_options_dist((superlu_dist_options_t *) *options);
}

void f_superlu_gridinit(int *Bcomm, int_t *nprow, int_t *npcol, fptr *grid)
{
  
   superlu_gridinit(f2c_comm(Bcomm), *nprow, *npcol, (gridinfo_t *) *grid);
}

void f_superlu_gridmap(int *Bcomm, int_t *nprow, int_t *npcol, 
                       int_t *usermap, int_t *ldumap,
	 fptr *grid)
{
   superlu_gridmap(f2c_comm(Bcomm), *nprow, *npcol, usermap, *ldumap, (gridinfo_t *) *grid);
}

void f_superlu_gridexit(fptr *grid)
{
   superlu_gridexit((gridinfo_t *) *grid);
}

void f_ScalePermstructInit(int_t *m, int_t *n, fptr *ScalePermstruct)
{
   dScalePermstructInit(*m, *n, (dScalePermstruct_t *) *ScalePermstruct);
}

void f_ScalePermstructFree(fptr *ScalePermstruct)
{
   dScalePermstructFree((dScalePermstruct_t *) *ScalePermstruct);
}

void f_PStatInit(fptr *stat)
{
   PStatInit((SuperLUStat_t *) *stat);
}

void f_PStatFree(fptr *stat)
{
   PStatFree((SuperLUStat_t *) *stat);
}

void f_LUstructInit(int_t *m, int_t *n, fptr *LUstruct)
{
   extern void dLUstructInit(const int_t, dLUstruct_t *);

   dLUstructInit(*m, (dLUstruct_t *) *LUstruct);
}

void f_LUstructFree(fptr *LUstruct)
{
   extern void dLUstructFree(dLUstruct_t *);

   dLUstructFree((dLUstruct_t *) *LUstruct);
}

void f_Destroy_LU(int_t *n, fptr *grid, fptr *LUstruct)
{
   dDestroy_LU(*n, (gridinfo_t *) *grid, (dLUstruct_t *) *LUstruct);
}

void f_dCreate_CompRowLoc_Mat_dist(fptr *A, int_t *m, int_t *n, int_t *nnz_loc,
				   int_t *m_loc, int_t *fst_row, double *nzval,
				   int_t *colind, int_t *rowptr, int *stype,
				   int *dtype, int *mtype)
{
   dCreate_CompRowLoc_Matrix_dist((SuperMatrix *) *A, *m, *n, *nnz_loc, *m_loc,
                                  *fst_row, (double *) nzval, colind, rowptr,
                                  (Stype_t) *stype, (Dtype_t) *dtype,
                                  (Mtype_t) *mtype);
}

void f_Destroy_CompRowLoc_Mat_dist(fptr *A)
{
   Destroy_CompRowLoc_Matrix_dist((SuperMatrix *) *A);
}

void f_Destroy_SuperMat_Store_dist(fptr *A)
{
   Destroy_SuperMatrix_Store_dist((SuperMatrix *) *A);
}

void f_dSolveFinalize(fptr *options, fptr *SOLVEstruct)
{
   dSolveFinalize((superlu_dist_options_t *) *options,
                  (dSOLVEstruct_t *) *SOLVEstruct);
}

void f_pdgssvx(fptr *options, fptr *A, fptr *ScalePermstruct, double *B,
               int *ldb, int *nrhs, fptr *grid, fptr *LUstruct,
               fptr *SOLVEstruct, double *berr, fptr *stat, int *info)
{
    pdgssvx((superlu_dist_options_t *) *options, (SuperMatrix *) *A,
	    (dScalePermstruct_t *) *ScalePermstruct, B, *ldb, *nrhs,
	    (gridinfo_t *) *grid, (dLUstruct_t *) *LUstruct,
	    (dSOLVEstruct_t *) *SOLVEstruct, berr,
	    (SuperLUStat_t *) *stat, info);

    PStatPrint((superlu_dist_options_t *) *options, (SuperLUStat_t *) *stat,
	       (gridinfo_t *) *grid);
}

/* Create the distributed matrix */

void f_dcreate_dist_matrix(fptr *A, int_t *m, int_t *n, int_t *nnz,
			   double *nzval, int_t *rowind, int_t *colptr,
			   fptr *grid)
{
   int dcreate_dist_matrix(SuperMatrix *, int_t, int_t, int_t, double *,
			   int_t * , int_t *, gridinfo_t *);

   dcreate_dist_matrix((SuperMatrix *) *A, (int_t) *m, *n, *nnz, 
		       (double *) nzval, (int_t *) rowind, (int_t *) colptr,
		       (gridinfo_t *) *grid);

}

/* Check malloc */

void f_check_malloc(int *iam)
{
#if ( DEBUGlevel>=1 )
    CHECK_MALLOC((int_t) *iam, "Check Malloc");
#endif
}
