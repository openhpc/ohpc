!> @file
!! \brief This module contains Fortran-side wrappers for the SuperLU
!! get/set functions.
!

module superlu_mod

!----------------------------------------------------
! This module contains Fortran-side wrappers for the SuperLU get/set
! functions, with optional arguments so the user doesn't have to provide
! the full set of components.
!----------------------------------------------------

use superlupara_mod

implicit none
contains

subroutine get_GridInfo(grid, iam, nprow, npcol)
  integer(superlu_ptr) :: grid
  integer*4, optional :: iam
  integer, optional :: nprow, npcol
  integer :: l_iam, l_nprow, l_npcol

  call  f_get_gridinfo(grid, l_iam, l_nprow, l_npcol)

  if (present(iam)) iam = l_iam
  if (present(nprow)) nprow = l_nprow
  if (present(npcol)) npcol = l_npcol

end subroutine get_GridInfo

subroutine get_SuperMatrix(A, nrow, ncol)
  integer(superlu_ptr) :: A
  integer, optional :: nrow, ncol
  integer :: l_nrow, l_ncol

  call f_get_SuperMatrix(A, l_nrow, l_ncol)

  if (present(nrow)) nrow = l_nrow
  if (present(ncol)) ncol = l_ncol

end subroutine get_SuperMatrix

subroutine set_SuperMatrix(A, nrow, ncol)
  integer(superlu_ptr) :: A
  integer, optional :: nrow, ncol
  integer :: l_nrow, l_ncol

  call f_get_SuperMatrix(A, l_nrow, l_ncol)
  
  if (present(nrow)) l_nrow = nrow
  if (present(ncol)) l_ncol = ncol

  call f_set_SuperMatrix(A, l_nrow, l_ncol)

end subroutine set_SuperMatrix

subroutine get_CompRowLoc_Matrix(A, nrow, ncol, nnz_loc, nrow_loc, fst_row)
  integer(superlu_ptr) :: A
  integer, optional :: nrow, ncol, nnz_loc, nrow_loc, fst_row
  integer :: l_nrow, l_ncol, l_nnz_loc, l_nrow_loc, l_fst_row

  call f_get_CompRowLoc_Matrix(A, l_nrow, l_ncol, l_nnz_loc, l_nrow_loc, &
                               l_fst_row)

  if (present(nrow)) nrow = l_nrow
  if (present(ncol)) ncol = l_ncol
  if (present(nnz_loc)) nnz_loc = l_nnz_loc
  if (present(nrow_loc)) nrow_loc = l_nrow_loc
  if (present(fst_row)) fst_row = l_fst_row

end subroutine get_CompRowLoc_Matrix

subroutine set_CompRowLoc_Matrix(A, nrow, ncol, nnz_loc, nrow_loc, fst_row)
  integer(superlu_ptr) :: A
  integer, optional :: nrow, ncol, nnz_loc, nrow_loc, fst_row
  integer :: l_nrow, l_ncol, l_nnz_loc, l_nrow_loc, l_fst_row

  call f_set_CompRowLoc_Matrix(A, l_nrow, l_ncol, l_nnz_loc, l_nrow_loc, &
                               l_fst_row)

  if (present(nrow)) l_nrow = nrow
  if (present(ncol)) l_ncol = ncol
  if (present(nnz_loc)) l_nnz_loc = nnz_loc
  if (present(nrow_loc)) l_nrow_loc = nrow_loc
  if (present(fst_row)) l_fst_row = fst_row

end subroutine set_CompRowLoc_Matrix


subroutine get_superlu_options(opt, Fact, Equil, ParSymbFact, ColPerm, &
     RowPerm, IterRefine, Trans, ReplaceTinyPivot, SolveInitialized, &
     RefineInitialized, PrintStat)
  integer(superlu_ptr) :: opt
  integer, optional :: Fact, Equil, ParSymbFact, ColPerm, RowPerm, &
       IterRefine, Trans, ReplaceTinyPivot, SolveInitialized, &
       RefineInitialized, PrintStat
!
  integer :: l_Fact, l_Equil, l_ParSymbFact, l_ColPerm, l_RowPerm, &
             l_IterRefine, l_Trans, l_ReplaceTinyPivot, l_SolveInitialized, &
             l_RefineInitialized, l_PrintStat

  call f_get_superlu_options(opt, l_Fact, l_Equil, l_ParSymbFact, l_ColPerm, &
                             l_RowPerm, l_IterRefine, l_Trans,  &
                             l_ReplaceTinyPivot, l_SolveInitialized, &
                             l_RefineInitialized, l_PrintStat)

  if (present(Fact)) Fact = l_Fact
  if (present(Equil)) Equil = l_Equil
  if (present(ParSymbFact)) ParSymbFact = l_ParSymbFact
  if (present(ColPerm)) ColPerm = l_ColPerm
  if (present(RowPerm)) RowPerm = l_RowPerm
  if (present(IterRefine)) IterRefine = l_IterRefine
  if (present(Trans)) Trans = l_Trans
  if (present(ReplaceTinyPivot)) ReplaceTinyPivot = l_ReplaceTinyPivot
  if (present(SolveInitialized)) SolveInitialized = l_SolveInitialized
  if (present(RefineInitialized)) RefineInitialized = l_RefineInitialized
  if (present(PrintStat)) PrintStat = l_PrintStat

end subroutine get_superlu_options


subroutine set_superlu_options(opt, Fact, Equil, ParSymbFact, ColPerm, &
     RowPerm, IterRefine, Trans, ReplaceTinyPivot, SolveInitialized, &
     RefineInitialized, PrintStat)
  integer(superlu_ptr) :: opt
  integer, optional :: Fact, Equil, ParSymbFact, ColPerm, RowPerm, &
       IterRefine, Trans, ReplaceTinyPivot, SolveInitialized, &
       RefineInitialized, PrintStat
!
  integer :: l_Fact, l_Equil, l_ParSymbFact, l_ColPerm, l_RowPerm, &
             l_IterRefine, l_Trans, l_ReplaceTinyPivot, l_SolveInitialized, &
             l_RefineInitialized, l_PrintStat

  call f_get_superlu_options(opt, l_Fact, l_Equil, l_ParSymbFact, l_ColPerm, &
                             l_RowPerm, l_IterRefine, l_Trans,  &
                             l_ReplaceTinyPivot, l_SolveInitialized, &
                             l_RefineInitialized, l_PrintStat)

  if (present(Fact)) l_Fact = Fact
  if (present(Equil)) l_Equil = Equil
  if (present(ParSymbFact)) l_ParSymbFact = ParSymbFact
  if (present(ColPerm)) l_ColPerm = ColPerm
  if (present(RowPerm)) l_RowPerm = RowPerm
  if (present(IterRefine)) l_IterRefine = IterRefine
  if (present(Trans)) l_Trans = Trans
  if (present(ReplaceTinyPivot)) l_ReplaceTinyPivot = ReplaceTinyPivot
  if (present(SolveInitialized)) l_SolveInitialized = SolveInitialized
  if (present(RefineInitialized)) l_RefineInitialized = RefineInitialized
  if (present(PrintStat)) l_PrintStat = PrintStat

  call f_set_superlu_options(opt, l_Fact, l_Equil, l_ParSymbFact, &
                             l_ColPerm, l_RowPerm, l_IterRefine, l_Trans, &
                             l_ReplaceTinyPivot, l_SolveInitialized, &
                             l_RefineInitialized, l_PrintStat)

end subroutine set_superlu_options

end module superlu_mod



