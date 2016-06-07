!
! Copyright (C) 2001-2007 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
MODULE g_psi_mod
  !
  ! ... These are the variables needed in g_psi
  !  
  USE kinds, only : DP
  !
  IMPLICIT NONE
  !
  REAL(DP), ALLOCATABLE :: &
    h_diag (:,:),&   ! diagonal part of the Hamiltonian
    s_diag (:,:)     ! diagonal part of the overlap matrix
  !
END MODULE g_psi_mod
