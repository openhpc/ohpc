!
! Copyright (C) 2001-2008 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE allocate_wfc()
  !----------------------------------------------------------------------------
  !
  ! ... dynamical allocation of arrays: wavefunctions
  ! ... must be called after allocate_nlpot 
  !
  USE io_global, ONLY : stdout
  USE wvfct,     ONLY : npwx, nbnd
  USE basis,     ONLY : natomwfc
  USE fixed_occ, ONLY : one_atom_occupations
  USE wavefunctions_module, ONLY : evc
  !
  IMPLICIT NONE
  !
  !
     ALLOCATE( evc( npwx, nbnd ) )    
  !
  RETURN
  !
END subroutine allocate_wfc
