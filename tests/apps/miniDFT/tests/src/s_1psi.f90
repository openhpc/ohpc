!
! Copyright (C) 2001-2004 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
SUBROUTINE s_1psi( npwx, n, psi, spsi )
  !----------------------------------------------------------------------------
  !
  ! ... spsi = S*psi for one wavefunction
  ! ... Wrapper routine - calls calbec and s_psi
  !
  USE kinds,  ONLY : DP
  USE uspp,   ONLY : vkb, nkb
  USE becmod, ONLY : bec_type, becp, calbec
  USE wvfct,                ONLY: nbnd
  !
  IMPLICIT NONE
  !
  integer, parameter :: npol=1 !substitute for noncollin_module%npol
  INTEGER          :: npwx, n, ibnd
  COMPLEX(DP) :: psi(npwx*npol,1), spsi(npwx*npol,1)
  !
  !
  CALL start_clock( 's_1psi' )
  !
     !
     CALL calbec( n, vkb, psi, becp )
     !
  CALL s_psi( npwx, n, 1, psi, spsi )
  !
  CALL stop_clock( 's_1psi' )
  !
  RETURN
  !
END SUBROUTINE s_1psi
