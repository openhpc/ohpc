
! Copyright (C) 2002-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
SUBROUTINE h_psi( lda, n, m, psi, hpsi )
  !----------------------------------------------------------------------------
  !
  ! ... This routine computes the product of the Hamiltonian
  ! ... matrix with m wavefunctions contained in psi
  !
  ! ... input:
  ! ...    lda   leading dimension of arrays psi, spsi, hpsi
  ! ...    n     true dimension of psi, spsi, hpsi
  ! ...    m     number of states psi
  ! ...    psi
  !
  ! ... output:
  ! ...    hpsi  H*psi
  !
  USE kinds,    ONLY : DP
  USE becmod,   ONLY : bec_type, becp, calbec
  USE lsda_mod, ONLY : current_spin
  USE scf,      ONLY : vrs  
  USE wvfct,    ONLY : g2kin
  USE uspp,     ONLY : vkb, nkb
  USE gvect,    ONLY : gstart
  USE fft_base, ONLY : dffts
  !
  IMPLICIT NONE
  !
  integer, parameter :: npol=1 !substitute for noncollin_module%npol
  INTEGER, INTENT(IN)     :: lda, n, m
  COMPLEX(DP), INTENT(IN)  :: psi(lda*npol,m) 
  COMPLEX(DP), INTENT(OUT) :: hpsi(lda*npol,m)   
  !
  INTEGER     :: ipol, ibnd, incr
  !
  CALL start_clock( 'h_psi' )
  !  
  ! ... Here we apply the kinetic energy (k+G)^2 psi
  !
  DO ibnd = 1, m
     hpsi (1:n, ibnd) = g2kin (1:n) * psi (1:n, ibnd)
     hpsi (n+1:lda,ibnd) = (0.0_dp, 0.0_dp)
  END DO
  !
  !
  !
  ! ... the local potential V_Loc psi
  !
  CALL start_clock( 'h_psi:vloc' )
  !
     CALL vloc_psi_k ( lda, n, m, psi, vrs(1,current_spin), hpsi )
     !
  CALL stop_clock( 'h_psi:vloc' )
  !
  ! ... Here the product with the non local potential V_NL psi
  ! ... (not in the real-space case: it is done together with V_loc)
  !
     !
     CALL start_clock( 'h_psi:vnl' )
     CALL calbec ( n, vkb, psi, becp, m )
     CALL add_vuspsi( lda, n, m, hpsi )
     CALL stop_clock( 'h_psi:vnl' )
     !
  !
  ! ... electric enthalpy if required
  !
  !
  CALL stop_clock( 'h_psi' )
  !
  RETURN
  !
END SUBROUTINE h_psi
