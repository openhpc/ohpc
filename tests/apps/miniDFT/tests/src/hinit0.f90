!
! Copyright (C) 2001-2005 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
SUBROUTINE hinit0()
  !-----------------------------------------------------------------------
  !
  ! ... hamiltonian initialization: 
  ! ... atomic position independent initialization for nonlocal PP,
  ! ... structure factors, local potential, core charge
  !
  USE ions_base,    ONLY : nat, nsp, ityp, tau
  USE basis,        ONLY : startingconfig
  USE cell_base,    ONLY : at, bg, omega, tpiba2
  USE klist,        ONLY : nks, xk
  USE fft_base,     ONLY : dfftp
  USE gvect,        ONLY : ngm, ig_l2g, g, eigts1, eigts2, eigts3
  USE vlocal,       ONLY : strf
  USE wvfct,        ONLY : npw, g2kin, igk, ecutwfc
  USE io_files,     ONLY : iunigk
  USE control_flags, ONLY : tqr 
  USE io_global,  ONLY : stdout
  !
  IMPLICIT NONE
  !
  INTEGER :: ik
  ! counter on k points
  !
  ! ... calculate the Fourier coefficients of the local part of the PP
  !
  CALL init_vloc()
  !
  ! ... k-point independent parameters of non-local pseudopotentials
  !
  CALL init_us_1()
  CALL init_at_1()
  !
#ifdef __IGKIO
  REWIND( iunigk )
#endif
  !
  ! ... The following loop must NOT be called more than once in a run
  ! ... or else there will be problems with variable-cell calculations
  !
  DO ik = 1, nks
     !
     ! ... g2kin is used here as work space
     !
     CALL gk_sort( xk(1,ik), ngm, g, ecutwfc / tpiba2, npw, igk, g2kin )
     !
     ! ... if there is only one k-point npw and igk stay in memory
     !
#ifdef __IGKIO
     IF ( nks > 1 ) WRITE( iunigk ) igk
#endif __IGKIO
     !
  END DO
  !
  !
  ! ... initialize the structure factor
  !
  CALL struc_fact( nat, tau, nsp, ityp, ngm, g, bg, &
                   dfftp%nr1, dfftp%nr2, dfftp%nr3, strf, eigts1, eigts2, eigts3 )
  !
  ! ... calculate the total local potential
  !
  CALL setlocal()
  !
  ! ... calculate the core charge (if any) for the nonlinear core correction
  !
  CALL set_rhoc()
  !
  !
  RETURN
  !
END SUBROUTINE hinit0

