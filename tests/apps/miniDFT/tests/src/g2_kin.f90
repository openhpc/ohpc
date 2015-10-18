!
! Copyright (C) 2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE g2_kin ( ik )
  !----------------------------------------------------------------------------
  !
  ! ... Calculation of kinetic energy - includes the case of the modified
  ! ... kinetic energy functional for variable-cell calculations
  !
  USE kinds,                ONLY : DP
  USE cell_base,            ONLY : tpiba2 
  USE klist,                ONLY : xk
  USE gvect,                ONLY : g
  USE wvfct,                ONLY : g2kin, igk, npw, ecfixed, qcutz, q2sigma
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT (IN) :: ik
  !
  ! ... local variables
  !
  INTEGER :: ig
  REAL(DP), EXTERNAL :: qe_erf
  !
  !
  g2kin(1:npw) = ( ( xk(1,ik) + g(1,igk(1:npw)) )**2 + &
                   ( xk(2,ik) + g(2,igk(1:npw)) )**2 + &
                   ( xk(3,ik) + g(3,igk(1:npw)) )**2 ) * tpiba2
  !
  IF ( qcutz > 0.D0 ) THEN
     !
     DO ig = 1, npw
        !
        g2kin(ig) = g2kin(ig) + qcutz * &
             ( 1.D0 + qe_erf( ( g2kin(ig) - ecfixed ) / q2sigma ) )
        !
     END DO
     !
  END IF
  !
  RETURN
  !
END SUBROUTINE g2_kin
