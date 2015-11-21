!
! Copyright (C) 2001-2006 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
SUBROUTINE remove_tot_torque( nat, tau, mass, force )
  !----------------------------------------------------------------------------
  !
  ! ... This routine sets to zero the total torque associated to the internal
  ! ... forces acting on the atoms by correcting the force vector.
  !
  ! ... The algorithm is based on the following expressions ( F' is the
  ! ... torqueless force ) :
  !                 _
  !        _    1  \   __      _        __       _     _
  ! ...    m = --- /_  dR_i /\ F_i ,    dR_i = ( R_i - R_cm ) ,
  !             N    i
  !
  !        __     _          1    _    __
  ! ...    F'_i = F_i -  -------- m /\ dR_i
  !                      |dR_i|^2
  !
  !
  ! ... written by carlo sbraccia (2006)
  !
  USE kinds, ONLY : DP
  !
  IMPLICIT NONE
  !
  INTEGER,  INTENT(IN)    :: nat
  REAL(DP), INTENT(IN)    :: tau(3,nat)
  REAL(DP), INTENT(IN)    :: mass(nat)
  REAL(DP), INTENT(INOUT) :: force(3,nat)
  !
  INTEGER  :: ia
  REAL(DP) :: m(3), mo(3), tauref(3), delta(3), sumf(3)
  REAL(DP) :: nrmsq
  !
  !
  tauref(:) = 0.D0
  !
  DO ia = 1, nat
     !
     tauref(:) = tauref(:) + tau(:,ia)*mass(ia)
     !
  END DO
  !
  tauref(:) = tauref(:) / SUM( mass(:) )
  !
  m(:) = 0.D0
  !
  DO ia = 1, nat
     !
     delta(:) = tau(:,ia) - tauref(:)
     !
     m(:) = m(:) + ext_prod( delta(:), force(:,ia) )
     !
  END DO
  !
  mo(:) = m(:)
  !
  m(:) = m(:) / DBLE( nat )
  !
  sumf(:) = 0.D0
  !
  DO ia = 1, nat
     !
     delta(:) = tau(:,ia) - tauref(:)
     !
     nrmsq = delta(1)**2 + delta(2)**2 + delta(3)**2
     !
     force(:,ia) = force(:,ia) - ext_prod( m(:), delta(:) ) / nrmsq
     !
     sumf(:) = sumf(:) + force(:,ia)
     !
  END DO
  !
  DO ia = 1, nat
     !
     force(:,ia) = force(:,ia) - sumf(:) / DBLE( nat )
     !
  END DO
  !
  m(:) = 0.D0
  !
  DO ia = 1, nat
     !
     delta(:) = tau(:,ia) - tauref(:)
     !
     m(:) = m(:) + ext_prod( delta(:), force(:,ia) )
     !
  END DO
  !
  IF ( m(1)**2+m(2)**2+m(3)**2 > mo(1)**2+mo(2)**2+mo(3)**2 ) &
     CALL errore( 'remove_tot_torque', &
                  'total torque has not been properly removed', 1 )
  !
  RETURN
  !
  CONTAINS
    !
    !------------------------------------------------------------------------
    FUNCTION ext_prod( a, b )
      !------------------------------------------------------------------------
      !
      REAL(DP), INTENT(IN) :: a(3), b(3)
      REAL(DP)             :: ext_prod(3)
      !
      ext_prod(1) = a(2)*b(3) - a(3)*b(2)
      ext_prod(2) = a(3)*b(1) - a(1)*b(3)
      ext_prod(3) = a(1)*b(2) - a(2)*b(1)
      !
    END FUNCTION ext_prod
    !
END SUBROUTINE remove_tot_torque
