!
! Copyright (C) 2001-2008 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE divide_et_impera( xk, wk, isk, lsda, nkstot, nks )
  !----------------------------------------------------------------------------
  !
  ! ... This routine divides the k points across nodes, sets the variable
  ! ... nks equal to the local (on this processors) number of k-points
  ! ... (nkstot on input is the total number of k-points)
  ! ... The distributed has "granularity kunit", that is, kunit consecutive 
  ! ... points stay on the same processor. Usually kunit=1; kunit=2 is used 
  ! ... in phonon calculations, when one has interspersed k_i and k_i+q and
  ! ... it is needed that they stay on the same processor
  !
  USE io_global, only : stdout
  USE kinds,     ONLY : DP
  USE mp_global, ONLY : my_pool_id, npool, kunit
  !
  IMPLICIT NONE
  !
  LOGICAL, INTENT(IN) :: lsda
    ! logical for local spin density approx.
  INTEGER, INTENT(IN)  :: nkstot
    ! total number of k-points
  INTEGER, INTENT(INOUT) :: isk(nkstot)
    ! spin index of each kpoint (when lsda=.t.)
  INTEGER, INTENT(OUT)  :: nks
    ! number of k-points per pool
  REAL (DP), INTENT(INOUT) :: xk(3,nkstot), wk(nkstot)
    ! k-points
    ! k-point weights
  !
  !
  INTEGER :: ik, nbase, rest
  !
  !
  IF ( MOD( nkstot, kunit ) /= 0 ) &
     CALL errore( 'd_&_i', ' nkstot/kunit is not an integer', nkstot )
  !
  nks    = kunit * ( nkstot / kunit / npool )
  !
  IF ( nks == 0 ) CALL errore( 'd_&_i', ' some nodes have no k-points', 1 )
  !
  rest = ( nkstot - nks * npool ) / kunit
  !
  IF ( ( my_pool_id + 1 ) <= rest ) nks = nks + kunit
  !
  ! ... calculates nbase = the position in the list of the first point that
  ! ...                    belong to this npool - 1
  !
  nbase = nks * my_pool_id
  !
  IF ( ( my_pool_id + 1 ) > rest ) nbase = nbase + rest * kunit
  !
  ! ... displaces these points in the first positions of the list
  !
  IF ( nbase > 0 ) THEN
     !
     xk(:,1:nks) =  xk(:,nbase+1:nbase+nks)
     !
     wk(1:nks) = wk(nbase+1:nbase+nks)
     !
     IF ( lsda ) isk(1:nks) = isk(nbase+1:nbase+nks)
     !
  !
  END IF
  !
  !
  RETURN
  !
END SUBROUTINE divide_et_impera
