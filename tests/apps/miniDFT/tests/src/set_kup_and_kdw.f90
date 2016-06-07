!
! Copyright (C) 2001-2007 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
subroutine set_kup_and_kdw (xk, wk, isk, nkstot, npk)
  !-----------------------------------------------------------------------
  !     This routine sets the k vectors for the up and down spin wfc
  !
  !     on input: xk and wk contain k-points and corresponding weights
  !
  !     on output: the number of points is doubled and xk and wk in the
  !                first (nkstot/2) positions correspond to up spin
  !                those in the second (nkstot/2) ones correspond to down spin
  !
  USE kinds, ONLY : DP
  implicit none
  !
  ! I/O variables first
  !
  integer :: npk, isk (npk), nkstot
  ! input: maximum allowed number of k-points
  ! output: spin associated to a given k-point
  ! input-output: starting and ending number of k-points 
  real(DP) :: xk (3, npk), wk (npk)
  ! input-output: coordinates of k points
  ! input-output: weights of k points
  !
  integer :: ik, iq, ikq
  !
  !
  if (2*nkstot > npk) call errore ('set_kup&kdw','too many k points',nkstot)
  do ik = 1, nkstot
     xk(:,ik+nkstot)= xk(:,ik)
     wk (ik+nkstot) = wk(ik)
     isk(ik)     = 1
     isk(ik+nkstot) = 2
  enddo
  nkstot = 2 * nkstot

  return

end subroutine set_kup_and_kdw
