!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!---------------------------------------------------------------------
subroutine volume (alat, a1, a2, a3, omega)
  !---------------------------------------------------------------------
  !
  !     Compute the volume of the unit cell
  !
  use kinds, ONLY: DP
  implicit none
  !
  !     First the I/O variables
  !
  real(DP) :: alat, a1 (3), a2 (3), a3 (3), omega
  ! input:  lattice parameter (unit length)
  ! input: the first lattice vector
  ! input: the second lattice vector
  ! input: the third lattice vector
  ! input: the volume of the unit cell
  !
  !    Here the local variables required by the routine
  !

  real(DP) :: s
  ! the sign of a permutation
  integer :: i, j, k, l, iperm
  !\
  ! \
  ! /   auxiliary indices
  !/
  ! counter on permutations
  !
  !   Compute the volume
  !
  omega = 0.d0
  s = 1.d0
  i = 1
  j = 2
  k = 3
101 do iperm = 1, 3
     omega = omega + s * a1 (i) * a2 (j) * a3 (k)
     l = i
     i = j
     j = k
     k = l
  enddo
  i = 2
  j = 1
  k = 3
  s = - s

  if (s.lt.0.d0) goto 101

  omega = abs (omega) * alat**3
  return
end subroutine volume
