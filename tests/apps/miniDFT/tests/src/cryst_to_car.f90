!
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
subroutine cryst_to_cart (nvec, vec, trmat, iflag)
  !-----------------------------------------------------------------------
  !
  !     This routine transforms the atomic positions or the k-point
  !     components from crystallographic to cartesian coordinates 
  !     ( iflag=1 ) and viceversa ( iflag=-1 ).
  !     Output cartesian coordinates are stored in the input ('vec') array
  !
  !
  USE kinds, ONLY : DP
  implicit none
  !
  integer, intent(in) :: nvec, iflag
  ! nvec:  number of vectors (atomic positions or k-points)
  !        to be transformed from crystal to cartesian and vice versa
  ! iflag: gives the direction of the transformation
  real(DP), intent(in) :: trmat (3, 3)
  ! trmat: transformation matrix
  ! if iflag=1:
  !    trmat = at ,  basis of the real-space lattice,       for atoms   or
  !          = bg ,  basis of the reciprocal-space lattice, for k-points
  ! if iflag=-1: the opposite
  real(DP), intent(inout) :: vec (3, nvec)
  ! coordinates of the vector (atomic positions or k-points) to be
  ! transformed - overwritten on output
  !
  !    local variables
  !
  integer :: nv, kpol
  ! counter on vectors
  ! counter on polarizations
  real(DP) :: vau (3)
  ! workspace
  !
  !     Compute the cartesian coordinates of each vectors
  !     (atomic positions or k-points components)
  !
  do nv = 1, nvec
     if (iflag.eq.1) then
        do kpol = 1, 3
           vau (kpol) = trmat (kpol, 1) * vec (1, nv) + trmat (kpol, 2) &
                * vec (2, nv) + trmat (kpol, 3) * vec (3, nv)
        enddo
     else
        do kpol = 1, 3
           vau (kpol) = trmat (1, kpol) * vec (1, nv) + trmat (2, kpol) &
                * vec (2, nv) + trmat (3, kpol) * vec (3, nv)
        enddo
     endif
     do kpol = 1, 3
        vec (kpol, nv) = vau (kpol)
     enddo
  enddo
  !
  return
end subroutine cryst_to_cart

