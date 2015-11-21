!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
subroutine deriv_drhoc (ngl, gl, omega, tpiba2, mesh, r, rab, rhoc, drhocg)
  !-----------------------------------------------------------------------
  USE kinds
  USE constants, ONLY : pi, fpi
  implicit none
  !
  !    first the dummy variables
  !

  integer :: ngl, mesh
  ! input: the number of g shell
  ! input: the number of radial mesh points

  real(DP), intent(in) :: gl (ngl), r (mesh), rab (mesh), rhoc (mesh), &
                          omega, tpiba2
  real(DP), intent(out) :: drhocg (ngl)
  ! input: the number of G shells
  ! input: the radial mesh
  ! input: the derivative of the radial mesh
  ! input: the radial core charge
  ! input: the volume of the unit cell
  ! input: 2 times pi / alat
  ! output: fourier transform of d Rho_c/dG
  !
  !     here the local variables
  !
  real(DP) :: gx, rhocg1
  ! the modulus of g for a given shell
  ! the fourier transform
  real(DP), allocatable :: aux (:)
  ! auxiliary memory for integration

  integer :: ir, igl, igl0
  ! counter on radial mesh points
  ! counter on g shells
  ! lower limit for loop on ngl

  !
  ! G=0 term
  !
  if (gl (1) < 1.0d-8) then
     drhocg (1) = 0.0d0
     igl0 = 2
  else
     igl0 = 1
  endif
  !
  ! G <> 0 term
  !
  allocate (aux( mesh))    
  do igl = igl0, ngl
     gx = sqrt (gl (igl) * tpiba2)
     do ir = 1, mesh
        aux (ir) = r (ir) * rhoc (ir) * (r (ir) * cos (gx * r (ir) ) &
             / gx - sin (gx * r (ir) ) / gx**2)
     enddo
     call simpson (mesh, aux, rab, rhocg1)
     drhocg (igl) = fpi / omega * rhocg1
  enddo
  deallocate (aux)

  return
end subroutine deriv_drhoc

