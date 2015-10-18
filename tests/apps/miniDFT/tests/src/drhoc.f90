!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
subroutine drhoc (ngl, gl, omega, tpiba2, mesh, r, rab, rhoc, rhocg)
  !-----------------------------------------------------------------------
  !
  USE kinds
  USE constants, ONLY : pi, fpi
  implicit none
  !
  !    first the dummy variables
  !
  integer :: ngl, mesh
  ! input: the number of g shell
  ! input: the number of radial mesh points

  real(DP) :: gl (ngl), r (mesh), rab (mesh), rhoc (mesh), omega, &
       tpiba2, rhocg (ngl)
  ! input: the number of G shells
  ! input: the radial mesh
  ! input: the derivative of the radial mesh
  ! input: the radial core charge
  ! input: the volume of the unit cell
  ! input: 2 times pi / alat
  ! output: the fourier transform of the core charge
  !
  !     here the local variables
  !
  real(DP) :: gx, rhocg1
  ! the modulus of g for a given shell
  ! the fourier transform
  real(DP), allocatable ::  aux (:)
  ! auxiliary memory for integration

  integer :: ir, igl, igl0
  ! counter on radial mesh points
  ! counter on g shells
  ! lower limit for loop on ngl

  allocate (aux( mesh))     
  !
  ! G=0 term
  !
  if (gl (1) < 1.0d-8) then
     do ir = 1, mesh
        aux (ir) = r (ir) **2 * rhoc (ir)
     enddo
     call simpson (mesh, aux, rab, rhocg1)
     rhocg (1) = fpi * rhocg1 / omega
     igl0 = 2
  else
     igl0 = 1
  endif
  !
  ! G <> 0 term
  !
  do igl = igl0, ngl
     gx = sqrt (gl (igl) * tpiba2)
     call sph_bes (mesh, r, gx, 0, aux)
     do ir = 1, mesh
        aux (ir) = r (ir) **2 * rhoc (ir) * aux (ir)
     enddo
     call simpson (mesh, aux, rab, rhocg1)
     rhocg (igl) = fpi * rhocg1 / omega
  enddo
  deallocate(aux)
  !
  return
end subroutine drhoc

