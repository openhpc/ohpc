!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
subroutine remove_atomic_rho
  !-----------------------------------------------------------------------
  USE io_global, ONLY: stdout
  USE io_files, ONLY: output_drho
  USE kinds, ONLY: DP
  USE fft_base, ONLY: dfftp
  USE lsda_mod, ONLY: nspin
  USE scf, ONLY: rho
  implicit none

  real(DP), allocatable :: work (:,:)
  ! workspace, is the difference between the charge density
  ! and the superposition of atomic charges

  allocate ( work( dfftp%nnr, 1 ) )
  work = 0.d0
  !
  IF ( nspin > 1 ) CALL errore &
       ( 'remove_atomic_rho', 'spin polarization not allowed in drho', 1 )

  WRITE( stdout, '(/5x,"remove atomic charge density from scf rho")')
  !
  !     subtract the old atomic charge density
  !
  call atomic_rho (work, nspin)
  !
  work = rho%of_r - work
  !
  call infomsg("remove_atomic_rho.f90:38","skipping write_rho, requires iotk")
  !call write_rho ( work, 1, output_drho )
  !
  deallocate(work)
  return

end subroutine remove_atomic_rho

