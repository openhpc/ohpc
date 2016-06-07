!
! Copyright (C) 2001-2010 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
SUBROUTINE allocate_fft
  !-----------------------------------------------------------------------
  !     This routine computes the data structure associated to the FFT
  !     grid and allocate memory for all the arrays which depend upon
  !     these dimensions
  !
  USE io_global, ONLY : stdout
  USE gvect,     ONLY : ngm, g, gg, nl, nlm, mill, igtongl
  USE gvecs,   ONLY : ngms, nls, nlsm
  USE fft_base,   ONLY : dfftp, dffts
! DCC
!  USE gcoarse,   ONLY : nr1c,nr2c,nr3c,nnr,ngmc, nlc, nlcm
!  USE ee_mod,    ONLY : do_coarse
  USE ions_base, ONLY : nat
  USE lsda_mod,  ONLY : nspin
  USE spin_orb,  ONLY : domag
  USE scf,       ONLY : rho, v, vnew, vltot, vrs, rho_core, rhog_core, &
                        kedtau, create_scf_type
  USE wavefunctions_module, ONLY : psic, psic_nc
  IMPLICIT NONE
  !
  !     determines the data structure for fft arrays
  !
  CALL data_structure( .false. )
  !
! DCC
!  IF( do_coarse ) CALL data_structure_coarse( gamma_only, nr1,nr2,nr3, ecutwfc )
  !

  IF (dfftp%nnr.lt.ngm) THEN
     WRITE( stdout, '(/,4x," nr1=",i4," nr2= ", i4, " nr3=",i4, &
          &" nrxx = ",i8," ngm=",i8)') dfftp%nr1, dfftp%nr2, dfftp%nr3, dfftp%nnr, ngm
     CALL errore ('allocate_fft', 'the nr"s are too small!', 1)

  ENDIF
  IF (dffts%nnr.lt.ngms) THEN
     WRITE( stdout, '(/,4x," nr1s=",i4," nr2s= ", i4, " nr3s=",i4, &
          &" nrxxs = ",i8," ngms=",i8)') dffts%nr1, dffts%nr2, dffts%nr3, dffts%nnr, ngms
     CALL errore ('allocate_fft', 'the nrs"s are too small!', 1)

  ENDIF
  IF (ngm  <= 0) CALL errore ('allocate_fft', 'wrong ngm', 1)
  IF (ngms <= 0) CALL errore ('allocate_fft', 'wrong ngms', 1)
  IF (dfftp%nnr <= 0) CALL errore ('allocate_fft', 'wrong nnr', 1)
  IF (dffts%nnr<= 0) CALL errore ('allocate_fft', 'wrong smooth nnr', 1)
  IF (nspin<= 0) CALL errore ('allocate_fft', 'wrong nspin', 1)
  !
  !     Allocate memory for all kind of stuff.
  !
  CALL create_scf_type(rho)
  CALL create_scf_type(v,    do_not_allocate_becsum = .true.)
  CALL create_scf_type(vnew, do_not_allocate_becsum = .true.)
  ALLOCATE (vltot( dfftp%nnr))
  ALLOCATE (rho_core( dfftp%nnr))
     ALLOCATE ( kedtau(1,nspin) )
  ALLOCATE( rhog_core( ngm ) )
  ALLOCATE (psic( dfftp%nnr))
  ALLOCATE (vrs( dfftp%nnr, nspin))

! DCC
!  IF( do_coarse ) THEN
!     ALLOCATE (nlc( ngmc))
!     IF (gamma_only) ALLOCATE (nlcm(ngmc))
!  ENDIF



  RETURN
END SUBROUTINE allocate_fft
