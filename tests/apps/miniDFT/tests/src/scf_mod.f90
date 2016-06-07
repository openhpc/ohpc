!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!--------------------------------------------------------------------------
!
MODULE scf
  !  
  !  This module contains variables and auxiliary routines needed for
  !  the self-consistent cycle
  !
  !  ROUTINES: allocate_scf_type
  !
  USE kinds,      ONLY : DP
  !
  USE lsda_mod,     ONLY : nspin
  USE ions_base,    ONLY : nat
  USE io_files,     ONLY : diropn
  USE fft_base,     ONLY : dfftp
  USE fft_interfaces,ONLY : invfft
  USE gvect,        ONLY : ngm
  USE gvecs,      ONLY : ngms
  USE uspp_param,   ONLY : nhm
  USE extfield,     ONLY : dipfield, emaxpos, eopreg, edir
  !
  SAVE
  !
! Details of PAW implementation:
! NOTE: scf_type is used for two different quantities: density and potential.
!       These correspond, for PAW, to becsum and D coefficients.
!       Due to interference with the ultrasoft routines only the becsum part
!       is stored in the structure (at the moment).
!       This only holds for scf_type; mix_type is not affected.
! NOTE: rho%bec is different from becsum for two reasons:
!       1. rho%bec is mixed, while becsum is not
!       2. for npool > 1 rho%bec is collected, becsum is not
!          ( this is necessary to make the stress work)
#ifdef __STD_F95
  TYPE scf_type
     REAL(DP),   POINTER :: of_r(:,:)  ! the charge density in R-space
     COMPLEX(DP),POINTER :: of_g(:,:)  ! the charge density in G-space
  END TYPE scf_type
  !
  TYPE mix_type
     COMPLEX(DP), POINTER :: of_g(:,:)  ! the charge density in G-space
     REAL(DP)                   :: el_dipole  ! electrons dipole
  END TYPE mix_type
#else
  TYPE scf_type
     REAL(DP),   ALLOCATABLE :: of_r(:,:)  ! the charge density in R-space
     COMPLEX(DP),ALLOCATABLE :: of_g(:,:)  ! the charge density in G-space
  END TYPE scf_type
  !
  TYPE mix_type
     COMPLEX(DP), ALLOCATABLE :: of_g(:,:)  ! the charge density in G-space
     REAL(DP)                   :: el_dipole  ! electrons dipole
  END TYPE mix_type
#endif

  type (scf_type) :: rho  ! the charge density and its other components

  type (scf_type) :: v    ! the scf potential
  type (scf_type) :: vnew ! used to correct the forces

  REAL(DP) :: v_of_0    ! vltot(G=0)      
  REAL(DP), ALLOCATABLE :: &
       vltot(:),       &! the local potential in real space
       vrs(:,:),       &! the total pot. in real space (smooth grid)
       rho_core(:),    &! the core charge in real space
       kedtau(:,:)      ! position dependent kinetic energy enhancement factor
  COMPLEX(DP), ALLOCATABLE :: &
       rhog_core(:)     ! the core charge in reciprocal space

  INTEGER, PRIVATE  :: record_length, &
                       rlen_rho=0,  rlen_kin=0,  rlen_ldaU=0,  rlen_bec=0,&
                       rlen_dip=0, &
                       start_rho=0, start_kin=0, start_ldaU=0, start_bec=0, &
                       start_dipole=0
  REAL(DP), PRIVATE, ALLOCATABLE:: io_buffer(:)
CONTAINS

 SUBROUTINE create_scf_type ( rho, do_not_allocate_becsum )
   IMPLICIT NONE
   TYPE (scf_type) :: rho
   LOGICAL,INTENT(IN),OPTIONAL :: do_not_allocate_becsum ! PAW hack
   allocate ( rho%of_r( dfftp%nnr, nspin) )
   allocate ( rho%of_g( ngm, nspin ) )
#ifdef __STD_F95
#endif
   
 return
 END SUBROUTINE create_scf_type

 SUBROUTINE destroy_scf_type ( rho )
   IMPLICIT NONE
   TYPE (scf_type) :: rho
#ifdef __STD_F95
   if (ASSOCIATED(rho%of_r))  deallocate(rho%of_r)
   if (ASSOCIATED(rho%of_g))  deallocate(rho%of_g)
#else
   if (ALLOCATED(rho%of_r))  deallocate(rho%of_r)
   if (ALLOCATED(rho%of_g))  deallocate(rho%of_g)
#endif
   return
 END SUBROUTINE destroy_scf_type
 !
 
 SUBROUTINE create_mix_type ( rho )
   IMPLICIT NONE
   TYPE (mix_type) :: rho
   allocate ( rho%of_g( ngms, nspin ) )

   rho%of_g = 0._dp
   if (dipfield)      rho%el_dipole =  0._dp
   
 return
 END SUBROUTINE create_mix_type

 SUBROUTINE destroy_mix_type ( rho )
   IMPLICIT NONE
   TYPE (mix_type) :: rho
#ifdef __STD_F95
#else
   if (ALLOCATED(rho%of_g))  deallocate(rho%of_g)
#endif
   return
 END SUBROUTINE destroy_mix_type
 !
 subroutine assign_scf_to_mix_type(rho_s, rho_m)
   IMPLICIT NONE
   TYPE (scf_type), INTENT(IN)  :: rho_s
   TYPE (mix_type), INTENT(INOUT) :: rho_m
   REAL(DP) :: e_dipole
      
   rho_m%of_g(1:ngms,:) = rho_s%of_g(1:ngms,:)
   
   
   if (dipfield) then
      write(*,*) "scf_mod.f90:226 skipping compute_el_dip"
      !!$ CALL compute_el_dip(emaxpos, eopreg, edir, rho_s%of_r,e_dipole)
      !!rho_m%el_dipole = e_dipole
      rho_m%el_dipole = 0.d0
   endif
   
 return
 end subroutine assign_scf_to_mix_type
 !
 subroutine assign_mix_to_scf_type(rho_m, rho_s)
   USE wavefunctions_module, ONLY : psic
   USE gvect,                ONLY : nl, nlm
   IMPLICIT NONE
   TYPE (mix_type), INTENT(IN) :: rho_m
   TYPE (scf_type), INTENT(INOUT) :: rho_s
   INTEGER :: is
   
   rho_s%of_g(1:ngms,:) = rho_m%of_g(1:ngms,:)
   ! define rho_s%of_r 

   DO is = 1, nspin
      psic(:) = ( 0.D0, 0.D0 )
      psic(nl(:)) = rho_s%of_g(:,is)
      CALL invfft ('Dense', psic, dfftp)
      rho_s%of_r(:,is) = psic(:)
   END DO

       
   return
 end subroutine assign_mix_to_scf_type
 !
 !----------------------------------------------------------------------------
 subroutine scf_type_COPY (X,Y)
  !----------------------------------------------------------------------------
  ! works like DCOPY for scf_type copy variables :  Y = X 
  USE kinds, ONLY : DP
  IMPLICIT NONE
  TYPE(scf_type), INTENT(IN)    :: X
  TYPE(scf_type), INTENT(INOUT) :: Y
  Y%of_r  = X%of_r
  Y%of_g  = X%of_g
  !
  RETURN
 end subroutine scf_type_COPY
 !
 !----------------------------------------------------------------------------
 subroutine mix_type_AXPY (A,X,Y)
  !----------------------------------------------------------------------------
  ! works like daxpy for scf_type variables :  Y = A * X + Y
  ! NB: A is a REAL(DP) number
  USE kinds, ONLY : DP
  IMPLICIT NONE
  REAL(DP)                      :: A
  TYPE(mix_type), INTENT(IN)    :: X
  TYPE(mix_type), INTENT(INOUT) :: Y
  Y%of_g  = Y%of_g  + A * X%of_g
  if (dipfield)  Y%el_dipole =  Y%el_dipole + A * X%el_dipole
  !
  RETURN
 END SUBROUTINE mix_type_AXPY
 !
 !----------------------------------------------------------------------------
 subroutine mix_type_COPY (X,Y)
  !----------------------------------------------------------------------------
  ! works like DCOPY for mix_type copy variables :  Y = X 
  USE kinds, ONLY : DP
  IMPLICIT NONE
  TYPE(mix_type), INTENT(IN)    :: X
  TYPE(mix_type), INTENT(INOUT) :: Y
  Y%of_g  = X%of_g
  if (dipfield)   Y%el_dipole =  X%el_dipole
  !
  RETURN
 end subroutine mix_type_COPY
 !
 !----------------------------------------------------------------------------
 subroutine mix_type_SCAL (A,X)
  !----------------------------------------------------------------------------
  ! works like DSCAL for mix_type copy variables :  X = A * X 
  ! NB: A is a REAL(DP) number
  USE kinds, ONLY : DP
  IMPLICIT NONE
  REAL(DP),       INTENT(IN)    :: A
  TYPE(mix_type), INTENT(INOUT) :: X
  X%of_g(:,:)  = A * X%of_g(:,:)
  if (dipfield)   X%el_dipole =  A * X%el_dipole
  !
  RETURN
 end subroutine mix_type_SCAL
 !
 subroutine high_frequency_mixing ( rhoin, input_rhout, alphamix )
   USE wavefunctions_module, ONLY : psic
   USE gvect,                ONLY : nl, nlm
 IMPLICIT NONE
   TYPE (scf_type), INTENT(INOUT)     :: rhoin
   TYPE (scf_type), INTENT(IN)  :: input_rhout
   REAL(DP), INTENT(IN) :: alphamix
   INTEGER :: is
   if (ngms < ngm ) then
      rhoin%of_g = rhoin%of_g + alphamix * ( input_rhout%of_g-rhoin%of_g)
      rhoin%of_g(1:ngms,1:nspin) = (0.d0,0.d0)
      ! define rho_s%of_r 
      DO is = 1, nspin
         psic(:) = ( 0.D0, 0.D0 )
         psic(nl(:)) = rhoin%of_g(:,is)
         CALL invfft ('Dense', psic, dfftp)
         rhoin%of_r(:,is) = psic(:)
      END DO
      !
   else
      rhoin%of_g(:,:)= (0.d0,0.d0)
      rhoin%of_r(:,:)= 0.d0
   endif
   return
 end subroutine high_frequency_mixing 


 subroutine diropn_mix_file( iunit, extension, exst )
   implicit none
   character(len=*), intent(in) :: extension
   integer, intent(in) :: iunit
   logical :: exst
   ! define lengths of different record chunks
   rlen_rho = 2 * ngms * nspin
   if (dipfield)       rlen_dip = 1
   ! define total record length
   record_length = rlen_rho + rlen_kin + rlen_ldaU + rlen_bec + rlen_dip
   ! and the starting point of different chunks
   start_rho = 1
   start_kin = start_rho + rlen_rho
   start_ldaU = start_kin + rlen_kin
   start_bec = start_ldaU + rlen_ldaU
   start_dipole = start_bec + rlen_bec
   ! open file and allocate io_buffer
   call diropn ( iunit, extension, record_length, exst)
   allocate (io_buffer(record_length+1))
   !
 return
 end subroutine diropn_mix_file
 !
 subroutine close_mix_file( iunit )
   implicit none
   integer, intent(in) :: iunit
   deallocate (io_buffer)
   close(iunit,status='keep')
   return
 end subroutine close_mix_file

 subroutine davcio_mix_type( rho, iunit, record, iflag )
   implicit none
   type (mix_type) :: rho
   integer, intent(in) :: iunit, record, iflag
   if (iflag > 0) then
      
      call DCOPY(rlen_rho,rho%of_g,1,io_buffer(start_rho),1)
      if (dipfield)      call DCOPY(1, rho%el_dipole,  1,io_buffer(start_dipole),1)
      
   end if
   CALL davcio( io_buffer, record_length, iunit, record, iflag )
   if (iflag < 0) then

      call DCOPY(rlen_rho,io_buffer(start_rho),1,rho%of_g,1)
      if (dipfield)      call DCOPY(1, io_buffer(start_dipole), 1, rho%el_dipole, 1)

   end if
 end subroutine davcio_mix_type
 !
 !----------------------------------------------------------------------------
 FUNCTION rho_ddot( rho1, rho2, gf )
  !----------------------------------------------------------------------------
  !
  ! ... calculates 4pi/G^2*rho1(-G)*rho2(G) = V1_Hartree(-G)*rho2(G)
  ! ... used as an estimate of the self-consistency error on the energy
  !
  USE kinds,         ONLY : DP
  USE constants,     ONLY : e2, tpi, fpi
  USE cell_base,     ONLY : omega, tpiba2
  USE gvect,         ONLY : gg, gstart
  USE spin_orb,      ONLY : domag
  USE mp_global,     ONLY : intra_bgrp_comm
  USE mp,            ONLY : mp_sum
  !
  IMPLICIT NONE
  !
  type(mix_type), INTENT(IN) :: rho1, rho2
  INTEGER,        INTENT(IN) :: gf
  REAL(DP)                :: rho_ddot
  !
  REAL(DP) :: fac
  INTEGER  :: ig
  !
  fac = e2 * fpi / tpiba2
  !
  rho_ddot = 0.D0
  
  IF ( nspin == 1 ) THEN
     !
     DO ig = gstart, gf
        !
        rho_ddot = rho_ddot + &
                   REAL( CONJG( rho1%of_g(ig,1) )*rho2%of_g(ig,1), DP ) / gg(ig)
        !
     END DO
     !
     rho_ddot = fac*rho_ddot
     !
     !
  ELSE IF ( nspin == 2 ) THEN
     !
     ! ... first the charge
     !
     DO ig = gstart, gf
        !
        rho_ddot = rho_ddot + &
                   REAL( CONJG( rho1%of_g(ig,1)+rho1%of_g(ig,2) ) * &
                              ( rho2%of_g(ig,1)+rho2%of_g(ig,2) ), DP ) / gg(ig)
        !
     END DO
     !
     rho_ddot = fac*rho_ddot
     !
     !
     ! ... then the magnetization
     !
     fac = e2 * fpi / tpi**2  ! lambda = 1 a.u.
     !
     ! ... G=0 term
     !
     IF ( gstart == 2 ) THEN
        !
        rho_ddot = rho_ddot + &
                   fac * REAL( CONJG( rho1%of_g(1,1) - rho1%of_g(1,2) ) * &
                                    ( rho2%of_g(1,1) - rho2%of_g(1,2) ), DP )
        !
     END IF
     !
     !
     DO ig = gstart, gf
        !
        rho_ddot = rho_ddot + &
                   fac * REAL( CONJG( rho1%of_g(ig,1) - rho1%of_g(ig,2) ) * &
                                    ( rho2%of_g(ig,1) - rho2%of_g(ig,2) ), DP )
        !
     END DO
     !
  ELSE IF ( nspin == 4 ) THEN
     !
     DO ig = gstart, gf
        !
        rho_ddot = rho_ddot + &
                   REAL( CONJG( rho1%of_g(ig,1) )*rho2%of_g(ig,1), DP ) / gg(ig)
        !
     END DO
     !
     rho_ddot = fac*rho_ddot
     !
     !
     IF (domag) THEN
        fac = e2*fpi / (tpi**2)  ! lambda=1 a.u.
        !
        IF ( gstart == 2 ) THEN
           !
           rho_ddot = rho_ddot + &
                   fac * ( REAL( CONJG( rho1%of_g(1,2))*(rho2%of_g(1,2) ),DP ) + &
                           REAL( CONJG( rho1%of_g(1,3))*(rho2%of_g(1,3) ),DP ) + &
                           REAL( CONJG( rho1%of_g(1,4))*(rho2%of_g(1,4) ),DP ) )
           !
        END IF
        !
        !
        DO ig = gstart, gf
           !
           rho_ddot = rho_ddot + &
                   fac *( REAL( CONJG( rho1%of_g(ig,2))*(rho2%of_g(ig,2) ), DP ) + &
                          REAL( CONJG( rho1%of_g(ig,3))*(rho2%of_g(ig,3) ), DP ) + &
                          REAL( CONJG( rho1%of_g(ig,4))*(rho2%of_g(ig,4) ), DP ) )
           !
        END DO
        !
     END IF
     !
  END IF
  !
  rho_ddot = rho_ddot * omega * 0.5D0
  !
  CALL mp_sum(  rho_ddot , intra_bgrp_comm )
  !
  IF (dipfield)      rho_ddot = rho_ddot + (e2/2.0_DP)* &
                                    (rho1%el_dipole * rho2%el_dipole)*omega/fpi

  RETURN
  !
 END FUNCTION rho_ddot
 !
!----------------------------------------------------------------------------
 !----------------------------------------------------------------------------
 FUNCTION local_tf_ddot( rho1, rho2, ngm0 )
  !----------------------------------------------------------------------------
  !
  ! ... calculates 4pi/G^2*rho1(-G)*rho2(G) = V1_Hartree(-G)*rho2(G)
  ! ... used as an estimate of the self-consistency error on the energy
  !
  USE kinds,         ONLY : DP
  USE constants,     ONLY : e2, fpi
  USE cell_base,     ONLY : omega, tpiba2
  USE gvect,         ONLY : gg, gstart
  USE mp_global,     ONLY : intra_bgrp_comm
  USE mp,            ONLY : mp_sum
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN)     :: ngm0
  COMPLEX(DP), INTENT(IN) :: rho1(ngm0), rho2(ngm0)
  REAL(DP)                :: local_tf_ddot
  !
  REAL(DP) :: fac
  INTEGER  :: ig
  !
  local_tf_ddot = 0.D0
  !
  fac = e2 * fpi / tpiba2
  !
  DO ig = gstart, ngm0
     local_tf_ddot = local_tf_ddot + REAL( CONJG(rho1(ig))*rho2(ig) ) / gg(ig)
  END DO
  !
  local_tf_ddot = fac * local_tf_ddot * omega * 0.5D0
  !
  !
  CALL mp_sum(  local_tf_ddot , intra_bgrp_comm )
  !
  RETURN
  !
 END FUNCTION local_tf_ddot
 !
END MODULE scf
