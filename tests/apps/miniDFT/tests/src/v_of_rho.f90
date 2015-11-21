!
! Copyright (C) 2001-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
SUBROUTINE v_of_rho( rho, rho_core, rhog_core, &
                     ehart, etxc, vtxc, eth, etotefield, charge, v )
  !----------------------------------------------------------------------------
  !
  ! ... This routine computes the Hartree and Exchange and Correlation
  ! ... potential and energies which corresponds to a given charge density
  ! ... The XC potential is computed in real space, while the
  ! ... Hartree potential is computed in reciprocal space.
  !
  USE kinds,            ONLY : DP
  USE fft_base,         ONLY : dfftp
  USE gvect,            ONLY : ngm
  USE ions_base,        ONLY : nat
  USE scf,              ONLY : scf_type
  !
  IMPLICIT NONE
  !
  TYPE(scf_type), INTENT(IN) :: rho  ! the valence charge
  TYPE(scf_type), INTENT(INOUT) :: v ! the scf (Hxc) potential 
  !!!!!!!!!!!!!!!!! NB: NOTE that in F90 derived data type must be INOUT and 
  !!!!!!!!!!!!!!!!! not just OUT because otherwise their allocatable or pointer
  !!!!!!!!!!!!!!!!! components are NOT defined !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  REAL(DP), INTENT(IN) :: rho_core(dfftp%nnr)
    ! the core charge
  COMPLEX(DP), INTENT(IN) :: rhog_core(ngm)
    ! the core charge in reciprocal space
  REAL(DP), INTENT(OUT) :: vtxc, etxc, ehart, eth, charge
    ! the integral V_xc * rho
    ! the E_xc energy
    ! the hartree energy
    ! the hubbard energy
    ! the integral of the charge
  REAL(DP), INTENT(INOUT) :: etotefield
    ! electric field energy - inout due to the screwed logic of add_efield
  ! ! 
  INTEGER :: is
  !
  CALL start_clock( 'v_of_rho' )
  !
  ! ... calculate exchange-correlation potential
  !
     CALL v_xc( rho, rho_core, rhog_core, etxc, vtxc, v%of_r )
  !
  ! ... add a magnetic field  (if any)
  !
  !
  ! ... calculate hartree potential
  !
  CALL v_h( rho%of_g, ehart, charge, v%of_r )
  !
  CALL stop_clock( 'v_of_rho' )
  !
  RETURN
  !
END SUBROUTINE v_of_rho
!----------------------------------------------------------------------------
SUBROUTINE v_xc( rho, rho_core, rhog_core, etxc, vtxc, v )
  !----------------------------------------------------------------------------
  !
  ! ... Exchange-Correlation potential Vxc(r) from n(r)
  !
  USE kinds,            ONLY : DP
  USE constants,        ONLY : e2, eps8
  USE io_global,        ONLY : stdout
  USE fft_base,         ONLY : dfftp
  USE gvect,            ONLY : ngm
  USE lsda_mod,         ONLY : nspin
  USE cell_base,        ONLY : omega
  USE spin_orb,         ONLY : domag
  USE funct,            ONLY : xc, xc_spin
  USE scf,              ONLY : scf_type
  USE mp_global,        ONLY : intra_pool_comm, intra_bgrp_comm, mpime
  USE mp,               ONLY : mp_sum

  !
  IMPLICIT NONE
  !
  TYPE (scf_type), INTENT(IN) :: rho
  REAL(DP), INTENT(IN) :: rho_core(dfftp%nnr)
    ! the core charge
  COMPLEX(DP), INTENT(IN) :: rhog_core(ngm)
    ! input: the core charge in reciprocal space
  REAL(DP), INTENT(OUT) :: v(dfftp%nnr,nspin), vtxc, etxc
    ! V_xc potential
    ! integral V_xc * rho
    ! E_xc energy
  !
  ! ... local variables
  !
  REAL(DP) :: rhox, arhox, zeta, amag, vs, ex, ec, vx(2), vc(2), rhoneg(2)
    ! the total charge in each point
    ! the absolute value of the charge
    ! the absolute value of the charge
    ! local exchange energy
    ! local correlation energy
    ! local exchange potential
    ! local correlation potential
  INTEGER :: ir, ipol
    ! counter on mesh points
    ! counter on nspin
  !
  REAL(DP), PARAMETER :: vanishing_charge = 1.D-10, &
                         vanishing_mag    = 1.D-20
  !
  !
  CALL start_clock( 'v_xc' )
  !
  etxc   = 0.D0
  vtxc   = 0.D0
  v(:,:) = 0.D0
  rhoneg = 0.D0
  !
  IF ( nspin == 1 .OR. ( nspin == 4 .AND. .NOT. domag ) ) THEN
     !
     ! ... spin-unpolarized case
     !
!$omp parallel do private( rhox, arhox, ex, ec, vx, vc ), &
!$omp             reduction(+:etxc,vtxc), reduction(-:rhoneg)
     DO ir = 1, dfftp%nnr
        !
        rhox = rho%of_r(ir,1) + rho_core(ir)
        !
        arhox = ABS( rhox )
        !
        IF ( arhox > vanishing_charge ) THEN
           !
           CALL xc( arhox, ex, ec, vx(1), vc(1) )
           !
           v(ir,1) = e2*( vx(1) + vc(1) )
           !
           etxc = etxc + e2*( ex + ec ) * rhox
           !
           vtxc = vtxc + v(ir,1) * rho%of_r(ir,1)
           !
        ENDIF
        !
        IF ( rho%of_r(ir,1) < 0.D0 ) rhoneg(1) = rhoneg(1) - rho%of_r(ir,1)
        !
     END DO
!$omp end parallel do
     !
  ELSE IF ( nspin == 2 ) THEN
     !
     ! ... spin-polarized case
     !
!$omp parallel do private( rhox, arhox, zeta, ex, ec, vx, vc ), &
!$omp             reduction(+:etxc,vtxc), reduction(-:rhoneg)
     DO ir = 1, dfftp%nnr
        !
        rhox = rho%of_r(ir,1) + rho%of_r(ir,2) + rho_core(ir)
        !
        arhox = ABS( rhox )
        !
        IF ( arhox > vanishing_charge ) THEN
           !
           zeta = ( rho%of_r(ir,1) - rho%of_r(ir,2) ) / arhox
           !
           IF ( ABS( zeta ) > 1.D0 ) zeta = SIGN( 1.D0, zeta )
           !
           IF ( rho%of_r(ir,1) < 0.D0 ) rhoneg(1) = rhoneg(1) - rho%of_r(ir,1)
           IF ( rho%of_r(ir,2) < 0.D0 ) rhoneg(2) = rhoneg(2) - rho%of_r(ir,2)
           !
           CALL xc_spin( arhox, zeta, ex, ec, vx(1), vx(2), vc(1), vc(2) )
           !
           v(ir,:) = e2*( vx(:) + vc(:) )
           !
           etxc = etxc + e2*( ex + ec ) * rhox
           !
           vtxc = vtxc + v(ir,1) * rho%of_r(ir,1) + v(ir,2) * rho%of_r(ir,2)
           !
        END IF
        !
     END DO
!$omp end parallel do
     !
  ELSE IF ( nspin == 4 ) THEN
     !
     ! ... noncolinear case
     !
     DO ir = 1,dfftp%nnr
        !
        amag = SQRT( rho%of_r(ir,2)**2 + rho%of_r(ir,3)**2 + rho%of_r(ir,4)**2 )
        !
        rhox = rho%of_r(ir,1) + rho_core(ir)
        !
        IF ( rho%of_r(ir,1) < 0.D0 )  rhoneg(1) = rhoneg(1) - rho%of_r(ir,1)
        !
        arhox = ABS( rhox )
        !
        IF ( arhox > vanishing_charge ) THEN
           !
           zeta = amag / arhox
           !
           IF ( ABS( zeta ) > 1.D0 ) THEN
              !
              rhoneg(2) = rhoneg(2) + 1.D0 / omega
              !
              zeta = SIGN( 1.D0, zeta )
              !
           END IF
           !
           CALL xc_spin( arhox, zeta, ex, ec, vx(1), vx(2), vc(1), vc(2) )
           !
           vs = 0.5D0*( vx(1) + vc(1) - vx(2) - vc(2) )
           !
           v(ir,1) = e2*( 0.5D0*( vx(1) + vc(1) + vx(2) + vc(2 ) ) )
           !
           IF ( amag > vanishing_mag ) THEN
              !
              DO ipol = 2, 4
                 !
                 v(ir,ipol) = e2 * vs * rho%of_r(ir,ipol) / amag
                 !
                 vtxc = vtxc + v(ir,ipol) * rho%of_r(ir,ipol)
                 !
              END DO
              !
           END IF
           !
           etxc = etxc + e2*( ex + ec ) * rhox
           vtxc = vtxc + v(ir,1) * rho%of_r(ir,1)
           !
        END IF
        !
     END DO
     !
  END IF
  !
  CALL mp_sum(  rhoneg , intra_bgrp_comm )
  !
  rhoneg(:) = rhoneg(:) * omega / ( dfftp%nr1*dfftp%nr2*dfftp%nr3 )
  !
  IF ( rhoneg(1) > eps8 .OR. rhoneg(2) > eps8 ) &
     WRITE( stdout,'(/,5X,"negative rho (up, down): ",2E10.3)') rhoneg
  !
  ! ... energy terms, local-density contribution
  !
  vtxc = omega * vtxc / ( dfftp%nr1*dfftp%nr2*dfftp%nr3 )
  etxc = omega * etxc / ( dfftp%nr1*dfftp%nr2*dfftp%nr3 )
  !
  ! ... add gradient corrections (if any)
  !
  CALL gradcorr( rho%of_r, rho%of_g, rho_core, rhog_core, etxc, vtxc, v )
 
  !
  CALL mp_sum(  vtxc , intra_bgrp_comm )
  CALL mp_sum(  etxc , intra_bgrp_comm )
  !
  CALL stop_clock( 'v_xc' )
  !
  RETURN
  !
END SUBROUTINE v_xc
!
!----------------------------------------------------------------------------
SUBROUTINE v_h( rhog, ehart, charge, v )
  !----------------------------------------------------------------------------
  !
  ! ... Hartree potential VH(r) from n(G)
  !
  USE constants, ONLY : fpi, e2
  USE kinds,     ONLY : DP
  USE fft_base,  ONLY : dfftp
  USE fft_interfaces,ONLY : invfft
  USE gvect,     ONLY : nl, nlm, ngm, gg, gstart
  USE lsda_mod,  ONLY : nspin
  USE cell_base, ONLY : omega, tpiba2
  USE mp_global, ONLY: intra_pool_comm, intra_bgrp_comm
  USE mp,        ONLY: mp_sum
  !
  IMPLICIT NONE
  !
  COMPLEX(DP), INTENT(IN)  :: rhog(ngm,nspin)
  REAL(DP),  INTENT(INOUT) :: v(dfftp%nnr,nspin)
  REAL(DP),    INTENT(OUT) :: ehart, charge
  !
  REAL(DP)              :: fac
  REAL(DP), ALLOCATABLE :: aux1(:,:)
  REAL(DP)              :: rgtot_re, rgtot_im, eh_corr
  INTEGER               :: is, ig
  COMPLEX(DP), ALLOCATABLE :: aux(:), rgtot(:), vaux(:)
  INTEGER               :: nt
  !
  CALL start_clock( 'v_h' )
  !
  ALLOCATE( aux( dfftp%nnr ), aux1( 2, ngm ) )
  charge = 0.D0
  !
  IF ( gstart == 2 ) THEN
     !
     charge = omega*REAL( rhog(1,1) )
     !
     IF ( nspin == 2 ) charge = charge + omega*REAL( rhog(1,2) )
     !
  END IF
  !
  CALL mp_sum(  charge , intra_bgrp_comm )
  !
  ! ... calculate hartree potential in G-space (NB: V(G=0)=0 )
  !
     !
     ehart     = 0.D0
     aux1(:,:) = 0.D0
     !
!$omp parallel do private( fac, rgtot_re, rgtot_im ), reduction(+:ehart)
     DO ig = gstart, ngm
        !
        fac = 1.D0 / gg(ig)
        !
        rgtot_re = REAL(  rhog(ig,1) )
        rgtot_im = AIMAG( rhog(ig,1) )
        !
        IF ( nspin == 2 ) THEN
           !
           rgtot_re = rgtot_re + REAL(  rhog(ig,2) )
           rgtot_im = rgtot_im + AIMAG( rhog(ig,2) )
           !
        END IF
        !
        ehart = ehart + ( rgtot_re**2 + rgtot_im**2 ) * fac
        !
        aux1(1,ig) = rgtot_re * fac
        aux1(2,ig) = rgtot_im * fac
        !
     ENDDO
!$omp end parallel do
     !
     fac = e2 * fpi / tpiba2
     !
     ehart = ehart * fac
     !
     aux1 = aux1 * fac
     !
        !
        ehart = ehart * 0.5D0 * omega
        !
     ! 
     !
     CALL mp_sum(  ehart , intra_bgrp_comm )
     ! 
     aux(:) = 0.D0
     !
     aux(nl(1:ngm)) = CMPLX ( aux1(1,1:ngm), aux1(2,1:ngm), KIND=dp )
     !
  !
  ! ... transform hartree potential to real space
  !
  CALL invfft ('Dense', aux, dfftp)
  !
  ! ... add hartree potential to the xc potential
  !
  IF ( nspin == 4 ) THEN
     !
     v(:,1) = v(:,1) + DBLE (aux(:))
     !
  ELSE
     !
     DO is = 1, nspin
        !
        v(:,is) = v(:,is) + DBLE (aux(:))
        !
     END DO
     !
  END IF
  !
  DEALLOCATE( aux, aux1 )
  !
  CALL stop_clock( 'v_h' )
  !
  RETURN
  !
END SUBROUTINE v_h
!

!----------------------------------------------------------------------------
SUBROUTINE v_h_of_rho_r( rhor, ehart, charge, v )
  !----------------------------------------------------------------------------
  !
  ! ... Hartree potential VH(r) from a density in R space n(r) 
  !
  USE kinds,           ONLY : DP
  USE fft_base,        ONLY : dfftp
  USE fft_interfaces,  ONLY : fwfft
  USE gvect,           ONLY : nl, ngm
  USE lsda_mod,        ONLY : nspin
  !
  IMPLICIT NONE
  !
  ! ... Declares variables
  !
  REAL( DP ), INTENT(IN)     :: rhor( dfftp%nnr, nspin )
  REAL( DP ), INTENT(INOUT)  :: v( dfftp%nnr, nspin )
  REAL( DP ), INTENT(OUT)    :: ehart, charge
  !
  ! ... Local variables
  !
  COMPLEX( DP ), ALLOCATABLE :: rhog( : , : )
  COMPLEX( DP ), ALLOCATABLE :: aux( : )
  INTEGER :: is
  !
  ! ... bring the (unsymmetrized) rho(r) to G-space (use aux as work array)
  !
  ALLOCATE( rhog( ngm, nspin ) )
  ALLOCATE( aux( dfftp%nnr ) )
  DO is = 1, nspin
     aux(:) = CMPLX(rhor( : , is ),0.D0,kind=dp) 
     CALL fwfft ('Dense', aux, dfftp)
     rhog(:,is) = aux(nl(:))
  END DO
  DEALLOCATE( aux )
  !
  ! ... compute VH(r) from n(G) 
  !
  CALL v_h( rhog, ehart, charge, v )
  DEALLOCATE( rhog )
  !
  RETURN
  !
END SUBROUTINE v_h_of_rho_r
!----------------------------------------------------------------------------
SUBROUTINE gradv_h_of_rho_r( rho, gradv )
  !----------------------------------------------------------------------------
  !
  ! ... Gradient of Hartree potential in R space from a total 
  !     (spinless) density in R space n(r)
  !
  USE kinds,           ONLY : DP
  USE fft_base,        ONLY : dfftp
  USE fft_interfaces,  ONLY : fwfft, invfft
  USE constants,       ONLY : fpi, e2
  USE cell_base,       ONLY : tpiba, omega
  USE gvect,           ONLY : nl, ngm, nlm, gg, gstart, g
  !
  IMPLICIT NONE
  !
  ! ... Declares variables
  !
  REAL( DP ), INTENT(IN)     :: rho( dfftp%nnr )
  REAL( DP ), INTENT(OUT)    :: gradv( 3, dfftp%nnr )
  !
  ! ... Local variables
  !
  COMPLEX( DP ), ALLOCATABLE :: rhoaux( : )
  COMPLEX( DP ), ALLOCATABLE :: gaux( : )
  COMPLEX( DP ), ALLOCATABLE :: rgtot(:), vaux(:)
  REAL( DP )                 :: fac, eh_corr
  INTEGER                    :: ig, ipol
  !
  ! ... Bring rho to G space
  !
  ALLOCATE( rhoaux( dfftp%nnr ) )
  rhoaux( : ) = CMPLX( rho( : ), 0.D0 ) 
  !
  CALL fwfft('Dense', rhoaux, dfftp)
  !
  ! ... Compute total potential in G space
  !
  ALLOCATE( gaux( dfftp%nnr ) )
  !
  DO ipol = 1, 3
    !
    gaux(:) = CMPLX(0.d0,0.d0,kind=dp)
    !
    DO ig = gstart, ngm
      !
      fac = g(ipol,ig) / gg(ig)
      gaux(nl(ig)) = CMPLX(-AIMAG(rhoaux(nl(ig))),REAL(rhoaux(nl(ig))),kind=dp) * fac 
      !
    END DO
    !
    ! ...and add the factor e2*fpi/2\pi/a coming from the missing prefactor of 
    !  V = e2 * fpi divided by the 2\pi/a factor missing in G  
    !
    fac = e2 * fpi / tpiba
    gaux = gaux * fac 
    !
    ! ... bring back to R-space, (\grad_ipol a)(r) ...
    !
    CALL invfft ('Dense', gaux, dfftp)
    !
    gradv(ipol,:) = REAL( gaux(:) )
    !
  ENDDO
  !
  DEALLOCATE(gaux)
  !
  RETURN
  !
END SUBROUTINE gradv_h_of_rho_r
