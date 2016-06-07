!
! Copyright (C) 2001-2008 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE gradcorr( rho, rhog, rho_core, rhog_core, etxc, vtxc, v )
  !----------------------------------------------------------------------------
  !
  USE constants,            ONLY : e2
  USE kinds,                ONLY : DP
  USE gvect,                ONLY : nl, ngm, g
  USE lsda_mod,             ONLY : nspin
  USE cell_base,            ONLY : omega, alat
  USE funct,                ONLY : gcxc, gcx_spin, gcc_spin, &
                                   dft_is_gradient, get_igcc
  USE spin_orb,             ONLY : domag
  USE wavefunctions_module, ONLY : psic
  USE fft_base,             ONLY : dfftp
  USE fft_interfaces,       ONLY : fwfft

  !
  IMPLICIT NONE
  !
  REAL(DP),    INTENT(IN)    :: rho(dfftp%nnr,nspin), rho_core(dfftp%nnr)
  COMPLEX(DP), INTENT(IN)    :: rhog(ngm,nspin), rhog_core(ngm)
  REAL(DP),    INTENT(INOUT) :: v(dfftp%nnr,nspin)
  REAL(DP),    INTENT(INOUT) :: vtxc, etxc
  !
  INTEGER :: k, ipol, is, nspin0, ir, jpol
  !
  REAL(DP),    ALLOCATABLE :: grho(:,:,:), h(:,:,:), dh(:)
  REAL(DP),    ALLOCATABLE :: rhoout(:,:), segni(:), vgg(:,:), vsave(:,:)
  REAL(DP),    ALLOCATABLE :: gmag(:,:,:)

  COMPLEX(DP), ALLOCATABLE :: rhogsum(:,:)
  !
  LOGICAL  :: igcc_is_lyp
  REAL(DP) :: grho2(2), sx, sc, v1x, v2x, v1c, v2c, &
              v1xup, v1xdw, v2xup, v2xdw, v1cup, v1cdw , &
              etxcgc, vtxcgc, segno, arho, fac, zeta, rh, grh2, amag 
  REAL(DP) :: v2cup, v2cdw,  v2cud, rup, rdw, &
              grhoup, grhodw, grhoud, grup, grdw, seg
  !
  REAL(DP), PARAMETER :: epsr = 1.D-6, epsg = 1.D-10
  !
  !
  IF ( .NOT. dft_is_gradient() ) RETURN

  igcc_is_lyp = (get_igcc() == 3 .or. get_igcc() == 7)
  !
  etxcgc = 0.D0
  vtxcgc = 0.D0
  !
  nspin0=nspin
  if (nspin==4) nspin0=1
  if (nspin==4.and.domag) nspin0=2
  fac = 1.D0 / DBLE( nspin0 )
  !
  ALLOCATE(    h( 3, dfftp%nnr, nspin0) )
  ALLOCATE( grho( 3, dfftp%nnr, nspin0) )
  ALLOCATE( rhoout( dfftp%nnr, nspin0) )
  IF (nspin==4.AND.domag) THEN
     ALLOCATE( vgg( dfftp%nnr, nspin0 ) )
     ALLOCATE( vsave( dfftp%nnr, nspin ) )
     ALLOCATE( segni( dfftp%nnr ) )
     vsave=v
     v=0.d0
  ENDIF
  !
  ALLOCATE( rhogsum( ngm, nspin0 ) )
  !
  ! ... calculate the gradient of rho + rho_core in real space
  !
  IF ( nspin == 4 .AND. domag ) THEN
     !
     write(*,*) "gradcor.f90:81 skipping compute_rho; shouldnt get here unless noncolin"
     !!$ CALL compute_rho(rho,rhoout,segni,dfftp%nnr)
     !
     ! ... bring starting rhoout to G-space
     !
     DO is = 1, nspin0
        !
        psic(:) = rhoout(:,is)
        !
        CALL fwfft ('Dense', psic, dfftp)
        !
        rhogsum(:,is) = psic(nl(:))
        !
     END DO
  ELSE
     !
     rhoout(:,1:nspin0)  = rho(:,1:nspin0)
     rhogsum(:,1:nspin0) = rhog(:,1:nspin0)
     !
  ENDIF
  DO is = 1, nspin0
     !
     rhoout(:,is)  = fac * rho_core(:)  + rhoout(:,is)
     rhogsum(:,is) = fac * rhog_core(:) + rhogsum(:,is)
     !
     CALL gradrho( dfftp%nnr, rhogsum(1,is), ngm, g, nl, grho(1,1,is) )
     !
  END DO
  !
  DEALLOCATE( rhogsum )
  !
  IF ( nspin0 == 1 ) THEN
     !
     ! ... This is the spin-unpolarised case
     !
     DO k = 1, dfftp%nnr
        !
        arho = ABS( rhoout(k,1) )
        !
        IF ( arho > epsr ) THEN
           !
           grho2(1) = grho(1,k,1)**2 + grho(2,k,1)**2 + grho(3,k,1)**2
           !
           IF ( grho2(1) > epsg ) THEN
              !
              segno = SIGN( 1.D0, rhoout(k,1) )
              !
              CALL gcxc( arho, grho2(1), sx, sc, v1x, v2x, v1c, v2c )
              !
              ! ... first term of the gradient correction : D(rho*Exc)/D(rho)
              !
              v(k,1) = v(k,1) + e2 * ( v1x + v1c )
              !
              ! ... h contains :
              !
              ! ...    D(rho*Exc) / D(|grad rho|) * (grad rho) / |grad rho|
              !
              h(:,k,1) = e2 * ( v2x + v2c ) * grho(:,k,1)
              !
              vtxcgc = vtxcgc+e2*( v1x + v1c ) * ( rhoout(k,1) - rho_core(k) )
              etxcgc = etxcgc+e2*( sx + sc ) * segno
              !
           ELSE
              h(:,k,1)=0.D0
           END IF
           !
        ELSE
           !
           h(:,k,1) = 0.D0
           !
        END IF
        !
     END DO
     !
  ELSE
     !
     ! ... spin-polarised case
     !
!$omp parallel do private( rh, grho2, sx, v1xup, v1xdw, v2xup, v2xdw, rup, rdw, &
!$omp             grhoup, grhodw, grhoud, sc, v1cup, v1cdw, v2cup, v2cdw, v2cud, &
!$omp             zeta, grh2, v2c, grup, grdw  ), &
!$omp             reduction(+:etxcgc,vtxcgc)
     DO k = 1, dfftp%nnr
        !
        rh = rhoout(k,1) + rhoout(k,2)
        !
        grho2(:) = grho(1,k,:)**2 + grho(2,k,:)**2 + grho(3,k,:)**2
        !
        CALL gcx_spin( rhoout(k,1), rhoout(k,2), grho2(1), &
                       grho2(2), sx, v1xup, v1xdw, v2xup, v2xdw )
        !
        IF ( rh > epsr ) THEN
           !
           IF ( igcc_is_lyp ) THEN
              !
              call errore('gradcorr','lyp type functionals not available in mini_DFT', 1 )
!!$              rup = rhoout(k,1)
!!$              rdw = rhoout(k,2)
!!$              !
!!$              grhoup = grho(1,k,1)**2 + grho(2,k,1)**2 + grho(3,k,1)**2
!!$              grhodw = grho(1,k,2)**2 + grho(2,k,2)**2 + grho(3,k,2)**2
!!$              !
!!$              grhoud = grho(1,k,1) * grho(1,k,2) + &
!!$                       grho(2,k,1) * grho(2,k,2) + &
!!$                       grho(3,k,1) * grho(3,k,2)
!!$              !
!!$              CALL gcc_spin_more( rup, rdw, grhoup, grhodw, grhoud, &
!!$                                  sc, v1cup, v1cdw, v2cup, v2cdw, v2cud )
              !
           ELSE
              !
              zeta = ( rhoout(k,1) - rhoout(k,2) ) / rh
              if (nspin.eq.4.and.domag) zeta=abs(zeta)*segni(k)
              !
              grh2 = ( grho(1,k,1) + grho(1,k,2) )**2 + &
                     ( grho(2,k,1) + grho(2,k,2) )**2 + &
                     ( grho(3,k,1) + grho(3,k,2) )**2
              !
              CALL gcc_spin( rh, zeta, grh2, sc, v1cup, v1cdw, v2c )
              !
              v2cup = v2c
              v2cdw = v2c
              v2cud = v2c
              !
           END IF
           !
        ELSE
           !
           sc    = 0.D0
           v1cup = 0.D0
           v1cdw = 0.D0
           v2c   = 0.D0
           v2cup = 0.D0
           v2cdw = 0.D0
           v2cud = 0.D0
           !
        ENDIF
        !
        ! ... first term of the gradient correction : D(rho*Exc)/D(rho)
        !
        v(k,1) = v(k,1) + e2 * ( v1xup + v1cup )
        v(k,2) = v(k,2) + e2 * ( v1xdw + v1cdw )
        !
        ! ... h contains D(rho*Exc)/D(|grad rho|) * (grad rho) / |grad rho|
        !
        DO ipol = 1, 3
           !
           grup = grho(ipol,k,1)
           grdw = grho(ipol,k,2)
           h(ipol,k,1) = e2 * ( ( v2xup + v2cup ) * grup + v2cud * grdw )
           h(ipol,k,2) = e2 * ( ( v2xdw + v2cdw ) * grdw + v2cud * grup )
           !
        END DO
        !
        vtxcgc = vtxcgc + &
                 e2 * ( v1xup + v1cup ) * ( rhoout(k,1) - rho_core(k) * fac )
        vtxcgc = vtxcgc + &
                 e2 * ( v1xdw + v1cdw ) * ( rhoout(k,2) - rho_core(k) * fac )
        etxcgc = etxcgc + e2 * ( sx + sc )
        !
     END DO
!$omp end parallel do
     !
  END IF
  !
  DO is = 1, nspin0
     !
     rhoout(:,is) = rhoout(:,is) - fac * rho_core(:)
     !
  END DO
  !
  DEALLOCATE( grho )
  !
  ALLOCATE( dh( dfftp%nnr ) )    
  !
  ! ... second term of the gradient correction :
  ! ... \sum_alpha (D / D r_alpha) ( D(rho*Exc)/D(grad_alpha rho) )
  !
  DO is = 1, nspin0
     !
     CALL grad_dot( dfftp%nnr, h(1,1,is), ngm, g, nl, alat, dh )
     !
     v(:,is) = v(:,is) - dh(:)
     !
     vtxcgc = vtxcgc - SUM( dh(:) * rhoout(:,is) )
     !
  END DO
  !
  vtxc = vtxc + omega * vtxcgc / ( dfftp%nr1 * dfftp%nr2 * dfftp%nr3 )
  etxc = etxc + omega * etxcgc / ( dfftp%nr1 * dfftp%nr2 * dfftp%nr3 )

  IF (nspin==4.AND.domag) THEN
     DO is=1,nspin0
        vgg(:,is)=v(:,is)
     ENDDO
     v=vsave
     DO k=1,dfftp%nnr
        v(k,1)=v(k,1)+0.5d0*(vgg(k,1)+vgg(k,2))
        amag=sqrt(rho(k,2)**2+rho(k,3)**2+rho(k,4)**2)
        IF (amag.GT.1.d-12) THEN
           v(k,2)=v(k,2)+segni(k)*0.5d0*(vgg(k,1)-vgg(k,2))*rho(k,2)/amag
           v(k,3)=v(k,3)+segni(k)*0.5d0*(vgg(k,1)-vgg(k,2))*rho(k,3)/amag
           v(k,4)=v(k,4)+segni(k)*0.5d0*(vgg(k,1)-vgg(k,2))*rho(k,4)/amag
        ENDIF
     ENDDO
  ENDIF
  !
  DEALLOCATE( dh )
  DEALLOCATE( h )
  DEALLOCATE( rhoout )
  IF (nspin==4.and.domag) THEN
     DEALLOCATE( vgg )
     DEALLOCATE( vsave )
     DEALLOCATE( segni )
  ENDIF
  !
  RETURN
  !
END SUBROUTINE gradcorr
!
!----------------------------------------------------------------------------
SUBROUTINE gradrho( nrxx, a, ngm, g, nl, ga )
  !----------------------------------------------------------------------------
  !
  ! ... Calculates ga = \grad a in R-space (a is in G-space)
  !
  USE kinds,     ONLY : DP
  USE constants, ONLY : tpi
  USE cell_base, ONLY : tpiba
  USE gvect,     ONLY : nlm
  USE fft_base,      ONLY : dfftp
  USE fft_interfaces,ONLY : invfft

  !
  IMPLICIT NONE
  !
  INTEGER,     INTENT(IN)  :: nrxx
  INTEGER,     INTENT(IN)  :: ngm, nl(ngm)
  COMPLEX(DP), INTENT(IN)  :: a(ngm)
  REAL(DP),    INTENT(IN)  :: g(3,ngm)
  REAL(DP),    INTENT(OUT) :: ga(3,nrxx)
  !
  INTEGER                  :: ipol
  COMPLEX(DP), ALLOCATABLE :: gaux(:)
  !
  !
  ALLOCATE( gaux( nrxx ) )
  !
  ! ... multiply by (iG) to get (\grad_ipol a)(G) ...
  !
  ga(:,:) = 0.D0
  !
  DO ipol = 1, 3
     !
     gaux(:) = CMPLX(0.d0,0.d0,kind=dp)
     !
     gaux(nl(:)) = g(ipol,:) * CMPLX( -AIMAG( a(:) ), REAL( a(:) ) ,kind=DP)
     !
     !
     ! ... bring back to R-space, (\grad_ipol a)(r) ...
     !
     CALL invfft ('Dense', gaux, dfftp)
     !
     ! ...and add the factor 2\pi/a  missing in the definition of G
     !
     ga(ipol,:) = ga(ipol,:) + tpiba * REAL( gaux(:) )
     !
  END DO
  !
  DEALLOCATE( gaux )
  !
  RETURN
  !
END SUBROUTINE gradrho
!
!----------------------------------------------------------------------------
SUBROUTINE grad_dot( nrxx, a, ngm, g, nl, alat, da )
  !----------------------------------------------------------------------------
  !
  ! ... Calculates da = \sum_i \grad_i a_i in R-space
  !
  USE constants, ONLY : tpi
  USE cell_base, ONLY : tpiba
  USE kinds,     ONLY : DP
  USE gvect,     ONLY : nlm
!!$  USE control_flags, ONLY : gamma_only
  USE fft_base,      ONLY : dfftp
  USE fft_interfaces,ONLY : fwfft, invfft
  !
  IMPLICIT NONE
  !
  INTEGER,  INTENT(IN)     :: nrxx, ngm, nl(ngm)
  REAL(DP), INTENT(IN)     :: a(3,nrxx), g(3,ngm), alat
  REAL(DP), INTENT(OUT)    :: da(nrxx)
  !
  INTEGER                  :: n, ipol
  COMPLEX(DP), ALLOCATABLE :: aux(:), gaux(:)
  !
  !
  ALLOCATE( aux( nrxx ), gaux( nrxx ) )
  !
  gaux(:) = CMPLX(0.d0,0.d0, kind=dp)
  !
  DO ipol = 1, 3
     !
     aux = CMPLX( a(ipol,:), 0.D0 ,kind=DP)
     !
     ! ... bring a(ipol,r) to G-space, a(G) ...
     !
     CALL fwfft ('Dense', aux, dfftp)
     !
     DO n = 1, ngm
        !
        gaux(nl(n)) = gaux(nl(n)) + g(ipol,n) * &
                      CMPLX( -AIMAG( aux(nl(n)) ), REAL( aux(nl(n)) ) ,kind=DP)
        !
     END DO
    !
  END DO
  !
  !
  ! ... bring back to R-space, (\grad_ipol a)(r) ...
  !
  CALL invfft ('Dense', gaux, dfftp)
  !
  ! ... add the factor 2\pi/a  missing in the definition of G and sum
  !
  da(:) = tpiba * REAL( gaux(:) )
  !
  DEALLOCATE( aux, gaux )
  !
  RETURN
  !
END SUBROUTINE grad_dot
