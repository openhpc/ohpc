!
! Copyright (C) 2002-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
#define ZERO ( 0._dp, 0._dp )
!
! This macro force the normalization of betamix matrix, usually not necessary
!#define __NORMALIZE_BETAMIX
!
#ifdef __GFORTRAN
! gfortran hack - for some mysterious reason gfortran doesn't save
!                 derived-type variables even with the SAVE attribute
MODULE mix_save
  USE scf, ONLY : mix_type
  TYPE(mix_type), ALLOCATABLE, SAVE :: &
    df(:),        &! information from preceding iterations
    dv(:)          !     "  "       "     "        "  "
END MODULE mix_save
#endif

!----------------------------------------------------------------------------
SUBROUTINE mix_rho( input_rhout, rhoin, alphamix, dr2, tr2_min, iter, n_iter, conv )
  !----------------------------------------------------------------------------
  !
  ! ... Modified Broyden's method for charge density mixing
  ! ...         D.D. Johnson PRB 38, 12807 (1988)
  !
  ! ... On output: the mixed density is in rhoin, mixed augmentation
  ! ...            channel occ. is in becin
  !                input_rhocout, input_becout etc are unchanged
  !
  USE kinds,          ONLY : DP
  USE ions_base,      ONLY : nat
  USE gvect,          ONLY : ngm
  USE gvecs,        ONLY : ngms
  USE lsda_mod,       ONLY : nspin
  USE control_flags,  ONLY : imix, ngm0, tr2, io_level
  ! ... for PAW:
  USE uspp_param,     ONLY : nhm
  USE scf,            ONLY : scf_type, create_scf_type, destroy_scf_type, &
                             mix_type, create_mix_type, destroy_mix_type, &
                             assign_scf_to_mix_type, assign_mix_to_scf_type, &
                             mix_type_AXPY, diropn_mix_file, close_mix_file, &
                             davcio_mix_type, rho_ddot, high_frequency_mixing, &
                             mix_type_COPY, mix_type_SCAL
  USE io_global,     ONLY : stdout
#ifdef __GFORTRAN
  USE mix_save
#endif
  !
  IMPLICIT NONE
  integer :: kilobytes
  !
  ! ... First the I/O variable
  !
  INTEGER, INTENT(IN) :: &
    iter,        &!  counter of the number of iterations
    n_iter        !  numb. of iterations used in mixing
  REAL(DP), INTENT(IN) :: &
    alphamix,    &! mixing factor
    tr2_min       ! estimated error in diagonalization. If the estimated
                  ! scf error is smaller than this, exit: a more accurate 
                  ! diagonalization is needed
  REAL(DP), INTENT(OUT) :: &
    dr2           ! the estimated errr on the energy
  LOGICAL, INTENT(OUT) :: &
    conv          ! .true. if the convergence has been reached

  type(scf_type), intent(in)    :: input_rhout
  type(scf_type), intent(inout) :: rhoin
  !
  ! ... Here the local variables
  !
  type(mix_type) :: rhout_m, rhoin_m
  INTEGER, PARAMETER :: &
    maxmix = 25             ! max number of iterations for charge mixing
  INTEGER ::    &
    iunmix,        &! I/O unit number of charge density file in G-space
    iunmix_paw,    &! I/O unit number of PAW file
    iter_used,     &! actual number of iterations used
    ipos,          &! index of the present iteration
    inext,         &! index of the next iteration
    i, j,          &! counters on number of iterations
    info,          &! flag saying if the exec. of libr. routines was ok
    ldim            ! 2 * Hubbard_lmax + 1
  type(mix_type) :: rhoin_save, rhout_save
  REAL(DP),ALLOCATABLE :: betamix(:,:), work(:)
  INTEGER, ALLOCATABLE :: iwork(:)
  REAL(DP) :: gamma0
#ifdef __NORMALIZE_BETAMIX
  REAL(DP) :: norm2, obn
#endif
  LOGICAL :: &
    savetofile,  &! save intermediate steps on file $prefix."mix",...
    exst          ! if true the file exists
  !
  ! ... saved variables and arrays
  !
  INTEGER, SAVE :: &
    mixrho_iter = 0    ! history of mixing
#ifndef __GFORTRAN
  TYPE(mix_type), ALLOCATABLE, SAVE :: &
    df(:),        &! information from preceding iterations
    dv(:)          !     "  "       "     "        "  "
#endif
  REAL(DP) :: dr2_paw, norm
!  REAL(DP),ALLOCATABLE :: e(:),v(:,:)
  INTEGER, PARAMETER :: read_ = -1, write_ = +1
  !
  ! ... external functions
  !
  INTEGER, EXTERNAL :: find_free_unit
  !
  CALL start_clock( 'mix_rho' )
  !
  !
  ngm0 = ngms
  !
  mixrho_iter = iter
  !
  IF ( n_iter > maxmix ) CALL errore( 'mix_rho', 'n_iter too big', 1 )
  !
  savetofile = (io_level > 1)
  !
  ! define rhocout variables and copy input_rhocout in there
  !
  call create_mix_type(rhout_m)
  call create_mix_type(rhoin_m)
  !
  call assign_scf_to_mix_type(rhoin, rhoin_m)
  call assign_scf_to_mix_type(input_rhout, rhout_m)

  call mix_type_AXPY ( -1.d0, rhoin_m, rhout_m )
  !
  dr2 = rho_ddot( rhout_m, rhout_m, ngms )  !!!! this used to be ngm NOT ngms
  !
  IF (dr2 < 0.0_DP) CALL errore('mix_rho','negative dr2',1)
  !
  conv = ( dr2 < tr2 )
  !
  IF ( conv .OR. dr2 < tr2_min ) THEN
     !
     ! ... if convergence is achieved or if the self-consistency error (dr2) is
     ! ... smaller than the estimated error due to diagonalization (tr2_min),
     ! ... exit and leave rhoin and rhocout unchanged
     !
     IF ( ALLOCATED( df ) ) THEN
         DO i=1, n_iter
            call destroy_mix_type(df(i))
         END DO
         DEALLOCATE( df )
     END IF
     IF ( ALLOCATED( dv ) ) THEN
         DO i=1, n_iter
            call destroy_mix_type(dv(i))
         END DO
         DEALLOCATE( dv )
     END IF
     !
     call destroy_mix_type(rhoin_m)
     call destroy_mix_type(rhout_m)
 
     CALL stop_clock( 'mix_rho' )
     !
     RETURN
     !
  END IF
  !
  IF ( savetofile ) THEN
     !
     iunmix = find_free_unit()
     CALL diropn_mix_file( iunmix, 'mix', exst )
     !
     IF ( mixrho_iter > 1 .AND. .NOT. exst ) THEN
        !
        CALL infomsg( 'mix_rho', 'file not found, restarting' )
        mixrho_iter = 1
        !
     END IF
     !
  END IF
  !
  IF ( savetofile .OR. mixrho_iter == 1 ) THEN
     !
     IF ( .NOT. ALLOCATED( df ) ) THEN
        ALLOCATE( df( n_iter ) )
        DO i=1,n_iter
           CALL create_mix_type( df(i) )
        END DO
     END IF
     IF ( .NOT. ALLOCATED( dv ) ) THEN
        ALLOCATE( dv( n_iter ) )
        DO i=1,n_iter
           CALL create_mix_type( dv(i) )
        END DO
     END IF
     !
  END IF
  !
  ! ... iter_used = mixrho_iter-1  if  mixrho_iter <= n_iter
  ! ... iter_used = n_iter         if  mixrho_iter >  n_iter
  !
  iter_used = MIN( ( mixrho_iter - 1 ), n_iter )
  !
  ! ... ipos is the position in which results from the present iteration
  ! ... are stored. ipos=mixrho_iter-1 until ipos=n_iter, then back to 1,2,...
  !
  ipos = mixrho_iter - 1 - ( ( mixrho_iter - 2 ) / n_iter ) * n_iter
  !
  IF ( mixrho_iter > 1 ) THEN
     !
     IF ( savetofile ) THEN
        !
        CALL davcio_mix_type( df(ipos), iunmix, 1, read_ )
        CALL davcio_mix_type( dv(ipos), iunmix, 2, read_ )
        !
     END IF
     !
     call mix_type_AXPY ( -1.d0, rhout_m, df(ipos) )
     call mix_type_AXPY ( -1.d0, rhoin_m, dv(ipos) )
#ifdef __NORMALIZE_BETAMIX
     ! NORMALIZE
     norm2 = rho_ddot( df(ipos), df(ipos), ngm0 )
     obn = 1.d0/sqrt(norm2)
     call mix_type_SCAL (obn,df(ipos))
     call mix_type_SCAL (obn,dv(ipos))
#endif
     !
  END IF
  !
  IF ( savetofile ) THEN
     !
     DO i = 1, iter_used
        !
        IF ( i /= ipos ) THEN
           !
           CALL davcio_mix_type( df(i), iunmix, 2*i+1, read_ )
           CALL davcio_mix_type( dv(i), iunmix, 2*i+2, read_ )
        END IF
        !
     END DO
     !
     CALL davcio_mix_type( rhout_m, iunmix, 1, write_ )
     CALL davcio_mix_type( rhoin_m, iunmix, 2, write_ )
     !
     IF ( mixrho_iter > 1 ) THEN
        CALL davcio_mix_type( df(ipos), iunmix, 2*ipos+1, write_ )
        CALL davcio_mix_type( dv(ipos), iunmix, 2*ipos+2, write_ )
     END IF
     !
  ELSE
     !
     call create_mix_type (rhoin_save)
     call create_mix_type (rhout_save)
     !
     call mix_type_COPY( rhoin_m, rhoin_save )
     call mix_type_COPY( rhout_m, rhout_save )
     !
  END IF
  ! Nothing else to do on first iteration
  skip_on_first: &
  IF (iter_used > 0) THEN
    !
    ALLOCATE(betamix(iter_used, iter_used)) !iter_used))
    betamix = 0._dp
    !
    DO i = 1, iter_used
        !
        DO j = i, iter_used
            !
            betamix(i,j) = rho_ddot( df(j), df(i), ngm0 )
            betamix(j,i) = betamix(i,j)
            !
        END DO
        !
    END DO
    !
    !   allocate(e(iter_used), v(iter_used, iter_used))
    !   CALL rdiagh(iter_used, betamix, iter_used, e, v)
    !   write(*,'(1e11.3)') e(:)
    !   write(*,*)
    !   deallocate(e,v)
    allocate(work(iter_used), iwork(iter_used))
    !write(*,*) betamix(:,:)
    CALL DSYTRF( 'U', iter_used, betamix, iter_used, iwork, work, iter_used, info )
    CALL errore( 'broyden', 'factorization', abs(info) )
    !
    CALL DSYTRI( 'U', iter_used, betamix, iter_used, iwork, work, info )
    CALL errore( 'broyden', 'DSYTRI', abs(info) )    !
    deallocate(iwork)
    !
    FORALL( i = 1:iter_used, &
            j = 1:iter_used, j > i ) betamix(j,i) = betamix(i,j)
    !
    DO i = 1, iter_used
        !
        work(i) = rho_ddot( df(i), rhout_m, ngm0 )
        !
    END DO
    !
    DO i = 1, iter_used
        !
        gamma0 = DOT_PRODUCT( betamix(1:iter_used,i), work(1:iter_used) )
        !
        call mix_type_AXPY ( -gamma0, dv(i), rhoin_m )
        call mix_type_AXPY ( -gamma0, df(i), rhout_m )
        !
    END DO
    DEALLOCATE(betamix, work)
    !
    ! ... auxiliary vectors dv and df not needed anymore
    !
  ENDIF skip_on_first
  !
  IF ( savetofile ) THEN
     !
     call close_mix_file( iunmix )
     !
     IF ( ALLOCATED( df ) ) THEN
         DO i=1, n_iter
            call destroy_mix_type(df(i))
         END DO
         DEALLOCATE( df )
     END IF
     IF ( ALLOCATED( dv ) ) THEN
         DO i=1, n_iter
            call destroy_mix_type(dv(i))
         END DO
         DEALLOCATE( dv )
     END IF
     !
  ELSE
     !
     inext = mixrho_iter - ( ( mixrho_iter - 1 ) / n_iter ) * n_iter
     !
     call mix_type_COPY( rhout_save, df(inext) )
     call mix_type_COPY( rhoin_save, dv(inext) )
     !
     call destroy_mix_type( rhoin_save )
     call destroy_mix_type( rhout_save )
     !
  END IF
  !
  ! ... preconditioning the new search direction
  !
  IF ( imix == 1 ) THEN
     !
     CALL approx_screening( rhout_m )
     !
  ELSE IF ( imix == 2 ) THEN
     !
     CALL approx_screening2( rhout_m, rhoin_m )
     !
  END IF
  !
  ! ... set new trial density
  !
  call mix_type_AXPY ( alphamix, rhout_m, rhoin_m )
  ! ... simple mixing for high_frequencies (and set to zero the smooth ones)
  call high_frequency_mixing ( rhoin, input_rhout, alphamix )
  ! ... add the mixed rho for the smooth frequencies
  call assign_mix_to_scf_type(rhoin_m,rhoin)
  !
  call destroy_mix_type(rhout_m)
  call destroy_mix_type(rhoin_m)

  CALL stop_clock( 'mix_rho' )
  !
  RETURN
  !
END SUBROUTINE mix_rho
!
!----------------------------------------------------------------------------
SUBROUTINE approx_screening( drho )
  !----------------------------------------------------------------------------
  !
  ! ... apply an average TF preconditioning to drho
  !
  USE kinds,         ONLY : DP
  USE constants,     ONLY : e2, pi, fpi
  USE cell_base,     ONLY : omega, tpiba2
  USE gvect,         ONLY : gg, ngm, nl, nlm
  USE klist,         ONLY : nelec
  USE lsda_mod,      ONLY : nspin
  USE control_flags, ONLY : ngm0
  USE scf,           ONLY : mix_type
  USE wavefunctions_module, ONLY : psic
  !
  IMPLICIT NONE  
  !
  type (mix_type), intent(INOUT) :: drho ! (in/out)
  !
  REAL(DP) :: rrho, rmag, rs, agg0
  INTEGER  :: ig, is
  !
  rs = ( 3.D0 * omega / fpi / nelec )**( 1.D0 / 3.D0 )
  !
  agg0 = ( 12.D0 / pi )**( 2.D0 / 3.D0 ) / tpiba2 / rs
  !
  IF ( nspin == 1 .OR. nspin == 4 ) THEN
     !
     drho%of_g(:ngm0,1) =  drho%of_g(:ngm0,1) * gg(:ngm0) / (gg(:ngm0)+agg0)
     !
  ELSE IF ( nspin == 2 ) THEN
     !
     DO ig = 1, ngm0
        !
        rrho = ( drho%of_g(ig,1) + drho%of_g(ig,2) ) * gg(ig) / (gg(ig)+agg0)
        rmag = ( drho%of_g(ig,1) - drho%of_g(ig,2) )
        !
        drho%of_g(ig,1) =  0.5D0*( rrho + rmag )
        drho%of_g(ig,2) =  0.5D0*( rrho - rmag )
        !
     END DO
     !
  END IF
  !
  RETURN
  !
END SUBROUTINE approx_screening
!
!----------------------------------------------------------------------------
SUBROUTINE approx_screening2( drho, rhobest )
  !----------------------------------------------------------------------------
  !
  ! ... apply a local-density dependent TF preconditioning to drho
  !
  USE kinds,                ONLY : DP
  USE constants,            ONLY : e2, pi, tpi, fpi, eps8, eps32
  USE cell_base,            ONLY : omega, tpiba2
  USE gvecs,              ONLY : nls, nlsm
  USE gvect,                ONLY : gg, ngm, nl, nlm
  USE wavefunctions_module, ONLY : psic
  USE klist,                ONLY : nelec
  USE lsda_mod,             ONLY : nspin
  USE control_flags,        ONLY : ngm0 
  USE scf,                  ONLY : mix_type, local_tf_ddot
  USE mp,                   ONLY : mp_max, mp_min, mp_sum
  USE mp_global,            ONLY : intra_image_comm, intra_bgrp_comm
  USE fft_base,             ONLY : dffts
  USE fft_interfaces,       ONLY : fwfft, invfft
  !
  IMPLICIT NONE
  !
  type(mix_type), intent(inout) :: drho
  type(mix_type), intent(in) :: rhobest
  !
  INTEGER, PARAMETER :: mmx = 12
  !
  INTEGER :: &
    iwork(mmx), i, j, m, info, is
  REAL(DP) :: &
    rs, min_rs, max_rs, avg_rsm1, target, dr2_best
  REAL(DP) :: &
    aa(mmx,mmx), invaa(mmx,mmx), bb(mmx), work(mmx), vec(mmx), agg0
  COMPLEX(DP), ALLOCATABLE :: &
    v(:,:),     &! v(ngm0,mmx)
    w(:,:),     &! w(ngm0,mmx)
    dv(:),      &! dv(ngm0)
    vbest(:),   &! vbest(ngm0)
    wbest(:)     ! wbest(ngm0)
  REAL(DP), ALLOCATABLE :: &
    alpha(:)     ! alpha(dffts%nnr)
  !
  COMPLEX(DP)         :: rrho, rmag
  INTEGER             :: ir, ig
  REAL(DP), PARAMETER :: one_third = 1.D0 / 3.D0
  !
  !
  IF ( nspin == 2 ) THEN
     !
     DO ig = 1, ngm0
        !
        rrho = drho%of_g(ig,1) + drho%of_g(ig,2)
        rmag = drho%of_g(ig,1) - drho%of_g(ig,2)
        !        
        drho%of_g(ig,1) = rrho
        drho%of_g(ig,2) = rmag
        !
     END DO
     !
  END IF
  !
  target = 0.D0
  !
  IF ( gg(1) < eps8 ) drho%of_g(1,1) = ZERO
  !
  ALLOCATE( alpha( dffts%nnr ) )
  ALLOCATE( v( ngm0, mmx ), &
            w( ngm0, mmx ), dv( ngm0 ), vbest( ngm0 ), wbest( ngm0 ) )
  !
  v(:,:)   = ZERO
  w(:,:)   = ZERO
  dv(:)    = ZERO
  vbest(:) = ZERO
  wbest(:) = ZERO
  !
  ! ... calculate alpha from density
  !
  psic(:) = ZERO
  !
  IF ( nspin == 2 ) THEN
     !
     psic(nls(:ngm0)) = ( rhobest%of_g(:ngm0,1) + rhobest%of_g(:ngm0,2) )
     !
  ELSE
     !
     psic(nls(:ngm0)) = rhobest%of_g(:ngm0,1)
     !
  END IF
  !
  !
  CALL invfft ('Smooth', psic, dffts)
  !
  alpha(:) = REAL( psic(1:dffts%nnr) )
  !
  min_rs   = ( 3.D0 * omega / fpi / nelec )**one_third
  max_rs   = min_rs
  avg_rsm1 = 0.D0
  !
  DO ir = 1, dffts%nnr
     !
     alpha(ir) = ABS( alpha(ir) )
     !
     IF ( alpha(ir) > eps32 ) THEN
        !
        rs        = ( 3.D0 / fpi / alpha(ir) )**one_third
        min_rs    = MIN( min_rs, rs )
        avg_rsm1  = avg_rsm1 + 1.D0 / rs
        max_rs    = MAX( max_rs, rs )
        alpha(ir) = rs
        !
     END IF   
     !
  END DO
  !
  CALL mp_sum( avg_rsm1 , intra_bgrp_comm )
  !
  CALL mp_min( min_rs, intra_image_comm )
  CALL mp_max( max_rs, intra_image_comm )
  !
  alpha = 3.D0 * ( tpi / 3.D0 )**( 5.D0 / 3.D0 ) * alpha
  !
  avg_rsm1 = ( dffts%nr1*dffts%nr2*dffts%nr3 ) / avg_rsm1
  rs       = ( 3.D0 * omega / fpi / nelec )**one_third
  agg0     = ( 12.D0 / pi )**( 2.D0 / 3.D0 ) / tpiba2 / avg_rsm1
  !
  ! ... calculate deltaV and the first correction vector
  !
  psic(:) = ZERO
  !
  psic(nls(:ngm0)) = drho%of_g(:ngm0,1)
  !
  !
  CALL invfft ('Smooth', psic, dffts)
  !
  psic(:dffts%nnr) = psic(:dffts%nnr) * alpha(:)
  !
  CALL fwfft ('Smooth', psic, dffts)
  !
  dv(:) = psic(nls(:ngm0)) * gg(:ngm0) * tpiba2
  v(:,1)= psic(nls(:ngm0)) * gg(:ngm0) / ( gg(:ngm0) + agg0 )
  !
  m       = 1
  aa(:,:) = 0.D0
  bb(:)   = 0.D0
  !
  repeat_loop: DO
     !
     ! ... generate the vector w
     !     
     w(:,m) = fpi * e2 * v(:,m)
     !
     psic(:) = ZERO
     !
     psic(nls(:ngm0)) = v(:,m)
     !
     !
     CALL invfft ('Smooth', psic, dffts)
     !
     psic(:dffts%nnr) = psic(:dffts%nnr) * alpha(:)
     !
     CALL fwfft ('Smooth', psic, dffts)
     !
     w(:,m) = w(:,m) + gg(:ngm0) * tpiba2 * psic(nls(:ngm0))
     !
     ! ... build the linear system
     !
     DO i = 1, m
        !
        aa(i,m) = local_tf_ddot( w(1,i), w(1,m), ngm0)
        !
        aa(m,i) = aa(i,m)
        !
     END DO
     !
     bb(m) = local_tf_ddot( w(1,m), dv, ngm0)
     !
     ! ... solve it -> vec
     !
     invaa = aa
     !
     CALL DSYTRF( 'U', m, invaa, mmx, iwork, work, mmx, info )
     CALL errore( 'broyden', 'factorization', info )
     !
     CALL DSYTRI( 'U', m, invaa, mmx, iwork, work, info )
     CALL errore( 'broyden', 'DSYTRI', info )
     !     
     FORALL( i = 1:m, j = 1:m, j > i ) invaa(j,i) = invaa(i,j)
     !
     FORALL( i = 1:m ) vec(i) = SUM( invaa(i,:)*bb(:) )
     !
     vbest(:) = ZERO
     wbest(:) = dv(:)
     !
     DO i = 1, m
        !
        vbest = vbest + vec(i) * v(:,i)
        wbest = wbest - vec(i) * w(:,i)
        !
     END DO
     !
     dr2_best = local_tf_ddot( wbest, wbest, ngm0 )
     !
     IF ( target == 0.D0 ) target = 1.D-6 * dr2_best
     !
     IF ( dr2_best < target ) THEN
        !
        drho%of_g(:ngm0,1) = vbest(:)
        !
        IF ( nspin == 2 ) THEN
           !
           DO ig = 1, ngm0
              !
              rrho = drho%of_g(ig,1)
              rmag = drho%of_g(ig,2)
              !
              drho%of_g(ig,1) = 0.5D0 * ( rrho + rmag )
              drho%of_g(ig,2) = 0.5D0 * ( rrho - rmag )
              !
           END DO
           !
        END IF
        !
        DEALLOCATE( alpha, v, w, dv, vbest, wbest )
        !
        EXIT repeat_loop
        !
     ELSE IF ( m >= mmx ) THEN
        !
        m = 1
        !
        v(:,m)  = vbest(:)
        aa(:,:) = 0.D0
        bb(:)   = 0.D0
        !
        CYCLE repeat_loop
        !
     END IF
     !
     m = m + 1
     !
     v(:,m) = wbest(:) / ( gg(:ngm0) + agg0 )
     !
  END DO repeat_loop
  !
  RETURN
  !
END SUBROUTINE approx_screening2
