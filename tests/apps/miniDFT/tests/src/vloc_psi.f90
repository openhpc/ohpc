!
! Copyright (C) 2003-2009 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
SUBROUTINE vloc_psi_gamma(lda, n, m, psi, v, hpsi)
  !-----------------------------------------------------------------------
  !
  ! Calculation of Vloc*psi using dual-space technique - Gamma point
  !
  USE parallel_include
  USE kinds,   ONLY : DP
  USE gvecs, ONLY : nls, nlsm
  USE wvfct,   ONLY : igk
  USE mp_global,     ONLY : me_pool, me_bgrp
  USE fft_base,      ONLY : dffts, tg_gather
  USE fft_interfaces,ONLY : fwfft, invfft
  USE wavefunctions_module,  ONLY: psic
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(in) :: lda, n, m
  COMPLEX(DP), INTENT(in)   :: psi (lda, m)
  COMPLEX(DP), INTENT(inout):: hpsi (lda, m)
  REAL(DP), INTENT(in) :: v(dffts%nnr)
  !
  INTEGER :: ibnd, j, incr
  COMPLEX(DP) :: fp, fm
  !
  LOGICAL :: use_tg
  ! Variables for task groups
  REAL(DP),    ALLOCATABLE :: tg_v(:)
  COMPLEX(DP), ALLOCATABLE :: tg_psic(:)
  INTEGER :: v_siz, idx, ioff
  !
  !
  incr = 2
  !
  ! The following is dirty trick to prevent usage of task groups if
  ! the number of bands is smaller than the number of task groups 
  ! 
  use_tg = dffts%have_task_groups
  dffts%have_task_groups  = dffts%have_task_groups .and. ( m >= dffts%nogrp )
  !
  IF( dffts%have_task_groups ) THEN
     !
     v_siz =  dffts%tg_nnr * dffts%nogrp
     !
     ALLOCATE( tg_v   ( v_siz ) )
     ALLOCATE( tg_psic( v_siz ) )
     !
     CALL tg_gather( dffts, v, tg_v )
     !
     incr = 2 * dffts%nogrp
     !
  ENDIF
  !
  ! the local potential V_Loc psi. First bring psi to real space
  !
  DO ibnd = 1, m, incr
     !
     IF( dffts%have_task_groups ) THEN
        !
        tg_psic = (0.d0, 0.d0)
        ioff   = 0
        !
        DO idx = 1, 2*dffts%nogrp, 2
           IF( idx + ibnd - 1 < m ) THEN
              DO j = 1, n
                 tg_psic(nls (igk(j))+ioff) =        psi(j,idx+ibnd-1) + &
                                      (0.0d0,1.d0) * psi(j,idx+ibnd)
                 tg_psic(nlsm(igk(j))+ioff) = conjg( psi(j,idx+ibnd-1) - &
                                      (0.0d0,1.d0) * psi(j,idx+ibnd) )
              ENDDO
           ELSEIF( idx + ibnd - 1 == m ) THEN
              DO j = 1, n
                 tg_psic(nls (igk(j))+ioff) =        psi(j,idx+ibnd-1)
                 tg_psic(nlsm(igk(j))+ioff) = conjg( psi(j,idx+ibnd-1) )
              ENDDO
           ENDIF

           ioff = ioff + dffts%tg_nnr

        ENDDO
        !
     ELSE
        !
        psic(:) = (0.d0, 0.d0)
        IF (ibnd < m) THEN
           ! two ffts at the same time
           DO j = 1, n
              psic(nls (igk(j)))=      psi(j,ibnd) + (0.0d0,1.d0)*psi(j,ibnd+1)
              psic(nlsm(igk(j)))=conjg(psi(j,ibnd) - (0.0d0,1.d0)*psi(j,ibnd+1))
           ENDDO
        ELSE
           DO j = 1, n
              psic (nls (igk(j))) =       psi(j, ibnd)
              psic (nlsm(igk(j))) = conjg(psi(j, ibnd))
           ENDDO
        ENDIF
        !
     ENDIF
     !
     !   fft to real space
     !   product with the potential v on the smooth grid
     !   back to reciprocal space
     !
     IF( dffts%have_task_groups ) THEN
        !
        CALL invfft ('Wave', tg_psic, dffts)
        !
        DO j = 1, dffts%nr1x*dffts%nr2x*dffts%tg_npp( me_bgrp + 1 )
           tg_psic (j) = tg_psic (j) * tg_v(j)
        ENDDO
        !
        CALL fwfft ('Wave', tg_psic, dffts)
        !
     ELSE
        !
        CALL invfft ('Wave', psic, dffts)
        !
        DO j = 1, dffts%nnr
           psic (j) = psic (j) * v(j)
        ENDDO
        !
        CALL fwfft ('Wave', psic, dffts)
        !
     ENDIF
     !
     !   addition to the total product
     !
     IF( dffts%have_task_groups ) THEN
        !
        ioff   = 0
        !
        DO idx = 1, 2*dffts%nogrp, 2
           !
           IF( idx + ibnd - 1 < m ) THEN
              DO j = 1, n
                 fp= ( tg_psic( nls(igk(j)) + ioff ) +  &
                       tg_psic( nlsm(igk(j)) + ioff ) ) * 0.5d0
                 fm= ( tg_psic( nls(igk(j)) + ioff ) -  &
                       tg_psic( nlsm(igk(j)) + ioff ) ) * 0.5d0
                 hpsi (j, ibnd+idx-1) = hpsi (j, ibnd+idx-1) + &
                                        cmplx( dble(fp), aimag(fm),kind=DP)
                 hpsi (j, ibnd+idx  ) = hpsi (j, ibnd+idx  ) + &
                                        cmplx(aimag(fp),- dble(fm),kind=DP)
              ENDDO
           ELSEIF( idx + ibnd - 1 == m ) THEN
              DO j = 1, n
                 hpsi (j, ibnd+idx-1) = hpsi (j, ibnd+idx-1) + &
                                         tg_psic( nls(igk(j)) + ioff )
              ENDDO
           ENDIF
           !
           ioff = ioff + dffts%nr3x * dffts%nsw( me_bgrp + 1 )
           !
        ENDDO
        !
     ELSE
        IF (ibnd < m) THEN
           ! two ffts at the same time
           DO j = 1, n
              fp = (psic (nls(igk(j))) + psic (nlsm(igk(j))))*0.5d0
              fm = (psic (nls(igk(j))) - psic (nlsm(igk(j))))*0.5d0
              hpsi (j, ibnd)   = hpsi (j, ibnd)   + &
                                 cmplx( dble(fp), aimag(fm),kind=DP)
              hpsi (j, ibnd+1) = hpsi (j, ibnd+1) + &
                                 cmplx(aimag(fp),- dble(fm),kind=DP)
           ENDDO
        ELSE
           DO j = 1, n
              hpsi (j, ibnd)   = hpsi (j, ibnd)   + psic (nls(igk(j)))
           ENDDO
        ENDIF
     ENDIF
     !
  ENDDO
  !
  IF( dffts%have_task_groups ) THEN
     !
     DEALLOCATE( tg_psic )
     DEALLOCATE( tg_v )
     !
  ENDIF
  dffts%have_task_groups = use_tg
  !
  RETURN
END SUBROUTINE vloc_psi_gamma
!
!-----------------------------------------------------------------------
SUBROUTINE vloc_psi_k(lda, n, m, psi, v, hpsi)
  !-----------------------------------------------------------------------
  !
  ! Calculation of Vloc*psi using dual-space technique - k-points
  !
  USE parallel_include
  USE kinds,   ONLY : DP
  USE gvecs, ONLY : nls, nlsm
  USE wvfct,   ONLY : igk
  USE mp_global,     ONLY : me_pool, me_bgrp
  USE fft_base,      ONLY : dffts, tg_gather
  USE fft_interfaces,ONLY : fwfft, invfft
  USE wavefunctions_module,  ONLY: psic
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(in) :: lda, n, m
  COMPLEX(DP), INTENT(in)   :: psi (lda, m)
  COMPLEX(DP), INTENT(inout):: hpsi (lda, m)
  REAL(DP), INTENT(in) :: v(dffts%nnr)
  !
  INTEGER :: ibnd, j, incr
  !
  LOGICAL :: use_tg
  ! Task Groups
  REAL(DP),    ALLOCATABLE :: tg_v(:)
  COMPLEX(DP), ALLOCATABLE :: tg_psic(:)
  INTEGER :: v_siz, idx, ioff
  !
  !
  ! The following is dirty trick to prevent usage of task groups if
  ! the number of bands is smaller than the number of task groups 
  ! 
  use_tg = dffts%have_task_groups
  dffts%have_task_groups  = dffts%have_task_groups .and. ( m >= dffts%nogrp )
  !
  incr = 1
  !
  IF( dffts%have_task_groups ) THEN
     !
     v_siz =  dffts%tg_nnr * dffts%nogrp
     !
     ALLOCATE( tg_v   ( v_siz ) )
     ALLOCATE( tg_psic( v_siz ) )
     !
     CALL tg_gather( dffts, v, tg_v )
     incr = dffts%nogrp
     !
  ENDIF
  !
  ! the local potential V_Loc psi. First bring psi to real space
  !
  DO ibnd = 1, m, incr
     !
     IF( dffts%have_task_groups ) THEN
        !
        tg_psic = (0.d0, 0.d0)
        ioff   = 0
        !
        DO idx = 1, dffts%nogrp

           IF( idx + ibnd - 1 <= m ) THEN
!$omp parallel do
              DO j = 1, n
                 tg_psic(nls (igk(j))+ioff) =  psi(j,idx+ibnd-1)
              ENDDO
!$omp end parallel do
           ENDIF

           ioff = ioff + dffts%tg_nnr

        ENDDO
        !
        CALL  invfft ('Wave', tg_psic, dffts)
        !
     ELSE
        !
        psic(:) = (0.d0, 0.d0)
        psic (nls (igk(1:n))) = psi(1:n, ibnd)
        !
        CALL invfft ('Wave', psic, dffts)
        !
     ENDIF
     !
     !   fft to real space
     !   product with the potential v on the smooth grid
     !   back to reciprocal space
     !
     IF( dffts%have_task_groups ) THEN
        !
!$omp parallel do
        DO j = 1, dffts%nr1x*dffts%nr2x*dffts%tg_npp( me_bgrp + 1 )
           tg_psic (j) = tg_psic (j) * tg_v(j)
        ENDDO
!$omp end parallel do
        !
        CALL fwfft ('Wave',  tg_psic, dffts)
        !
     ELSE
        !
!$omp parallel do
        DO j = 1, dffts%nnr
           psic (j) = psic (j) * v(j)
        ENDDO
!$omp end parallel do
        !
        CALL fwfft ('Wave', psic, dffts)
        !
     ENDIF
     !
     !   addition to the total product
     !
     IF( dffts%have_task_groups ) THEN
        !
        ioff   = 0
        !
        DO idx = 1, dffts%nogrp
           !
           IF( idx + ibnd - 1 <= m ) THEN
!$omp parallel do
              DO j = 1, n
                 hpsi (j, ibnd+idx-1) = hpsi (j, ibnd+idx-1) + tg_psic( nls(igk(j)) + ioff )
              ENDDO
!$omp end parallel do
           ENDIF
           !
           ioff = ioff + dffts%nr3x * dffts%nsw( me_bgrp + 1 )
           !
        ENDDO
        !
     ELSE
!$omp parallel do
        DO j = 1, n
           hpsi (j, ibnd)   = hpsi (j, ibnd)   + psic (nls(igk(j)))
        ENDDO
!$omp end parallel do
     ENDIF
     !
  ENDDO
  !
  IF( dffts%have_task_groups ) THEN
     !
     DEALLOCATE( tg_psic )
     DEALLOCATE( tg_v )
     !
  ENDIF
  dffts%have_task_groups = use_tg
  !
  RETURN
END SUBROUTINE vloc_psi_k
