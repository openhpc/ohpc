!
! Copyright (C) 2001-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE sum_band()
  !----------------------------------------------------------------------------
  !
  ! ... calculates the symmetrized charge density and sum of occupied
  ! ... eigenvalues.
  ! ... this version works also for metals (gaussian spreading technique)
  !
  USE kinds,                ONLY : DP
  USE ener,                 ONLY : eband
  USE control_flags,        ONLY : diago_full_acc, tqr
  USE cell_base,            ONLY : at, bg, omega, tpiba
  USE ions_base,            ONLY : nat, ntyp => nsp, ityp
  USE fft_base,             ONLY : dfftp, dffts
  USE fft_interfaces,       ONLY : fwfft, invfft
  USE gvect,                ONLY : ngm, g, nl, nlm
  USE gvecs,              ONLY : nls, nlsm, doublegrid
  USE klist,                ONLY : nks, nkstot, wk, xk, ngk
  USE fixed_occ,            ONLY : one_atom_occupations
  USE lsda_mod,             ONLY : lsda, nspin, current_spin, isk
  USE scf,                  ONLY : rho
  USE symme,                ONLY : sym_rho
  USE io_files,             ONLY : iunwfc, nwordwfc, iunigk
  USE buffers,              ONLY : get_buffer
  USE uspp,                 ONLY : nkb, vkb, becsum, nhtol, nhtoj, indv, okvan
  USE uspp_param,           ONLY : upf, nh, nhm
  USE wavefunctions_module, ONLY : evc, psic, psic_nc
  USE spin_orb,             ONLY : lspinorb, domag, fcoef
  USE wvfct,                ONLY : nbnd, npwx, npw, igk, wg, et, btype
  USE mp_global,            ONLY : inter_pool_comm, intra_bgrp_comm
  USE mp,                   ONLY : mp_bcast, mp_sum
  USE becmod,               ONLY : allocate_bec_type, deallocate_bec_type, &
                                   bec_type, becp
  USE wvfct,                ONLY: nbnd
  !
  IMPLICIT NONE
  integer, parameter :: nspin_mag=1 !substitute for noncollin_module
  ! ... local variables
  !
  INTEGER :: ikb, jkb, ijkb0, ih, jh, ijh, na, np
    ! counters on beta functions, atoms, pseudopotentials
  INTEGER :: ir, is, ig, ibnd, ik
    ! counter on 3D r points
    ! counter on spin polarizations
    ! counter on g vectors
    ! counter on bands
    ! counter on k points
  REAL (DP), ALLOCATABLE :: kplusg (:)
  !
  !
  CALL start_clock( 'sum_band' )
  !
  becsum(:,:,:) = 0.D0
  rho%of_r(:,:)      = 0.D0
  rho%of_g(:,:)      = 0.D0
  eband         = 0.D0

  !
  ! ... calculates weights of Kohn-Sham orbitals used in calculation of rho
  !
  CALL weights ( )
  !
  !
  IF ( diago_full_acc ) THEN
     !
     ! ... for diagonalization purposes all the bands are considered occupied
     !
     btype(:,:) = 1
     !
  ELSE
     !
     ! ... for diagonalization purposes a band is considered empty when its
     ! ... occupation is less than 1.0 %
     !
     btype(:,:) = 1
     !
     FORALL( ik = 1:nks, wk(ik) > 0.D0 )
        !
        WHERE( wg(:,ik) / wk(ik) < 0.01D0 ) btype(:,ik) = 0
        !
     END FORALL
     !
  END IF
  IF ( one_atom_occupations ) CALL allocate_bec_type (nkb,nbnd, becp,intra_bgrp_comm)
  !
  ! ... specific routines are called to sum for each k point the contribution
  ! ... of the wavefunctions to the charge
  !
     !
     CALL sum_band_k()
     !
  !
  IF ( one_atom_occupations ) CALL deallocate_bec_type ( becp )
  !
  ! ... If a double grid is used, interpolate onto the fine grid
  !
  IF ( doublegrid ) THEN
     !
     DO is = 1, nspin
        !
        CALL interpolate( rho%of_r(1,is), rho%of_r(1,is), 1 )
        !
     END DO
     !
  END IF
  !
  CALL mp_sum( eband, inter_pool_comm )
  !
  !
  ! ... reduce charge density across pools
  !
  CALL mp_sum( rho%of_r, inter_pool_comm )
  !
  ! ... bring the (unsymmetrized) rho(r) to G-space (use psic as work array)
  !
  DO is = 1, nspin
     psic(:) = rho%of_r(:,is)
     CALL fwfft ('Dense', psic, dfftp)
     rho%of_g(:,is) = psic(nl(:))
  END DO
  !
  ! ... symmetrize rho(G) 
  !
  CALL sym_rho ( nspin_mag, rho%of_g )
  !
  ! ... synchronize rho%of_r to the calculated rho%of_g (use psic as work array)
  !
  DO is = 1, nspin_mag
     !
     psic(:) = ( 0.D0, 0.D0 )
     psic(nl(:)) = rho%of_g(:,is)
     CALL invfft ('Dense', psic, dfftp)
     rho%of_r(:,is) = psic(:)
     !
  END DO
  !
  CALL stop_clock( 'sum_band' )
  !
  RETURN
  !
  CONTAINS
     !
     ! ... internal procedures
     !
     !-----------------------------------------------------------------------
     !
     !
     !-----------------------------------------------------------------------
     SUBROUTINE sum_band_k()
       !-----------------------------------------------------------------------
       !
       ! ... k-points version
       !
       USE becmod, ONLY : bec_type, becp, calbec
       USE mp_global,     ONLY : me_pool
       USE mp,            ONLY : mp_sum
       !
       IMPLICIT NONE
       !
       ! ... local variables
       !
       REAL(DP) :: w1
       ! weights
       COMPLEX(DP), ALLOCATABLE :: becsum_nc(:,:,:,:)
       !
       INTEGER :: ipol, js
       !
       INTEGER  :: idx, ioff, incr, v_siz, j
       COMPLEX(DP), ALLOCATABLE :: tg_psi(:)
       REAL(DP),    ALLOCATABLE :: tg_rho(:)
       LOGICAL  :: use_tg
#ifdef __OPENMP
       INTEGER :: mytid, ntids, omp_get_thread_num, omp_get_num_threads, icnt
#endif
       !
       !
       ! ... here we sum for each k point the contribution
       ! ... of the wavefunctions to the charge
       !
#ifdef __IGKIO
       IF ( nks > 1 ) REWIND( iunigk )
#endif
       !
       use_tg = dffts%have_task_groups
       dffts%have_task_groups = ( dffts%have_task_groups ) .AND. ( nbnd >= dffts%nogrp )
       !
       incr = 1
       !
       IF( dffts%have_task_groups ) THEN
          !
          v_siz = dffts%tg_nnr * dffts%nogrp
          !
          ALLOCATE( tg_psi( v_siz ) )
          ALLOCATE( tg_rho( v_siz ) )
          !
          incr  = dffts%nogrp
          !
       END IF
       !
       k_loop: DO ik = 1, nks
          !
          IF( dffts%have_task_groups ) tg_rho = 0.0_DP

          IF ( lsda ) current_spin = isk(ik)
          npw = ngk (ik)
          !
          IF ( nks > 1 ) THEN
             !
#ifdef __IGKIO
             READ( iunigk ) igk 
             CALL get_buffer ( evc, nwordwfc, iunwfc, ik )
#endif __IGKIO
             !
          END IF
          !
          IF ( nkb > 0 ) &
             CALL init_us_2( npw, igk, xk(1,ik), vkb )
          !
          ! ... here we compute the band energy: the sum of the eigenvalues
          !
          DO ibnd = 1, nbnd, incr
             !
             IF( dffts%have_task_groups ) THEN
                DO idx = 1, dffts%nogrp
                   IF( idx + ibnd - 1 <= nbnd ) eband = eband + et( idx + ibnd - 1, ik ) * wg( idx + ibnd - 1, ik )
                END DO
             ELSE
                eband = eband + et( ibnd, ik ) * wg( ibnd, ik )
             END IF
             !
             ! ... the sum of eband and demet is the integral for e < ef of
             ! ... e n(e) which reduces for degauss=0 to the sum of the
             ! ... eigenvalues
             w1 = wg(ibnd,ik) / omega
             !
                !
                IF( dffts%have_task_groups ) THEN
                   !
!$omp parallel default(shared), private(j,ioff,idx)
!$omp do
                   DO j = 1, SIZE( tg_psi )
                      tg_psi(j) = ( 0.D0, 0.D0 )
                   END DO
!$omp end do
                   !
                   ioff   = 0
                   !
                   DO idx = 1, dffts%nogrp
                      !
                      ! ... dffts%nogrp ffts at the same time
                      !
                      IF( idx + ibnd - 1 <= nbnd ) THEN
!$omp do
                         DO j = 1, npw
                            tg_psi( nls( igk( j ) ) + ioff ) = evc( j, idx+ibnd-1 )
                         END DO
!$omp end do
                      END IF

                      ioff = ioff + dffts%tg_nnr

                   END DO
!$omp end parallel
                   !
                   CALL invfft ('Wave', tg_psi, dffts)
                   !
                   ! Now the first proc of the group holds the first band
                   ! of the dffts%nogrp bands that we are processing at the same time,
                   ! the second proc. holds the second and so on
                   !
                   ! Compute the proper factor for each band
                   !
                   DO idx = 1, dffts%nogrp
                      IF( dffts%nolist( idx ) == me_pool ) EXIT
                   END DO
                   !
                   ! Remember
                   ! proc 0 has bands ibnd
                   ! proc 1 has bands ibnd+1
                   ! ....
                   !
                   IF( idx + ibnd - 1 <= nbnd ) THEN
                      w1 = wg( idx + ibnd - 1, ik) / omega
                   ELSE
                      w1 = 0.0d0
                   END IF
                   !
                   CALL get_rho(tg_rho, dffts%tg_npp( me_pool + 1 ) * dffts%nr1x * dffts%nr2x, w1, tg_psi)
                   !
                ELSE
                   !
                   psic(:) = ( 0.D0, 0.D0 )
                   !
                   psic(nls(igk(1:npw))) = evc(1:npw,ibnd)
                   !
                   CALL invfft ('Wave', psic, dffts)
                   !
                   ! ... increment the charge density ...
                   !
                   CALL get_rho(rho%of_r(:,current_spin), dffts%nnr, w1, psic)

                END IF
                !
             !
          END DO
          !
          IF( dffts%have_task_groups ) THEN
             !
             ! reduce the group charge
             !
             CALL mp_sum( tg_rho, gid = dffts%ogrp_comm )
             !
             ioff = 0
             DO idx = 1, dffts%nogrp
                IF( me_pool == dffts%nolist( idx ) ) EXIT
                ioff = ioff + dffts%nr1x * dffts%nr2x * dffts%npp( dffts%nolist( idx ) + 1 )
             END DO
             !
             ! copy the charge back to the proper processor location
             !
!$omp parallel do
             DO ir = 1, dffts%nnr
                rho%of_r(ir,current_spin) = rho%of_r(ir,current_spin) + tg_rho(ir+ioff)
             END DO
!$omp end parallel do
             !
          END IF
          !
          ! ... If we have a US pseudopotential we compute here the becsum term
          !
          CYCLE k_loop
          !
       END DO k_loop

       IF( dffts%have_task_groups ) THEN
          DEALLOCATE( tg_psi )
          DEALLOCATE( tg_rho )
       END IF
       dffts%have_task_groups = use_tg

       !
       IF ( ALLOCATED (becsum_nc) ) DEALLOCATE( becsum_nc )
       !
       RETURN
       !
     END SUBROUTINE sum_band_k
     !
     !
     SUBROUTINE get_rho(rho_loc, nrxxs_loc, w1_loc, psic_loc)

        IMPLICIT NONE

        INTEGER :: nrxxs_loc
        REAL(DP) :: rho_loc(nrxxs_loc)
        REAL(DP) :: w1_loc
        COMPLEX(DP) :: psic_loc(nrxxs_loc)

        INTEGER :: ir

!$omp parallel do
        DO ir = 1, nrxxs_loc
           !
           rho_loc(ir) = rho_loc(ir) + &
                         w1_loc * ( DBLE( psic_loc(ir) )**2 + &
                                   AIMAG( psic_loc(ir) )**2 )
           !
        END DO
!$omp end parallel do

     END SUBROUTINE get_rho

     SUBROUTINE get_rho_gamma(rho_loc, nrxxs_loc, w1_loc, w2_loc, psic_loc)

        IMPLICIT NONE

        INTEGER :: nrxxs_loc
        REAL(DP) :: rho_loc(nrxxs_loc)
        REAL(DP) :: w1_loc, w2_loc
        COMPLEX(DP) :: psic_loc(nrxxs_loc)

        INTEGER :: ir

!$omp parallel do
        DO ir = 1, nrxxs_loc
           !
           rho_loc(ir) = rho_loc(ir) + &
                         w1_loc * DBLE( psic_loc(ir) )**2 + &
                         w2_loc * AIMAG( psic_loc(ir) )**2
           !
        END DO
!$omp end parallel do

     END SUBROUTINE get_rho_gamma


     SUBROUTINE get_rho_domag(rho_loc, nrxxs_loc, w1_loc, psic_loc)

        IMPLICIT NONE

        INTEGER :: nrxxs_loc
        REAL(DP) :: rho_loc(:, :)
        REAL(DP) :: w1_loc
        COMPLEX(DP) :: psic_loc(:, :)

        INTEGER :: ir

!$omp parallel do
        DO ir = 1, nrxxs_loc
           !
           rho_loc(ir,2) = rho_loc(ir,2) + w1_loc*2.D0* &
                          (DBLE(psic_loc(ir,1))* DBLE(psic_loc(ir,2)) + &
                          AIMAG(psic_loc(ir,1))*AIMAG(psic_loc(ir,2)))
 
           rho_loc(ir,3) = rho_loc(ir,3) + w1_loc*2.D0* &
                          (DBLE(psic_loc(ir,1))*AIMAG(psic_loc(ir,2)) - &
                           DBLE(psic_loc(ir,2))*AIMAG(psic_loc(ir,1)))

           rho_loc(ir,4) = rho_loc(ir,4) + w1_loc* &
                          (DBLE(psic_loc(ir,1))**2+AIMAG(psic_loc(ir,1))**2 &
                          -DBLE(psic_loc(ir,2))**2-AIMAG(psic_loc(ir,2))**2)
           !
        END DO
!$omp end parallel do

     END SUBROUTINE get_rho_domag

END SUBROUTINE sum_band
