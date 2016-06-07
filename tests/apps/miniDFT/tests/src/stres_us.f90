!
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE stres_us( ik, gk, sigmanlc )
  !----------------------------------------------------------------------------
  !
  ! nonlocal (separable pseudopotential) contribution to the stress
  !
  USE kinds,                ONLY : DP
  USE ions_base,            ONLY : nat, ntyp => nsp, ityp
  USE constants,            ONLY : eps8
  USE klist,                ONLY : nks, xk
  USE lsda_mod,             ONLY : current_spin, lsda, isk
  USE wvfct,                ONLY : npw, npwx, nbnd, igk, wg, et
  USE uspp_param,           ONLY : upf, lmaxkb, nh, newpseudo, nhm
  USE uspp,                 ONLY : nkb, vkb, qq, deeq, deeq_nc, qq_so
  USE wavefunctions_module, ONLY : evc
  USE spin_orb,             ONLY : lspinorb
  USE lsda_mod,             ONLY : nspin
  USE mp_global,            ONLY : me_pool, root_pool, intra_bgrp_comm,inter_bgrp_comm, mpime
  USE becmod,               ONLY : allocate_bec_type, deallocate_bec_type, &
                                   bec_type, becp, calbec
  USE mp,                   ONLY : mp_sum, mp_get_comm_null, mp_circular_shift_left 
  !
  IMPLICIT NONE
  !
  ! ... First the dummy variables
  !  
  INTEGER       :: ik
  REAL(DP) :: sigmanlc(3,3), gk(3,npw)
  !
  CALL allocate_bec_type ( nkb, nbnd, becp, intra_bgrp_comm ) 
  
  !
     !
     CALL stres_us_k()
     !
  !
  CALL deallocate_bec_type ( becp ) 
  !
  RETURN
  !
  CONTAINS
     !
     !-----------------------------------------------------------------------
     !
     !
     !----------------------------------------------------------------------
     SUBROUTINE stres_us_k()
       !----------------------------------------------------------------------  
       !
       ! ... k-points version
       !
       IMPLICIT NONE
       !
       ! ... local variables
       !
       INTEGER                       :: na, np, ibnd, ipol, jpol, l, i, &
                                        ikb, jkb, ih, jh, ijkb0, is, js, ijs
       REAL(DP)                 :: fac, xyz (3, 3), q, evps, ddot
       REAL(DP), ALLOCATABLE    :: qm1(:)
       COMPLEX(DP), ALLOCATABLE :: work1(:), work2(:), dvkb(:,:)
       COMPLEX(DP), ALLOCATABLE :: work2_nc(:,:)
       COMPLEX(DP), ALLOCATABLE :: deff_nc(:,:,:,:)
       REAL(DP), ALLOCATABLE :: deff(:,:,:)
       ! dvkb contains the derivatives of the kb potential
       COMPLEX(DP)              :: ps, ps_nc(2)
       ! xyz are the three unit vectors in the x,y,z directions
       DATA xyz / 1.0d0, 0.0d0, 0.0d0, 0.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, 1.0d0 /
       !
       !
       IF ( nkb == 0 ) RETURN
       !
       IF ( lsda ) current_spin = isk(ik)
       IF ( nks > 1 ) CALL init_us_2( npw, igk, xk(1,ik), vkb )
       !
       CALL calbec( npw, vkb, evc, becp )
          ALLOCATE( deff(nhm,nhm,nat) )
       !
       ALLOCATE( work1(npwx), work2(npwx), qm1( npwx ) )
       !
       DO i = 1, npw
          q = SQRT( gk(1,i)**2 + gk(2,i)**2 + gk(3,i)**2 )
          IF ( q > eps8 ) THEN
             qm1(i) = 1.D0 / q
          ELSE
             qm1(i) = 0.D0
          END IF
       END DO
       !
       evps = 0.D0
       ! ... diagonal contribution
       !
       IF ( me_pool /= root_pool ) GO TO 100
       !
       ! ... the contribution is calculated only on one processor because
       ! ... partial results are later summed over all processors
       !
       DO ibnd = 1, nbnd
          fac = wg(ibnd,ik)
          IF (ABS(fac) < 1.d-9) CYCLE
             CALL compute_deff(deff,et(ibnd,ik))
          ijkb0 = 0
          DO np = 1, ntyp
             DO na = 1, nat
                IF ( ityp(na) == np ) THEN
                   DO ih = 1, nh(np)
                      ikb = ijkb0 + ih
                         evps = evps+fac*deff(ih,ih,na)*ABS(becp%k(ikb,ibnd) )**2
                      IF ( newpseudo(np) ) THEN
                         !
                         ! ... only in the US case there is a contribution 
                         ! ... for jh<>ih
                         ! ... we use here the symmetry in the interchange of 
                         ! ... ih and jh
                         !
                         DO jh = ( ih + 1 ), nh(np)
                            jkb = ijkb0 + jh
                               evps = evps + deff(ih,jh,na) * fac * 2.D0 * &
                                     DBLE( CONJG( becp%k(ikb,ibnd) ) * &
                                                  becp%k(jkb,ibnd) )
                         END DO
                      END IF
                   END DO
                   ijkb0 = ijkb0 + nh(np)
                END IF
             END DO
          END DO
       END DO
       DO l = 1, 3
          sigmanlc(l,l) = sigmanlc(l,l) - evps
       END DO
       !
100    CONTINUE
       !
       ! ... non diagonal contribution - derivative of the bessel function
       !
       ALLOCATE( dvkb( npwx, nkb ) )
       !
       CALL gen_us_dj( ik, dvkb )
       !
       DO ibnd = 1, nbnd
             work2 = (0.D0,0.D0)
             CALL compute_deff(deff,et(ibnd,ik))
          ijkb0 = 0
          DO np = 1, ntyp
             DO na = 1, nat
                IF ( ityp(na) == np ) THEN
                   DO ih = 1, nh(np)
                      ikb = ijkb0 + ih
                         !
                         ! ... in the US case there is a contribution 
                         ! ... also for jh<>ih
                         !
                         ps = (0.D0,0.D0)
                         ps_nc = (0.D0,0.D0)
                         DO jh = 1, nh(np)
                            jkb = ijkb0 + jh
                               ps = ps + becp%k(jkb,ibnd) * deff(ih,jh,na)
                         END DO
                         CALL zaxpy( npw, ps, dvkb(1,ikb), 1, work2, 1 )
                   END DO
                   ijkb0 = ijkb0 + nh(np)
                END IF
             END DO
          END DO
          DO ipol = 1, 3
             DO jpol = 1, ipol
                   DO i = 1, npw
                      work1(i) = evc(i,ibnd)*gk(ipol,i)*gk(jpol,i)*qm1(i)
                   END DO
                   sigmanlc(ipol,jpol) = sigmanlc(ipol,jpol) - &
                                      2.D0 * wg(ibnd,ik) * &
                                      ddot( 2 * npw, work1, 1, work2, 1 )
             END DO
          END DO
       END DO
       !
       ! ... non diagonal contribution - derivative of the spherical harmonics
       ! ... (no contribution from l=0)
       !
       IF ( lmaxkb == 0 ) GO TO 10
       !
       DO ipol = 1, 3
          CALL gen_us_dy( ik, xyz(1,ipol), dvkb )
          DO ibnd = 1, nbnd
                work2 = (0.D0,0.D0)
                CALL compute_deff(deff,et(ibnd,ik))

             ijkb0 = 0
             DO np = 1, ntyp
                DO na = 1, nat
                   IF ( ityp(na) == np ) THEN
                      DO ih = 1, nh(np)
                         ikb = ijkb0 + ih
                               ps = becp%k(ikb,ibnd) * deeq(ih,ih,na,current_spin)
                            !
                            ! ... in the US case there is a contribution 
                            ! ... also for jh<>ih
                            !
                            ps = (0.D0,0.D0)
                            ps_nc = (0.D0,0.D0)
                            DO jh = 1, nh(np)
                               jkb = ijkb0 + jh
                                  ps = ps + becp%k(jkb,ibnd) * deff(ih,jh,na)
                            END DO
                            CALL zaxpy( npw, ps, dvkb(1,ikb), 1, work2, 1 )
                      END DO
                      ijkb0 = ijkb0 + nh(np)
                   END IF
                END DO
             END DO
             DO jpol = 1, ipol
                   DO i = 1, npw
                      work1(i) = evc(i,ibnd) * gk(jpol,i)
                   END DO
                   sigmanlc(ipol,jpol) = sigmanlc(ipol,jpol) - &
                                      2.D0 * wg(ibnd,ik) * & 
                                      ddot( 2 * npw, work1, 1, work2, 1 )
             END DO
          END DO
       END DO
       !
10     CONTINUE
       !
           DEALLOCATE( work2 )
           DEALLOCATE( deff )
       DEALLOCATE( dvkb )
       DEALLOCATE( work1, qm1 )
       !
       RETURN
       !
     END SUBROUTINE stres_us_k
     !
END SUBROUTINE stres_us
