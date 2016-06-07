!
! Copyright (C) 2001-2010 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
MODULE dfunct

CONTAINS
!---------------------------------------
SUBROUTINE newq(vr,deeq,skip_vltot) 
  !
  !   This routine computes the integral of the perturbed potential with
  !   the Q function 
  !
  USE kinds,                ONLY : DP
  USE ions_base,            ONLY : nat, ntyp => nsp, ityp
  USE cell_base,            ONLY : omega
  USE fft_base,             ONLY : dfftp
  USE fft_interfaces,       ONLY : fwfft
  USE gvect,                ONLY : g, gg, ngm, gstart, mill, &
                                   eigts1, eigts2, eigts3, nl
  USE lsda_mod,             ONLY : nspin
  USE scf,                  ONLY : vltot
  USE uspp_param,           ONLY : upf, lmaxq, nh, nhm
  USE wavefunctions_module, ONLY : psic
  USE spin_orb,             ONLY : lspinorb, domag
  USE mp_global,            ONLY : intra_bgrp_comm
  USE mp,                   ONLY : mp_sum
  !
  IMPLICIT NONE
  !
  integer, parameter :: nspin_mag = 1 !substitute for noncollin_module%nspin_mag

  ! Input: potential , output: contribution to integral
  REAL(kind=dp), intent(in)  :: vr(dfftp%nnr,nspin)
  REAL(kind=dp), intent(out) :: deeq( nhm, nhm, nat, nspin )
  LOGICAL, intent(in) :: skip_vltot !If .false. vltot is added to vr when necessary
  ! INTERNAL
  INTEGER :: ig, nt, ih, jh, na, is, nht, nb, mb
  ! counters on g vectors, atom type, beta functions x 2,
  !   atoms, spin, aux, aux, beta func x2 (again)
#ifdef __OPENMP
  INTEGER :: mytid, ntids, omp_get_thread_num, omp_get_num_threads
#endif
  COMPLEX(DP), ALLOCATABLE :: aux(:,:), qgm(:), qgm_na(:)
    ! work space
  COMPLEX(DP) :: dtmp
  REAL(DP), ALLOCATABLE :: ylmk0(:,:), qmod(:)
    ! spherical harmonics, modulus of G
  REAL(DP) :: fact, ddot

     !
     fact = 1.D0
     !
  !
  CALL start_clock( 'newd' )
  !
  ALLOCATE( aux( ngm, nspin_mag ),  &
            qgm( ngm ), qmod( ngm ), ylmk0( ngm, lmaxq*lmaxq ) )
  !
  deeq(:,:,:,:) = 0.D0
  !
  CALL ylmr2( lmaxq * lmaxq, ngm, g, gg, ylmk0 )
  !
  qmod(1:ngm) = SQRT( gg(1:ngm) )
  !
  ! ... fourier transform of the total effective potential
  !
  DO is = 1, nspin_mag
     !
     IF ( (nspin_mag == 4 .AND. is /= 1) .or. skip_vltot ) THEN 
        !
        psic(:) = vr(:,is)
        !
     ELSE
        !
        psic(:) = vltot(:) + vr(:,is)
        !
     END IF
     !
     CALL fwfft ('Dense', psic, dfftp)
     !
     aux(1:ngm,is) = psic( nl(1:ngm) )
     !
  END DO
  !
  ! ... here we compute the integral Q*V for each atom,
  ! ...       I = sum_G exp(-iR.G) Q_nm v^*
  !
  !
  CALL mp_sum( deeq( :, :, :, 1:nspin_mag ), intra_bgrp_comm )
  !
  DEALLOCATE( aux, qgm, qmod, ylmk0 )
  !
END SUBROUTINE newq
!---------------------------------------
SUBROUTINE newd()
  USE uspp,          ONLY : deeq
  USE control_flags, ONLY : tqr
  IMPLICIT NONE
  if (tqr) then
     write(*,*)"newd.f90:188 skipping newd_r: realus module requires iotk"
     !call newd_r()
  else
     call newd_g()
  end if
  !!$ IF (.not.noncolin) call add_paw_to_deeq(deeq)
  return
END SUBROUTINE newd
!----------------------------------------------------------------------------
SUBROUTINE newd_g()
  !----------------------------------------------------------------------------
  !
  ! ... This routine computes the integral of the effective potential with
  ! ... the Q function and adds it to the bare ionic D term which is used
  ! ... to compute the non-local term in the US scheme.
  !
  USE kinds,                ONLY : DP
  USE ions_base,            ONLY : nat, ntyp => nsp, ityp
  USE lsda_mod,             ONLY : nspin
  USE uspp,                 ONLY : deeq, dvan, deeq_nc, dvan_so!!$, okvan, indv
  USE uspp_param,           ONLY : upf, lmaxq, nh, nhm
  USE spin_orb,             ONLY : lspinorb, domag
  USE uspp,                 ONLY : nhtol, nhtolm
  USE scf,                  ONLY : v
  !
  IMPLICIT NONE
  !
  INTEGER :: ig, nt, ih, jh, na, is, nht, nb, mb
    ! counters on g vectors, atom type, beta functions x 2,
    !   atoms, spin, aux, aux, beta func x2 (again)
  !
  !
     !
     ! ... no ultrasoft potentials: use bare coefficients for projectors
     !
     DO na = 1, nat
        !
        nt  = ityp(na)
        nht = nh(nt)
        !
        IF ( lspinorb ) THEN
           !
           deeq_nc(1:nht,1:nht,na,1:nspin) = dvan_so(1:nht,1:nht,1:nspin,nt)
           !
        ELSE
           !
           DO is = 1, nspin
              !
              deeq(1:nht,1:nht,na,is) = dvan(1:nht,1:nht,nt)
              !
           END DO
           !
        END IF
        !
     END DO
     !
     ! ... early return
     !
     RETURN
     !
  !
  call newq(v%of_r,deeq,.false.)
  !
  atoms : &
  DO na = 1, nat
     !
     nt  = ityp(na)
        DO is = 1, nspin
           !
           DO ih = 1, nh(nt)
              DO jh = ih, nh(nt)
                 deeq(ih,jh,na,is) = deeq(ih,jh,na,is) + dvan(ih,jh,nt)
                 deeq(jh,ih,na,is) = deeq(ih,jh,na,is)
              END DO
           END DO
           !
        END DO
        !
     !
  END DO atoms
  !
  CALL stop_clock( 'newd' )
  !
  RETURN
  !
  CONTAINS
    !
    !------------------------------------------------------------------------
    SUBROUTINE newd_so(na)
      !------------------------------------------------------------------------
      !
      USE spin_orb, ONLY : fcoef
      !
      IMPLICIT NONE
      !
      INTEGER :: na

      INTEGER :: ijs, is1, is2, kh, lh
      !
      !
      nt=ityp(na)
      ijs = 0
      !
      DO is1 = 1, 2
         !
         DO is2 =1, 2
            !
            ijs = ijs + 1
            !
            IF (domag) THEN
               DO ih = 1, nh(nt)
                  !
                  DO jh = 1, nh(nt)
                     !
                     deeq_nc(ih,jh,na,ijs) = dvan_so(ih,jh,ijs,nt)
                     !
                     DO kh = 1, nh(nt)
                        !
                        DO lh = 1, nh(nt)
                           !
                           deeq_nc(ih,jh,na,ijs) = deeq_nc(ih,jh,na,ijs) +   &
                                deeq (kh,lh,na,1)*            &
                             (fcoef(ih,kh,is1,1,nt)*fcoef(lh,jh,1,is2,nt)  + &
                             fcoef(ih,kh,is1,2,nt)*fcoef(lh,jh,2,is2,nt)) + &
                             deeq (kh,lh,na,2)*            &
                             (fcoef(ih,kh,is1,1,nt)*fcoef(lh,jh,2,is2,nt)  + &
                             fcoef(ih,kh,is1,2,nt)*fcoef(lh,jh,1,is2,nt)) + &
                             (0.D0,-1.D0)*deeq (kh,lh,na,3)*            &
                             (fcoef(ih,kh,is1,1,nt)*fcoef(lh,jh,2,is2,nt)  - &
                             fcoef(ih,kh,is1,2,nt)*fcoef(lh,jh,1,is2,nt)) + &
                             deeq (kh,lh,na,4)*            &
                             (fcoef(ih,kh,is1,1,nt)*fcoef(lh,jh,1,is2,nt)  - &
                             fcoef(ih,kh,is1,2,nt)*fcoef(lh,jh,2,is2,nt))   
                           !
                        END DO
                        !
                     END DO
                     !
                  END DO
                  !
               END DO
               !
            ELSE
               !
               DO ih = 1, nh(nt)
                  !
                  DO jh = 1, nh(nt)
                     !
                     deeq_nc(ih,jh,na,ijs) = dvan_so(ih,jh,ijs,nt)
                     !
                     DO kh = 1, nh(nt)
                        !
                        DO lh = 1, nh(nt)
                           !
                           deeq_nc(ih,jh,na,ijs) = deeq_nc(ih,jh,na,ijs) +   &
                                deeq (kh,lh,na,1)*            &
                             (fcoef(ih,kh,is1,1,nt)*fcoef(lh,jh,1,is2,nt)  + &
                             fcoef(ih,kh,is1,2,nt)*fcoef(lh,jh,2,is2,nt) ) 
                           !
                        END DO
                        !
                     END DO
                     !
                  END DO
                  !
               END DO
               !
            END IF
            !
         END DO
         !
      END DO
      !
    RETURN
      !
    END SUBROUTINE newd_so
    !
    !------------------------------------------------------------------------
    SUBROUTINE newd_nc(na)
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      !
      INTEGER :: na
      !
      nt = ityp(na)
      !
      DO ih = 1, nh(nt)
         !
         DO jh = 1, nh(nt)
            !
            IF (lspinorb) THEN
               deeq_nc(ih,jh,na,1) = dvan_so(ih,jh,1,nt) + &
                                     deeq(ih,jh,na,1) + deeq(ih,jh,na,4)
               !                      
               deeq_nc(ih,jh,na,4) = dvan_so(ih,jh,4,nt) + &
                                     deeq(ih,jh,na,1) - deeq(ih,jh,na,4)
               !
            ELSE
               deeq_nc(ih,jh,na,1) = dvan(ih,jh,nt) + &
                                     deeq(ih,jh,na,1) + deeq(ih,jh,na,4)
               !                      
               deeq_nc(ih,jh,na,4) = dvan(ih,jh,nt) + &
                                     deeq(ih,jh,na,1) - deeq(ih,jh,na,4)
               !
            END IF
            deeq_nc(ih,jh,na,2) = deeq(ih,jh,na,2) - &
                                  ( 0.D0, 1.D0 ) * deeq(ih,jh,na,3)
            !                      
            deeq_nc(ih,jh,na,3) = deeq(ih,jh,na,2) + &
                                  ( 0.D0, 1.D0 ) * deeq(ih,jh,na,3)
            !                      
         END DO
         !
      END DO
      !
    RETURN
    END SUBROUTINE newd_nc
    !
END SUBROUTINE newd_g

END MODULE dfunct
