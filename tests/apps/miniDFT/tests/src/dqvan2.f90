!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
subroutine dqvan2 (ngy, ih, jh, np, qmod, dqg, ylmk0, dylmk0, ipol)
  !-----------------------------------------------------------------------
  !
  !    This routine computes the derivatives of the fourier transform of
  !    the Q function needed in stress assuming that the radial fourier
  !    trasform is already computed and stored in table qrad.
  !
  !    The formula implemented here is
  !
  !     dq(g,l,k) = sum_lm (-i)^l ap(lm,l,k) *
  !                ( yr_lm(g^) dqrad(g,l,l,k) + dyr_lm(g^) qrad(g,l,l,k))
  !
  !     here the dummy variables
  !
  USE kinds, ONLY: DP
  USE gvect, ONLY: g
  USE us, ONLY: dq, qrad
  USE uspp_param, ONLY: lmaxq, nbetam
  USE uspp, ONLY: nlx, lpl, lpx, ap, indv, nhtol, nhtolm
  implicit none
  integer :: ngy, ih, jh, np, ipol
  ! input: the number of G vectors to compute
  ! input: the first index of Q
  ! input: the second index of Q
  ! input: the number of the pseudopotential
  ! input: the polarization of the derivative

  real(DP) :: ylmk0 (ngy, lmaxq * lmaxq), dylmk0 (ngy, lmaxq * lmaxq), &
       qmod (ngy)
  ! the spherical harmonics
  ! the spherical harmonics derivetives
  ! input: moduli of the q+g vectors
  complex(DP) :: dqg (ngy)
  ! output: the fourier transform of interest
  !
  !     here the local variables
  !

  complex(DP) :: sig
  ! (-i)^L

  integer :: nb, mb, ijv, ivl, jvl, ig, lp, l, lm, i0, i1, i2, i3
  ! the atomic index corresponding to ih
  ! the atomic index corresponding to jh
  ! combined index (nb,mb)
  ! the lm corresponding to ih
  ! the lm corresponding to jh
  ! counter on g vectors
  ! the actual LM
  ! the angular momentum L
  ! the possible LM's compatible with ih,j
  ! counters for interpolation table

  real(DP) :: sixth, dqi, qm, px, ux, vx, wx, uvx, pwx, work, work1, qm1
  ! 1 divided by six
  ! 1 divided dq
  ! qmod/dq
  ! measures for interpolation table
  ! auxiliary variables for intepolation
  ! auxiliary variable
  ! auxiliary variable
  !
  !     compute the indices which correspond to ih,jh
  !
  sixth = 1.d0 / 6.d0
  dqi = 1 / dq
  nb = indv (ih, np)
  mb = indv (jh, np)
  if (nb.ge.mb) then
     ijv = nb * (nb - 1) / 2 + mb
  else
     ijv = mb * (mb - 1) / 2 + nb
  endif
  ivl = nhtolm (ih, np)
  jvl = nhtolm (jh, np)

  if (nb > nbetam .OR. mb > nbetam) &
       call errore (' dqvan2 ', ' wrong dimensions (1)', MAX(nb,mb))
  if (ivl > nlx .OR. jvl > nlx) &
       call errore (' dqvan2 ', ' wrong dimensions (2)', MAX(ivl,jvl))

  dqg(:) = (0.d0,0.d0) 
  !
  !    and make the sum over the non zero LM
  !
  do lm = 1, lpx (ivl, jvl)
     lp = lpl (ivl, jvl, lm)
     !
     !     extraction of angular momentum l from lp:
     !
     if (lp.eq.1) then
        l = 1
     elseif ( (lp.ge.2) .and. (lp.le.4) ) then
        l = 2
     elseif ( (lp.ge.5) .and. (lp.le.9) ) then
        l = 3
     elseif ( (lp.ge.10) .and. (lp.le.16) ) then
        l = 4
     elseif ( (lp.ge.17) .and. (lp.le.25) ) then
        l = 5
     elseif ( (lp.ge.26) .and. (lp.le.36) ) then
        l = 6
     elseif ( (lp.ge.37) .and. (lp.le.49) ) then
        l = 7
     else
        call errore (' qvan ', ' lp.gt.49 ', lp)
     endif

     sig = (0.d0, -1.d0) ** (l - 1)
     sig = sig * ap (lp, ivl, jvl)
     !
     qm1 = -1.0_dp !  any number smaller than qmod(1)
     !
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(qm,px,ux,vx,wx,i0,i1,i2,i3,uvx,pwx,work,work1)
     do ig = 1, ngy
        !
        ! calculate quantites depending on the module of G only when needed
        !
#if !defined(__OPENMP)
        IF ( ABS( qmod(ig) - qm1 ) > 1.0D-6 ) THEN
#endif
           qm = qmod (ig) * dqi
           px = qm - int (qm)
           ux = 1.d0 - px
           vx = 2.d0 - px
           wx = 3.d0 - px
           i0 = qm + 1
           i1 = qm + 2
           i2 = qm + 3
           i3 = qm + 4
           uvx = ux * vx * sixth

           pwx = px * wx * 0.5d0

           work = qrad (i0, ijv, l, np) * uvx * wx + &
                  qrad (i1, ijv, l, np) * pwx * vx - &
                  qrad (i2, ijv, l, np) * pwx * ux + &
                  qrad (i3, ijv, l, np) * px * uvx
           work1 = - qrad(i0, ijv, l, np) * (ux*vx + vx*wx + ux*wx) * sixth &
                   + qrad(i1, ijv, l, np) * (wx*vx - px*wx - px*vx) * 0.5d0 &
                   - qrad(i2, ijv, l, np) * (wx*ux - px*wx - px*ux) * 0.5d0 &
                   + qrad(i3, ijv, l, np) * (ux*vx - px*ux - px*vx) * sixth

           work1 = work1 * dqi

#if !defined(__OPENMP)
           qm1 = qmod(ig)
        END IF
#endif

        dqg (ig) = dqg (ig) + sig * dylmk0 (ig, lp) * work
        if (qmod (ig) > 1.d-9) dqg (ig) = dqg (ig) + &
            sig * ylmk0 (ig, lp) * work1 * g (ipol, ig) / qmod (ig)
     enddo
!$OMP END PARALLEL DO

  enddo
  return

end subroutine dqvan2

