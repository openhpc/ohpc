!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------
subroutine gen_us_dy (ik, u, dvkb)
  !----------------------------------------------------------------------
  !
  !  Calculates the kleinman-bylander pseudopotentials with the
  !  derivative of the spherical harmonics projected on vector u
  !
  USE kinds,      ONLY : DP
  USE io_global,  ONLY : stdout
  USE constants,  ONLY : tpi
  USE ions_base,  ONLY : nat, ntyp => nsp, ityp, tau
  USE cell_base,  ONLY : tpiba
  USE klist,      ONLY : xk
  USE gvect,      ONLY : mill, eigts1, eigts2, eigts3, g
  USE wvfct,      ONLY : npw, npwx, igk
  USE uspp,       ONLY : nkb, indv, nhtol, nhtolm
  USE us,         ONLY : nqx, tab, tab_d2y, dq, spline_ps
  USE splinelib
  USE uspp_param, ONLY : upf, lmaxkb, nbetam, nh
  !
  implicit none
  !
  integer :: ik
  real(DP) :: u (3)

  complex(DP) :: dvkb (npwx, nkb)
  integer :: na, nt, nb, ih, l, lm, ikb, iig, ipol, i0, i1, i2, &
       i3, ig
  real(DP), allocatable :: gk(:,:), q (:)
  real(DP) :: px, ux, vx, wx, arg

  real(DP), allocatable :: vkb0 (:,:,:), dylm (:,:), dylm_u (:,:)
  ! dylm = d Y_lm/dr_i in cartesian axes
  ! dylm_u as above projected on u

  complex(DP), allocatable :: sk (:)
  complex(DP) :: phase, pref

  integer :: iq
  real(DP), allocatable :: xdata(:)

  dvkb(:,:) = (0.d0, 0.d0)
  if (lmaxkb.le.0) return

  allocate ( vkb0(npw,nbetam,ntyp), dylm_u(npw,(lmaxkb+1)**2), gk(3,npw) )
  allocate ( q(npw) )

  do ig = 1, npw
     gk (1, ig) = xk (1, ik) + g (1, igk (ig) )
     gk (2, ig) = xk (2, ik) + g (2, igk (ig) )
     gk (3, ig) = xk (3, ik) + g (3, igk (ig) )
     q (ig) = gk(1, ig)**2 +  gk(2, ig)**2 + gk(3, ig)**2
  enddo

  allocate ( dylm(npw,(lmaxkb+1)**2) )
  dylm_u(:,:) = 0.d0
  do ipol = 1, 3
     call dylmr2  ((lmaxkb+1)**2, npw, gk, q, dylm, ipol)
     call daxpy (npw * (lmaxkb + 1) **2, u (ipol), dylm, 1, dylm_u, 1)
  enddo
  deallocate (dylm)

  do ig = 1, npw
     q (ig) = sqrt ( q(ig) ) * tpiba
  end do

  if (spline_ps) then
    allocate(xdata(nqx))
    do iq = 1, nqx
      xdata(iq) = (iq - 1) * dq
    enddo
  endif

  do nt = 1, ntyp
     ! calculate beta in G-space using an interpolation table
     do nb = 1, upf(nt)%nbeta
        do ig = 1, npw
           if (spline_ps) then
             vkb0(ig,nb,nt) = splint(xdata, tab(:,nb,nt), &
                                     tab_d2y(:,nb,nt), q(ig))
           else
             px = q (ig) / dq - int (q (ig) / dq)
             ux = 1.d0 - px
             vx = 2.d0 - px
             wx = 3.d0 - px
             i0 = q (ig) / dq + 1
             i1 = i0 + 1
             i2 = i0 + 2
             i3 = i0 + 3
             vkb0 (ig, nb, nt) = tab (i0, nb, nt) * ux * vx * wx / 6.d0 + &
                                 tab (i1, nb, nt) * px * vx * wx / 2.d0 - &
                                 tab (i2, nb, nt) * px * ux * wx / 2.d0 + &
                                 tab (i3, nb, nt) * px * ux * vx / 6.d0
           endif
        enddo
     enddo
  enddo

  deallocate (q)
  allocate ( sk(npw) )

  ikb = 0
  do nt = 1, ntyp
     do na = 1, nat
        if (ityp (na) .eq.nt) then
           arg = (xk (1, ik) * tau (1, na) + xk (2, ik) * tau (2, na) &
                + xk (3, ik) * tau (3, na) ) * tpi
           phase = CMPLX(cos (arg), - sin (arg) ,kind=DP)
           do ig = 1, npw
              iig = igk (ig)
              sk (ig) = eigts1 (mill (1,iig), na) * &
                        eigts2 (mill (2,iig), na) * &
                        eigts3 (mill (3,iig), na) * phase
           enddo
           do ih = 1, nh (nt)
              nb = indv (ih, nt)
              l = nhtol (ih, nt)
              lm = nhtolm(ih, nt)
              ikb = ikb + 1
              pref = (0.d0, -1.d0) **l
              !
              do ig = 1, npw
                 dvkb (ig, ikb) = vkb0(ig, nb, nt) * sk(ig) * dylm_u(ig, lm) &
                      * pref / tpiba
              enddo
           enddo
        endif
     enddo
  enddo

  if (ikb.ne.nkb) then
     WRITE( stdout, * ) ikb, nkb
     call errore ('gen_us_dy', 'unexpected error', 1)
  endif

  deallocate ( sk )
  deallocate ( vkb0, dylm_u, gk )
  if (spline_ps) deallocate(xdata)

  return
end subroutine gen_us_dy

