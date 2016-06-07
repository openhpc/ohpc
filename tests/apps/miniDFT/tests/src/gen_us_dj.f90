!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------
subroutine gen_us_dj (ik, dvkb)
  !----------------------------------------------------------------------
  !
  !   Calculates the beta function pseudopotentials with
  !   the derivative of the Bessel functions
  !
  USE kinds,      ONLY : DP
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
  complex(DP) :: dvkb (npwx, nkb)
  !
  ! local variables
  !
  integer :: ikb, nb, ih, ig, i0, i1, i2, i3 , nt
  ! counter on beta functions
  ! counter on beta functions
  ! counter on beta functions
  ! counter on G vectors
  ! index of the first nonzero point in the r
  ! counter on atomic type

  real(DP) :: arg, px, ux, vx, wx
  ! argument of the atomic phase factor

  complex(DP) :: phase, pref
  ! atomic phase factor
  ! prefactor

  integer :: na, l, iig, lm
  real(DP), allocatable :: djl (:,:,:), ylm (:,:), q (:), gk (:,:)
  real(DP) ::  qt, eps
  parameter (eps = 1.0d-8)

  complex(DP), allocatable :: sk (:)

  integer :: iq
  real(DP), allocatable :: xdata(:)

  if (nkb.eq.0) return

  call start_clock('stres_us31')

  allocate (djl( npw , nbetam , ntyp))    
  allocate (ylm( npw ,(lmaxkb + 1) **2))    
  allocate (gk( 3, npw))    
  allocate (q( npw))    
  do ig = 1, npw
     gk (1,ig) = xk (1, ik) + g(1, igk(ig) )
     gk (2,ig) = xk (2, ik) + g(2, igk(ig) )
     gk (3,ig) = xk (3, ik) + g(3, igk(ig) )
     q (ig) = gk(1, ig)**2 +  gk(2, ig)**2 + gk(3, ig)**2
  enddo

  call stop_clock('stres_us31')
  call start_clock('stres_us32')
  call ylmr2 ((lmaxkb+1)**2, npw, gk, q, ylm)
  call stop_clock('stres_us32')
  call start_clock('stres_us33')


  if (spline_ps) then
    allocate(xdata(nqx))
    do iq = 1, nqx
      xdata(iq) = (iq - 1) * dq
    enddo
  endif

  do nt = 1, ntyp
     do nb = 1, upf(nt)%nbeta
        do ig = 1, npw
           qt = sqrt(q (ig)) * tpiba
           if (spline_ps) then
             djl(ig,nb,nt) = splint_deriv(xdata, tab(:,nb,nt), & 
                                                 tab_d2y(:,nb,nt), qt)
           else
             px = qt / dq - int (qt / dq)
             ux = 1.d0 - px
             vx = 2.d0 - px
             wx = 3.d0 - px
             i0 = qt / dq + 1
             i1 = i0 + 1
             i2 = i0 + 2
             i3 = i0 + 3
             djl(ig,nb,nt) = ( tab (i0, nb, nt) * (-vx*wx-ux*wx-ux*vx)/6.d0 + &
                               tab (i1, nb, nt) * (+vx*wx-px*wx-px*vx)/2.d0 - &
                               tab (i2, nb, nt) * (+ux*wx-px*wx-px*ux)/2.d0 + &
                               tab (i3, nb, nt) * (+ux*vx-px*vx-px*ux)/6.d0 )/dq
           endif
        enddo
     enddo
  enddo
  call stop_clock('stres_us33')
  call start_clock('stres_us34')

  deallocate (q)
  deallocate (gk)

  allocate (sk( npw))    
  ikb = 0
  do nt = 1, ntyp
     do na = 1, nat
        if (ityp (na) .eq.nt) then
           arg = (xk (1, ik) * tau(1,na) + &
                  xk (2, ik) * tau(2,na) + &
                  xk (3, ik) * tau(3,na) ) * tpi
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
              lm= nhtolm(ih, nt)
              ikb = ikb + 1
              pref = (0.d0, -1.d0) **l
              !
              do ig = 1, npw
                 dvkb (ig, ikb) = djl (ig, nb, nt) * sk (ig) * ylm (ig, lm) &
                      * pref
              enddo
           enddo
        endif
     enddo

  enddo
  call stop_clock('stres_us34')

  if (ikb.ne.nkb) call errore ('gen_us_dj', 'unexpected error', 1)
  deallocate (sk)
  deallocate (ylm)
  deallocate (djl)
  if (spline_ps) deallocate(xdata)
  return
end subroutine gen_us_dj

