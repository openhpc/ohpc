!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
subroutine ylmr2 (lmax2, ng, g, gg, ylm)
  !-----------------------------------------------------------------------
  !
  !     Real spherical harmonics ylm(G) up to l=lmax
  !     lmax2 = (lmax+1)^2 is the total number of spherical harmonics
  !     Numerical recursive algorithm based on the one given in Numerical 
  !     Recipes but avoiding the calculation of factorials that generate 
  !     overflow for lmax > 11
  !
  USE kinds, ONLY : DP
  USE constants, ONLY : pi, fpi
  implicit none
  !
  integer, intent(in) :: lmax2, ng
  real(DP), intent(in) :: g (3, ng), gg (ng)
  !
  ! BEWARE: gg = g(1)^2 + g(2)^2 +g(3)^2  is not checked on input
  !         incorrect results will ensue if the above does not hold
  !
  real(DP), intent(out) :: ylm (ng,lmax2)
  !
  ! local variables
  !
  real(DP), parameter :: eps = 1.0d-9
  real(DP), allocatable :: cost (:), sent(:), phi (:), Q(:,:,:)
  real(DP) :: c, gmod
  integer :: lmax, ig, l, m, lm
  !
  if (ng < 1 .or. lmax2 < 1) return
  do lmax = 0, 25
     if ((lmax+1)**2 == lmax2) go to 10
  end do
  call errore (' ylmr', 'l > 25 or wrong number of Ylm required',lmax2)
10 continue

  !
  if (lmax == 0) then
     ylm(:,1) =  sqrt (1.d0 / fpi)
     return
  end if
  !
  !  theta and phi are polar angles, cost = cos(theta)
  !
  allocate(cost(ng), sent(ng), phi(ng), Q(ng,0:lmax,0:lmax) )
  !
!$omp parallel default(shared), private(ig,gmod,lm,l,c,m)

!$omp do
  do ig = 1, ng
     gmod = sqrt (gg (ig) )
     if (gmod < eps) then
        cost(ig) = 0.d0
     else
        cost(ig) = g(3,ig)/gmod
     endif
     !
     !  beware the arc tan, it is defined modulo pi
     !
     if (g(1,ig) > eps) then
        phi (ig) = atan( g(2,ig)/g(1,ig) )
     else if (g(1,ig) < -eps) then
        phi (ig) = atan( g(2,ig)/g(1,ig) ) + pi
     else
        phi (ig) = sign( pi/2.d0,g(2,ig) )
     end if
     sent(ig) = sqrt(max(0d0,1.d0-cost(ig)**2))
  enddo
  !
  !  Q(:,l,m) are defined as sqrt ((l-m)!/(l+m)!) * P(:,l,m) where
  !  P(:,l,m) are the Legendre Polynomials (0 <= m <= l)
  !
  lm = 0
  do l = 0, lmax
     c = sqrt (DBLE(2*l+1) / fpi)
     if ( l == 0 ) then
!$omp do
        do ig = 1, ng
           Q (ig,0,0) = 1.d0
        end do
     else if ( l == 1 ) then
!$omp do
        do ig = 1, ng
           Q (ig,1,0) = cost(ig)
           Q (ig,1,1) =-sent(ig)/sqrt(2.d0)
        end do
     else
        !
        !  recursion on l for Q(:,l,m)
        !
        do m = 0, l - 2
!$omp do
           do ig = 1, ng
              Q(ig,l,m) = cost(ig)*(2*l-1)/sqrt(DBLE(l*l-m*m))*Q(ig,l-1,m) &
                       - sqrt(DBLE((l-1)*(l-1)-m*m))/sqrt(DBLE(l*l-m*m))*Q(ig,l-2,m)
           end do
        end do
!$omp do
        do ig = 1, ng
           Q(ig,l,l-1) = cost(ig) * sqrt(DBLE(2*l-1)) * Q(ig,l-1,l-1)
        end do
!$omp do
        do ig = 1, ng
           Q(ig,l,l)   = - sqrt(DBLE(2*l-1))/sqrt(DBLE(2*l))*sent(ig)*Q(ig,l-1,l-1) 
        end do
     end if
     !
     ! Y_lm, m = 0
     !
     lm = lm + 1
!$omp do
     do ig = 1, ng
        ylm(ig, lm) = c * Q(ig,l,0)
     end do
     !
     do m = 1, l
        !
        ! Y_lm, m > 0
        !
        lm = lm + 1
!$omp do
        do ig = 1, ng
           ylm(ig, lm) = c * sqrt(2.d0) * Q(ig,l,m) * cos (m*phi(ig))
        end do
        !
        ! Y_lm, m < 0
        !
        lm = lm + 1
!$omp do
        do ig = 1, ng
           ylm(ig, lm) = c * sqrt(2.d0) * Q(ig,l,m) * sin (m*phi(ig))
        end do
     end do
  end do
  !
!$omp end parallel
  !
  deallocate(cost, sent, phi, Q)
  !
  return
end subroutine ylmr2

