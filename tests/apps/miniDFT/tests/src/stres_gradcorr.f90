!
! Copyright (C) 2001-2006 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
subroutine stres_gradcorr( rho, rhog, rho_core, rhog_core, nspin, &
                           nr1, nr2, nr3, nrxx, nl, &
                           ngm, g, alat, omega, sigmaxc )
  !----------------------------------------------------------------------------
  !
  USE kinds,            ONLY : DP
  use funct,            ONLY : gcxc, gcx_spin, gcc_spin, & !!$gcc_spin_more, &
                               dft_is_gradient, get_igcc
  USE mp_global,        ONLY : intra_pool_comm, intra_bgrp_comm
  USE mp,               ONLY : mp_sum
  !
  IMPLICIT NONE
  !
  integer :: nspin, nr1, nr2, nr3, nrxx, ngm, nl (ngm)
  real(DP) :: rho (nrxx, nspin), rho_core (nrxx), g (3, ngm), &
       alat, omega, sigmaxc (3, 3)
  complex(DP) :: rhog(ngm, nspin), rhog_core(ngm)
  integer :: k, l, m, ipol, is, nspin0
  real(DP) , allocatable :: grho (:,:,:)
  real(DP), parameter :: epsr = 1.0d-6, epsg = 1.0d-10, e2 = 2.d0
  real(DP) :: grh2, grho2 (2), sx, sc, v1x, v2x, v1c, v2c, fac, &
       v1xup, v1xdw, v2xup, v2xdw, v1cup, v1cdw, v2cup, v2cdw, v2cud, &
       zeta, rh, rup, rdw, grhoup, grhodw, grhoud, grup, grdw, &
       sigma_gradcorr (3, 3), rhok
  logical :: igcc_is_lyp

  if ( .not. dft_is_gradient() ) return

  igcc_is_lyp = (get_igcc() == 3)

  sigma_gradcorr(:,:) = 0.d0

  allocate (grho( 3, nrxx, nspin))    
  nspin0=nspin
  if (nspin==4) nspin0=1
  fac = 1.d0 / DBLE (nspin0)
  !
  !    calculate the gradient of rho+rhocore in real space
  !
  DO is = 1, nspin0
     !
     rho(:,is)  = fac * rho_core(:)  + rho(:,is)
     rhog(:,is) = fac * rhog_core(:) + rhog(:,is)
     !
     CALL gradrho( nrxx, rhog(1,is), ngm, g, nl, grho(1,1,is) )
     !
  END DO
  !
  if (nspin.eq.1) then
     !
     !    This is the LDA case
     !
     ! sigma_gradcor_{alpha,beta} ==
     !     omega^-1 \int (grad_alpha rho) ( D(rho*Exc)/D(grad_alpha rho) ) d3
     !
     do k = 1, nrxx
        grho2 (1) = grho(1,k,1)**2 + grho(2,k,1)**2 + grho(3,k,1)**2
        if (abs (rho (k, 1) ) .gt.epsr.and.grho2 (1) .gt.epsg) then
           call gcxc (rho (k, 1), grho2(1), sx, sc, v1x, v2x, v1c, v2c)
           do l = 1, 3
              do m = 1, l
                 sigma_gradcorr (l, m) = sigma_gradcorr (l, m) + &
                                grho(l,k,1) * grho(m,k,1) * e2 * (v2x + v2c)
              enddo
           enddo
        endif

     enddo
  else
     !
     !    This is the LSDA case
     !
     do k = 1, nrxx
        grho2 (1) = grho(1,k,1)**2 + grho(2,k,1)**2 + grho(3,k,1)**2
        grho2 (2) = grho(1,k,2)**2 + grho(2,k,2)**2 + grho(3,k,2)**2

        if ( (abs (rho (k, 1) ) .gt.epsr.and.grho2 (1) .gt.epsg) .and. &
             (abs (rho (k, 2) ) .gt.epsr.and.grho2 (2) .gt.epsg) ) then
           call gcx_spin (rho (k, 1), rho (k, 2), grho2 (1), grho2 (2), &
                sx, v1xup, v1xdw, v2xup, v2xdw)
           rh = rho (k, 1) + rho (k, 2)
           if (rh.gt.epsr) then
              if ( igcc_is_lyp ) then
                 call errore('stres_gradcor','lyp type functionals not available in mini_DFT',1)
!!$                 rup = rho (k, 1)
!!$                 rdw = rho (k, 2)
!!$                 grhoup = grho(1,k,1)**2 + grho(2,k,1)**2 + grho(3,k,1)**2
!!$                 grhodw = grho(1,k,2)**2 + grho(2,k,2)**2 + grho(3,k,2)**2
!!$                 grhoud = grho(1,k,1) * grho(1,k,2) + &
!!$                          grho(2,k,1) * grho(2,k,2) + &
!!$                          grho(3,k,1) * grho(3,k,2)
!!$                 call gcc_spin_more(rup, rdw, grhoup, grhodw, grhoud, sc, &
!!$                                 v1cup, v1cdw, v2cup, v2cdw, v2cud)
              else
                 zeta = (rho (k, 1) - rho (k, 2) ) / rh

                 grh2 = (grho (1, k, 1) + grho (1, k, 2) ) **2 + &
                        (grho (2, k, 1) + grho (2, k, 2) ) **2 + &
                        (grho (3, k, 1) + grho (3, k, 2) ) **2
                 call gcc_spin (rh, zeta, grh2, sc, v1cup, v1cdw, v2c)
                 v2cup = v2c
                 v2cdw = v2c
                 v2cud = v2c
              end if
           else
              sc = 0.d0
              v1cup = 0.d0
              v1cdw = 0.d0
              v2c = 0.d0
              v2cup = 0.d0
              v2cdw = 0.d0
              v2cud = 0.d0
           endif
           do l = 1, 3
              do m = 1, l
                 !    exchange
                 sigma_gradcorr (l, m) = sigma_gradcorr (l, m) + &
                      grho (l, k, 1) * grho (m, k, 1) * e2 * v2xup + &
                      grho (l, k, 2) * grho (m, k, 2) * e2 * v2xdw
                 !    correlation
                 sigma_gradcorr (l, m) = sigma_gradcorr (l, m) + &
                     ( grho (l, k, 1) * grho (m, k, 1) * v2cup + &
                       grho (l, k, 2) * grho (m, k, 2) * v2cdw + &
                      (grho (l, k, 1) * grho (m, k, 2) +         &
                       grho (l, k, 2) * grho (m, k, 1) ) * v2cud ) * e2
              enddo
           enddo
        endif
     enddo

  endif
  do l = 1, 3
     do m = 1, l - 1
        sigma_gradcorr (m, l) = sigma_gradcorr (l, m)
     enddo

  enddo
  call mp_sum(  sigma_gradcorr, intra_bgrp_comm )

  call dscal (9, 1.d0 / (nr1 * nr2 * nr3), sigma_gradcorr, 1)

  call daxpy (9, 1.d0, sigma_gradcorr, 1, sigmaxc, 1)
  
  DO is = 1, nspin0
     !
     rho(:,is)  = rho(:,is)  - fac * rho_core(:)
     rhog(:,is) = rhog(:,is) - fac * rhog_core(:)
     !
  END DO
  !
  deallocate(grho)
  return

end subroutine stres_gradcorr

