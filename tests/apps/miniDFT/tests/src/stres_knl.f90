!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
subroutine stres_knl (sigmanlc, sigmakin)
  !-----------------------------------------------------------------------
  !
  USE kinds,                ONLY: DP
  USE constants,            ONLY: pi, e2
  USE cell_base,            ONLY: omega, alat, at, bg, tpiba
  USE gvect,                ONLY: g
  USE klist,                ONLY: nks, xk, ngk
  USE io_files,             ONLY: iunwfc, nwordwfc, iunigk
  USE buffers,              ONLY: get_buffer
  USE symme,                ONLY: symmatrix
  USE wvfct,                ONLY: npw, npwx, nbnd, igk, wg, qcutz, ecfixed, q2sigma
  USE wavefunctions_module, ONLY: evc
  USE mp_global,            ONLY: inter_pool_comm, intra_pool_comm, intra_bgrp_comm
  USE mp,                   ONLY: mp_sum
  implicit none
  real(DP) :: sigmanlc (3, 3), sigmakin (3, 3)
  real(DP), allocatable :: gk (:,:), kfac (:)
  real(DP) :: twobysqrtpi, gk2, arg
  integer :: ik, l, m, i, ibnd, is

  allocate (gk(  3, npwx))    
  allocate (kfac(   npwx))    

  sigmanlc(:,:) =0.d0
  sigmakin(:,:) =0.d0
  twobysqrtpi = 2.d0 / sqrt (pi)

  kfac(:) = 1.d0

  !if (nks.gt.1) rewind (iunigk) !BMA: eliminate unused fs activty for miniApp
  do ik = 1, nks
     npw = ngk(ik)
     !if (nks > 1) then
     !   read (iunigk) igk
     !   call get_buffer (evc, nwordwfc, iunwfc, ik)
     !endif
     do i = 1, npw
        gk (1, i) = (xk (1, ik) + g (1, igk (i) ) ) * tpiba
        gk (2, i) = (xk (2, ik) + g (2, igk (i) ) ) * tpiba
        gk (3, i) = (xk (3, ik) + g (3, igk (i) ) ) * tpiba
        if (qcutz.gt.0.d0) then
           gk2 = gk (1, i) **2 + gk (2, i) **2 + gk (3, i) **2
           arg = ( (gk2 - ecfixed) / q2sigma) **2
           kfac (i) = 1.d0 + qcutz / q2sigma * twobysqrtpi * exp ( - arg)
        endif
     enddo
     !
     !   kinetic contribution
     !
     do l = 1, 3
        do m = 1, l
           do ibnd = 1, nbnd
              do i = 1, npw
                    sigmakin (l, m) = sigmakin (l, m) + wg (ibnd, ik) * &
                        gk (l, i) * gk (m, i) * kfac (i) * &
                          DBLE (CONJG(evc (i, ibnd) ) * evc (i, ibnd) )
              enddo
           enddo
        enddo

     enddo
     !
     !  contribution from the  nonlocal part
     !
     call stres_us (ik, gk, sigmanlc)

  enddo
  !
  call mp_sum( sigmakin, intra_bgrp_comm )
  call mp_sum( sigmanlc, intra_bgrp_comm )
  call mp_sum( sigmakin, inter_pool_comm )
  call mp_sum( sigmanlc, inter_pool_comm )
  !
  do l = 1, 3
     do m = 1, l - 1
        sigmanlc (m, l) = sigmanlc (l, m)
        sigmakin (m, l) = sigmakin (l, m)
     enddo
  enddo
  !
     sigmakin(:,:) = e2 / omega * sigmakin(:,:)
  sigmanlc(:,:) = -1.d0 / omega * sigmanlc(:,:)
  !
  ! symmetrize stress
  !
  call symmatrix ( sigmakin )
  call symmatrix ( sigmanlc )

  deallocate(kfac)
  deallocate(gk)
  return
end subroutine stres_knl

