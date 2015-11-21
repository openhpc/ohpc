!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
subroutine stres_cc (sigmaxcc)
  !-----------------------------------------------------------------------
  !
  USE kinds,                ONLY : DP
  USE atom,                 ONLY : rgrid
  USE uspp_param,           ONLY : upf
  USE ions_base,            ONLY : ntyp => nsp
  USE cell_base,            ONLY : alat, omega, tpiba, tpiba2
  USE fft_base,             ONLY : dfftp
  USE fft_interfaces,       ONLY : fwfft
  USE gvect,                ONLY : ngm, gstart, nl, g, gg, ngl, gl,igtongl
  USE ener,                 ONLY : etxc, vtxc
  USE lsda_mod,             ONLY : nspin
  USE scf,                  ONLY : rho, rho_core, rhog_core
  USE vlocal,               ONLY : strf
  USE wavefunctions_module, ONLY : psic
  USE mp_global,            ONLY : intra_bgrp_comm
  USE mp,                   ONLY : mp_sum
  !
  implicit none
  ! output
  real(DP) :: sigmaxcc (3, 3)
  ! local variables

  integer :: nt, ng, l, m, ir
  ! counters
  real(DP) :: fact, sigmadiag
  real(DP) , allocatable:: rhocg (:), vxc (:,:)

  sigmaxcc(:,:) = 0.d0
  if ( ANY (upf(1:ntyp)%nlcc) ) goto 15

  return

15 continue
  !
  ! recalculate the exchange-correlation potential
  !
  allocate ( vxc(dfftp%nnr,nspin) )
  call v_xc (rho, rho_core, rhog_core, etxc, vtxc, vxc)
  if (nspin.eq.1.or.nspin.eq.4) then
     do ir = 1, dfftp%nnr
        psic (ir) = vxc (ir, 1)
     enddo
  else
     do ir = 1, dfftp%nnr
        psic (ir) = 0.5d0 * (vxc (ir, 1) + vxc (ir, 2) )
     enddo
  endif
  deallocate (vxc)
  CALL fwfft ('Dense', psic, dfftp)
  !
  ! psic contains now Vxc(G)
  !
  allocate(rhocg(ngl))
  sigmadiag = 0.0d0
     fact = 1.d0
  do nt = 1, ntyp
     if ( upf(nt)%nlcc ) then
        call drhoc (ngl, gl, omega, tpiba2, rgrid(nt)%mesh, rgrid(nt)%r, &
              rgrid(nt)%rab, upf(nt)%rho_atc, rhocg)
        ! diagonal term
        if (gstart==2) sigmadiag = sigmadiag + &
             CONJG(psic (nl(1) ) ) * strf (1,nt) * rhocg (igtongl (1) )
        do ng = gstart, ngm
           sigmadiag = sigmadiag + CONJG(psic (nl (ng) ) ) * &
                strf (ng,nt) * rhocg (igtongl (ng) ) * fact
        enddo

        call deriv_drhoc (ngl, gl, omega, tpiba2, rgrid(nt)%mesh, &
             rgrid(nt)%r, rgrid(nt)%rab, upf(nt)%rho_atc, rhocg)
        ! non diagonal term (g=0 contribution missing)
        do ng = gstart, ngm
           do l = 1, 3
              do m = 1, 3
                 sigmaxcc (l, m) = sigmaxcc (l, m) + CONJG(psic (nl (ng) ) ) &
                      * strf (ng, nt) * rhocg (igtongl (ng) ) * tpiba * &
                      g (l, ng) * g (m, ng) / sqrt (gg (ng) ) * fact
              enddo
           enddo
        enddo
     endif
  enddo

  do l = 1, 3
     sigmaxcc (l, l) = sigmaxcc (l, l) + sigmadiag
  enddo
  call mp_sum(  sigmaxcc, intra_bgrp_comm )
  deallocate (rhocg)
  return
end subroutine stres_cc

