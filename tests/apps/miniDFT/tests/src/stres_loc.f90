
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------
subroutine stres_loc (sigmaloc)
  !----------------------------------------------------------------------
  !
  USE kinds,                ONLY : DP
  USE atom,                 ONLY : msh, rgrid
  USE ions_base,            ONLY : ntyp => nsp
  USE cell_base,            ONLY : omega, tpiba2
  USE fft_base,             ONLY : dfftp
  USE fft_interfaces,       ONLY : fwfft
  USE gvect,                ONLY : ngm, gstart, nl, g, ngl, gl, igtongl
  USE lsda_mod,             ONLY : nspin
  USE scf,                  ONLY : rho
  USE vlocal,               ONLY : strf, vloc
  USE wavefunctions_module, ONLY : psic
  USE uspp_param,           ONLY : upf
  USE mp_global,            ONLY : intra_pool_comm, intra_bgrp_comm
  USE mp,                   ONLY : mp_sum
  !
  implicit none
  !
  integer, parameter :: nspin_lsda = 1 !substitute for noncollin_module
  real(DP) :: sigmaloc (3, 3)
  real(DP) , allocatable :: dvloc(:)
  real(DP) :: evloc, fact
  integer :: ng, nt, l, m, is
  ! counter on g vectors
  ! counter on atomic type
  ! counter on angular momentum
  ! counter on spin components

  allocate(dvloc(ngl))
  sigmaloc(:,:) = 0.d0
  psic(:)=(0.d0,0.d0)
  do is = 1, nspin_lsda
     call daxpy (dfftp%nnr, 1.d0, rho%of_r (1, is), 1, psic, 2)
  enddo

  CALL fwfft ('Dense', psic, dfftp)
  ! psic contains now the charge density in G space
     fact = 1.d0
  evloc = 0.0d0
  do nt = 1, ntyp
     if (gstart==2) evloc = evloc + &
          psic (nl (1) ) * strf (1, nt) * vloc (igtongl (1), nt)
     do ng = gstart, ngm
        evloc = evloc +  DBLE (CONJG(psic (nl (ng) ) ) * strf (ng, nt) ) &
             * vloc (igtongl (ng), nt) * fact
     enddo
  enddo
  !
  !      WRITE( 6,*) ' evloc ', evloc, evloc*omega   ! DEBUG
  !
  do nt = 1, ntyp
     IF ( .NOT. ASSOCIATED ( upf(nt)%vloc ) ) THEN
        !
        ! special case: pseudopotential is coulomb 1/r potential
        !
        call dvloc_coul (upf(nt)%zp, tpiba2, ngl, gl, omega, dvloc)
        !
     ELSE
        !
        ! normal case: dvloc contains dV_loc(G)/dG
        !
        call dvloc_of_g (rgrid(nt)%mesh, msh (nt), rgrid(nt)%rab, rgrid(nt)%r,&
          upf(nt)%vloc(1), upf(nt)%zp, tpiba2, ngl, gl, omega, dvloc)
        !
     END IF
     ! no G=0 contribution
     do ng = 1, ngm
        do l = 1, 3
           do m = 1, l
              sigmaloc(l, m) = sigmaloc(l, m) +  DBLE( CONJG( psic(nl(ng) ) ) &
                    * strf (ng, nt) ) * 2.0d0 * dvloc (igtongl (ng) ) &
                    * tpiba2 * g (l, ng) * g (m, ng) * fact
           enddo
        enddo
     enddo
  enddo
  !
  do l = 1, 3
     sigmaloc (l, l) = sigmaloc (l, l) + evloc
     do m = 1, l - 1
        sigmaloc (m, l) = sigmaloc (l, m)
     enddo
  enddo
  !
  call mp_sum(  sigmaloc, intra_bgrp_comm )
  !
  deallocate(dvloc)
  return
end subroutine stres_loc

