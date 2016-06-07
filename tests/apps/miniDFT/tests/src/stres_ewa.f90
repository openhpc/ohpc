!
! Copyright (C) 2001-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
subroutine stres_ewa (alat, nat, ntyp, ityp, zv, at, bg, tau, &
     omega, g, gg, ngm, gstart, gamma_only, gcutm, sigmaewa)
  !-----------------------------------------------------------------------
  !
  ! Ewald contribution, both real- and reciprocal-space terms are present
  !
  USE kinds
  USE constants, only : tpi, e2, eps6
  USE mp_global, ONLY : intra_pool_comm, intra_bgrp_comm
  USE mp,        ONLY : mp_sum

  implicit none
  !
  !   first the dummy variables
  !

  integer :: nat, ntyp, ityp (nat), ngm, gstart
  ! input: number of atoms in the unit cell
  ! input: number of different types of atoms
  ! input: the type of each atom
  ! input: number of plane waves for G sum
  ! input: first nonzero g vector

  logical :: gamma_only

  real(DP) :: tau (3, nat), g (3, ngm), gg (ngm), zv (ntyp), &
       at (3, 3), bg (3, 3), omega, alat, gcutm, sigmaewa (3, 3)
  ! input: the positions of the atoms in the cell
  ! input: the coordinates of G vectors
  ! input: the square moduli of G vectors
  ! input: the charge of each type of atoms
  ! input: the direct lattice vectors
  ! input: the reciprocal lattice vectors
  ! input: the volume of the unit cell
  ! input: measure of length
  ! input: cut-off of g vectors
  ! output: the ewald stress
  !
  !    here the local variables
  !
  integer, parameter :: mxr = 50
  ! the maximum number of R vectors included in r sum
  integer :: ng,  nr, na, nb, l, m, nrm
  ! counter over reciprocal G vectors
  ! counter over direct vectors
  ! counter on atoms
  ! counter on atoms
  ! counter on atoms
  ! number of R vectors included in r sum

  real(DP) :: charge, arg, tpiba2, dtau (3), alpha, r (3, mxr), &
       r2 (mxr), rmax, rr, upperbound, fact, fac, g2, g2a, sdewald, sewald
  ! total ionic charge in the cell
  ! the argument of the phase
  ! length in reciprocal space
  ! the difference tau_s - tau_s'
  ! alpha term in ewald sum
  ! input of the rgen routine ( not used here )
  ! the square modulus of R_j-tau_s-tau_s'
  ! the maximum radius to consider real space sum
  ! buffer variable
  ! used to optimize alpha
  ! auxiliary variables
  ! diagonal term
  ! nondiagonal term
  complex(DP) :: rhostar
  real(DP), external :: qe_erfc
  ! the erfc function
  !
  tpiba2 = (tpi / alat) **2
  sigmaewa(:,:) = 0.d0
  charge = 0.d0
  do na = 1, nat
     charge = charge+zv (ityp (na) )
  enddo
  !
  ! choose alpha in order to have convergence in the sum over G
  ! upperbound is a safe upper bound for the error ON THE ENERGY
  !
  alpha = 2.9d0
12 alpha = alpha - 0.1d0
  if (alpha.eq.0.0) call errore ('stres_ew', 'optimal alpha not found &
       &', 1)
  upperbound = e2 * charge**2 * sqrt (2 * alpha / tpi) * &
               qe_erfc ( sqrt (tpiba2 * gcutm / 4.0d0 / alpha) )
  if (upperbound.gt.1d-7) goto 12
  !
  ! G-space sum here
  !
  ! Determine if this processor contains G=0 and set the constant term
  !
  if (gstart == 2) then
     sdewald = tpi * e2 / 4.d0 / alpha * (charge / omega) **2
  else
     sdewald = 0.d0
  endif
  ! sdewald is the diagonal term
  if (gamma_only) then
     fact = 2.d0
  else
    fact = 1.d0
  end if
  do ng = gstart, ngm
     g2 = gg (ng) * tpiba2
     g2a = g2 / 4.d0 / alpha
     rhostar = (0.d0, 0.d0)
     do na = 1, nat
        arg = (g (1, ng) * tau (1, na) + g (2, ng) * tau (2, na) + &
               g (3, ng) * tau (3, na) ) * tpi
        rhostar = rhostar + zv (ityp (na) ) * CMPLX(cos (arg), sin (arg),kind=DP)
     enddo
     rhostar = rhostar / omega
     sewald = fact * tpi * e2 * exp ( - g2a) / g2 * abs (rhostar) **2
     sdewald = sdewald-sewald
     do l = 1, 3
        do m = 1, l
           sigmaewa (l, m) = sigmaewa (l, m) + sewald * tpiba2 * 2.d0 * &
                g (l, ng) * g (m, ng) / g2 * (g2a + 1)
        enddo
     enddo
  enddo
  do l = 1, 3
     sigmaewa (l, l) = sigmaewa (l, l) + sdewald
  enddo
  !
  ! R-space sum here (only for the processor that contains G=0)
  !
  if (gstart.eq.2) then
     rmax = 4.0d0 / sqrt (alpha) / alat
     !
     ! with this choice terms up to ZiZj*erfc(5) are counted (erfc(5)=2x10^-1
     !
     do na = 1, nat
        do nb = 1, nat
              dtau (:) = tau (:, na) - tau (:, nb)
           !
           !     generates nearest-neighbors shells r(i)=R(i)-dtau(i)
           !
           call rgen (dtau, rmax, mxr, at, bg, r, r2, nrm)
           do nr = 1, nrm
              rr = sqrt (r2 (nr) ) * alat
              fac = - e2 / 2.0d0 / omega * alat**2 * zv (ityp (na) ) * &
                   zv ( ityp (nb) ) / rr**3 * (qe_erfc (sqrt (alpha) * rr) + &
                   rr * sqrt (8 * alpha / tpi) * exp ( - alpha * rr**2) )
              do l = 1, 3
                 do m = 1, l
                    sigmaewa (l, m) = sigmaewa (l, m) + fac * r(l,nr) * r(m,nr)
                 enddo
              enddo
           enddo
        enddo
     enddo
  endif
  !
  do l = 1, 3
     do m = 1, l - 1
        sigmaewa (m, l) = sigmaewa (l, m)
     enddo

  enddo
  do l = 1, 3
     do m = 1, 3
        sigmaewa (l, m) = - sigmaewa (l, m)
     enddo
  enddo
  call mp_sum(  sigmaewa, intra_bgrp_comm )
  return
end subroutine stres_ewa

