!
! Copyright (C) 2001-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
subroutine force_ew (alat, nat, ntyp, ityp, zv, at, bg, tau, &
     omega, g, gg, ngm, gstart, gamma_only, gcutm, strf, forceion)
  !-----------------------------------------------------------------------
  !
  !  This routine computes the Ewald contribution to the forces,
  !  both the real- and reciprocal-space terms are present
  !
  USE kinds
  USE constants, ONLY : tpi, e2
  USE mp_global, ONLY : intra_bgrp_comm
  USE mp,        ONLY : mp_sum
  implicit none
  !
  !   First the dummy variables
  !

  integer :: nat, ntyp, ngm, ityp (nat), gstart
  ! input: the number of atoms
  ! input: the number of types of atom
  ! input: the number of G vectors
  ! input: the type of each atom
  ! input: first non-zero G vector
  logical :: gamma_only

  real(DP) :: factor, tau (3, nat), g (3, ngm), gg (ngm), zv (ntyp), &
       at (3, 3), bg (3, 3), omega, gcutm, alat
  ! input: the coordinates of the atoms
  ! input: the G vectors
  ! input: the moduli of G vectors
  ! input: the charge of the atoms
  ! input: the direct lattice vectors
  ! input: the reciprocal lattice vectors
  ! input: the volume of the unit cell
  ! input: cut-off of g vectors
  ! input: the edge of the cell
  !
  complex(DP) :: strf (ngm, ntyp)
  ! input: the structure factor on the potential
  !
  real(DP) :: forceion (3, nat)
  ! output: the ewald part of the forces
  !
  integer, parameter :: mxr=50
  ! the maximum number of R vectors

  integer :: ig, n, na, nb, nt, nrm, ipol
  ! counter on G vectos
  ! counter on r vectors
  ! counter on atoms
  ! counter on atoms
  ! counter on atomic types
  ! the number of R vectors for real space su
  ! counter on polarization

  real(DP) :: sumnb, arg, tpiba2, alpha, dtau (3), r (3, mxr), &
       r2 (mxr), rmax, rr, charge, upperbound, fact
  ! auxiliary variable for speed
  ! the argument of the exponential
  ! 2 pi /alat
  ! the alpha parameter
  ! the difference of two tau
  ! the position of the atoms in the shell
  ! the square of r
  ! the maximum r
  ! the modulus of the r vectors
  ! the total charge
  ! used to determine alpha

  complex(DP), allocatable :: aux (:)
  ! auxiliary space
  real(DP), external :: qe_erfc
  !
  forceion(:,:) = 0.d0
  tpiba2 = (tpi / alat) **2
  charge = 0.d0
  do na = 1, nat
     charge = charge+zv (ityp (na) )
  enddo
  !
  ! choose alpha in order to have convergence in the sum over G
  ! upperbound is a safe upper bound for the error ON THE ENERGY
  !
  alpha = 1.1d0
10 alpha = alpha - 0.1d0
  if (alpha.eq.0.d0) call errore ('force_ew', 'optimal alpha not found', 1)
  upperbound = e2 * charge**2 * sqrt (2.d0 * alpha / tpi) * &
       qe_erfc ( sqrt (tpiba2 * gcutm / 4.d0 / alpha) )
  if (upperbound > 1.0d-6) goto 10
  !
  ! G-space sum here
  !
     allocate(aux(ngm))
     aux(:) = (0.d0, 0.d0)

     do nt = 1, ntyp
        do ig = gstart, ngm
           aux (ig) = aux (ig) + zv (nt) * CONJG(strf (ig, nt) )
        enddo
     enddo
     do ig = gstart, ngm
        aux (ig) = aux (ig) * exp ( - gg (ig) * tpiba2 / alpha / 4.d0) &
             / (gg (ig) * tpiba2)
     enddo
     if (gamma_only) then
        fact = 4.d0
     else
        fact = 2.d0
     end if
     do na = 1, nat
        do ig = gstart, ngm
           arg = tpi * (g (1, ig) * tau (1, na) + g (2, ig) * tau (2, na) &
                + g (3, ig) * tau (3, na) )
           sumnb = cos (arg) * AIMAG (aux(ig)) - sin (arg) *  DBLE (aux(ig) )
           forceion (1, na) = forceion (1, na) + g (1, ig) * sumnb
           forceion (2, na) = forceion (2, na) + g (2, ig) * sumnb
           forceion (3, na) = forceion (3, na) + g (3, ig) * sumnb
        enddo
        do ipol = 1, 3
           forceion (ipol, na) = - zv (ityp (na) ) * fact * e2 * tpi**2 / &
                omega / alat * forceion (ipol, na)
        enddo
     enddo
     deallocate (aux)
  if (gstart == 1) goto 100
  !
  ! R-space sum here (only for the processor that contains G=0)
  !
  rmax = 5.d0 / (sqrt (alpha) * alat)
  !
  ! with this choice terms up to ZiZj*erfc(5) are counted (erfc(5)=2x10^-1
  !
  do na = 1, nat
     do nb = 1, nat
        if (nb.eq.na) goto 50
         dtau (:) = tau (:, na) - tau (:, nb)
        !
        ! generates nearest-neighbors shells r(i)=R(i)-dtau(i)
        !
        call rgen (dtau, rmax, mxr, at, bg, r, r2, nrm)
        do n = 1, nrm
           rr = sqrt (r2 (n) ) * alat
           factor = zv (ityp (na) ) * zv (ityp (nb) ) * e2 / rr**2 * &
                (qe_erfc (sqrt (alpha) * rr) / rr + &
                sqrt (8.0d0 * alpha / tpi) * exp ( - alpha * rr**2) ) * alat
           do ipol = 1, 3
              forceion (ipol, na) = forceion (ipol, na) - factor * r (ipol, n)
           enddo
        enddo
50      continue
     enddo
  enddo
100 continue
  !
  CALL mp_sum( forceion, intra_bgrp_comm )
  !
  return
end subroutine force_ew

