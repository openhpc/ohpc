!
! Copyright (C) 2001-2011 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE weights()
  !----------------------------------------------------------------------------
  !
  ! ... calculates weights of Kohn-Sham orbitals used in calculation of rho,
  ! ... Fermi energies, HOMO and LUMO, "-TS" term (gaussian)
  !
  USE kinds,                ONLY : DP
  USE ener,                 ONLY : demet, ef, ef_up, ef_dw
  USE fixed_occ,            ONLY : f_inp, tfixed_occ
  USE klist,                ONLY : lgauss, degauss, ngauss, nks, &
                                   nkstot, wk, xk, nelec, nelup, neldw, &
                                   two_fermi_energies
  USE ktetra,               ONLY : ltetra, ntetra, tetra
  USE lsda_mod,             ONLY : nspin, current_spin, isk
  USE wvfct,                ONLY : nbnd, wg, et
  USE mp_global,            ONLY : intra_image_comm, inter_pool_comm
  USE mp,                   ONLY : mp_bcast, mp_sum
  USE io_global,            ONLY : ionode, ionode_id
  !
  IMPLICIT NONE
  !
  ! ... local variables
  !
  INTEGER :: ibnd, ik ! counters: bands, k-points
  real (DP) demet_up, demet_dw
  !
  demet         = 0.D0
  !
        !
        ! ... calculate weights for the insulator case
        !
        IF ( two_fermi_energies ) THEN
           !
           CALL iweights( nks, wk, nbnd, nelup, et, ef_up, wg, 1, isk )
           CALL iweights( nks, wk, nbnd, neldw, et, ef_dw, wg, 2, isk )
           !
           ! the following line to prevent NaN in Ef
           !
           ef = ( ef_up + ef_dw ) / 2.0_dp
           !
        ELSE
           !
           CALL iweights( nks, wk, nbnd, nelec, et, ef,    wg, 0, isk )
           !
        END IF
        !
     !
     ! ... collect all weights on the first pool;
     ! ... not needed for calculation but useful for printout 
     !
     CALL poolrecover( wg, nbnd, nkstot, nks )
     !
  !
  RETURN
  !
END SUBROUTINE weights
