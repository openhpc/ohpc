!
! Copyright (C) 2001-2006 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
SUBROUTINE init_run()
  !----------------------------------------------------------------------------
  !
  USE klist,              ONLY : nkstot, nks
  USE symme,              ONLY : sym_rho_init
  USE wvfct,              ONLY : nbnd, et, wg, btype
  USE control_flags,      ONLY : lmd !!$, gamma_only
  USE cell_base,          ONLY : at, bg
  USE recvec_subs,        ONLY : ggen
  USE dfunct,             ONLY : newd
  !
  IMPLICIT NONE
  !
  !
  CALL start_clock( 'init_run' )
  !
  ! ... calculate limits of some indices, used in subsequent allocations
  !
  CALL pre_init()
  !
  ! ... allocate memory for G- and R-space fft arrays
  !
  CALL allocate_fft()
  !
  ! ... generate reciprocal-lattice vectors and fft indices
  !
  CALL ggen ( .false. , at, bg )
  CALL gshells ( .false. )
  !
  ! ... variable initialization for parallel symmetrization
  !
  CALL sym_rho_init (.false.)
  !
  CALL summary()
  !
#ifndef __IGKIO
  if( nks .ne. 1 )then
     !bma 20May2013 
     !  A small amount of file I/O is required for multiple k-points per pool.
     !  MiniDFT skips this I/O (a feature), which introduces a bug if nks>1
     !  Stop here to count k-points without manually cancelling the job.
     !  To enable multiple k-points per pool, recompile with -D__IGKIO
     write(*,*) "================================"
     write(*,*) "This job uses multiple k-points."
     write(*,*) "Please restart with -npool",nkstot
     stop
  end if
#endif
  !
  ! ... allocate memory for all other arrays (potentials, wavefunctions etc)
  !
  CALL allocate_nlpot()
  CALL allocate_locpot()
  CALL allocate_wfc()
  !
  CALL memory_report()
  !
  ALLOCATE( et( nbnd, nkstot ) , wg( nbnd, nkstot ), btype( nbnd, nkstot ) )
  !
  et(:,:) = 0.D0
  wg(:,:) = 0.D0
  !
  btype(:,:) = 1
  !
  CALL openfil()
  !
  CALL hinit0()
  !
  CALL potinit()
  !
  CALL newd()
  !
  CALL wfcinit()
  !
  !
  CALL stop_clock( 'init_run' )
  !

  RETURN
  !
END SUBROUTINE init_run
  !
!----------------------------------------------------------------------------
SUBROUTINE pre_init()
  !----------------------------------------------------------------------------
  !
  USE ions_base,        ONLY : nat, nsp, ityp
  USE uspp_param,       ONLY : upf, lmaxkb, nh, nhm, nbetam
  USE uspp,             ONLY : nkb, nkbus
  IMPLICIT NONE
  INTEGER :: na, nt, nb
  !
  !     calculate the number of beta functions for each atomic type
  !
  lmaxkb = - 1
  DO nt = 1, nsp
     !
     nh (nt) = 0
     !
     ! do not add any beta projector if pseudo in 1/r fmt (AF)
     IF ( upf(nt)%tcoulombp ) CYCLE 
     !
     DO nb = 1, upf(nt)%nbeta
        nh (nt) = nh (nt) + 2 * upf(nt)%lll(nb) + 1
        lmaxkb = MAX (lmaxkb, upf(nt)%lll(nb) )
     ENDDO
     !
  ENDDO
  !
  ! calculate the maximum number of beta functions
  !
  nhm = MAXVAL (nh (1:nsp))
  nbetam = MAXVAL (upf(:)%nbeta)
  !
  ! calculate the number of beta functions of the solid
  !
  nkb = 0
  nkbus = 0
  do na = 1, nat
     nt = ityp(na)
     nkb = nkb + nh (nt)
  enddo


END SUBROUTINE pre_init
