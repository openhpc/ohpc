
! Copyright (C) 2001-2011 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE c_bands( iter, ik_, dr2 )
  !----------------------------------------------------------------------------
  !
  ! ... Driver routine for Hamiltonian diagonalization routines
  ! ... It reads the Hamiltonian and an initial guess of the wavefunctions
  ! ... from a file and computes initialization quantities for the
  ! ... diagonalization routines.
  !
  USE kinds,                ONLY : DP
  USE io_global,            ONLY : stdout
  USE io_files,             ONLY : iunigk, nwordatwfc, iunsat, iunwfc, &
                                   nwordwfc
  USE buffers,              ONLY : get_buffer, save_buffer
  USE klist,                ONLY : nkstot, nks, xk, ngk
  USE uspp,                 ONLY : vkb, nkb
  USE gvect,                ONLY : g
  USE wvfct,                ONLY : et, nbnd, npwx, igk, npw, current_k
  USE control_flags,        ONLY : ethr, isolve, io_level
  USE lsda_mod,             ONLY : current_spin, lsda, isk
  USE wavefunctions_module, ONLY : evc
  USE mp_global,            ONLY : inter_pool_comm
  USE mp,                   ONLY : mp_sum
  !
  IMPLICIT NONE
  !
  ! ... First the I/O variables
  !
  INTEGER :: ik_, iter
  ! k-point already done
  ! current iterations
  REAL(DP),INTENT(IN) :: dr2
  ! current accuracy of self-consistency
  !
  ! ... local variables
  !
  REAL(DP) :: avg_iter
  ! average number of H*psi products
  INTEGER :: ik
  ! counter on k points
  !
  IF ( ik_ == nks ) THEN
     !
     ik_ = 0
     !
     RETURN
     !
  END IF
  !
  CALL start_clock( 'c_bands' )
  !
  IF ( isolve == 0 ) THEN
     !
     WRITE( stdout, '(5X,"Davidson diagonalization with overlap")' )
     !
  ELSE IF ( isolve == 1 ) THEN
     !
     WRITE( stdout, '(5X,"CG style diagonalization")')
     !
  ELSE
     !
     CALL errore ( 'c_bands', 'invalid type of diagonalization', isolve)
     !!! WRITE( stdout, '(5X,"DIIS style diagonalization")')
     !
  END IF
  !
  avg_iter = 0.D0
  !
#ifdef __IGKIO
  if ( nks > 1 ) REWIND( iunigk )  !BMA: eliminate fs activity for miniDFT
#endif
  !
  ! ... For each k point diagonalizes the hamiltonian
  !
  k_loop: DO ik = 1, nks
     !
     current_k = ik
     !
     IF ( lsda ) current_spin = isk(ik)
     !
     ! ... Reads the list of indices k+G <-> G of this k point
     !
#ifdef __IGKIO
     IF ( nks > 1 ) READ( iunigk ) igk !BMA: eliminate fs activity for miniDFT
#endif
     !
     npw = ngk(ik)
     !
     ! ... do not recalculate k-points if restored from a previous run
     !
     IF ( ik <= ik_ ) THEN
        !
        CYCLE k_loop
        !
     END IF
     !
     ! ... various initializations
     !
     IF ( nkb > 0 ) CALL init_us_2( npw, igk, xk(1,ik), vkb )
     !
     ! ... kinetic energy
     !
     call g2_kin( ik )
     !
     ! ... read in wavefunctions from the previous iteration
     !
#ifdef __IGKIO
     IF ( nks > 1 .OR. (io_level > 1) ) &
          CALL get_buffer ( evc, nwordwfc, iunwfc, ik )
#endif
     ! ... Needed for LDA+U
     !
     !
     ! ... diagonalization of bands for k-point ik
     !
     call diag_bands ( iter, ik, avg_iter )
     !
     ! ... save wave-functions to be used as input for the
     ! ... iterative diagonalization of the next scf iteration
     ! ... and for rho calculation
     !
#ifdef __IGKIO
     IF ( nks > 1 .OR. (io_level > 1) ) &
          CALL save_buffer ( evc, nwordwfc, iunwfc, ik )
#endif
     ! ... save restart information
     !
     IF ( io_level > 1 ) CALL save_in_cbands( iter, ik, dr2 )
     !
  END DO k_loop
  !
  ik_ = 0
  !
  CALL mp_sum( avg_iter, inter_pool_comm )
  !
  avg_iter = avg_iter / nkstot
  !
  WRITE( stdout, &
       '( 5X,"ethr = ",1PE9.2,",  avg # of iterations =",0PF5.1 )' ) &
       ethr, avg_iter
  !
  CALL stop_clock( 'c_bands' )
  !
  RETURN
  !
END SUBROUTINE c_bands
!
!----------------------------------------------------------------------------
SUBROUTINE diag_bands( iter, ik, avg_iter )
  !----------------------------------------------------------------------------
  !
  ! ... Driver routine for diagonalization at each k-point
  ! ... Two types of iterative diagonalizations are currently used:
  ! ... a) Davidson algorithm (all-band)
  ! ... b) Conjugate Gradient (band-by-band)
  ! ...
  ! ... internal procedures :
  !
  ! ... c_bands_gamma()   : optimized algorithms for gamma sampling of the BZ
  ! ...                     (real Hamiltonian)
  ! ... c_bands_k()       : general algorithm for arbitrary BZ sampling
  ! ...                     (complex Hamiltonian)
  ! ... test_exit_cond()  : the test on the iterative diagonalization
  !
  !
  USE kinds,                ONLY : DP
  USE io_global,            ONLY : stdout
  USE io_files,             ONLY : nwordwfc, iunefieldp, iunefieldm
  USE uspp,                 ONLY : vkb, nkb, okvan
  USE gvect,                ONLY : gstart
  USE wvfct,                ONLY : g2kin, nbndx, et, nbnd, npwx, npw, &
       current_k, btype
  USE control_flags,        ONLY : ethr, lscf, max_cg_iter, isolve, istep
  USE wavefunctions_module, ONLY : evc
  USE g_psi_mod,            ONLY : h_diag, s_diag
  USE scf,                  ONLY : v_of_0
  USE becmod,               ONLY : bec_type, becp, calbec, &
                                   allocate_bec_type, deallocate_bec_type
  USE klist,                ONLY : nks
  USE mp_global,            ONLY : nproc_bgrp, intra_bgrp_comm
  !
  IMPLICIT NONE
  !
  integer, parameter :: npol=1 !substitute for noncollin_module%npol
  INTEGER, INTENT(IN)  :: iter, ik
  !
  REAL (KIND=DP), INTENT(INOUT) :: avg_iter
  !
  REAL (KIND=DP) :: cg_iter
  ! (weighted) number of iterations in Conjugate-Gradient
  INTEGER :: ig, dav_iter, ntry, notconv
  ! number of iterations in Davidson
  ! number or repeated call to diagonalization in case of non convergence
  ! number of notconverged elements
  INTEGER :: ierr
  !
  LOGICAL :: lrot
  ! .TRUE. if the wfc have already be rotated
  !
  ALLOCATE( h_diag( npwx, npol ), STAT=ierr )
  IF( ierr /= 0 ) &
     CALL errore( ' diag_bands ', ' cannot allocate h_diag ', ABS(ierr) )
  !
  ALLOCATE( s_diag( npwx, npol ), STAT=ierr )
  IF( ierr /= 0 ) &
     CALL errore( ' diag_bands ', ' cannot allocate s_diag ', ABS(ierr) )
  !
  ! ... allocate space for <beta_i|psi_j> - used in h_psi and s_psi
  !
  IF ( nbndx > npwx*nproc_bgrp ) &
     CALL errore ( 'diag_bands', 'too many bands, or too few plane waves',1)
  !
  CALL allocate_bec_type ( nkb, nbnd, becp, intra_bgrp_comm )
  !
     !
     CALL c_bands_k()
     !
  !
  ! ... deallocate work space
  !
  CALL deallocate_bec_type ( becp )
  DEALLOCATE( s_diag )
  DEALLOCATE( h_diag )
  !
  IF ( notconv > MAX( 5, nbnd / 4 ) ) THEN
     !
     CALL errore( 'c_bands', &
          & 'too many bands are not converged', 1 )
     !
  ELSE IF ( notconv > 0 ) THEN
     !
     WRITE( stdout, '(5X,"c_bands: ",I2, &
               &   " eigenvalues not converged")' ) notconv
     !
  END IF
  !
  RETURN
  !
CONTAINS
  !
  ! ... internal procedures
  !
  !-----------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------
  SUBROUTINE c_bands_k()
    !-----------------------------------------------------------------------
    !
    ! ... Complex Hamiltonian diagonalization
    !
    IMPLICIT NONE
    !
    ! ... here the local variables
    !
    INTEGER :: ipol, ierr
    !
    !
    IF ( isolve == 1 ) THEN
       !
       ! ... Conjugate-Gradient diagonalization
       !
       ! ... h_diag is the precondition matrix
       !
       h_diag = 1.D0
       !
       FORALL( ig = 1 : npwx )
          !
          h_diag(ig,:) = 1.D0 + g2kin(ig) + &
             SQRT( 1.D0 + ( g2kin(ig) - 1.D0 )**2 )
          !
       END FORALL
       !
       ntry = 0
       !
       CG_loop : DO
          !
          lrot = ( iter == 1 .AND. istep ==0 .AND. ntry == 0 )
          !
          IF ( .NOT. lrot ) THEN
             !
             CALL rotate_wfc ( npwx, npw, nbnd, gstart, nbnd, &
                               evc, npol, okvan, evc, et(1,ik) )
             !
             avg_iter = avg_iter + 1.D0
             !
          END IF
          !
          CALL ccgdiagg( npwx, npw, nbnd, npol, evc, et(1,ik), btype(1,ik), &
               h_diag, ethr, max_cg_iter, .NOT. lscf, notconv, cg_iter )
          !
          avg_iter = avg_iter + cg_iter
          !
          ntry = ntry + 1
          !
          ! ... exit condition
          !
          IF ( test_exit_cond() ) EXIT  CG_loop
          !
       END DO CG_loop
       !
    ELSE
       !
       ! ... Davidson diagonalization
       !
       ! ... h_diag are the diagonal matrix elements of the
       ! ... hamiltonian used in g_psi to evaluate the correction
       ! ... to the trial eigenvectors
       !
       DO ipol = 1, npol
          !
          h_diag(1:npw, ipol) = g2kin(1:npw) + v_of_0
          !
       END DO
       !
       CALL usnldiag( h_diag, s_diag )
       !
       ntry = 0
       !
       david_loop: DO
          !
          lrot = ( iter == 1 )
          !
             !
             CALL pcegterg( npw, npwx, nbnd, nbndx, npol, evc, ethr, &
                         okvan, et(1,ik), btype(1,ik), &
                         notconv, lrot, dav_iter )
          !
          avg_iter = avg_iter + dav_iter
          !
          ! ... save wave-functions to be used as input for the
          ! ... iterative diagonalization of the next scf iteration
          ! ... and for rho calculation
          !
          ntry = ntry + 1
          !
          ! ... exit condition
          !
          IF ( test_exit_cond() ) EXIT david_loop
          !
       END DO david_loop
       !
    END IF
    !
    !
    RETURN
    !
  END SUBROUTINE c_bands_k
  !
  !-----------------------------------------------------------------------
  FUNCTION test_exit_cond()
    !-----------------------------------------------------------------------
    !
    ! ... this logical function is .TRUE. when iterative diagonalization
    ! ... is converged
    !
    IMPLICIT NONE
    !
    LOGICAL :: test_exit_cond
    !
    !
    test_exit_cond = .NOT. ( ( ntry <= 5 ) .AND. &
         ( ( .NOT. lscf .AND. ( notconv > 0 ) ) .OR. &
         (       lscf .AND. ( notconv > 5 ) ) ) )
    !
  END FUNCTION test_exit_cond
  !
END SUBROUTINE diag_bands
!
