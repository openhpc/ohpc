!
! Copyright (C) 2001-2007 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
#define ZERO ( 0.D0, 0.D0 )
#define ONE  ( 1.D0, 0.D0 )
!
!----------------------------------------------------------------------------
SUBROUTINE ccgdiagg( npwx, npw, nbnd, npol, psi, e, btype, precondition, &
                     ethr, maxter, reorder, notconv, avg_iter )
  !----------------------------------------------------------------------------
  !
  ! ... "poor man" iterative diagonalization of a complex hermitian matrix
  ! ... through preconditioned conjugate gradient algorithm
  ! ... Band-by-band algorithm with minimal use of memory
  ! ... Calls h_1psi and s_1psi to calculate H|psi> and S|psi>
  ! ... Works for generalized eigenvalue problem (US pseudopotentials) as well
  !
  USE constants,        ONLY : pi
  USE kinds,            ONLY : DP
  USE mp_global,        ONLY : intra_bgrp_comm
  USE mp,               ONLY : mp_sum
  !
  IMPLICIT NONE
  !
  ! ... I/O variables
  !
  INTEGER,     INTENT(IN)    :: npwx, npw, nbnd, npol, maxter
  INTEGER,     INTENT(IN)    :: btype(nbnd)
  REAL(DP),    INTENT(IN)    :: precondition(npwx*npol), ethr
  COMPLEX(DP), INTENT(INOUT) :: psi(npwx*npol,nbnd)
  REAL(DP),    INTENT(INOUT) :: e(nbnd)
  INTEGER,     INTENT(OUT)   :: notconv
  REAL(DP),    INTENT(OUT)   :: avg_iter
  !
  ! ... local variables
  !
  INTEGER                  :: i, j, m, iter, moved
  COMPLEX(DP), ALLOCATABLE :: hpsi(:), spsi(:), lagrange(:), &
                              g(:), cg(:), scg(:), ppsi(:), g0(:)  
  REAL(DP)                 :: psi_norm, a0, b0, gg0, gamma, gg, gg1, &
                              cg0, e0, es(2)
  REAL(DP)                 :: theta, cost, sint, cos2t, sin2t
  LOGICAL                  :: reorder
  INTEGER                  :: kdim, kdmx, kdim2
  REAL(DP)                 :: empty_ethr, ethr_m
  !
  ! ... external functions
  !
  REAL (DP), EXTERNAL :: ddot
  !
  !
  CALL start_clock( 'ccgdiagg' )
  !
  empty_ethr = MAX( ( ethr * 5.D0 ), 1.D-5 )
  !
  IF ( npol == 1 ) THEN
     !
     kdim = npw
     kdmx = npwx
     !
  ELSE
     !
     kdim = npwx * npol
     kdmx = npwx * npol
     !
  END IF
  !
  kdim2 = 2 * kdim
  !
  ALLOCATE( spsi( kdmx ) )
  ALLOCATE( scg(  kdmx ) )
  ALLOCATE( hpsi( kdmx ) )
  ALLOCATE( g(    kdmx ) )
  ALLOCATE( cg(   kdmx ) )
  ALLOCATE( g0(   kdmx ) )
  ALLOCATE( ppsi( kdmx ) )
  !    
  ALLOCATE( lagrange( nbnd ) )
  !
  avg_iter = 0.D0
  notconv  = 0
  moved    = 0
  !
  ! ... every eigenfunction is calculated separately
  !
  DO m = 1, nbnd
     !
     IF ( btype(m) == 1 ) THEN
        !
        ethr_m = ethr
        !
     ELSE
        !
        ethr_m = empty_ethr
        !
     END IF
     !
     spsi     = ZERO
     scg      = ZERO
     hpsi     = ZERO
     g        = ZERO
     cg       = ZERO
     g0       = ZERO
     ppsi     = ZERO
     lagrange = ZERO
     !
     ! ... calculate S|psi>
     !
     CALL s_1psi( npwx, npw, psi(1,m), spsi )
     !
     ! ... orthogonalize starting eigenfunction to those already calculated
     !
     CALL ZGEMV( 'C', kdim, m, ONE, psi, kdmx, spsi, 1, ZERO, lagrange, 1 )
     !
     CALL mp_sum( lagrange( 1:m ), intra_bgrp_comm )
     !
     psi_norm = DBLE( lagrange(m) )
     !
     DO j = 1, m - 1
        !
        psi(:,m)  = psi(:,m) - lagrange(j) * psi(:,j)
        !
        psi_norm = psi_norm - &
                   ( DBLE( lagrange(j) )**2 + AIMAG( lagrange(j) )**2 )
        !
     END DO
     !
     psi_norm = SQRT( psi_norm )
     !
     psi(:,m) = psi(:,m) / psi_norm
     !
     ! ... calculate starting gradient (|hpsi> = H|psi>) ...
     !
     CALL h_1psi( npwx, npw, psi(1,m), hpsi, spsi )
     !
     ! ... and starting eigenvalue (e = <y|PHP|y> = <psi|H|psi>)
     !
     ! ... NB:  ddot(2*npw,a,1,b,1) = REAL( zdotc(npw,a,1,b,1) )
     !
     e(m) = ddot( kdim2, psi(1,m), 1, hpsi, 1 )
     !
     CALL mp_sum( e(m), intra_bgrp_comm )
     !
     ! ... start iteration for this band
     !
     iterate: DO iter = 1, maxter
        !
        ! ... calculate  P (PHP)|y>
        ! ... ( P = preconditioning matrix, assumed diagonal )
        !
        g(:)    = hpsi(:) / precondition(:)
        ppsi(:) = spsi(:) / precondition(:)
        !
        ! ... ppsi is now S P(P^2)|y> = S P^2|psi>)
        !
        es(1) = ddot( kdim2, spsi(1), 1, g(1), 1 )
        es(2) = ddot( kdim2, spsi(1), 1, ppsi(1), 1 )
        !
        CALL mp_sum( es , intra_bgrp_comm )
        !
        es(1) = es(1) / es(2)
        !
        g(:) = g(:) - es(1) * ppsi(:)
        !
        ! ... e1 = <y| S P^2 PHP|y> / <y| S S P^2|y> ensures that 
        ! ... <g| S P^2|y> = 0
        ! ... orthogonalize to lowest eigenfunctions (already calculated)
        !
        ! ... scg is used as workspace
        !
        CALL s_1psi( npwx, npw, g(1), scg(1) )
        !
        CALL ZGEMV( 'C', kdim, ( m - 1 ), ONE, psi, &
                    kdmx, scg, 1, ZERO, lagrange, 1  )
        !
        CALL mp_sum( lagrange( 1:m-1 ), intra_bgrp_comm )
        !
        DO j = 1, ( m - 1 )
           !
           g(:)   = g(:)   - lagrange(j) * psi(:,j)
           scg(:) = scg(:) - lagrange(j) * psi(:,j)
           !
        END DO
        !
        IF ( iter /= 1 ) THEN
           !
           ! ... gg1 is <g(n+1)|S|g(n)> (used in Polak-Ribiere formula)
           !
           gg1 = ddot( kdim2, g(1), 1, g0(1), 1 )
           !
           CALL mp_sum( gg1, intra_bgrp_comm )
           !
        END IF
        !
        ! ... gg is <g(n+1)|S|g(n+1)>
        !
        g0(:) = scg(:)
        !
        g0(:) = g0(:) * precondition(:)
        !
        gg = ddot( kdim2, g(1), 1, g0(1), 1 )
        !
        CALL mp_sum( gg, intra_bgrp_comm )
        !
        IF ( iter == 1 ) THEN
           !
           ! ... starting iteration, the conjugate gradient |cg> = |g>
           !
           gg0 = gg
           !
           cg(:) = g(:)
           !
        ELSE
           !
           ! ... |cg(n+1)> = |g(n+1)> + gamma(n) * |cg(n)>
           !
           ! ... Polak-Ribiere formula :
           !
           gamma = ( gg - gg1 ) / gg0
           gg0   = gg
           !
           cg(:) = cg(:) * gamma
           cg(:) = g + cg(:)
           !
           ! ... The following is needed because <y(n+1)| S P^2 |cg(n+1)> 
           ! ... is not 0. In fact :
           ! ... <y(n+1)| S P^2 |cg(n)> = sin(theta)*<cg(n)|S|cg(n)>
           !
           psi_norm = gamma * cg0 * sint
           !
           cg(:) = cg(:) - psi_norm * psi(:,m)
           !
        END IF
        !
        ! ... |cg> contains now the conjugate gradient
        !
        ! ... |scg> is S|cg>
        !
        CALL h_1psi( npwx, npw, cg(1), ppsi(1), scg(1) )
        !
        cg0 = ddot( kdim2, cg(1), 1, scg(1), 1 )
        !
        CALL mp_sum(  cg0 , intra_bgrp_comm )
        !
        cg0 = SQRT( cg0 )
        !
        ! ... |ppsi> contains now HP|cg>
        ! ... minimize <y(t)|PHP|y(t)> , where :
        ! ...                         |y(t)> = cos(t)|y> + sin(t)/cg0 |cg>
        ! ... Note that  <y|P^2S|y> = 1, <y|P^2S|cg> = 0 ,
        ! ...           <cg|P^2S|cg> = cg0^2
        ! ... so that the result is correctly normalized :
        ! ...                           <y(t)|P^2S|y(t)> = 1
        !
        a0 = 2.D0 * ddot( kdim2, psi(1,m), 1, ppsi(1), 1 ) / cg0
        !
        CALL mp_sum(  a0 , intra_bgrp_comm )
        !
        b0 = ddot( kdim2, cg(1), 1, ppsi(1), 1 ) / cg0**2
        !
        CALL mp_sum(  b0 , intra_bgrp_comm )
        !
        e0 = e(m)
        !
        theta = 0.5D0 * ATAN( a0 / ( e0 - b0 ) )
        !
        cost = COS( theta )
        sint = SIN( theta )
        !
        cos2t = cost*cost - sint*sint
        sin2t = 2.D0*cost*sint
        !
        es(1) = 0.5D0 * (   ( e0 - b0 ) * cos2t + a0 * sin2t + e0 + b0 )
        es(2) = 0.5D0 * ( - ( e0 - b0 ) * cos2t - a0 * sin2t + e0 + b0 )
        !
        ! ... there are two possible solutions, choose the minimum
        !
        IF ( es(2) < es(1) ) THEN
           !
           theta = theta + 0.5D0 * pi
           !
           cost = COS( theta )
           sint = SIN( theta )
           !
        END IF
        !
        ! ... new estimate of the eigenvalue
        !
        e(m) = MIN( es(1), es(2) )
        !
        ! ... upgrade |psi>
        !
        psi(:,m) = cost * psi(:,m) + sint / cg0 * cg(:)
        !
        ! ... here one could test convergence on the energy
        !
        IF ( ABS( e(m) - e0 ) < ethr_m ) EXIT iterate
        !
        ! ... upgrade H|psi> and S|psi>
        !
        spsi(:) = cost * spsi(:) + sint / cg0 * scg(:)
        !
        hpsi(:) = cost * hpsi(:) + sint / cg0 * ppsi(:)
        !
     END DO iterate
     !
     IF ( iter >= maxter ) notconv = notconv + 1
     !
     avg_iter = avg_iter + iter + 1
     !
     ! ... reorder eigenvalues if they are not in the right order
     ! ... ( this CAN and WILL happen in not-so-special cases )
     !
     IF ( m > 1 .AND. reorder ) THEN
        !
        IF ( e(m) - e(m-1) < - 2.D0 * ethr_m ) THEN
           !
           ! ... if the last calculated eigenvalue is not the largest...
           !
           DO i = m - 2, 1, - 1
              !
              IF ( e(m) - e(i) > 2.D0 * ethr_m ) EXIT
              !
           END DO
           !
           i = i + 1
           !
           moved = moved + 1
           !
           ! ... last calculated eigenvalue should be in the 
           ! ... i-th position: reorder
           !
           e0 = e(m)
           !
           ppsi(:) = psi(:,m)
           !
           DO j = m, i + 1, - 1
              !
              e(j) = e(j-1)
              !
              psi(:,j) = psi(:,j-1)
              !
           END DO
           !
           e(i) = e0
           !
           psi(:,i) = ppsi(:)
           !
           ! ... this procedure should be good if only a few inversions occur,
           ! ... extremely inefficient if eigenvectors are often in bad order
           ! ... ( but this should not happen )
           !
        END IF
        !
     END IF
     !
  END DO
  !
  avg_iter = avg_iter / DBLE( nbnd )
  !
  DEALLOCATE( lagrange )
  DEALLOCATE( ppsi )
  DEALLOCATE( g0 )
  DEALLOCATE( cg )
  DEALLOCATE( g )
  DEALLOCATE( hpsi )
  DEALLOCATE( scg )
  DEALLOCATE( spsi )
  !
  CALL stop_clock( 'ccgdiagg' )
  !
  RETURN
  !
END SUBROUTINE ccgdiagg
