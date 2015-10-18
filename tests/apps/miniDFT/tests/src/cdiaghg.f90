!
! Copyright (C) 2001-2006 Quantum ESPRESSO group
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
!
!----------------------------------------------------------------------------
SUBROUTINE pcdiaghg( n, h, s, ldh, e, v, desc )
  !----------------------------------------------------------------------------
  !
  ! ... calculates eigenvalues and eigenvectors of the generalized problem
  ! ... Hv=eSv, with H hermitean matrix, S overlap matrix.
  ! ... On output both matrix are unchanged
  !
  ! ... Parallel version, with full data distribution
  !
  USE kinds,            ONLY : DP
  USE mp,               ONLY : mp_bcast
  USE mp_global,        ONLY : root_bgrp, intra_bgrp_comm
  USE zhpev_module,     ONLY : pzhpev_drv, zhpev_drv
  USE descriptors,      ONLY : la_descriptor
!!$  USE parallel_toolkit, ONLY : zsqmdst, zsqmcll
  USE mp_global,        ONLY : ortho_cntx, me_blacs, np_ortho, me_ortho
  USE zhpev_module,     ONLY : pzheevd_drv

  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: n, ldh
    ! dimension of the matrix to be diagonalized
    ! leading dimension of h, as declared in the calling pgm unit
  COMPLEX(DP), INTENT(INOUT) :: h(ldh,ldh), s(ldh,ldh)
    ! actually intent(in) but compilers don't know and complain
    ! matrix to be diagonalized
    ! overlap matrix
  REAL(DP), INTENT(OUT) :: e(n)
    ! eigenvalues
  COMPLEX(DP), INTENT(OUT) :: v(ldh,ldh)
    ! eigenvectors (column-wise)
  TYPE(la_descriptor), INTENT(IN) :: desc
  !
  INTEGER             :: nx
  INTEGER             :: descsca( 16 ), info
    ! local block size
  COMPLEX(DP), ALLOCATABLE :: ss(:,:), hh(:,:), tt(:,:)
    ! work space used only in parallel diagonalization
  !
  ! ... input s and h are copied so that they are not destroyed
  !
  CALL start_clock( 'cdiaghg' )
  !
  IF( desc%active_node > 0 ) THEN
     !
     nx   = desc%nrcx
     !
     IF( nx /= ldh ) &
        CALL errore(" pcdiaghg ", " inconsistent leading dimension ", ldh )
     !
     ALLOCATE( hh( nx, nx ) )
     ALLOCATE( ss( nx, nx ) )
     !
     hh(1:nx,1:nx) = h(1:nx,1:nx)
     ss(1:nx,1:nx) = s(1:nx,1:nx)
     !
  END IF

  CALL start_clock( 'cdiaghg:choldc' )
  !
  ! ... Cholesky decomposition of sl ( L is stored in sl )
  !
  IF( desc%active_node > 0 ) THEN
     !
     CALL descinit( descsca, n, n, desc%nrcx, desc%nrcx, 0, 0, ortho_cntx, SIZE( ss, 1 ) , info )
     !
     IF( info /= 0 ) CALL errore( ' cdiaghg ', ' desckinit ', ABS( info ) )
     !

     CALL pzpotrf( 'L', n, ss, 1, 1, descsca, info )

     IF( info /= 0 ) CALL errore( ' cdiaghg ', ' problems computing cholesky ', ABS( info ) )
     !
  END IF
  !
  CALL stop_clock( 'cdiaghg:choldc' )
  !
  ! ... L is inverted ( sl = L^-1 )
  !
  CALL start_clock( 'cdiaghg:inversion' )
  !
  IF( desc%active_node > 0 ) THEN
     !
     !CALL clear_upper_tr( ss )
     ! set to zero the upper triangle of ss
     !
     CALL sqr_zsetmat( 'U', n, ZERO, ss, size(ss,1), desc )
     !
     CALL pztrtri( 'L', 'N', n, ss, 1, 1, descsca, info )
     !
     IF( info /= 0 ) CALL errore( ' cdiaghg ', ' problems computing inverse ', ABS( info ) )
     !
  END IF
  !
  CALL stop_clock( 'cdiaghg:inversion' )
  !
  ! ... vl = L^-1*H
  !
  CALL start_clock( 'cdiaghg:paragemm' )
  !
  IF( desc%active_node > 0 ) THEN
     !
     CALL sqr_zmm_cannon( 'N', 'N', n, ONE, ss, nx, hh, nx, ZERO, v, nx, desc )
     !
  END IF
  !
  ! ... hl = ( L^-1*H )*(L^-1)^T
  !
  IF( desc%active_node > 0 ) THEN
     !
     CALL sqr_zmm_cannon( 'N', 'C', n, ONE, v, nx, ss, nx, ZERO, hh, nx, desc )
     !
     ! ensure that "hh" is really Hermitian, it is sufficient to set the diagonal
     ! properly, because only the lower triangle of hh will be used
     ! 
     CALL sqr_zsetmat( 'H', n, ZERO, hh, size(hh,1), desc )
     !
  END IF
  !
  CALL stop_clock( 'cdiaghg:paragemm' )
  !
  !
  IF ( desc%active_node > 0 ) THEN
     ! 

     !
     CALL pzheevd_drv( .true., n, desc%nrcx, hh, e, ortho_cntx )
     !
     !
  END IF
  !
  ! ... v = (L^T)^-1 v
  !
  CALL start_clock( 'cdiaghg:paragemm' )
  !
  IF ( desc%active_node > 0 ) THEN
     !
     CALL sqr_zmm_cannon( 'C', 'N', n, ONE, ss, nx, hh, nx, ZERO, v, nx, desc )
     !
  END IF
  !
  CALL mp_bcast( e, root_bgrp, intra_bgrp_comm )
  !
  CALL stop_clock( 'cdiaghg:paragemm' )
  !
  IF ( desc%active_node > 0 ) THEN
     DEALLOCATE( ss, hh )
  END IF
  !
  CALL stop_clock( 'cdiaghg' )
  !
  RETURN
  !
  !
END SUBROUTINE pcdiaghg
!
