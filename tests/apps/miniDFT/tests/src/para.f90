!
! Copyright (C) 2001-2006 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!
! ... here are all parallel subroutines (wrappers to MPI calls) used 
! ... by the PWscf code
!
!----------------------------------------------------------------------------
SUBROUTINE poolscatter( nsize, nkstot, f_in, nks, f_out )
  !----------------------------------------------------------------------------
  !
  ! ... This routine scatters a quantity ( typically the eigenvalues )
  ! ... among the pools. 
  ! ... On input, f_in is required only on the first node of the first pool. 
  ! ... f_in and f_out may coincide.
  ! ... Not a smart implementation!
  !
  USE kinds,     ONLY : DP
  USE mp_global, ONLY : intra_pool_comm, inter_pool_comm, &
                        my_pool_id, npool, me_pool, root_pool, kunit
  USE mp,        ONLY : mp_bcast  
  !
  IMPLICIT NONE
  !
  INTEGER :: nsize, nkstot, nks
    ! first dimension of vectors f_in and f_out
    ! number of k-points per pool
    ! total number of k-points
  REAL(DP) :: f_in(nsize,nkstot), f_out(nsize,nks)
    ! input  ( contains values for all k-point )
    ! output ( only for k-points of mypool )
  !
  !
  INTEGER :: rest, nbase
    ! the rest of the integer division nkstot / npo
    ! the position in the original list
  !
  !
  ! ... copy from the first node of the first pool
  ! ... to the first node of all the other pools
  !
  IF ( me_pool == root_pool ) &
     CALL mp_bcast( f_in, root_pool, inter_pool_comm )
  !
  ! ... distribute the vector on the first node of each pool
  !
  rest = nkstot / kunit - ( nkstot / kunit / npool ) * npool 
  !
  nbase = nks * my_pool_id
  !
  IF ( ( my_pool_id + 1 ) > rest ) nbase = nbase + rest * kunit
  !
  f_out(:,1:nks) = f_in(:,(nbase+1):(nbase+nks))
  !
  ! ... copy from the first node of every pool
  ! ... to the other nodes of every pool
  !
  CALL mp_bcast( f_out, root_pool, intra_pool_comm )
  !
  !
  RETURN
  !
END SUBROUTINE poolscatter
!
! ... other parallel subroutines
!
!-----------------------------------------------------------------------
SUBROUTINE poolrecover( vec, length, nkstot, nks )
  !----------------------------------------------------------------------- 
  !
  ! ... recovers on the first processor of the first pool a 
  ! ... distributed vector
  !
  USE kinds,     ONLY : DP
  USE mp_global, ONLY : inter_pool_comm, intra_image_comm, &
                        npool, me_pool, root_pool, my_pool_id, kunit
  USE mp,        ONLY : mp_barrier  
  USE parallel_include    
  !
  IMPLICIT NONE
  !
  INTEGER  :: length, nks, nkstot
  REAL(DP) :: vec(length,nkstot)
  !
  !
  INTEGER :: status(MPI_STATUS_SIZE)
  INTEGER :: i, nks1, rest, fine, nbase, info
  !
  !
  IF ( npool <= 1 ) RETURN
  !
  IF ( MOD( nkstot, kunit ) /= 0 ) &
       CALL errore( 'poolrecover', 'nkstot/kunit is not an integer', nkstot )
  !
  nks1 = kunit * ( nkstot / kunit / npool )
  !
  rest = ( nkstot - nks1 * npool ) / kunit
  !
  CALL mp_barrier( intra_image_comm )
  !
  IF ( me_pool == root_pool .AND. my_pool_id > 0 ) THEN
     !
     CALL MPI_SEND( vec, (length*nks), MPI_DOUBLE_PRECISION, 0, 17, &
                    inter_pool_comm, info )
     !     
     CALL errore( 'poolrecover', 'info<>0 in send', info )
     !
  END IF
  !
  DO i = 2, npool
     !
     IF ( i <= rest ) THEN
        !
        fine = nks1 + kunit
        !
        nbase = ( nks1 + kunit ) * ( i - 1 )
        !
     ELSE
        !
        fine = nks1
        !
        nbase = rest * (nks1 + kunit) + (i - 1 - rest) * nks1
        !
     END IF
     !
     IF ( me_pool == root_pool .AND. my_pool_id == 0 ) THEN
        !
        CALL MPI_RECV( vec(1,nbase+1), (length*fine), MPI_DOUBLE_PRECISION, &
                       (i-1), 17, inter_pool_comm, status, info )
        !
        CALL errore( 'poolrecover', 'info<>0 in recv', info )
        !
     END IF
     !
  END DO
  !
  !
  RETURN
  !
END SUBROUTINE poolrecover
!
!------------------------------------------------------------------------
SUBROUTINE ipoolrecover( ivec, length, nkstot, nks )
  !------------------------------------------------------------------------
  !
  ! ... as above, for an integer vector
  !
  USE mp_global, ONLY : inter_pool_comm, intra_image_comm, &
                        npool, me_pool, root_pool, my_pool_id, kunit
  USE mp,        ONLY : mp_barrier  
  USE parallel_include    
  !
  IMPLICIT NONE
  !
  INTEGER :: length, nks, nkstot
  INTEGER :: ivec(length,nkstot)
  !
  !
  INTEGER :: status(MPI_STATUS_SIZE)
  INTEGER :: i, nks1, rest, fine, nbase, info
  !
  !
  IF ( npool <= 1 ) RETURN
  !
  IF ( MOD( nkstot, kunit ) /= 0 ) & 
       CALL errore( 'poolrecover', 'nkstot/kunit is not an integer', nkstot )
  !
  nks1 = kunit * ( nkstot / kunit / npool )
  !
  rest = ( nkstot - nks1 * npool ) / kunit
  !
  CALL mp_barrier( intra_image_comm )
  !
  IF ( me_pool == root_pool .AND. my_pool_id > 0 ) THEN
     !
     CALL MPI_SEND( ivec, (length*nks), MPI_INTEGER, 0, 17, &
                    inter_pool_comm, info )
     !
     CALL errore( 'ipoolrecover', 'info<>0 in send', info )
     !
  END IF
  !
  DO i = 2, npool
     !
     IF ( i <= rest ) THEN
        !
        fine = nks1 + kunit
        !
        nbase = ( nks1 + kunit ) * ( i - 1 )
        !
     ELSE
        !
        fine = nks1
        !
        nbase = rest * ( nks1 + kunit ) + ( i - 1 - rest ) * nks1
        !
     END IF
     !
     IF ( me_pool == root_pool .AND. my_pool_id == 0 ) THEN
        !
        CALL MPI_RECV( ivec(1,nbase+1), (length*fine), MPI_INTEGER, &
                       (i-1), 17, inter_pool_comm, status, info )
        !
        CALL errore( 'ipoolrecover', 'info<>0 in recv', info )
        !
     END IF
     !
  END DO
  !
  !
  RETURN
  !
END SUBROUTINE ipoolrecover
