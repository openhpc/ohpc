!
! Copyright (C) 2002-2008 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!  Wrapper for MPI implementations that have problems with large messages
!


!  In some MPI implementation the communication subsystem
!  crashes when message exceeds a given size, so we need
!  to break down MPI communications in smaller pieces
!
#define __MSGSIZ_MAX 100000
#define __BCAST_MSGSIZ_MAX 100000

!  Some implementation of MPI (OpenMPI) if it is not well tuned for the given
!  network hardware (InfiniBand) tend to lose performance or get stuck inside
!  collective routines if processors are not well synchronized
!  A barrier fixes the problem
!
#define __USE_BARRIER


!=----------------------------------------------------------------------------=!
!

SUBROUTINE mp_synchronize( gid )
   USE parallel_include  
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: gid
#if defined __MPI && defined __USE_BARRIER
   INTEGER :: ierr
   CALL mpi_barrier( gid, ierr )
   IF( ierr /= 0 ) CALL errore( 'mp_synchronize ', ' error in mpi_barrier ', ierr )
#endif
   RETURN
END SUBROUTINE mp_synchronize


!=----------------------------------------------------------------------------=!
!

   SUBROUTINE BCAST_REAL( array, n, root, gid )
        USE kinds, ONLY: DP
        USE parallel_include  
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n, root, gid
        REAL(DP) :: array( n )
#if defined __MPI
        INTEGER :: msgsiz_max = __BCAST_MSGSIZ_MAX
        INTEGER :: nblk, blksiz, iblk, istart, ierr

#if defined __TRACE
        write(*,*) 'BCAST_REAL IN'
#endif
        IF( n <= 0 ) GO TO 1

#if defined __USE_BARRIER
        CALL mp_synchronize( gid )
#endif

        IF( n <= msgsiz_max ) THEN
           CALL MPI_BCAST( array, n, MPI_DOUBLE_PRECISION, root, gid, ierr )
           IF( ierr /= 0 ) CALL errore( ' bcast_real ', ' error in mpi_bcast 1 ', ierr )
        ELSE
           nblk   = n / msgsiz_max
           blksiz = msgsiz_max
           DO iblk = 1, nblk
              istart = (iblk-1)*msgsiz_max + 1
              CALL MPI_BCAST( array( istart ), blksiz, MPI_DOUBLE_PRECISION, root, gid, ierr )
              IF( ierr /= 0 ) CALL errore( ' bcast_real ', ' error in mpi_bcast 2 ', ierr )
           END DO
           blksiz = MOD( n, msgsiz_max )
           IF( blksiz > 0 ) THEN
              istart = nblk * msgsiz_max + 1
              CALL MPI_BCAST( array( istart ), blksiz, MPI_DOUBLE_PRECISION, root, gid, ierr )
              IF( ierr /= 0 ) CALL errore( ' bcast_real ', ' error in mpi_bcast 3 ', ierr )
           END IF
        END IF

1       CONTINUE
#if defined __TRACE
        write(*,*) 'BCAST_REAL OUT'
#endif

#endif

        RETURN
   END SUBROUTINE BCAST_REAL 


   SUBROUTINE BCAST_INTEGER( array, n, root, gid )
        USE parallel_include  
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n, root, gid
        INTEGER :: array( n )
#if defined __MPI
        INTEGER :: msgsiz_max = __MSGSIZ_MAX
        INTEGER :: nblk, blksiz, iblk, istart, ierr

#if defined __TRACE
        write(*,*) 'BCAST_INTEGER IN'
#endif

        IF( n <= 0 ) GO TO 1

#if defined __USE_BARRIER
        CALL mp_synchronize( gid )
#endif

        IF( n <= msgsiz_max ) THEN
           CALL MPI_BCAST( array, n, MPI_INTEGER, root, gid, ierr )
           IF( ierr /= 0 ) CALL errore( ' bcast_integer ', ' error in mpi_bcast 1 ', ierr )
        ELSE
           nblk   = n / msgsiz_max
           blksiz = msgsiz_max
           DO iblk = 1, nblk
              istart = (iblk-1)*msgsiz_max + 1
              CALL MPI_BCAST( array( istart ), blksiz, MPI_INTEGER, root, gid, ierr )
              IF( ierr /= 0 ) CALL errore( ' bcast_integer ', ' error in mpi_bcast 2 ', ierr )
           END DO
           blksiz = MOD( n, msgsiz_max )
           IF( blksiz > 0 ) THEN
              istart = nblk * msgsiz_max + 1
              CALL MPI_BCAST( array( istart ), blksiz, MPI_INTEGER, root, gid, ierr )
              IF( ierr /= 0 ) CALL errore( ' bcast_integer ', ' error in mpi_bcast 3 ', ierr )
           END IF
        END IF
1       CONTINUE
#if defined __TRACE
        write(*,*) 'BCAST_INTEGER OUT'
#endif
#endif
        RETURN
   END SUBROUTINE BCAST_INTEGER


   SUBROUTINE BCAST_LOGICAL( array, n, root, gid )
        USE parallel_include  
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n, root, gid
        LOGICAL :: array( n )
#if defined __MPI
        INTEGER :: msgsiz_max = __MSGSIZ_MAX
        INTEGER :: nblk, blksiz, iblk, istart, ierr

#if defined __TRACE
        write(*,*) 'BCAST_LOGICAL IN'
#endif

        IF( n <= 0 ) GO TO 1

#if defined __USE_BARRIER
        CALL mp_synchronize( gid )
#endif

        IF( n <= msgsiz_max ) THEN
           CALL MPI_BCAST( array, n, MPI_LOGICAL, root, gid, ierr )
           IF( ierr /= 0 ) CALL errore( ' bcast_logical ', ' error in mpi_bcast 1 ', ierr )
        ELSE
           nblk   = n / msgsiz_max
           blksiz = msgsiz_max
           DO iblk = 1, nblk
              istart = (iblk-1)*msgsiz_max + 1
              CALL MPI_BCAST( array( istart ), blksiz, MPI_LOGICAL, root, gid, ierr )
              IF( ierr /= 0 ) CALL errore( ' bcast_logical ', ' error in mpi_bcast 2 ', ierr )
           END DO
           blksiz = MOD( n, msgsiz_max )
           IF( blksiz > 0 ) THEN
              istart = nblk * msgsiz_max + 1
              CALL MPI_BCAST( array( istart ), blksiz, MPI_LOGICAL, root, gid, ierr )
              IF( ierr /= 0 ) CALL errore( ' bcast_logical ', ' error in mpi_bcast 3 ', ierr )
           END IF
        END IF

1       CONTINUE
#if defined __TRACE
        write(*,*) 'BCAST_LOGICAL OUT'
#endif
#endif
        RETURN
   END SUBROUTINE BCAST_LOGICAL


!
! ... "reduce"-like subroutines
!
!----------------------------------------------------------------------------
SUBROUTINE reduce_base_real( dim, ps, comm, root )
  !----------------------------------------------------------------------------
  !
  ! ... sums a distributed variable ps(dim) over the processors.
  ! ... This version uses a fixed-length buffer of appropriate (?) dim
  !
  USE kinds, ONLY : DP
  USE parallel_include  
  !
  IMPLICIT NONE
  !
  INTEGER,  INTENT(IN)    :: dim     ! size of the array
  REAL(DP)                :: ps(dim) ! array whose elements have to be reduced
  INTEGER,  INTENT(IN)    :: comm    ! communicator
  INTEGER,  INTENT(IN)    :: root    ! if root <  0 perform a reduction to all procs
                                     ! if root >= 0 perform a reduce only to root proc.
  !
#if defined (__MPI)  
  !
  INTEGER            :: info, n, nbuf, nproc, myid
  INTEGER, PARAMETER :: maxb = __MSGSIZ_MAX
  !
  REAL(DP) :: buff(maxb)  
  ! the use of the common here could help the transfer of data to the network device
  COMMON / mp_base_real / buff
  !
#if defined __TRACE
  write(*,*) 'reduce_base_real IN'
#endif

  CALL mpi_comm_size( comm, nproc, info )
  IF( info /= 0 ) CALL errore( 'reduce_base_real', 'error in mpi_comm_size', info )

  CALL mpi_comm_rank( comm, myid, info )
  IF( info /= 0 ) CALL errore( 'reduce_base_real', 'error in mpi_comm_rank', info )
  !
  IF ( dim <= 0 .OR. nproc <= 1 ) GO TO 1  ! go to the end of the subroutine
  !
  ! ... synchronize processes
  !
#if defined __USE_BARRIER
  CALL mp_synchronize( comm )
#endif
  !
  nbuf = dim / maxb
  !
  DO n = 1, nbuf
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_DOUBLE_PRECISION, MPI_SUM, root, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_real', 'error in mpi_reduce 1', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_DOUBLE_PRECISION, MPI_SUM, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_real', 'error in mpi_allreduce 1', info )
     END IF
     !                    
     IF( root < 0 ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     ELSE IF( root == myid ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     END IF
     !
  END DO
  !
  ! ... possible remaining elements < maxb
  !
  IF ( ( dim - nbuf * maxb ) > 0 ) THEN
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_DOUBLE_PRECISION, MPI_SUM, root, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_real', 'error in mpi_reduce 2', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_DOUBLE_PRECISION, MPI_SUM, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_real', 'error in mpi_allreduce 2', info )
     END IF
     !
     IF( root < 0 ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     ELSE IF( root == myid ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     END IF
     !
  END IF
  !
1 CONTINUE
  !
#if defined __TRACE
  write(*,*) 'reduce_base_real OUT'
#endif
  !
#endif
  !
  RETURN
  !
END SUBROUTINE reduce_base_real
!
!
!
!----------------------------------------------------------------------------
SUBROUTINE reduce_base_integer( dim, ps, comm, root )
  !----------------------------------------------------------------------------
  !
  ! ... sums a distributed variable ps(dim) over the processors.
  ! ... This version uses a fixed-length buffer of appropriate (?) dim
  !
  USE kinds, ONLY : DP
  USE parallel_include  
  !
  IMPLICIT NONE
  !
  INTEGER,  INTENT(IN)    :: dim
  INTEGER                 :: ps(dim)
  INTEGER,  INTENT(IN)    :: comm    ! communicator
  INTEGER,  INTENT(IN)    :: root    ! if root <  0 perform a reduction to all procs
                                     ! if root >= 0 perform a reduce only to root proc.
  !
#if defined (__MPI)  
  !
  INTEGER            :: info, n, nbuf, nproc, myid
  INTEGER, PARAMETER :: maxb = __MSGSIZ_MAX
  !
  INTEGER :: buff(maxb)  
  COMMON / mp_base_integer / buff
  !
#if defined __TRACE
  write(*,*) 'reduce_base_integer IN'
#endif
  !
  CALL mpi_comm_size( comm, nproc, info )
  IF( info /= 0 ) CALL errore( 'reduce_base_integer', 'error in mpi_comm_size', info )

  CALL mpi_comm_rank( comm, myid, info )
  IF( info /= 0 ) CALL errore( 'reduce_base_integer', 'error in mpi_comm_rank', info )
  !
  IF ( dim <= 0 .OR. nproc <= 1 ) GO TO 1  ! go to the end of the subroutine
  !
  ! ... synchronize processes
  !
#if defined __USE_BARRIER
  CALL mp_synchronize( comm )
#endif
  !
  nbuf = dim / maxb
  !
  DO n = 1, nbuf
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_INTEGER, MPI_SUM, root, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_integer', 'error in mpi_reduce 1', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_INTEGER, MPI_SUM, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_integer', 'error in mpi_allreduce 1', info )
     END IF
     !
     IF( root < 0 ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     ELSE IF( root == myid ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     END IF
     !
  END DO
  !
  ! ... possible remaining elements < maxb
  !
  IF ( ( dim - nbuf * maxb ) > 0 ) THEN
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_INTEGER, MPI_SUM, root, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_integer', 'error in mpi_reduce 2', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_INTEGER, MPI_SUM, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_integer', 'error in mpi_allreduce 2', info )
     END IF
     !
     IF( root < 0 ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     ELSE IF( root == myid ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     END IF
     !
  END IF
  !
1 CONTINUE
  !
#if defined __TRACE
  write(*,*) 'reduce_base_integer OUT'
#endif
  !
#endif
  !
  RETURN
  !
END SUBROUTINE reduce_base_integer

!
! ... "reduce"-like subroutines
!
!----------------------------------------------------------------------------
SUBROUTINE reduce_base_real_to( dim, ps, psout, comm, root )
  !----------------------------------------------------------------------------
  !
  ! ... sums a distributed variable ps(dim) over the processors,
  ! ... and store the results in variable psout.
  ! ... This version uses a fixed-length buffer of appropriate (?) length
  !
  USE kinds, ONLY : DP
  USE parallel_include  
  !
  IMPLICIT NONE
  !
  INTEGER,  INTENT(IN)  :: dim
  REAL(DP), INTENT(IN)  :: ps(dim)
  REAL(DP)              :: psout(dim)
  INTEGER,  INTENT(IN)  :: comm    ! communecator
  INTEGER,  INTENT(IN)  :: root    ! if root <  0 perform a reduction to all procs
                                   ! if root >= 0 perform a reduce only to root proc.
  !
#if defined (__MPI)  
  !
  INTEGER            :: info, n, nbuf, nproc, myid
  INTEGER, PARAMETER :: maxb = __MSGSIZ_MAX
  !
#if defined __TRACE
  write(*,*) 'reduce_base_real_to IN'
#endif

  CALL mpi_comm_size( comm, nproc, info )
  IF( info /= 0 ) CALL errore( 'reduce_base_real_to', 'error in mpi_comm_size', info )

  CALL mpi_comm_rank( comm, myid, info )
  IF( info /= 0 ) CALL errore( 'reduce_base_real_to', 'error in mpi_comm_rank', info )
  !
  IF ( dim > 0 .AND. nproc <= 1 ) THEN
     psout = ps
  END IF
  IF( dim <= 0 .OR. nproc <= 1 ) GO TO 1 ! go to the end of the subroutine
  !
  ! ... synchronize processes
  !
#if defined __USE_BARRIER
  CALL mp_synchronize( comm )
#endif
  !
  nbuf = dim / maxb
  !
  DO n = 1, nbuf
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+(n-1)*maxb), psout(1+(n-1)*maxb), maxb, MPI_DOUBLE_PRECISION, MPI_SUM, root, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_real_to', 'error in mpi_reduce 1', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+(n-1)*maxb), psout(1+(n-1)*maxb), maxb, MPI_DOUBLE_PRECISION, MPI_SUM, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_real_to', 'error in mpi_allreduce 1', info )
     END IF
     !                    
  END DO
  !
  ! ... possible remaining elements < maxb
  !
  IF ( ( dim - nbuf * maxb ) > 0 ) THEN
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+nbuf*maxb), psout(1+nbuf*maxb), (dim-nbuf*maxb), MPI_DOUBLE_PRECISION, MPI_SUM, root, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_real_to', 'error in mpi_reduce 2', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+nbuf*maxb), psout(1+nbuf*maxb), (dim-nbuf*maxb), MPI_DOUBLE_PRECISION, MPI_SUM, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_real_to', 'error in mpi_allreduce 2', info )
     END IF
     !
  END IF
  !
1 CONTINUE
  !
#if defined __TRACE
  write(*,*) 'reduce_base_real_to OUT'
#endif
  !
#endif
  !
  RETURN
  !
END SUBROUTINE reduce_base_real_to
!
!
!
!----------------------------------------------------------------------------
SUBROUTINE reduce_base_integer_to( dim, ps, psout, comm, root )
  !----------------------------------------------------------------------------
  !
  ! ... sums a distributed integer variable ps(dim) over the processors, and
  ! ... saves the result on the output variable psout.
  ! ... This version uses a fixed-length buffer of appropriate (?) length
  !
  USE kinds, ONLY : DP
  USE parallel_include  
  !
  IMPLICIT NONE
  !
  INTEGER,  INTENT(IN)  :: dim
  INTEGER,  INTENT(IN)  :: ps(dim)
  INTEGER               :: psout(dim)
  INTEGER,  INTENT(IN)  :: comm    ! communecator
  INTEGER,  INTENT(IN)  :: root    ! if root <  0 perform a reduction to all procs
                                     ! if root >= 0 perform a reduce only to root proc.
  !
#if defined (__MPI)  
  !
  INTEGER            :: info, n, nbuf, nproc, myid
  INTEGER, PARAMETER :: maxb = __MSGSIZ_MAX
  !
#if defined __TRACE
  write(*,*) 'reduce_base_integer_to IN'
#endif

  CALL mpi_comm_size( comm, nproc, info )
  IF( info /= 0 ) CALL errore( 'reduce_base_integer_to', 'error in mpi_comm_size', info )

  CALL mpi_comm_rank( comm, myid, info )
  IF( info /= 0 ) CALL errore( 'reduce_base_integer_to', 'error in mpi_comm_rank', info )
  !
  IF ( dim > 0 .AND. nproc <= 1 ) THEN
     psout = ps
  END IF
  IF( dim <= 0 .OR. nproc <= 1 ) GO TO 1 ! go to the end of the subroutine
  !
  ! ... synchronize processes
  !
#if defined __USE_BARRIER
  CALL mp_synchronize( comm )
#endif
  !
  nbuf = dim / maxb
  !
  DO n = 1, nbuf
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+(n-1)*maxb), psout( 1+(n-1)*maxb ), maxb, MPI_INTEGER, MPI_SUM, root, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_integer_to', 'error in mpi_reduce 1', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+(n-1)*maxb), psout( 1+(n-1)*maxb ), maxb, MPI_INTEGER, MPI_SUM, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_integer_to', 'error in mpi_allreduce 1', info )
     END IF
     !                    
  END DO
  !
  ! ... possible remaining elements < maxb
  !
  IF ( ( dim - nbuf * maxb ) > 0 ) THEN
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+nbuf*maxb), psout(1+nbuf*maxb), (dim-nbuf*maxb), MPI_INTEGER, MPI_SUM, root, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_integer_to', 'error in mpi_reduce 2', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+nbuf*maxb), psout(1+nbuf*maxb), (dim-nbuf*maxb), MPI_INTEGER, MPI_SUM, comm, info )
        IF( info /= 0 ) CALL errore( 'reduce_base_integer_to', 'error in mpi_allreduce 2', info )
     END IF
     !
  END IF
  !
1 CONTINUE
  !
#if defined __TRACE
  write(*,*) 'reduce_base_integer_to OUT'
#endif
  !
#endif
  !
  RETURN
  !
END SUBROUTINE reduce_base_integer_to
!
!
!  Parallel MIN and MAX
!

!----------------------------------------------------------------------------
SUBROUTINE parallel_min_integer( dim, ps, comm, root )
  !----------------------------------------------------------------------------
  !
  ! ... compute the minimum of a distributed variable ps(dim) over the processors.
  ! ... This version uses a fixed-length buffer of appropriate (?) dim
  !
  USE kinds, ONLY : DP
  USE parallel_include  
  !
  IMPLICIT NONE
  !
  INTEGER,  INTENT(IN)    :: dim
  INTEGER                 :: ps(dim)
  INTEGER,  INTENT(IN)    :: comm    ! communecator
  INTEGER,  INTENT(IN)    :: root    ! if root <  0 perform a reduction to all procs
                                     ! if root >= 0 perform a reduce only to root proc.
  !
#if defined (__MPI)  
  !
  INTEGER            :: info, n, nbuf, nproc, myid
  INTEGER, PARAMETER :: maxb = __MSGSIZ_MAX
  !
  INTEGER :: buff(maxb)  
  COMMON / mp_base_integer / buff
  !
#if defined __TRACE
  write(*,*) 'parallel_min_integer IN'
#endif
  !
  CALL mpi_comm_size( comm, nproc, info )
  IF( info /= 0 ) CALL errore( 'parallel_min_integer', 'error in mpi_comm_size', info )

  CALL mpi_comm_rank( comm, myid, info )
  IF( info /= 0 ) CALL errore( 'parallel_min_integer', 'error in mpi_comm_rank', info )
  !
  IF ( dim <= 0 .OR. nproc <= 1 ) GO TO 1
  !
  ! ... synchronize processes
  !
#if defined __USE_BARRIER
  CALL mp_synchronize( comm )
#endif
  !
  nbuf = dim / maxb
  !
  DO n = 1, nbuf
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_INTEGER, MPI_MIN, root, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_min_integer', 'error in mpi_reduce 1', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_INTEGER, MPI_MIN, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_min_integer', 'error in mpi_allreduce 1', info )
     END IF
     !
     IF( root < 0 ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     ELSE IF( root == myid ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     END IF
     !
  END DO
  !
  ! ... possible remaining elements < maxb
  !
  IF ( ( dim - nbuf * maxb ) > 0 ) THEN
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_INTEGER, MPI_MIN, root, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_min_integer', 'error in mpi_reduce 2', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_INTEGER, MPI_MIN, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_min_integer', 'error in mpi_allreduce 2', info )
     END IF
     !
     IF( root < 0 ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     ELSE IF( root == myid ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     END IF
     !
  END IF
  !
1 CONTINUE
  !
#if defined __TRACE
  write(*,*) 'parallel_min_integer OUT'
#endif
  !
#endif
  !
  RETURN
  !
END SUBROUTINE parallel_min_integer

!
!----------------------------------------------------------------------------
SUBROUTINE parallel_max_integer( dim, ps, comm, root )
  !----------------------------------------------------------------------------
  !
  ! ... compute the maximum of a distributed variable ps(dim) over the processors.
  ! ... This version uses a fixed-length buffer of appropriate (?) dim
  !
  USE kinds, ONLY : DP
  USE parallel_include  
  !
  IMPLICIT NONE
  !
  INTEGER,  INTENT(IN)    :: dim
  INTEGER                 :: ps(dim)
  INTEGER,  INTENT(IN)    :: comm    ! communecator
  INTEGER,  INTENT(IN)    :: root    ! if root <  0 perform a reduction to all procs
                                     ! if root >= 0 perform a reduce only to root proc.
  !
#if defined (__MPI)  
  !
  INTEGER            :: info, n, nbuf, nproc, myid
  INTEGER, PARAMETER :: maxb = __MSGSIZ_MAX
  !
  INTEGER :: buff(maxb)  
  COMMON / mp_base_integer / buff
  !
#if defined __TRACE
  write(*,*) 'parallel_max_integer IN'
#endif
  CALL mpi_comm_size( comm, nproc, info )
  IF( info /= 0 ) CALL errore( 'parallel_max_integer', 'error in mpi_comm_size', info )

  CALL mpi_comm_rank( comm, myid, info )
  IF( info /= 0 ) CALL errore( 'parallel_max_integer', 'error in mpi_comm_rank', info )
  !
  IF ( dim <= 0 .OR. nproc <= 1 ) GO TO 1
  !
  ! ... synchronize processes
  !
#if defined __USE_BARRIER
  CALL mp_synchronize( comm )
#endif
  !
  nbuf = dim / maxb
  !
  DO n = 1, nbuf
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_INTEGER, MPI_MAX, root, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_max_integer', 'error in mpi_reduce 1', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_INTEGER, MPI_MAX, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_max_integer', 'error in mpi_allreduce 1', info )
     END IF
     !
     IF( root < 0 ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     ELSE IF( root == myid ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     END IF
     !
  END DO
  !
  ! ... possible remaining elements < maxb
  !
  IF ( ( dim - nbuf * maxb ) > 0 ) THEN
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_INTEGER, MPI_MAX, root, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_max_integer', 'error in mpi_reduce 2', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_INTEGER, MPI_MAX, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_max_integer', 'error in mpi_allreduce 2', info )
     END IF
     !
     IF( root < 0 ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     ELSE IF( root == myid ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     END IF
     !
  END IF
  !
1 CONTINUE
  !
#if defined __TRACE
  write(*,*) 'parallel_max_integer OUT'
#endif
#endif
  !
  RETURN
  !
END SUBROUTINE parallel_max_integer


!----------------------------------------------------------------------------
SUBROUTINE parallel_min_real( dim, ps, comm, root )
  !----------------------------------------------------------------------------
  !
  ! ... compute the minimum value of a distributed variable ps(dim) over the processors.
  ! ... This version uses a fixed-length buffer of appropriate (?) dim
  !
  USE kinds, ONLY : DP
  USE parallel_include  
  !
  IMPLICIT NONE
  !
  INTEGER,  INTENT(IN)    :: dim
  REAL(DP)                :: ps(dim)
  INTEGER,  INTENT(IN)    :: comm    ! communecator
  INTEGER,  INTENT(IN)    :: root    ! if root <  0 perform a reduction to all procs
                                     ! if root >= 0 perform a reduce only to root proc.
  !
#if defined (__MPI)  
  !
  INTEGER            :: info, n, nbuf, nproc, myid
  INTEGER, PARAMETER :: maxb = __MSGSIZ_MAX
  !
  REAL(DP) :: buff(maxb)  
  COMMON / mp_base_real / buff
  !
#if defined __TRACE
  write(*,*) 'parallel_min_real IN'
#endif
  CALL mpi_comm_size( comm, nproc, info )
  IF( info /= 0 ) CALL errore( 'parallel_min_real', 'error in mpi_comm_size', info )

  CALL mpi_comm_rank( comm, myid, info )
  IF( info /= 0 ) CALL errore( 'parallel_min_real', 'error in mpi_comm_rank', info )
  !
  IF ( dim <= 0 .OR. nproc <= 1 ) GO TO 1
  !
  ! ... synchronize processes
  !
#if defined __USE_BARRIER
  CALL mp_synchronize( comm )
#endif
  !
  nbuf = dim / maxb
  !
  DO n = 1, nbuf
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_DOUBLE_PRECISION, MPI_MIN, root, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_min_real', 'error in mpi_reduce 1', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_DOUBLE_PRECISION, MPI_MIN, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_min_real', 'error in mpi_allreduce 1', info )
     END IF
     !
     IF( root < 0 ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     ELSE IF( root == myid ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     END IF
     !
  END DO
  !
  ! ... possible remaining elements < maxb
  !
  IF ( ( dim - nbuf * maxb ) > 0 ) THEN
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_DOUBLE_PRECISION, MPI_MIN, root, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_min_real', 'error in mpi_reduce 2', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_DOUBLE_PRECISION, MPI_MIN, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_min_real', 'error in mpi_allreduce 2', info )
     END IF
     !
     IF( root < 0 ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     ELSE IF( root == myid ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     END IF
     !
  END IF
  !
1 CONTINUE
  !
#if defined __TRACE
  write(*,*) 'parallel_min_real OUT'
#endif
#endif
  !
  RETURN
  !
END SUBROUTINE parallel_min_real

!
!----------------------------------------------------------------------------
SUBROUTINE parallel_max_real( dim, ps, comm, root )
  !----------------------------------------------------------------------------
  !
  ! ... compute the maximum value of a distributed variable ps(dim) over the processors.
  ! ... This version uses a fixed-length buffer of appropriate (?) dim
  !
  USE kinds, ONLY : DP
  USE parallel_include  
  !
  IMPLICIT NONE
  !
  INTEGER,  INTENT(IN)    :: dim
  REAL(DP)                :: ps(dim)
  INTEGER,  INTENT(IN)    :: comm    ! communecator
  INTEGER,  INTENT(IN)    :: root    ! if root <  0 perform a reduction to all procs
                                     ! if root >= 0 perform a reduce only to root proc.
  !
#if defined (__MPI)  
  !
  INTEGER            :: info, n, nbuf, nproc, myid
  INTEGER, PARAMETER :: maxb = __MSGSIZ_MAX
  !
  REAL(DP) :: buff(maxb)  
  COMMON / mp_base_real / buff
  !
#if defined __TRACE
  write(*,*) 'parallel_max_real IN'
#endif

  CALL mpi_comm_size( comm, nproc, info )
  IF( info /= 0 ) CALL errore( 'parallel_max_real', 'error in mpi_comm_size', info )

  CALL mpi_comm_rank( comm, myid, info )
  IF( info /= 0 ) CALL errore( 'parallel_max_real', 'error in mpi_comm_rank', info )
  !
  IF ( dim <= 0 .OR. nproc <= 1 ) GO TO 1
  !
  ! ... synchronize processes
  !
#if defined __USE_BARRIER
  CALL mp_synchronize( comm )
#endif
  !
  nbuf = dim / maxb
  !
  DO n = 1, nbuf
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_DOUBLE_PRECISION, MPI_MAX, root, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_max_real', 'error in mpi_reduce 1', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+(n-1)*maxb), buff, maxb, MPI_DOUBLE_PRECISION, MPI_MAX, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_max_real', 'error in mpi_allreduce 1', info )
     END IF
     !
     IF( root < 0 ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     ELSE IF( root == myid ) THEN
        ps((1+(n-1)*maxb):(n*maxb)) = buff(1:maxb)
     END IF
     !
  END DO
  !
  ! ... possible remaining elements < maxb
  !
  IF ( ( dim - nbuf * maxb ) > 0 ) THEN
     !
     IF( root >= 0 ) THEN
        CALL MPI_REDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_DOUBLE_PRECISION, MPI_MAX, root, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_max_real', 'error in mpi_reduce 2', info )
     ELSE
        CALL MPI_ALLREDUCE( ps(1+nbuf*maxb), buff, (dim-nbuf*maxb), MPI_DOUBLE_PRECISION, MPI_MAX, comm, info )
        IF( info /= 0 ) CALL errore( 'parallel_max_real', 'error in mpi_allreduce 2', info )
     END IF
     !
     IF( root < 0 ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     ELSE IF( root == myid ) THEN
        ps((1+nbuf*maxb):dim) = buff(1:(dim-nbuf*maxb))
     END IF
     !
  END IF
  !
1 CONTINUE
  !
#if defined __TRACE
  write(*,*) 'parallel_max_real OUT'
#endif
  !
#endif
  !
  RETURN
  !
END SUBROUTINE parallel_max_real


SUBROUTINE hangup()
#if defined (__MPI)  
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER IERR
  CALL MPI_BARRIER( MPI_COMM_WORLD, ierr )
  IF( ierr /= 0 ) CALL errore( ' hangup ', ' error in mpi_barrier ', ierr )
  CALL MPI_FINALIZE( ierr )
  IF( ierr /= 0 ) CALL errore( ' hangup ', ' error in mpi_finalize ', ierr )
#endif
  STOP 'hangup'
END SUBROUTINE hangup
