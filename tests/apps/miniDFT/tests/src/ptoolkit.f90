!
! Copyright (C) 2001-2006 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!==----------------------------------------------==!
    MODULE parallel_toolkit
!==----------------------------------------------==!

    USE kinds,     ONLY : DP
    USE io_global, ONLY : stdout
    USE parallel_include

    IMPLICIT NONE
    SAVE
    PRIVATE

    PUBLIC :: zsqmred, zsqmher

    CONTAINS
SUBROUTINE zsqmher( n, a, lda, desc )
   !
   ! double complex (Z) SQuare Matrix HERmitianize
   !
   USE kinds
   USE descriptors
   !
   IMPLICIT NONE
   !
   INTEGER, INTENT(IN) :: n
   INTEGER, INTENT(IN) :: lda
   COMPLEX(DP)         :: a(lda,lda) 
   TYPE(la_descriptor), INTENT(IN) :: desc
   INTEGER :: istatus( MPI_STATUS_SIZE )
   INTEGER :: i, j
   INTEGER :: comm, myid
   INTEGER :: nr, nc, dest, sreq, ierr, sour
   COMPLEX(DP) :: atmp
   COMPLEX(DP), ALLOCATABLE :: tst1(:,:)
   COMPLEX(DP), ALLOCATABLE :: tst2(:,:)


   IF( desc%active_node <= 0 ) THEN
      RETURN
   END IF

   IF( n /= desc%n ) &
      CALL errore( " zsqmsym ", " wrong global dim n ", n )
   IF( lda /= desc%nrcx ) &
      CALL errore( " zsqmsym ", " wrong leading dim lda ", lda )

   comm = desc%comm

   nr = desc%nr 
   nc = desc%nc 
   IF( desc%myc == desc%myr ) THEN
      !
      !  diagonal block, procs work locally
      !
      DO j = 1, nc
         a(j,j) = CMPLX( DBLE( a(j,j) ), 0_DP, KIND=DP )
         DO i = j + 1, nr
            a(i,j) = CONJG( a(j,i) )
         END DO
      END DO
      !
   ELSE IF( desc%myc > desc%myr ) THEN
      !
      !  super diagonal block, procs send the block to sub diag.
      !
      CALL GRID2D_RANK( 'R', desc%npr, desc%npc, &
                             desc%myc, desc%myr, dest )
      CALL mpi_isend( a, lda*lda, MPI_DOUBLE_COMPLEX, dest, 1, comm, sreq, ierr )
      !
      IF( ierr /= 0 ) &
         CALL errore( " zsqmher ", " in mpi_isend ", ABS( ierr ) )
      !
   ELSE IF( desc%myc < desc%myr ) THEN
      !
      !  sub diagonal block, procs receive the block from super diag,
      !  then transpose locally
      !
      CALL GRID2D_RANK( 'R', desc%npr, desc%npc, &
                             desc%myc, desc%myr, sour )
      CALL mpi_recv( a, lda*lda, MPI_DOUBLE_COMPLEX, sour, 1, comm, istatus, ierr )
      !
      IF( ierr /= 0 ) &
         CALL errore( " zsqmher ", " in mpi_recv ", ABS( ierr ) )
      !
      DO j = 1, lda
         DO i = j + 1, lda
            atmp   = a(i,j)
            a(i,j) = a(j,i)
            a(j,i) = atmp
         END DO
      END DO
      DO j = 1, nc
         DO i = 1, nr
            a(i,j)  = CONJG( a(i,j) )
         END DO
      END DO
      !
   END IF

   IF( desc%myc > desc%myr ) THEN
      !
      CALL MPI_Wait( sreq, istatus, ierr )
      !
      IF( ierr /= 0 ) &
         CALL errore( " zsqmher ", " in MPI_Wait ", ABS( ierr ) )
      !
   END IF


   RETURN
END SUBROUTINE zsqmher

SUBROUTINE zsqmred( na, a, lda, desca, nb, b, ldb, descb )
   !
   ! double complex (Z) SQuare Matrix REDistribution
   ! 
   ! Copy a global "na * na" matrix locally stored in "a",
   !  and distributed as described by "desca", into a larger
   !  global "nb * nb" matrix stored in "b" and distributed
   !  as described in "descb".
   ! 
   ! If you want to read, get prepared for an headache!
   ! Written struggling by Carlo Cavazzoni.
   !
   USE kinds
   USE descriptors
   !
   IMPLICIT NONE
   !
   INTEGER, INTENT(IN) :: na
   INTEGER, INTENT(IN) :: lda
   COMPLEX(DP)         :: a(lda,lda)  !  matrix to be redistributed into b
   TYPE(la_descriptor), INTENT(IN) :: desca
   INTEGER, INTENT(IN) :: nb
   INTEGER, INTENT(IN) :: ldb
   COMPLEX(DP)         :: b(ldb,ldb)
   TYPE(la_descriptor), INTENT(IN) :: descb

   INTEGER :: ipc, ipr, npc, npr
   INTEGER :: ipr_old, ir_old, nr_old, irx_old
   INTEGER :: ipc_old, ic_old, nc_old, icx_old
   INTEGER :: myrow, mycol, ierr, rank
   INTEGER :: col_comm, row_comm, comm, sreq
   INTEGER :: nr_new, ir_new, irx_new, ir, nr, nrtot, irb, ire
   INTEGER :: nc_new, ic_new, icx_new, ic, nc, nctot, icb, ice
   INTEGER :: ib, i, j, myid
   INTEGER :: nrsnd( desca%npr )
   INTEGER :: ncsnd( desca%npr )
   INTEGER :: displ( desca%npr )
   INTEGER :: irb_new( desca%npr )
   INTEGER :: ire_new( desca%npr )
   INTEGER :: icb_new( desca%npr )
   INTEGER :: ice_new( desca%npr )
   COMPLEX(DP), ALLOCATABLE :: buf(:)
   COMPLEX(DP), ALLOCATABLE :: ab(:,:)
   COMPLEX(DP), ALLOCATABLE :: tst1(:,:)
   COMPLEX(DP), ALLOCATABLE :: tst2(:,:)
    INTEGER :: istatus( MPI_STATUS_SIZE )

   IF( desca%active_node <= 0 ) THEN
      RETURN
   END IF

   ! preliminary consistency checks

   IF( nb < na ) &
      CALL errore( " zsqmred ", " nb < na, this sub. work only with nb >= na ", nb )
   IF( nb /= descb%n ) &
      CALL errore( " zsqmred ", " wrong global dim nb ", nb )
   IF( na /= desca%n ) &
      CALL errore( " zsqmred ", " wrong global dim na ", na )
   IF( ldb /= descb%nrcx ) &
      CALL errore( " zsqmred ", " wrong leading dim ldb ", ldb )
   IF( lda /= desca%nrcx ) &
      CALL errore( " zsqmred ", " wrong leading dim lda ", lda )

   npr   = desca%npr
   myrow = desca%myr
   npc   = desca%npc
   mycol = desca%myc
   comm  = desca%comm


   ! split communicator into row and col communicators

   CALL MPI_Comm_rank( comm, myid, ierr )
   IF( ierr /= 0 ) &
      CALL errore( " zsqmred ", " in MPI_Comm_rank 1 ", ABS( ierr ) )

   CALL MPI_Comm_split( comm, mycol, myrow, col_comm, ierr )
   IF( ierr /= 0 ) &
      CALL errore( " zsqmred ", " in MPI_Comm_split 1 ", ABS( ierr ) )

   CALL MPI_Comm_split( comm, myrow, mycol, row_comm, ierr )
   IF( ierr /= 0 ) &
      CALL errore( " zsqmred ", " in MPI_Comm_split 2 ", ABS( ierr ) )

   CALL MPI_Comm_rank( col_comm, rank, ierr )
   IF( ierr /= 0 ) &
      CALL errore( " zsqmred ", " in MPI_Comm_rank 2 ", ABS( ierr ) )
   IF( rank /= myrow ) &
      CALL errore( " zsqmred ", " building col_comm ", rank )

   CALL MPI_Comm_rank( row_comm, rank, ierr )
   IF( ierr /= 0 ) &
      CALL errore( " zsqmred ", " in MPI_Comm_rank 3 ", ABS( ierr ) )
   IF( rank /= mycol ) &
      CALL errore( " zsqmred ", " building row_comm ", rank )

   ALLOCATE( buf( descb%nrcx * descb%nrcx ) )
   ALLOCATE( ab( descb%nrcx, desca%nrcx ) )

   DO j = 1, descb%nc
      DO i = 1, descb%nr
         b( i, j ) = ( 0_DP , 0_DP )
      END DO
   END DO

   ab = ( 0_DP , 0_DP )

   ! first redistribute rows, column groups work in parallel

   DO ipr = 1, npr
      !
      CALL descla_local_dims( ir_new, nr_new, nb, descb%nx, npr, ipr-1 )
      !
      irx_new = ir_new + nr_new - 1
      !
      DO ipr_old = 1, npr
         !
         CALL descla_local_dims( ir_old, nr_old, na, desca%nx, npr, ipr_old-1 )
         !
         irx_old = ir_old + nr_old - 1
         !
         IF( ir_old >= ir_new .AND. ir_old <= irx_new ) THEN
            !
            nrsnd( ipr_old ) = MIN( nr_old, irx_new - ir_old + 1 )
            irb = 1
            ire = nrsnd( ipr_old )
            irb_new( ipr_old ) = ir_old - ir_new + 1
            ire_new( ipr_old ) = irb_new( ipr_old ) + nrsnd( ipr_old ) - 1
            !
         ELSE IF( ir_new >= ir_old .AND. ir_new <= irx_old ) THEN
            !
            nrsnd( ipr_old ) = irx_old - ir_new + 1
            irb = ir_new - ir_old + 1 
            ire = nr_old
            irb_new( ipr_old ) = 1
            ire_new( ipr_old ) = nrsnd( ipr_old )
            !
         ELSE
            nrsnd( ipr_old ) = 0
            irb = 0
            ire = 0
            irb_new( ipr_old ) = 0
            ire_new( ipr_old ) = 0
         END IF
         !
         IF( ( myrow == ipr_old - 1 ) .AND. ( nrsnd( ipr_old ) > 0 ) ) THEN
            IF(  myrow /= ipr - 1 ) THEN
               ib = 0
               DO j = 1, desca%nc
                  DO i = irb, ire
                     ib = ib + 1
                     buf( ib ) = a( i, j )
                  END DO
               END DO
               CALL mpi_isend( buf, ib, MPI_DOUBLE_COMPLEX, ipr-1, ipr, col_comm, sreq, ierr )
               IF( ierr /= 0 ) &
                  CALL errore( " zsqmred ", " in mpi_isend 1 ", ABS( ierr ) )
            ELSE
               DO j = 1, desca%nc
                  ib = irb
                  DO i = irb_new( ipr_old ), ire_new( ipr_old )
                     ab( i, j ) = a( ib, j )
                     ib = ib + 1
                  END DO
               END DO
            END IF
         END IF
         !
         IF( nrsnd( ipr_old ) /= ire - irb + 1 ) &
            CALL errore( " zsqmred ", " somthing wrong with row 1 ", nrsnd( ipr_old ) )
         IF( nrsnd( ipr_old ) /= ire_new( ipr_old ) - irb_new( ipr_old ) + 1 ) &
            CALL errore( " zsqmred ", " somthing wrong with row 2 ", nrsnd( ipr_old ) )
         !
         nrsnd( ipr_old ) = nrsnd( ipr_old ) * desca%nc
         !
      END DO
      !
      IF( myrow == ipr - 1 ) THEN
         DO ipr_old = 1, npr
            IF( nrsnd( ipr_old ) > 0 ) THEN
               IF(  myrow /= ipr_old - 1 ) THEN
                  CALL mpi_recv( buf, nrsnd(ipr_old), MPI_DOUBLE_COMPLEX, ipr_old-1, ipr, col_comm, istatus, ierr )
                  IF( ierr /= 0 ) &
                     CALL errore( " zsqmred ", " in mpi_recv 1 ", ABS( ierr ) )
                  CALL MPI_GET_COUNT( istatus, MPI_DOUBLE_COMPLEX, ib, ierr) 
                  IF( ierr /= 0 ) &
                     CALL errore( " zsqmred ", " in MPI_GET_COUNT 1 ", ABS( ierr ) )
                  IF( ib /= nrsnd(ipr_old) ) &
                     CALL errore( " zsqmred ", " somthing wrong with row 3 ", ib )
                  ib = 0
                  DO j = 1, desca%nc
                     DO i = irb_new( ipr_old ), ire_new( ipr_old )
                        ib = ib + 1
                        ab( i, j ) = buf( ib )
                     END DO
                  END DO
               END IF
            END IF
         END DO
      ELSE
         DO ipr_old = 1, npr
            IF( myrow == ipr_old - 1 .AND. nrsnd( ipr_old ) > 0 ) THEN
               CALL MPI_Wait( sreq, istatus, ierr )
               IF( ierr /= 0 ) &
                  CALL errore( " zsqmred ", " in MPI_Wait 1 ", ABS( ierr ) )
            END IF
         END DO
      END IF
      !
   END DO

   ! then redistribute cols, row groups work in parallel

   DO ipc = 1, npc
      !
      CALL descla_local_dims( ic_new, nc_new, nb, descb%nx, npc, ipc-1 )
      !
      icx_new = ic_new + nc_new - 1
      !
      DO ipc_old = 1, npc
         !
         CALL descla_local_dims( ic_old, nc_old, na, desca%nx, npc, ipc_old-1 )
         !
         icx_old = ic_old + nc_old - 1
         !
         IF( ic_old >= ic_new .AND. ic_old <= icx_new ) THEN
            !
            ncsnd( ipc_old ) = MIN( nc_old, icx_new - ic_old + 1 )
            icb = 1
            ice = ncsnd( ipc_old )
            icb_new( ipc_old ) = ic_old - ic_new + 1
            ice_new( ipc_old ) = icb_new( ipc_old ) + ncsnd( ipc_old ) - 1
            !
         ELSE IF( ic_new >= ic_old .AND. ic_new <= icx_old ) THEN
            !
            ncsnd( ipc_old ) = icx_old - ic_new + 1
            icb = ic_new - ic_old + 1 
            ice = nc_old
            icb_new( ipc_old ) = 1
            ice_new( ipc_old ) = ncsnd( ipc_old )
            !
         ELSE
            ncsnd( ipc_old ) = 0
            icb = 0
            ice = 0
            icb_new( ipc_old ) = 0
            ice_new( ipc_old ) = 0
         END IF
         !
         IF( ( mycol == ipc_old - 1 ) .AND. ( ncsnd( ipc_old ) > 0 ) ) THEN
            IF(  mycol /= ipc - 1 ) THEN
               ib = 0
               DO j = icb, ice
                  DO i = 1, descb%nrcx
                     ib = ib + 1
                     buf( ib ) = ab( i, j )
                  END DO
               END DO
               CALL mpi_isend( buf, ib, MPI_DOUBLE_COMPLEX, ipc-1, ipc, row_comm, sreq, ierr )
               IF( ierr /= 0 ) &
                  CALL errore( " zsqmred ", " in mpi_isend 2 ", ABS( ierr ) )
            ELSE
               ib = icb
               DO j = icb_new( ipc_old ), ice_new( ipc_old )
                  DO i = 1, descb%nrcx
                        b( i, j ) = ab( i, ib )
                  END DO
                  ib = ib + 1
               END DO
            END IF
         END IF

         IF( ncsnd( ipc_old ) /= ice-icb+1 ) &
            CALL errore( " zsqmred ", " somthing wrong with col 1 ", ncsnd( ipc_old ) )
         IF( ncsnd( ipc_old ) /= ice_new( ipc_old ) - icb_new( ipc_old ) + 1 ) &
            CALL errore( " zsqmred ", " somthing wrong with col 2 ", ncsnd( ipc_old ) )
         !
         ncsnd( ipc_old ) = ncsnd( ipc_old ) * descb%nrcx
         !
      END DO
      !
      IF( mycol == ipc - 1 ) THEN
         DO ipc_old = 1, npc
            IF( ncsnd( ipc_old ) > 0 ) THEN
               IF(  mycol /= ipc_old - 1 ) THEN
                  ib = icb_new( ipc_old )
                  CALL mpi_recv( b( 1, ib ), ncsnd(ipc_old), MPI_DOUBLE_COMPLEX, ipc_old-1, ipc, row_comm, istatus, ierr )
                  IF( ierr /= 0 ) &
                     CALL errore( " zsqmred ", " in mpi_recv 2 ", ABS( ierr ) )
                  CALL MPI_GET_COUNT( istatus, MPI_DOUBLE_COMPLEX, ib, ierr ) 
                  IF( ierr /= 0 ) &
                     CALL errore( " zsqmred ", " in MPI_GET_COUNT 2 ", ABS( ierr ) )
                  IF( ib /= ncsnd(ipc_old) ) &
                     CALL errore( " zsqmred ", " somthing wrong with col 3 ", ib )
               END IF
            END IF
         END DO
      ELSE
         DO ipc_old = 1, npc
            IF( mycol == ipc_old - 1 .AND. ncsnd( ipc_old ) > 0 ) THEN
               CALL MPI_Wait( sreq, istatus, ierr )
               IF( ierr /= 0 ) &
                  CALL errore( " zsqmred ", " in MPI_Wait 2 ", ABS( ierr ) )
            END IF
         END DO
      END IF
      !
   END DO

   DEALLOCATE( ab )
   DEALLOCATE( buf )

   CALL mpi_comm_free( col_comm, ierr )
   IF( ierr /= 0 ) &
      CALL errore( " zsqmred ", " in mpi_comm_free 1 ", ABS( ierr ) )

   CALL mpi_comm_free( row_comm, ierr )
   IF( ierr /= 0 ) &
      CALL errore( " zsqmred ", " in mpi_comm_free 2 ", ABS( ierr ) )


   RETURN
END SUBROUTINE zsqmred

END MODULE parallel_toolkit
!==----------------------------------------------==!

!=----------------------------------------------------------------------------=!
!
!  Cannon's algorithms for parallel matrix multiplication
!  written by Carlo Cavazzoni
!  

SUBROUTINE sqr_zmm_cannon( transa, transb, n, alpha, a, lda, b, ldb, beta, c, ldc, desc )
   !
   !  Parallel square matrix multiplication with Cannon's algorithm
   !
   USE kinds,       ONLY : DP
   USE descriptors
   !
   IMPLICIT NONE
   !
   CHARACTER(LEN=1), INTENT(IN) :: transa, transb
   INTEGER, INTENT(IN) :: n
   COMPLEX(DP), INTENT(IN) :: alpha, beta
   INTEGER, INTENT(IN) :: lda, ldb, ldc
   COMPLEX(DP) :: a(lda,*), b(ldb,*), c(ldc,*)
   TYPE(la_descriptor), INTENT(IN) :: desc
   !
   !  performs one of the matrix-matrix operations
   !
   !     C := ALPHA*OP( A )*OP( B ) + BETA*C,
   !
   !  where  op( x ) is one of
   !
   !     OP( X ) = X   OR   OP( X ) = X',
   !
   !  alpha and beta are scalars, and a, b and c are square matrices
   !
   !
   include 'mpif.h'
   !
   !
   INTEGER :: ierr
   INTEGER :: np
   INTEGER :: i, j, nr, nc, nb, iter, rowid, colid
   LOGICAL :: ta, tb
   INTEGER :: comm
   !
   !
   COMPLEX(DP), ALLOCATABLE :: bblk(:,:), ablk(:,:)
   COMPLEX(DP) :: zone = ( 1.0_DP, 0.0_DP )
   COMPLEX(DP) :: zzero = ( 0.0_DP, 0.0_DP )
   !
   !
   integer :: istatus( MPI_STATUS_SIZE )
   !
   !
   IF( desc%active_node < 0 ) THEN
      !
      !  processors not interested in this computation return quickly
      !
      RETURN
      !
   END IF

   IF( n < 1 ) THEN
      RETURN
   END IF

   IF( desc%npr == 1 ) THEN 
      !
      !  quick return if only one processor is used 
      !
      CALL zgemm( TRANSA, TRANSB, n, n, n, alpha, a, lda, b, ldb, beta, c, ldc)
      !
      RETURN
      !
   END IF

   IF( desc%npr /= desc%npc ) &
      CALL errore( ' sqr_zmm_cannon ', ' works only with square processor mesh ', 1 )
   !
   !  Retrieve communicator and mesh geometry
   !
   np    = desc%npr
   comm  = desc%comm
   rowid = desc%myr
   colid = desc%myc
   !
   !  Retrieve the size of the local block
   !
   nr    = desc%nr 
   nc    = desc%nc 
   nb    = desc%nrcx
   !
   CALL MPI_BARRIER( comm, ierr )
   IF( ierr /= 0 ) &
      CALL errore( " sqr_zmm_cannon ", " in MPI_BARRIER ", ABS( ierr ) )
   !
   allocate( ablk( nb, nb ) )
   DO j = 1, nc
      DO i = 1, nr
         ablk( i, j ) = a( i, j )
      END DO
   END DO
   !
   !  Clear memory outside the matrix block
   !
   DO j = nc+1, nb
      DO i = 1, nb
         ablk( i, j ) = zzero
      END DO
   END DO
   DO j = 1, nb
      DO i = nr+1, nb
         ablk( i, j ) = zzero
      END DO
   END DO
   !
   !
   allocate( bblk( nb, nb ) )
   DO j = 1, nc
      DO i = 1, nr
         bblk( i, j ) = b( i, j )
      END DO
   END DO
   !
   !  Clear memory outside the matrix block
   !
   DO j = nc+1, nb
      DO i = 1, nb
         bblk( i, j ) = zzero
      END DO
   END DO
   DO j = 1, nb
      DO i = nr+1, nb
         bblk( i, j ) = zzero
      END DO
   END DO
   !
   !
   ta = ( TRANSA == 'C' .OR. TRANSA == 'c' )
   tb = ( TRANSB == 'C' .OR. TRANSB == 'c' )
   !
   !  Shift A rowid+1 places to the west
   ! 
   IF( ta ) THEN
      CALL shift_exch_block( ablk, 'W', 1 )
   ELSE
      CALL shift_block( ablk, 'W', rowid+1, 1 )
   END IF
   !
   !  Shift B colid+1 places to the north
   ! 
   IF( tb ) THEN
      CALL shift_exch_block( bblk, 'N', np+1 )
   ELSE
      CALL shift_block( bblk, 'N', colid+1, np+1 )
   END IF
   !
   !  Accumulate on C
   !
   CALL zgemm( TRANSA, TRANSB, nr, nc, nb, alpha, ablk, nb, bblk, nb, beta, c, ldc)
   !
   DO iter = 2, np
      !
      !  Shift A 1 places to the east
      ! 
      CALL shift_block( ablk, 'E', 1, iter )
      !
      !  Shift B 1 places to the south
      ! 
      CALL shift_block( bblk, 'S', 1, np+iter )
      !
      !  Accumulate on C
      !
      CALL zgemm( TRANSA, TRANSB, nr, nc, nb, alpha, ablk, nb, bblk, nb, zone, c, ldc)
      !
   END DO

   deallocate( ablk, bblk )
   
   RETURN

CONTAINS

   SUBROUTINE shift_block( blk, dir, ln, tag )
      !
      !   Block shift 
      !
      IMPLICIT NONE
      COMPLEX(DP) :: blk( :, : )
      CHARACTER(LEN=1), INTENT(IN) :: dir      ! shift direction
      INTEGER,          INTENT(IN) :: ln       ! shift length
      INTEGER,          INTENT(IN) :: tag      ! communication tag
      !
      INTEGER :: icdst, irdst, icsrc, irsrc, idest, isour
      !
      IF( dir == 'W' ) THEN
         !
         irdst = rowid
         irsrc = rowid
         icdst = MOD( colid - ln + np, np )
         icsrc = MOD( colid + ln + np, np )
         !
      ELSE IF( dir == 'E' ) THEN
         !
         irdst = rowid
         irsrc = rowid
         icdst = MOD( colid + ln + np, np )
         icsrc = MOD( colid - ln + np, np )
         !
      ELSE IF( dir == 'N' ) THEN

         irdst = MOD( rowid - ln + np, np )
         irsrc = MOD( rowid + ln + np, np )
         icdst = colid
         icsrc = colid

      ELSE IF( dir == 'S' ) THEN

         irdst = MOD( rowid + ln + np, np )
         irsrc = MOD( rowid - ln + np, np )
         icdst = colid
         icsrc = colid

      ELSE

         CALL errore( ' sqr_zmm_cannon ', ' unknown shift direction ', 1 )

      END IF
      !
      CALL GRID2D_RANK( 'R', np, np, irdst, icdst, idest )
      CALL GRID2D_RANK( 'R', np, np, irsrc, icsrc, isour )
      !
      !
      CALL MPI_SENDRECV_REPLACE(blk, nb*nb, MPI_DOUBLE_COMPLEX, &
           idest, tag, isour, tag, comm, istatus, ierr)
      IF( ierr /= 0 ) &
         CALL errore( " sqr_zmm_cannon ", " in MPI_SENDRECV_REPLACE 1 ", ABS( ierr ) )
      !
      RETURN
   END SUBROUTINE shift_block
   !
   SUBROUTINE shift_exch_block( blk, dir, tag )
      !
      !   Combined block shift and exchange
      !   only used for the first step
      !
      IMPLICIT NONE
      COMPLEX(DP) :: blk( :, : )
      CHARACTER(LEN=1), INTENT(IN) :: dir
      INTEGER,          INTENT(IN) :: tag
      !
      INTEGER :: icdst, irdst, icsrc, irsrc, idest, isour
      INTEGER :: icol, irow
      !
      IF( dir == 'W' ) THEN
         !
         icol = rowid
         irow = colid
         !
         irdst = irow
         icdst = MOD( icol - irow-1 + np, np )
         !
         irow = rowid
         icol = MOD( colid + rowid+1 + np, np )
         !
         irsrc = icol
         icsrc = irow
         !
      ELSE IF( dir == 'N' ) THEN
         !
         icol = rowid
         irow = colid
         !
         icdst = icol
         irdst = MOD( irow - icol-1 + np, np )
         !
         irow = MOD( rowid + colid+1 + np, np )
         icol = colid
         !
         irsrc = icol
         icsrc = irow

      ELSE

         CALL errore( ' sqr_zmm_cannon ', ' unknown shift_exch direction ', 1 )

      END IF
      !
      CALL GRID2D_RANK( 'R', np, np, irdst, icdst, idest )
      CALL GRID2D_RANK( 'R', np, np, irsrc, icsrc, isour )
      !
      !
      CALL MPI_SENDRECV_REPLACE(blk, nb*nb, MPI_DOUBLE_COMPLEX, &
           idest, tag, isour, tag, comm, istatus, ierr)
      IF( ierr /= 0 ) &
         CALL errore( " sqr_zmm_cannon ", " in MPI_SENDRECV_REPLACE 2 ", ABS( ierr ) )
      !
      RETURN
   END SUBROUTINE shift_exch_block

END SUBROUTINE sqr_zmm_cannon

SUBROUTINE sqr_zsetmat( what, n, alpha, a, lda, desc )
   !
   !  Set the values of a square distributed matrix 
   !
   USE kinds,       ONLY : DP
   USE descriptors
   !
   IMPLICIT NONE
   !
   CHARACTER(LEN=1), INTENT(IN) :: what
     ! what = 'A' set all the values of "a" equal to alpha
     ! what = 'U' set the values in the upper triangle of "a" equal to alpha
     ! what = 'L' set the values in the lower triangle of "a" equal to alpha
     ! what = 'D' set the values in the diagonal of "a" equal to alpha
     ! what = 'H' clear the imaginary part of the diagonal of "a" 
   INTEGER, INTENT(IN) :: n
     ! dimension of the matrix
   COMPLEX(DP), INTENT(IN) :: alpha
     ! value to be assigned to elements of "a"
   INTEGER, INTENT(IN) :: lda
     ! leading dimension of a
   COMPLEX(DP) :: a(lda,*)
     ! matrix whose values have to be set
   TYPE(la_descriptor), INTENT(IN) :: desc
     ! descriptor of matrix a

   INTEGER :: i, j

   IF( desc%active_node < 0 ) THEN
      !
      !  processors not interested in this computation return quickly
      !
      RETURN
      !
   END IF

   SELECT CASE( what )
     CASE( 'U', 'u' )
        IF( desc%myc > desc%myr ) THEN
           DO j = 1, desc%nc
              DO i = 1, desc%nr
                 a( i, j ) = alpha
              END DO
           END DO
        ELSE IF( desc%myc == desc%myr ) THEN
           DO j = 1, desc%nc
              DO i = 1, j - 1
                 a( i, j ) = alpha
              END DO
           END DO
        END IF
     CASE( 'L', 'l' )
        IF( desc%myc < desc%myr ) THEN
           DO j = 1, desc%nc
              DO i = 1, desc%nr
                 a( i, j ) = alpha
              END DO
           END DO
        ELSE IF( desc%myc == desc%myr ) THEN
           DO j = 1, desc%nc
              DO i = j - 1, desc%nr
                 a( i, j ) = alpha
              END DO
           END DO
        END IF
     CASE( 'D', 'd' )
        IF( desc%myc == desc%myr ) THEN
           DO i = 1, desc%nr
              a( i, i ) = alpha
           END DO
        END IF
     CASE( 'H', 'h' )
        IF( desc%myc == desc%myr ) THEN
           DO i = 1, desc%nr
              a( i, i ) = CMPLX( DBLE( a(i,i) ), 0_DP, KIND=DP )
           END DO
        END IF
     CASE DEFAULT
        DO j = 1, desc%nc
           DO i = 1, desc%nr
              a( i, j ) = alpha
           END DO
        END DO
   END SELECT
   !
   RETURN
END SUBROUTINE sqr_zsetmat
