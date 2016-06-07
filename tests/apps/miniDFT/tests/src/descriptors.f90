!
! Copyright (C) 2002 FPMD group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

   MODULE descriptors
      !
      IMPLICIT NONE
      SAVE

      INTEGER  ldim_block, ldim_cyclic, ldim_block_cyclic, ldim_block_sca
      INTEGER  gind_block, gind_cyclic, gind_block_cyclic, gind_block_sca
      EXTERNAL ldim_block, ldim_cyclic, ldim_block_cyclic, ldim_block_sca
      EXTERNAL gind_block, gind_cyclic, gind_block_cyclic, gind_block_sca

      !  Descriptor for linear algebra data distribution (like in Cannon's algorithm)
      !
      !  Remember here we use square matrixes block distributed on a square grid of processors
      !
      TYPE la_descriptor  
         INTEGER :: ir        = 0 !  globla index of the first row in the local block of the distributed matrix
         INTEGER :: nr        = 0 !  number of row in the local block of the distributed matrix
         INTEGER :: ic        = 0 !  global index of the first column in the local block of the distributed matrix
         INTEGER :: nc        = 0 !  number of column in the local block of the distributed matrix
         INTEGER :: nrcx        = 0 !  leading dimension of the distribute matrix (greather than nr and nc)
         INTEGER :: active_node = 0 !  if > 0 the proc holds a block of the lambda matrix
         INTEGER :: n        = 0 !  global dimension of the matrix
         INTEGER :: nx       = 0 !  global leading dimension ( >= n )
         INTEGER :: npr      = 0 !  number of row processors 
         INTEGER :: npc      = 0 !  number of column processors 
         INTEGER :: myr      = 0 !  processor row index
         INTEGER :: myc      = 0 !  processor column index
         INTEGER :: comm     = 0 !  communicator
         INTEGER :: mype     = 0 !  processor index ( from 0 to desc( la_npr_ ) * desc( la_npc_ ) - 1 )
         INTEGER :: nrl      = 0 !  number of local rows, when the matrix rows are cyclically distributed across proc
         INTEGER :: nrlx     = 0 !  leading dimension, when the matrix is distributed by row
      END TYPE
      !
   CONTAINS

   !------------------------------------------------------------------------
   !
   SUBROUTINE descla_local_dims( i2g, nl, n, nx, np, me )
      IMPLICIT NONE
      INTEGER, INTENT(OUT) :: i2g  !  global index of the first local element
      INTEGER, INTENT(OUT) :: nl   !  local number of elements
      INTEGER, INTENT(IN)  :: n    !  number of actual element in the global array
      INTEGER, INTENT(IN)  :: nx   !  dimension of the global array (nx>=n) to be distributed
      INTEGER, INTENT(IN)  :: np   !  number of processors
      INTEGER, INTENT(IN)  :: me   !  taskid for which i2g and nl are computed
      !
      !  note that we can distribute a global array larger than the
      !  number of actual elements. This could be required for performance
      !  reasons, and to have an equal partition of matrix having different size
      !  like matrixes of spin-up and spin-down 
      !
      nl  = ldim_block_sca( nx, np, me )
      i2g = gind_block_sca( 1, nx, np, me )
      ! This is to try to keep a matrix N * N into the same
      ! distribution of a matrix NX * NX, useful to have 
      ! the matrix of spin-up distributed in the same way
      ! of the matrix of spin-down
      !
      IF( i2g + nl - 1 > n ) nl = n - i2g + 1
      IF( nl < 0 ) nl = 0
      RETURN
      !
   END SUBROUTINE descla_local_dims
   !
   !
   SUBROUTINE descla_init( descla, n, nx, np, me, comm, includeme )
      !
      IMPLICIT NONE  
      TYPE(la_descriptor), INTENT(OUT) :: descla
      INTEGER, INTENT(IN)  :: n   !  the size of this matrix
      INTEGER, INTENT(IN)  :: nx  !  the max among different matrixes sharing 
                                  !  this descriptor or the same data distribution
      INTEGER, INTENT(IN)  :: np(2), me(2), comm
      INTEGER, INTENT(IN)  :: includeme
      INTEGER  :: ir, nr, ic, nc, lnode, nrcx, nrl, nrlx
      INTEGER  :: ip, npp
      
      IF( np(1) /= np(2) ) &
         CALL errore( ' descla_init ', ' only square grid of proc are allowed ', 2 )
      IF( n < 0 ) &
         CALL errore( ' descla_init ', ' dummy argument n less than 1 ', 3 )
      IF( nx < n ) &
         CALL errore( ' descla_init ', ' dummy argument nx less than n ', 4 )
      IF( np(1) < 1 ) &
         CALL errore( ' descla_init ', ' dummy argument np less than 1 ', 5 )

      ! find the block maximum dimensions

      nrcx = ldim_block_sca( nx, np(1), 0 )
      !
      ! find local dimensions, if appropriate
      !
      IF( includeme == 1 ) THEN
         !
         CALL descla_local_dims( ir, nr, n, nx, np(1), me(1) )
         CALL descla_local_dims( ic, nc, n, nx, np(2), me(2) )
         !
         lnode = 1
         !
      ELSE
         !
         nr = 0
         nc = 0
         !  
         ir = 0
         ic = 0
         !
         lnode = -1
         !
      END IF

      descla%ir = ir    ! globla index of the first row in the local block of lambda
      descla%nr = nr    ! number of row in the local block of lambda ( the "2" accounts for spin)
      descla%ic = ic    ! global index of the first column in the local block of lambda
      descla%nc = nc    ! number of column in the local block of lambda
      descla%nrcx = nrcx  ! leading dimension of the distribute lambda matrix
      descla%active_node = lnode 
                          !  if > 0 the proc holds a block of the lambda matrix
      descla%n = n        ! global dimension of the matrix
      descla%nx = nx      ! global leading dimension
      descla%npr = np(1)  ! number of row processors 
      descla%npc = np(2)  ! number of column processors 
      descla%myr = me(1)  ! processor row index
      descla%myc = me(2)  ! processor column index
      descla%comm = comm  ! communicator
      descla%mype = descla%myc + descla%myr * descla%npr 
                          ! processor index ( from 0 to desc( la_npr_ ) * desc( la_npc_ ) - 1 )
     
      npp = np(1) * np(2)

      !  Compute local dimension of the cyclically distributed matrix
      !
      IF( includeme == 1 ) THEN
         nrl  = ldim_cyclic( n, npp, descla%mype )
      ELSE
         nrl = 0
      END IF
      nrlx = n / npp + 1

      descla%nrl  = nrl  ! number of local rows, when the matrix rows are cyclically distributed across procs
      descla%nrlx = nrlx ! leading dimension 

      IF( nr < 0 .OR. nc < 0 ) &
         CALL errore( ' descla_init ', ' wrong valune for computed nr and nc ', 1 )
      IF( nrcx < 1 ) &
         CALL errore( ' descla_init ', ' wrong value for computed nrcx ', 2 )
      IF( nrcx < nr ) &
         CALL errore( ' descla_init ', ' nrcx < nr ', ( nr - nrcx ) )
      IF( nrcx < nc ) &
         CALL errore( ' descla_init ', ' nrcx < nc ', ( nc - nrcx ) )
      IF( nrlx < nrl ) &
         CALL errore( ' descla_init ', ' nrlx < nrl ', ( nrl - nrlx ) )
      IF( nrl < 0 ) &
         CALL errore( ' descla_init ', ' nrl < 0 ', ABS( nrl ) )


      RETURN
   END SUBROUTINE descla_init

   END MODULE descriptors
