!
! Copyright (C) 2001-2009 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!-----------------------------------------------------------------------
!
SUBROUTINE block_distribute( nat, me_image, nproc_image, ia_s, ia_e, mykey )
    INTEGER, INTENT(IN)  :: nat, me_image, nproc_image  
    INTEGER, INTENT(OUT) :: ia_s, ia_e, mykey
    INTEGER :: na_loc, r, nproc_ia

    INTEGER, EXTERNAL :: ldim_block, gind_block

    ! Parallel: divide among processors for the same image
    !
    ! compute how many processors we have for a given atom
    !
    nproc_ia = nproc_image / nat
    !
    IF( nproc_ia == 0 ) THEN
       !       
       ! here we have less than one processor per atom
       !       
       mykey  = 0
       na_loc = ldim_block( nat, nproc_image, me_image)
       ia_s   = gind_block( 1, nat, nproc_image, me_image )
       ia_e   = ia_s + na_loc - 1
       !       
    ELSE 
       !       
       ! here we have more than one proc per atom
       !
       r = MOD( nproc_image, nat )
       !
       IF( me_image < (nproc_ia + 1)*r ) THEN
          ! processors that do the work, more procs work on a single atom
          ia_s  = me_image/(nproc_ia + 1) + 1
          mykey = MOD( me_image, nproc_ia + 1 )
       ELSE
          ia_s  = ( me_image - (nproc_ia + 1)*r ) / nproc_ia + 1 + r
          mykey = MOD( me_image - (nproc_ia + 1)*r , nproc_ia )
       END IF
       !
       ia_e = ia_s
       !
    END IF

    RETURN

END SUBROUTINE
!

!
SUBROUTINE GRID2D_DIMS( grid_shape, nproc, nprow, npcol )
   !
   ! This subroutine factorizes the number of processors (NPROC)
   ! into NPROW and NPCOL according to the shape
   !
   !    Written by Carlo Cavazzoni
   !
   IMPLICIT NONE
   CHARACTER, INTENT(IN) :: grid_shape
   INTEGER, INTENT(IN)  :: nproc
   INTEGER, INTENT(OUT) :: nprow, npcol
   INTEGER :: sqrtnp, i
   !
   sqrtnp = INT( SQRT( REAL( nproc ) + 0.1 ) )
   !
   IF( grid_shape == 'S' ) THEN
      ! Square grid
      nprow = sqrtnp
      npcol = sqrtnp
   ELSE
      ! Rectangular grid
      DO i = 1, sqrtnp + 1
         IF( MOD( nproc, i ) == 0 ) nprow = i
      end do
      npcol = nproc / nprow
   END IF
   RETURN
END SUBROUTINE

SUBROUTINE GRID2D_COORDS( order, rank, nprow, npcol, row, col )
   !
   !  this subroutine compute the cartesian coordinetes "row" and "col"
   !  of the processor whose MPI task id is "rank". 
   !  Note that if the rank is larger that the grid size
   !  all processors whose MPI task id is greather or equal 
   !  than nprow * npcol are placed on the diagonal extension of the grid itself
   !
   IMPLICIT NONE
   CHARACTER, INTENT(IN) :: order
   INTEGER, INTENT(IN)  ::  rank          ! process index starting from 0
   INTEGER, INTENT(IN)  ::  nprow, npcol  ! dimensions of the processor grid
   INTEGER, INTENT(OUT) ::  row, col
   IF( rank >= 0 .AND. rank < nprow * npcol ) THEN
      IF( order == 'C' .OR. order == 'c' ) THEN
         !  grid in COLUMN MAJOR ORDER
         row = MOD( rank, nprow )
         col = rank / nprow
      ELSE
         !  grid in ROW MAJOR ORDER
         row = rank / npcol
         col = MOD( rank, npcol )
      END IF
   ELSE
      row = rank
      col = rank
   END IF
   RETURN
END SUBROUTINE

SUBROUTINE GRID2D_RANK( order, nprow, npcol, row, col, rank )
   !
   !  this subroutine compute the processor MPI task id "rank" of the processor  
   !  whose cartesian coordinate are "row" and "col".
   !  Note that the subroutine assume cyclic indexing ( row = nprow = 0 )
   !
   IMPLICIT NONE
   CHARACTER, INTENT(IN) :: order
   INTEGER, INTENT(OUT) ::  rank         ! process index starting from 0
   INTEGER, INTENT(IN)  ::  nprow, npcol  ! dimensions of the processor grid
   INTEGER, INTENT(IN)  ::  row, col
   
   IF( order == 'C' .OR. order == 'c' ) THEN
     !  grid in COLUMN MAJOR ORDER
     rank = MOD( row + nprow, nprow ) + MOD( col + npcol, npcol ) * nprow
   ELSE
     !  grid in ROW MAJOR ORDER
     rank = MOD( col + npcol, npcol ) + MOD( row + nprow, nprow ) * npcol
   END IF
   !
   RETURN
END SUBROUTINE
!
! Copyright (C) 2002 FPMD group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!


       INTEGER FUNCTION ldim_cyclic(gdim, np, me)

!   gdim = global dimension of distributed array
!   np   = number of processors
!   me   = index of the calling processor (starting from 0)
!  
!   this function return the number of elements of the distributed array
!   stored in the local memory of the processor "me" for a cyclic 
!   data distribution.
!   Example of the cyclic distribution of a 10 elements array on 4 processors
!   array elements  |  PEs
!    a(1)           |   0
!    a(2)           |   1
!    a(3)           |   2
!    a(4)           |   3
!    a(5)           |   0
!    a(6)           |   1
!    a(7)           |   2
!    a(8)           |   3
!    a(9)           |   0
!    a(10)          |   1

       IMPLICIT NONE
       INTEGER :: gdim, np, me, r, q

       IF( me >= np .OR. me < 0 ) THEN
         WRITE(6,*) ' ** ldim_cyclic: arg no. 3 out of range '
         STOP
       END IF

       q = INT(gdim / np)
       r = MOD(gdim, np)

       IF( me .LT. r ) THEN
         ldim_cyclic = q+1
       ELSE
         ldim_cyclic = q
       END IF
 
       RETURN
       END FUNCTION ldim_cyclic

!=----------------------------------------------------------------------------=!

       INTEGER FUNCTION ldim_block(gdim, np, me)

!   gdim = global dimension of distributed array
!   np   = number of processors
!   me   = index of the calling processor (starting from 0)
!  
!   this function return the number of elements of the distributed array
!   stored in the local memory of the processor "me" for a balanced block 
!   data distribution, with the larger block on the lower index processors.
!   Example of the block distribution of 10 elements array a on 4 processors
!   array elements  |  PEs
!    a(1)           |   0
!    a(2)           |   0
!    a(3)           |   0
!    a(4)           |   1
!    a(5)           |   1
!    a(6)           |   1
!    a(7)           |   2
!    a(8)           |   2
!    a(9)           |   3
!    a(10)          |   3

       IMPLICIT NONE
       INTEGER :: gdim, np, me, r, q

       IF( me >= np .OR. me < 0 ) THEN
         WRITE(6,*) ' ** ldim_block: arg no. 3 out of range '
         STOP
       END IF

       q = INT(gdim / np)
       r = MOD(gdim, np)

       IF( me .LT. r ) THEN
! ...    if my index is less than the reminder I got an extra element
         ldim_block = q+1
       ELSE
         ldim_block = q
       END IF
 
       RETURN
       END FUNCTION ldim_block

!=----------------------------------------------------------------------------=!

       INTEGER FUNCTION ldim_block_sca( gdim, np, me )

!   gdim = global dimension of distributed array
!   np   = number of processors
!   me   = index of the calling processor (starting from 0)
!  
!   this function return the number of elements of the distributed array
!   stored in the local memory of the processor "me" for equal block
!   data distribution, all block have the same size but the last one.
!   Example of the block distribution of 10 elements array a on 4 processors
!   array elements  |  PEs
!    a(1)           |   0
!    a(2)           |   0
!    a(3)           |   0
!    a(4)           |   1
!    a(5)           |   1
!    a(6)           |   1
!    a(7)           |   2
!    a(8)           |   2
!    a(9)           |   2
!    a(10)          |   3

       IMPLICIT NONE
       INTEGER :: gdim, np, me, nb

       IF( me >= np .OR. me < 0 ) THEN
         WRITE(6,*) ' ** ldim_block: arg no. 3 out of range '
         STOP
       END IF

       nb = INT( gdim / np )
       IF( MOD( gdim,  np ) /= 0 ) THEN
         nb = nb+1
         ! ... last processor take the rest
         IF( me == ( np - 1 ) ) nb = gdim - (np-1)*nb
       END IF

       ldim_block_sca = nb


       RETURN
       END FUNCTION ldim_block_sca

!=----------------------------------------------------------------------------=!



      INTEGER FUNCTION ldim_block_cyclic( N, NB, NPROCS, IPROC )

!  -- Derived from:  NUMROC( N, NB, IPROC, ISRCPROC, NPROCS )
!  -- ScaLAPACK tools routine (version 1.5) --
!     University of Tennessee, Knoxville, Oak Ridge National Laboratory,
!     and University of California, Berkeley.
!     May 1, 1997
!
!     .. Scalar Arguments ..
      IMPLICIT NONE
      INTEGER              IPROC, ISRCPROC, N, NB, NPROCS, NUMROC
!     ..
!
!  Purpose
!  =======
!
!  NUMROC computes the NUMber of Rows Or Columns of a distributed
!  matrix owned by the process indicated by IPROC.
!
!  Arguments
!  =========
!
!  N         (global input) INTEGER
!            The number of rows/columns in distributed matrix.
!
!  NB        (global input) INTEGER
!            Block size, size of the blocks the distributed matrix is
!            split into.
!
!  IPROC     (local input) INTEGER
!            The coordinate of the process whose local array row or
!            column is to be determined.
!
!  ISRCPROC  (global input) INTEGER
!            The coordinate of the process that possesses the first
!            row or column of the distributed matrix.
!
!  NPROCS    (global input) INTEGER
!            The total number processes over which the matrix is
!            distributed.
!
!  =====================================================================
!
!     .. Local Scalars ..
      INTEGER              EXTRABLKS, MYDIST, NBLOCKS
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC            MOD
!     ..
!     .. Executable Statements ..
!
!     Figure PROC's distance from source process
!
      ISRCPROC = 0
      MYDIST = MOD( NPROCS+IPROC-ISRCPROC, NPROCS )
!
!     Figure the total number of whole NB blocks N is split up into
!
      NBLOCKS = N / NB
!
!     Figure the minimum number of rows/cols a process can have
!
      NUMROC = (NBLOCKS/NPROCS) * NB
!
!     See if there are any extra blocks
!
      EXTRABLKS = MOD( NBLOCKS, NPROCS )
!
!     If I have an extra block
!
      IF( MYDIST.LT.EXTRABLKS ) THEN
          NUMROC = NUMROC + NB
!
!         If I have last block, it may be a partial block
!
      ELSE IF( MYDIST.EQ.EXTRABLKS ) THEN
          NUMROC = NUMROC + MOD( N, NB )
      END IF
!

      ldim_block_cyclic = numroc
      RETURN
!
!     End of NUMROC
!
      END FUNCTION ldim_block_cyclic
!
! Copyright (C) 2002 FPMD group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!


       INTEGER FUNCTION lind_block(ig, nx, np, me)
!
!   INPUT :
!      ig  global index of the x dimension of array element
!      nx  dimension of the global array
!      np  number of processor in the x dimension of the processors grid
!      me  index of the local processor in the processor grid
!                (starting from zero)
!
!   OUTPUT :
!
!      lind_block return the local index corresponding to the
!      global index "ig" for a balanced block distribution
!   

       IMPLICIT NONE

       INTEGER :: ig, nx, np, me, r, q

       q = INT(nx/np)
       r = MOD(nx,np) 

       IF( me < r ) THEN
         lind_block = ig - (q+1) * me
       ELSE
         lind_block = ig - (q+1) * r - q * (me - r)
       END IF

       RETURN 
       END FUNCTION lind_block


!=----------------------------------------------------------------------------=!


       INTEGER FUNCTION lind_block_sca(ig, nx, np, me)
!
!   INPUT :
!      ig  global index of the x dimension of array element
!      nx  dimension of the global array
!      np  number of processor in the x dimension of the processors grid
!      me  index of the local processor in the processor grid
!                (starting from zero)
!
!   OUTPUT :
!
!      lind_block_sca return the local index corresponding to the
!      global index "ig" for an equal block distribution
!   

       IMPLICIT NONE

       INTEGER :: ig, nx, np, me, nb

       nb = INT( nx / np )
       IF( MOD( nx,  np ) /= 0 ) nb = nb+1

       lind_block_sca = ig - me * nb

       RETURN
       END FUNCTION lind_block_sca


!=----------------------------------------------------------------------------=!



       INTEGER FUNCTION lind_cyclic(ig, nx, np, me)
!
!   INPUT :
!      ig  global index of the x dimension of array element
!      nx  dimension of the global array
!      np  number of processor in the x dimension of the processors grid
!      me  index of the local processor in the processor grid
!                (starting from zero)
!
!   OUTPUT :
!
!      lind_cyclic return the local index corresponding to the
!      global index "ig" for a cyclic distribution
!   

       IMPLICIT NONE

       INTEGER :: ig, nx, np, me

       lind_cyclic = (ig-1)/np + 1

       RETURN 
       END FUNCTION lind_cyclic


!=----------------------------------------------------------------------------=!


       INTEGER FUNCTION lind_block_cyclic( INDXGLOB, NB, NPROCS, IPROC )      

!     Derived from:  INDXG2L( INDXGLOB, NB, IPROC, ISRCPROC, NPROCS )
!  -- ScaLAPACK tools routine (version 1.5) --
!     University of Tennessee, Knoxville, Oak Ridge National Laboratory,
!     and University of California, Berkeley.
!     May 1, 1997
!
!     .. Scalar Arguments ..
      IMPLICIT NONE
      INTEGER  INDXGLOB, IPROC, ISRCPROC, NB, NPROCS, INDXG2L
!     ..
!
!  Purpose
!  =======
!
!  INDXG2L computes the local index of a distributed matrix entry
!  pointed to by the global index INDXGLOB.
!
!  Arguments
!  =========
!
!  INDXGLOB  (global input) INTEGER
!            The global index of the distributed matrix entry.
!
!  NB        (global input) INTEGER
!            Block size, size of the blocks the distributed matrix is
!            split into.
!
!  IPROC     (local dummy) INTEGER
!            Dummy argument in this case in order to unify the calling
!            sequence of the tool-routines.
!
!  ISRCPROC  (local dummy) INTEGER
!            Dummy argument in this case in order to unify the calling
!            sequence of the tool-routines.
!
!  NPROCS    (global input) INTEGER
!            The total number processes over which the distributed
!            matrix is distributed.
!
!  =====================================================================
!
!     .. Intrinsic Functions ..
      INTRINSIC          MOD
!     ..
!     .. Executable Statements ..
!
      ISRCPROC = 0      
      INDXG2L = NB*((INDXGLOB-1)/(NB*NPROCS))+MOD(INDXGLOB-1,NB)+1
      lind_block_cyclic = INDXG2L
!
      RETURN
!
!     End of INDXG2L
!
      END FUNCTION lind_block_cyclic


!=----------------------------------------------------------------------------=!


      INTEGER FUNCTION gind_cyclic( lind, n, np, me )

!  This function computes the global index of a distributed array entry
!  pointed to by the local index lind of the process indicated by me.
!  lind      local index of the distributed matrix entry.
!  N         is the size of the global array.
!  me        The coordinate of the process whose local array row or
!            column is to be determined.
!  np        The total number processes over which the distributed
!            matrix is distributed.
!

            INTEGER, INTENT(IN) :: lind, n, me, np
            INTEGER r, q

            gind_cyclic = (lind-1) * np + me + 1

            RETURN
      END FUNCTION gind_cyclic


!=----------------------------------------------------------------------------=!


      INTEGER FUNCTION gind_block( lind, n, np, me )

!  This function computes the global index of a distributed array entry
!  pointed to by the local index lind of the process indicated by me.
!  lind      local index of the distributed matrix entry.
!  N         is the size of the global array.
!  me        The coordinate of the process whose local array row or
!            column is to be determined.
!  np        The total number processes over which the distributed
!            matrix is distributed.


            INTEGER, INTENT(IN) :: lind, n, me, np
            INTEGER r, q

              q = INT(n/np)
              r = MOD(n,np)
              IF( me < r ) THEN
                gind_block = (Q+1)*me + lind
              ELSE
                gind_block = Q*me + R + lind
              END IF

         RETURN
      END FUNCTION gind_block

!=----------------------------------------------------------------------------=!

      INTEGER FUNCTION gind_block_sca( lind, n, np, me )

!  This function computes the global index of a distributed array entry
!  pointed to by the local index lind of the process indicated by me.
!  lind      local index of the distributed matrix entry.
!  N         is the size of the global array.
!  me        The coordinate of the process whose local array row or
!            column is to be determined.
!  np        The total number processes over which the distributed
!            matrix is distributed.


       INTEGER, INTENT(IN) :: lind, n, me, np
       INTEGER nb

       IF( me >= np .OR. me < 0 ) THEN
         WRITE(6,*) ' ** ldim_block: arg no. 3 out of range '
         STOP
       END IF

       nb = INT( n / np )
       IF( MOD( n,  np ) /= 0 ) nb = nb+1

       gind_block_sca = lind + me * nb

       RETURN

    END FUNCTION gind_block_sca


!=----------------------------------------------------------------------------=!

      INTEGER FUNCTION gind_block_cyclic( lind, n, nb, np, me )

!  This function computes the global index of a distributed array entry
!  pointed to by the local index lind of the process indicated by me.
!  lind      local index of the distributed matrix entry.
!  N         is the size of the global array.
!  NB        size of the blocks the distributed matrix is split into.
!  me        The coordinate of the process whose local array row or
!            column is to be determined.
!  np        The total number processes over which the distributed
!            matrix is distributed.


            INTEGER, INTENT(IN) :: lind, n, nb, me, np
            INTEGER r, q, isrc

            isrc = 0
            gind_block_cyclic = np*NB*((lind-1)/NB) + &
                MOD(lind-1,NB) + MOD(np+me-isrc, np)*NB + 1

         RETURN
      END FUNCTION gind_block_cyclic

