      SUBROUTINE PDSCAEXINFO( SUMMRY, NOUT, N, NRHS, NB, NPROW, NPCOL,
     $                        WORK, IAM, NPROCS )
*
*  -- ScaLAPACK example code --
*     University of Tennessee, Knoxville, Oak Ridge National Laboratory,
*     and University of California, Berkeley.
*
*     Written by Antoine Petitet, August 1995  (petitet@cs.utk.edu)
*
*     This program solves a linear system by calling the ScaLAPACK 
*     routine PDGESV. The input matrix and right-and-sides are
*     read from a file. The solution is written to a file.
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    SUMMRY
      INTEGER            IAM, N, NRHS, NB, NOUT, NPCOL, NPROCS, NPROW
*     ..
*     .. Array Arguments ..
      INTEGER            WORK( * )
*     ..
*
* ======================================================================
*
*     .. Parameters ..
      INTEGER            NIN
      PARAMETER          ( NIN = 11 )
*     ..
*     .. Local Scalars ..
      CHARACTER*79       USRINFO
      INTEGER            ICTXT
*     ..
*     .. External Subroutines ..
      EXTERNAL           BLACS_ABORT, BLACS_GET, BLACS_GRIDEXIT,
     $                   BLACS_GRIDINIT, BLACS_SETUP, IGEBR2D, IGEBS2D
*     ..
*     .. Executable Statements ..
*
*     Process 0 reads the input data, broadcasts to other processes and
*     writes needed information to NOUT
*
      IF( IAM.EQ.0 ) THEN
*
*        Open file and skip data file header
*
         OPEN( NIN, FILE='SCAEX.dat', STATUS='OLD' )
         READ( NIN, FMT = * ) SUMMRY
         SUMMRY = ' '
*
*        Read in user-supplied info about machine type, compiler, etc.
*
         READ( NIN, FMT = 9999 ) USRINFO
*
*        Read name and unit number for summary output file
*
         READ( NIN, FMT = * ) SUMMRY
         READ( NIN, FMT = * ) NOUT
         IF( NOUT.NE.0 .AND. NOUT.NE.6 )
     $      OPEN( NOUT, FILE = SUMMRY, STATUS = 'UNKNOWN' )
*
*        Read and check the parameter values for the tests.
*
*        Get matrix dimensions
*
         READ( NIN, FMT = * ) N
         READ( NIN, FMT = * ) NRHS
*
*        Get value of NB
*
         READ( NIN, FMT = * ) NB
*
*        Get grid shape
*
         READ( NIN, FMT = * ) NPROW
         READ( NIN, FMT = * ) NPCOL
*
*        Close input file
*
         CLOSE( NIN )
*
*        If underlying system needs additional set up, do it now
*
         IF( NPROCS.LT.1 ) THEN
            NPROCS = NPROW * NPCOL
            CALL BLACS_SETUP( IAM, NPROCS )
         END IF
*
*        Temporarily define blacs grid to include all processes so
*        information can be broadcast to all processes
*
         CALL BLACS_GET( -1, 0, ICTXT )
         CALL BLACS_GRIDINIT( ICTXT, 'Row-major', 1, NPROCS )
*
*        Pack information arrays and broadcast
*
         WORK( 1 ) = N
         WORK( 2 ) = NRHS
         WORK( 3 ) = NB
         WORK( 4 ) = NPROW
         WORK( 5 ) = NPCOL
         CALL IGEBS2D( ICTXT, 'All', ' ', 5, 1, WORK, 5 )
*
*        regurgitate input
*
         WRITE( NOUT, FMT = 9999 )
     $               'SCALAPACK example driver.'
         WRITE( NOUT, FMT = 9999 ) USRINFO
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = 9999 )
     $               'The matrices A and B are read from '//
     $               'a file.'
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = 9999 )
     $               'An explanation of the input/output '//
     $               'parameters follows:'
*
         WRITE( NOUT, FMT = 9999 )
     $               'N       : The order of the matrix A.'
         WRITE( NOUT, FMT = 9999 )
     $               'NRHS    : The number of right and sides.'
         WRITE( NOUT, FMT = 9999 )
     $               'NB      : The size of the square blocks the'//
     $               ' matrices A and B are split into.'
         WRITE( NOUT, FMT = 9999 )
     $               'P       : The number of process rows.'
         WRITE( NOUT, FMT = 9999 )
     $               'Q       : The number of process columns.'
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = 9999 )
     $               'The following parameter values will be used:'
         WRITE( NOUT, FMT = 9998 ) 'N    ', N
         WRITE( NOUT, FMT = 9998 ) 'NRHS ', NRHS
         WRITE( NOUT, FMT = 9998 ) 'NB   ', NB
         WRITE( NOUT, FMT = 9998 ) 'P    ', NPROW
         WRITE( NOUT, FMT = 9998 ) 'Q    ', NPCOL
         WRITE( NOUT, FMT = * )
*
      ELSE
*
*        If underlying system needs additional set up, do it now
*
         IF( NPROCS.LT.1 )
     $      CALL BLACS_SETUP( IAM, NPROCS )
*
*        Temporarily define blacs grid to include all processes so
*        information can be broadcast to all processes
*
         CALL BLACS_GET( -1, 0, ICTXT )
         CALL BLACS_GRIDINIT( ICTXT, 'Row-major', 1, NPROCS )
*
         CALL IGEBR2D( ICTXT, 'All', ' ', 5, 1, WORK, 5, 0, 0 )
         N     = WORK( 1 )
         NRHS  = WORK( 2 )
         NB    = WORK( 3 )
         NPROW = WORK( 4 )
         NPCOL = WORK( 5 )
*
      END IF
*
      CALL BLACS_GRIDEXIT( ICTXT )
*
      RETURN
*
   20 WRITE( NOUT, FMT = 9997 )
      CLOSE( NIN )
      IF( NOUT.NE.6 .AND. NOUT.NE.0 )
     $   CLOSE( NOUT )
      CALL BLACS_ABORT( ICTXT, 1 )
*
      STOP
*
 9999 FORMAT( A )
 9998 FORMAT( 2X, A5, '   :        ', I6 )
 9997 FORMAT( ' Illegal input in file ',40A,'.  Aborting run.' )
*
*     End of PDSCAEXINFO
*
      END
