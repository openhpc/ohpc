      SUBROUTINE DERRLS( PATH, NUNIT )
*
      INCLUDE 'plasmaf.h'
*
*  -- LAPACK test routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  Purpose
*  =======
*
*  DERRLS tests the error exits for the DOUBLE PRECISION least squares
*  driver routines (DGELS, SGELSS, SGELSX, SGELSY, SGELSD).
*
*  Arguments
*  =========
*
*  PATH    (input) CHARACTER*3
*          The LAPACK path name for the routines to be tested.
*
*  NUNIT   (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NMAX
      PARAMETER          ( NMAX = 2 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            INFO, IRNK
      DOUBLE PRECISION   RCOND
      INTEGER            HT( 2 )
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      DOUBLE PRECISION   A( NMAX, NMAX ), B( NMAX, NMAX ), S( NMAX ),
     $                   W( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, DGELS, DGELSD, DGELSS, DGELSX,
     $                   DGELSY
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*32       SRNAMT
      INTEGER            INFOT, NOUT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NOUT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
      A( 1, 1 ) = 1.0D+0
      A( 1, 2 ) = 2.0D+0
      A( 2, 2 ) = 3.0D+0
      A( 2, 1 ) = 4.0D+0
      OK = .TRUE.
*
*     Disable PLASMA warnings/errors
* 
      CALL PLASMA_DISABLE( PLASMA_WARNINGS, INFO )
      CALL PLASMA_DISABLE( PLASMA_ERRORS,   INFO )
*
      IF( LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        Test error exits for the least squares driver routines.
*
*        DGELS
*
         CALL PLASMA_ALLOC_WORKSPACE_DGELS( 2, 2, HT, INFO )
*
         SRNAMT = 'DGELS '
         INFOT = 103
         CALL PLASMA_DGELS( '/', 0, 0, 0, A, 1, HT, B, 1, INFO )
         CALL CHKXER( 'DGELS ', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_DGELS( PLASMANOTRANS, -1, 0, 0, A, 1, HT,
     $                     B, 1, INFO )
         CALL CHKXER( 'DGELS ', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_DGELS( PLASMANOTRANS, 0, -1, 0, A, 1, HT,
     $                     B, 1, INFO )
         CALL CHKXER( 'DGELS ', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_DGELS( PLASMANOTRANS, 0, 0, -1, A, 1, HT,
     $                     B, 1, INFO )
         CALL CHKXER( 'DGELS ', INFOT, NOUT, INFO, OK )
         INFOT = 6
         CALL PLASMA_DGELS( PLASMANOTRANS, 2, 0, 0, A, 1, HT,
     $                     B, 2, INFO )
         CALL CHKXER( 'DGELS ', INFOT, NOUT, INFO, OK )
         INFOT = 9
         CALL PLASMA_DGELS( PLASMANOTRANS, 2, 0, 0, A, 2, HT,
     $                     B, 1, INFO )
         CALL CHKXER( 'DGELS ', INFOT, NOUT, INFO, OK )
*
         CALL PLASMA_DEALLOC_HANDLE( HT, INFO )
*
      END IF
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
*     Enable PLASMA warnings/errors
* 
      CALL PLASMA_ENABLE( PLASMA_WARNINGS, INFO )
      CALL PLASMA_ENABLE( PLASMA_ERRORS,   INFO )
*
      RETURN
*
*     End of DERRLS
*
      END
