      SUBROUTINE CERRLS( PATH, NUNIT )
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
*  CERRLS tests the error exits for the COMPLEX least squares
*  driver routines (CGELS, CGELSS, CGELSX, CGELSY, CGELSD).
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
      REAL               RCOND
      INTEGER            HT( 2 )
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      REAL               RW( NMAX ), S( NMAX )
      COMPLEX            A( NMAX, NMAX ), B( NMAX, NMAX ), W( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CGELS, CGELSD, CGELSS, CGELSX, CGELSY,
     $                   CHKXER
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
      C2 = PATH( 2: 3 )
      A( 1, 1 ) = ( 1.0E+0, 0.0E+0 )
      A( 1, 2 ) = ( 2.0E+0, 0.0E+0 )
      A( 2, 2 ) = ( 3.0E+0, 0.0E+0 )
      A( 2, 1 ) = ( 4.0E+0, 0.0E+0 )
      OK = .TRUE.
      WRITE( NOUT, FMT = * )
*
*     Disable PLASMA warnings/errors
* 
      CALL PLASMA_DISABLE( PLASMA_WARNINGS, INFO )
      CALL PLASMA_DISABLE( PLASMA_ERRORS,   INFO )
*
*     Test error exits for the least squares driver routines.
*
      IF( LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        CGELS
*
         CALL PLASMA_ALLOC_WORKSPACE_CGELS( 2, 2, HT, INFO )
*
         SRNAMT = 'CGELS '
         INFOT = 103
         CALL PLASMA_CGELS( '/', 0, 0, 0, A, 1, HT, B, 1, INFO )
         CALL CHKXER( 'CGELS ', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_CGELS( PLASMANOTRANS, -1, 0, 0, A, 1, HT,
     $                     B, 1, INFO )
         CALL CHKXER( 'CGELS ', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_CGELS( PLASMANOTRANS, 0, -1, 0, A, 1, HT,
     $                     B, 1, INFO )
         CALL CHKXER( 'CGELS ', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_CGELS( PLASMANOTRANS, 0, 0, -1, A, 1, HT,
     $                     B, 1, INFO )
         CALL CHKXER( 'CGELS ', INFOT, NOUT, INFO, OK )
         INFOT = 6
         CALL PLASMA_CGELS( PLASMANOTRANS, 2, 0, 0, A, 1, HT,
     $                     B, 2, INFO )
         CALL CHKXER( 'CGELS ', INFOT, NOUT, INFO, OK )
         INFOT = 9
         CALL PLASMA_CGELS( PLASMANOTRANS, 2, 0, 0, A, 2, HT,
     $                     B, 1, INFO )
         CALL CHKXER( 'CGELS ', INFOT, NOUT, INFO, OK )
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
*     End of CERRLS
*
      END
