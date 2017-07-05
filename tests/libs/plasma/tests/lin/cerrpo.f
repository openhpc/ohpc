      SUBROUTINE CERRPO( PATH, NUNIT )
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
*  CERRPO tests the error exits for the COMPLEX routines
*  for Hermitian positive definite matrices.
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
      PARAMETER          ( NMAX = 4 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J
      REAL               ANRM, RCOND
*     ..
*     .. Local Arrays ..
*     REAL               R( NMAX )
      REAL               R1( NMAX )
      REAL               R2( NMAX )
      COMPLEX            A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, CPBCON, CPBEQU, CPBRFS, CPBTF2,
     $                   CPBTRF, CPBTRS, CPOCON, CPOEQU, CPORFS, CPOTF2,
     $                   CPOTRF, CPOTRI, CPOTRS, CPPCON, CPPEQU, CPPRFS,
     $                   CPPTRF, CPPTRI, CPPTRS
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
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, REAL
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
*
*     Disable PLASMA warnings/errors
* 
      CALL PLASMA_DISABLE( PLASMA_WARNINGS, INFO )
      CALL PLASMA_DISABLE( PLASMA_ERRORS,   INFO )
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = CMPLX( 1. / REAL( I+J ), -1. / REAL( I+J ) )
            AF( I, J ) = CMPLX( 1. / REAL( I+J ), -1. / REAL( I+J ) )
   10    CONTINUE
         B( J ) = 0.
         R1( J ) = 0.
         R2( J ) = 0.
         W( J ) = 0.
         X( J ) = 0.
   20 CONTINUE
      ANRM = 1.
      OK = .TRUE.
*
*     Test error exits of the routines that use the Cholesky
*     decomposition of a Hermitian positive definite matrix.
*
      IF( LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        CPOTRF
*
         SRNAMT = 'CPOTRF'
         INFOT = 1
         CALL PLASMA_CPOTRF( '/', 0, A, 1, INFO )
         CALL CHKXER( 'CPOTRF', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_CPOTRF( PLASMAUPPER, -1, A, 1, INFO )
         CALL CHKXER( 'CPOTRF', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_CPOTRF( PLASMAUPPER, 2, A, 1, INFO )
         CALL CHKXER( 'CPOTRF', INFOT, NOUT, INFO, OK )
*
*        CPOTRI
*
         SRNAMT = 'CPOTRI'
         INFOT = 1
         CALL PLASMA_CPOTRI( '/', 0, A, 1, INFO )
         CALL CHKXER( 'CPOTRI', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_CPOTRI( PLASMAUPPER, -1, A, 1, INFO )
         CALL CHKXER( 'CPOTRI', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_CPOTRI( PLASMAUPPER, 2, A, 1, INFO )
         CALL CHKXER( 'CPOTRI', INFOT, NOUT, INFO, OK )
*
*        CPOTRS
*
         SRNAMT = 'CPOTRS'
         INFOT = 1
         CALL PLASMA_CPOTRS( '/', 0, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'CPOTRS', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_CPOTRS( PLASMAUPPER, -1, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'CPOTRS', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_CPOTRS( PLASMAUPPER, 0, -1, A, 1, B, 1, INFO )
         CALL CHKXER( 'CPOTRS', INFOT, NOUT, INFO, OK )
         INFOT = 5
         CALL PLASMA_CPOTRS( PLASMAUPPER, 2, 1, A, 1, B, 2, INFO )
         CALL CHKXER( 'CPOTRS', INFOT, NOUT, INFO, OK )
         INFOT = 7
         CALL PLASMA_CPOTRS( PLASMAUPPER, 2, 1, A, 2, B, 1, INFO )
*
*        CPOCON
*
         SRNAMT = 'CPOCON'
         INFOT = 1
         CALL PLASMA_CPOCON( '/', 0, A, 1, ANRM, RCOND, INFO )
         CALL CHKXER( 'CPOCON', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_CPOCON( PLASMAUPPER, -1, A, 1, ANRM, RCOND, INFO )
         CALL CHKXER( 'CPOCON', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_CPOCON( PLASMAUPPER, 2, A, 1, ANRM, RCOND, INFO )
         CALL CHKXER( 'CPOCON', INFOT, NOUT, INFO, OK )
         INFOT = 5
         CALL PLASMA_CPOCON( PLASMAUPPER, 1, A, 1, -ANRM, RCOND, INFO )
         CALL CHKXER( 'CPOCON', INFOT, NOUT, INFO, OK )
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
*     End of CERRPO
*
      END
