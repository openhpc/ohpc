      SUBROUTINE ZERRPO( PATH, NUNIT )
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
*  ZERRPO tests the error exits for the COMPLEX*16 routines
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
      DOUBLE PRECISION   ANRM, RCOND
*     ..
*     .. Local Arrays ..
*     DOUBLE PRECISION   R( NMAX )
      DOUBLE PRECISION   R1( NMAX )
      DOUBLE PRECISION   R2( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, ZPBCON, ZPBEQU, ZPBRFS, ZPBTF2,
     $                   ZPBTRF, ZPBTRS, ZPOCON, ZPOEQU, ZPORFS, ZPOTF2,
     $                   ZPOTRF, ZPOTRI, ZPOTRS, ZPPCON, ZPPEQU, ZPPRFS,
     $                   ZPPTRF, ZPPTRI, ZPPTRS
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
      INTRINSIC          DBLE, DCMPLX
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
            A( I, J ) = DCMPLX( 1.D0 / DBLE( I+J ),
     $                  -1.D0 / DBLE( I+J ) )
            AF( I, J ) = DCMPLX( 1.D0 / DBLE( I+J ),
     $                   -1.D0 / DBLE( I+J ) )
   10    CONTINUE
         B( J ) = 0.D0
         R1( J ) = 0.D0
         R2( J ) = 0.D0
         W( J ) = 0.D0
         X( J ) = 0.D0
   20 CONTINUE
      ANRM = 1.D0
      OK = .TRUE.
*
*     Test error exits of the routines that use the Cholesky
*     decomposition of a Hermitian positive definite matrix.
*
      IF( LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        ZPOTRF
*
         SRNAMT = 'ZPOTRF'
         INFOT = 1
         CALL PLASMA_ZPOTRF( '/', 0, A, 1, INFO )
         CALL CHKXER( 'ZPOTRF', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_ZPOTRF( PLASMAUPPER, -1, A, 1, INFO )
         CALL CHKXER( 'ZPOTRF', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_ZPOTRF( PLASMAUPPER, 2, A, 1, INFO )
         CALL CHKXER( 'ZPOTRF', INFOT, NOUT, INFO, OK )
*
*        ZPOTRI
*
         SRNAMT = 'ZPOTRI'
         INFOT = 1
         CALL PLASMA_ZPOTRI( '/', 0, A, 1, INFO )
         CALL CHKXER( 'ZPOTRI', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_ZPOTRI( PLASMAUPPER, -1, A, 1, INFO )
         CALL CHKXER( 'ZPOTRI', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_ZPOTRI( PLASMAUPPER, 2, A, 1, INFO )
         CALL CHKXER( 'ZPOTRI', INFOT, NOUT, INFO, OK )
*
*        ZPOTRS
*
         SRNAMT = 'ZPOTRS'
         INFOT = 1
         CALL PLASMA_ZPOTRS( '/', 0, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'ZPOTRS', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_ZPOTRS( PLASMAUPPER, -1, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'ZPOTRS', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_ZPOTRS( PLASMAUPPER, 0, -1, A, 1, B, 1, INFO )
         CALL CHKXER( 'ZPOTRS', INFOT, NOUT, INFO, OK )
         INFOT = 5
         CALL PLASMA_ZPOTRS( PLASMAUPPER, 2, 1, A, 1, B, 2, INFO )
         CALL CHKXER( 'ZPOTRS', INFOT, NOUT, INFO, OK )
         INFOT = 7
         CALL PLASMA_ZPOTRS( PLASMAUPPER, 2, 1, A, 2, B, 1, INFO )
*
*        ZPOCON
*
         SRNAMT = 'ZPOCON'
         INFOT = 1
         CALL PLASMA_ZPOCON( '/', 0, A, 1, ANRM, RCOND, INFO )
         CALL CHKXER( 'ZPOCON', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_ZPOCON( PLASMAUPPER, -1, A, 1, ANRM, RCOND, INFO )
         CALL CHKXER( 'ZPOCON', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_ZPOCON( PLASMAUPPER, 2, A, 1, ANRM, RCOND, INFO )
         CALL CHKXER( 'ZPOCON', INFOT, NOUT, INFO, OK )
         INFOT = 5
         CALL PLASMA_ZPOCON( PLASMAUPPER, 1, A, 1, -ANRM, RCOND, INFO )
         CALL CHKXER( 'ZPOCON', INFOT, NOUT, INFO, OK )
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
*     End of ZERRPO
*
      END
