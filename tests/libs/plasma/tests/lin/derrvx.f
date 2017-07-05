      SUBROUTINE DERRVX( PATH, NUNIT )
*
      INCLUDE 'plasmaf.h'
*
*  -- LAPACK test routine (version 3.1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     January 2007
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  Purpose
*  =======
*
*  DERRVX tests the error exits for the DOUBLE PRECISION driver routines
*  for solving linear systems of equations.
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
      CHARACTER          EQ
      CHARACTER*2        C2
      INTEGER            I, INFO, J
      DOUBLE PRECISION   RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            HL( 2 ), HPIV( 2 )
      INTEGER            IP( NMAX ), IW( NMAX )
      DOUBLE PRECISION   A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   C( NMAX ), R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, DGBSV, DGBSVX, DGESV, DGESVX, DGTSV,
     $                   DGTSVX, DPBSV, DPBSVX, DPOSV, DPOSVX, DPPSV,
     $                   DPPSVX, DPTSV, DPTSVX, DSPSV, DSPSVX, DSYSV,
     $                   DSYSVX
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
      INTRINSIC          DBLE
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
            A( I, J ) = 1.D0 / DBLE( I+J )
            AF( I, J ) = 1.D0 / DBLE( I+J )
   10    CONTINUE
         B( J ) = 0.D0
         R1( J ) = 0.D0
         R2( J ) = 0.D0
         W( J ) = 0.D0
         X( J ) = 0.D0
         C( J ) = 0.D0
         R( J ) = 0.D0
         IP( J ) = J
   20 CONTINUE
      EQ = ' '
      OK = .TRUE.
*
      IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        ALLOCATE HL and HPIV
*
         CALL PLASMA_ALLOC_WORKSPACE_DGETRF_INCPIV( 
     $        2, 1, HL, HPIV, INFO )
*
*        PLASMA_DGESV
*
         SRNAMT = 'DGESV '
         INFOT = 1
         CALL PLASMA_DGESV_INCPIV( -1, 0, A, 1, HL, HPIV, B, 1, INFO )
         CALL CHKXER( 'DGESV ', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_DGESV_INCPIV( 0, -1, A, 1, HL, HPIV, B, 1, INFO )
         CALL CHKXER( 'DGESV ', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_DGESV_INCPIV( 2, 1, A, 1, HL, HPIV, B, 2, INFO )
         CALL CHKXER( 'DGESV ', INFOT, NOUT, INFO, OK )
         INFOT = 8
         CALL PLASMA_DGESV_INCPIV( 2, 1, A, 2, HL, HPIV, B, 1, INFO )
         CALL CHKXER( 'DGESV ', INFOT, NOUT, INFO, OK )
*
*        DEALLOCATE HL and HPIV
*
         CALL PLASMA_DEALLOC_HANDLE( HL, INFO )
         CALL PLASMA_DEALLOC_HANDLE( HPIV, INFO )
*
*
*        DGESV
*
         SRNAMT = 'DGESV '
         INFOT = 1
         CALL PLASMA_DGESV( -1, 0, A, 1, IWORK, B, 1, INFO )
         CALL CHKXER( 'DGESV ', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_DGESV( 0, -1, A, 1, IWORK, B, 1, INFO )
         CALL CHKXER( 'DGESV ', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_DGESV( 2, 1, A, 1, IWORK, B, 2, INFO )
         CALL CHKXER( 'DGESV ', INFOT, NOUT, INFO, OK )
         INFOT = 7
         CALL PLASMA_DGESV( 2, 1, A, 2, IWORK, B, 1, INFO )
         CALL CHKXER( 'DGESV ', INFOT, NOUT, INFO, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        DPOSV
*
         SRNAMT = 'DPOSV '
         INFOT = 1
         CALL PLASMA_DPOSV( '/', 0, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'DPOSV ', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_DPOSV( PLASMAUPPER, -1, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'DPOSV ', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_DPOSV( PLASMAUPPER, 0, -1, A, 1, B, 1, INFO )
         CALL CHKXER( 'DPOSV ', INFOT, NOUT, INFO, OK )
         INFOT = 5
         CALL PLASMA_DPOSV( PLASMAUPPER, 2, 0, A, 1, B, 2, INFO )
         CALL CHKXER( 'DPOSV ', INFOT, NOUT, INFO, OK )
         INFOT = 7
         CALL PLASMA_DPOSV( PLASMAUPPER, 2, 0, A, 2, B, 1, INFO )
         CALL CHKXER( 'DPOSV ', INFOT, NOUT, INFO, OK )
*
*        DPOSVX
*
         SRNAMT = 'DPOSVX'
         INFOT = 1
         CALL DPOSVX( '/', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL CHKXER( 'DPOSVX', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL DPOSVX( 'N', '/', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL CHKXER( 'DPOSVX', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL DPOSVX( 'N', 'U', -1, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL CHKXER( 'DPOSVX', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL DPOSVX( 'N', 'U', 0, -1, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL CHKXER( 'DPOSVX', INFOT, NOUT, INFO, OK )
         INFOT = 6
         CALL DPOSVX( 'N', 'U', 2, 0, A, 1, AF, 2, EQ, C, B, 2, X, 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL CHKXER( 'DPOSVX', INFOT, NOUT, INFO, OK )
         INFOT = 8
         CALL DPOSVX( 'N', 'U', 2, 0, A, 2, AF, 1, EQ, C, B, 2, X, 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL CHKXER( 'DPOSVX', INFOT, NOUT, INFO, OK )
         INFOT = 9
         EQ = '/'
         CALL DPOSVX( 'F', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL CHKXER( 'DPOSVX', INFOT, NOUT, INFO, OK )
         INFOT = 10
         EQ = 'Y'
         CALL DPOSVX( 'F', 'U', 1, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL CHKXER( 'DPOSVX', INFOT, NOUT, INFO, OK )
         INFOT = 12
         CALL DPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 1, X, 2,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL CHKXER( 'DPOSVX', INFOT, NOUT, INFO, OK )
         INFOT = 14
         CALL DPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 2, X, 1,
     $                RCOND, R1, R2, W, IW, INFO )
         CALL CHKXER( 'DPOSVX', INFOT, NOUT, INFO, OK )
      END IF
*
*     Print a summary line.
*
      IF( OK ) THEN
         WRITE( NOUT, FMT = 9999 )PATH
      ELSE
         WRITE( NOUT, FMT = 9998 )PATH
      END IF
*
 9999 FORMAT( 1X, A3, ' drivers passed the tests of the error exits' )
 9998 FORMAT( ' *** ', A3, ' drivers failed the tests of the error ',
     $      'exits ***' )
*
*     Enable PLASMA warnings/errors
* 
      CALL PLASMA_ENABLE( PLASMA_WARNINGS, INFO )
      CALL PLASMA_ENABLE( PLASMA_ERRORS,   INFO )
*
      RETURN
*
*     End of DERRVX
*
      END
