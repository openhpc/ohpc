      SUBROUTINE SERRLQ( PATH, NUNIT )
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
*  SERRLQ tests the error exits for the REAL routines
*  that use the LQ decomposition of a general matrix.
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
      INTEGER            I, INFO, J
*     ..
*     .. Local Arrays ..
      REAL               A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( NMAX ), X( NMAX )
      INTEGER            HT( 2 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, SGELQ2, SGELQF, SORGL2,
     $                   SORGLQ, SORML2, SORMLQ
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
      INTRINSIC          REAL
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
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
            A( I, J ) = 1. / REAL( I+J )
            AF( I, J ) = 1. / REAL( I+J )
   10    CONTINUE
         B( J ) = 0.
         W( J ) = 0.
         X( J ) = 0.
   20 CONTINUE
      OK = .TRUE.
*
*     Allocate HT
*
      CALL PLASMA_ALLOC_WORKSPACE_SGELQF( 2, 2, HT, INFO )
*
*     Error exits for LQ factorization
*
*     SGELQF
*
      SRNAMT = 'SGELQF'
      INFOT = 1
      CALL PLASMA_SGELQF( -1, 0, A, 1, HT, INFO )
      CALL CHKXER( 'SGELQF', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_SGELQF( 0, -1, A, 1, HT, INFO )
      CALL CHKXER( 'SGELQF', INFOT, NOUT, INFO, OK )
      INFOT = 4
      CALL PLASMA_SGELQF( 2, 1, A, 1, HT, INFO )
      CALL CHKXER( 'SGELQF', INFOT, NOUT, INFO, OK )
*
*     SGELQS
*
      SRNAMT = 'SGELQS'
      INFOT = 1
      CALL PLASMA_SGELQS( -1, 0, 0, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'SGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_SGELQS( 0, -1, 0, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'SGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_SGELQS( 2, 1, 0, A, 2, HT, B, 1, INFO )
      CALL CHKXER( 'SGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_SGELQS( 0, 0, -1, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'SGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_SGELQS( 2, 2, 0, A, 1, HT, B, 2, INFO )
      CALL CHKXER( 'SGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 8
      CALL PLASMA_SGELQS( 1, 2, 0, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'SGELQS', INFOT, NOUT, INFO, OK )
*
*     SORGLQ
*
      SRNAMT = 'SORGLQ'
      INFOT = 1
      CALL PLASMA_SORGLQ( -1, 0, 0, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'SORGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_SORGLQ( 0, -1, 0, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'SORGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_SORGLQ( 2, 1, 0, A, 2, HT, W, 2, INFO )
      CALL CHKXER( 'SORGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_SORGLQ( 0, 0, -1, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'SORGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_SORGLQ( 1, 1, 2, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'SORGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_SORGLQ( 2, 2, 0, A, 1, HT, W, 2, INFO )
      CALL CHKXER( 'SORGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 8
      CALL PLASMA_SORGLQ( 2, 2, 0, A, 2, HT, W, 1, INFO )
      CALL CHKXER( 'SORGLQ', INFOT, NOUT, INFO, OK )
*
*     SORMLQ
*
      SRNAMT = 'SORMLQ'
      INFOT = 1
      CALL PLASMA_SORMLQ( '/', PLASMATRANS, 0, 0, 0, A, 1, HT, AF, 1,
     $                   INFO )
      CALL CHKXER( 'SORMLQ', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_SORMLQ( PLASMALEFT, '/', 0, 0, 0, A, 1, HT, AF, 1,
     $                   INFO )
      CALL CHKXER( 'SORMLQ', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_SORMLQ( PLASMALEFT, PLASMATRANS, -1, 0, 0, A, 1, HT,
     $                   AF, 1, INFO )
      CALL CHKXER( 'SORMLQ', INFOT, NOUT, INFO, OK )
      INFOT = 4
      CALL PLASMA_SORMLQ( PLASMALEFT, PLASMATRANS, 0, -1, 0, A, 1, HT,
     $                   AF, 1, INFO )
      CALL CHKXER( 'SORMLQ', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_SORMLQ( PLASMALEFT, PLASMATRANS, 0, 0, -1, A, 1, HT,
     $                   AF, 1, INFO )
      CALL CHKXER( 'SORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 5
*      CALL PLASMA_SORMLQ( PLASMALEFT, PLASMATRANS, 0, 1, 1, A, 1, HT, AF, 1, INFO )
*      CALL CHKXER( 'SORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 5
*      CALL PLASMA_SORMLQ( PLASMARIGHT, PLASMATRANS, 1, 0, 1, A, 1, HT, AF, 1, INFO )
*      CALL CHKXER( 'SORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 7
*      CALL PLASMA_SORMLQ( PLASMALEFT, PLASMATRANS, 2, 0, 2, A, 1, HT, AF, 2, INFO )
*      CALL CHKXER( 'SORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 7
*      CALL PLASMA_SORMLQ( PLASMARIGHT, PLASMATRANS, 0, 2, 2, A, 1, HT, AF, 1, INFO )
*      CALL CHKXER( 'SORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 10
*      CALL PLASMA_SORMLQ( PLASMALEFT, PLASMATRANS, 2, 1, 0, A, 2, HT, AF, 1, INFO )
*      CALL CHKXER( 'SORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 12
*      CALL PLASMA_SORMLQ( PLASMALEFT, PLASMATRANS, 1, 2, 0, A, 1, HT, AF, 1, INFO )
*      CALL CHKXER( 'SORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 12
*      CALL PLASMA_SORMLQ( PLASMARIGHT, PLASMATRANS, 2, 1, 0, A, 1, HT, AF, 2, INFO )
*      CALL CHKXER( 'SORMLQ', INFOT, NOUT, INFO, OK )
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
*     Deallocate HT
*
      CALL PLASMA_DEALLOC_HANDLE( HT, INFO )
*
*     Enable PLASMA warnings/errors
* 
      CALL PLASMA_ENABLE( PLASMA_WARNINGS, INFO )
      CALL PLASMA_ENABLE( PLASMA_ERRORS,   INFO )
*
      RETURN
*
*     End of SERRLQ
*
      END
