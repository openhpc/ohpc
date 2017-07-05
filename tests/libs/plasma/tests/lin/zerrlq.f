      SUBROUTINE ZERRLQ( PATH, NUNIT )
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
*  ZERRLQ tests the error exits for the COMPLEX*16 routines
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
      COMPLEX*16         A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( NMAX ), X( NMAX )
      INTEGER            HT( 2 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, ZGELQ2, ZGELQF, ZUNGL2,
     $                   ZUNGLQ, ZUNML2, ZUNMLQ
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
         W( J ) = 0.D0
         X( J ) = 0.D0
   20 CONTINUE
      OK = .TRUE.
*
*     Disable PLASMA warnings/errors
* 
      CALL PLASMA_DISABLE( PLASMA_WARNINGS, INFO )
      CALL PLASMA_DISABLE( PLASMA_ERRORS,   INFO )
*
*     Allocate HT
*
      CALL PLASMA_ALLOC_WORKSPACE_ZGELQF( 2, 2, HT, INFO )
*
*     Error exits for LQ factorization
*
*     ZGELQF
*
      SRNAMT = 'ZGELQF'
      INFOT = 1
      CALL PLASMA_ZGELQF( -1, 0, A, 1, HT, INFO )
      CALL CHKXER( 'ZGELQF', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_ZGELQF( 0, -1, A, 1, HT, INFO )
      CALL CHKXER( 'ZGELQF', INFOT, NOUT, INFO, OK )
      INFOT = 4
      CALL PLASMA_ZGELQF( 2, 1, A, 1, HT, INFO )
      CALL CHKXER( 'ZGELQF', INFOT, NOUT, INFO, OK )
*
*     PLASMA_ZGELQS
*
      SRNAMT = 'ZGELQS'
      INFOT = 1
      CALL PLASMA_ZGELQS( -1, 0, 0, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'ZGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_ZGELQS( 0, -1, 0, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'ZGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_ZGELQS( 2, 1, 0, A, 2, HT, B, 1, INFO )
      CALL CHKXER( 'ZGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_ZGELQS( 0, 0, -1, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'ZGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_ZGELQS( 2, 2, 0, A, 1, HT, B, 2, INFO )
      CALL CHKXER( 'ZGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 8
      CALL PLASMA_ZGELQS( 1, 2, 0, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'ZGELQS', INFOT, NOUT, INFO, OK )
*
*     ZUNGLQ
*
      SRNAMT = 'ZUNGLQ'
      INFOT = 1
      CALL PLASMA_ZUNGLQ( -1, 0, 0, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'ZUNGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_ZUNGLQ( 0, -1, 0, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'ZUNGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_ZUNGLQ( 2, 1, 0, A, 2, HT, W, 1, INFO )
      CALL CHKXER( 'ZUNGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_ZUNGLQ( 0, 0, -1, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'ZUNGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_ZUNGLQ( 1, 1, 2, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'ZUNGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_ZUNGLQ( 2, 2, 0, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'ZUNGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 8
      CALL PLASMA_ZUNGLQ( 2, 2, 0, A, 2, HT, W, 1, INFO )
      CALL CHKXER( 'ZUNGLQ', INFOT, NOUT, INFO, OK )
*
*     ZUNMLQ
*
      SRNAMT = 'ZUNMLQ'
      INFOT = 1
      CALL PLASMA_ZUNMLQ( '/', PLASMACONJTRANS, 0, 0, 0, A, 1, X, AF, 1,
     $                   INFO )
      CALL CHKXER( 'ZUNMLQ', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_ZUNMLQ( PLASMALEFT, '/', 0, 0, 0, A, 1, X, AF, 1,
     $                   INFO )
      CALL CHKXER( 'ZUNMLQ', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_ZUNMLQ( PLASMALEFT, PLASMACONJTRANS, -1, 0, 0, A, 1,
     $                   X, AF, 1, INFO )
      CALL CHKXER( 'ZUNMLQ', INFOT, NOUT, INFO, OK )
      INFOT = 4
      CALL PLASMA_ZUNMLQ( PLASMALEFT, PLASMACONJTRANS, 0, -1, 0, A, 1,
     $                   X, AF, 1, INFO )
      CALL CHKXER( 'ZUNMLQ', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_ZUNMLQ( PLASMALEFT, PLASMACONJTRANS, 0, 0, -1, A, 1,
     $                   X, AF, 1, INFO )
      CALL CHKXER( 'ZUNMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 5
*      CALL PLASMA_ZUNMLQ( PLASMALEFT, PLASMACONJTRANS, 0, 1, 1, A, 1, X, AF, 1, INFO )
*      CALL CHKXER( 'ZUNMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 5
*      CALL PLASMA_ZUNMLQ( PLASMARIGHT, PLASMACONJTRANS, 1, 0, 1, A, 1, X, AF, 1, INFO )
*      CALL CHKXER( 'ZUNMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 7
*      CALL PLASMA_ZUNMLQ( PLASMALEFT, PLASMACONJTRANS, 2, 0, 2, A, 1, X, AF, 2, INFO )
*      CALL CHKXER( 'ZUNMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 7
*      CALL PLASMA_ZUNMLQ( PLASMARIGHT, PLASMACONJTRANS, 0, 2, 2, A, 1, X, AF, 1, INFO )
*      CALL CHKXER( 'ZUNMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 10
*      CALL PLASMA_ZUNMLQ( PLASMALEFT, PLASMACONJTRANS, 2, 1, 0, A, 2, X, AF, 1, INFO )
*      CALL CHKXER( 'ZUNMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 12
*      CALL PLASMA_ZUNMLQ( PLASMALEFT, PLASMACONJTRANS, 1, 2, 0, A, 1, X, AF, 1, INFO )
*      CALL CHKXER( 'ZUNMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 12
*      CALL PLASMA_ZUNMLQ( PLASMARIGHT, PLASMACONJTRANS, 2, 1, 0, A, 1, X, AF, 2, INFO )
*      CALL CHKXER( 'ZUNMLQ', INFOT, NOUT, INFO, OK )
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
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
*     Enable PLASMA warnings/errors
* 
      CALL PLASMA_ENABLE( PLASMA_WARNINGS, INFO )
      CALL PLASMA_ENABLE( PLASMA_ERRORS,   INFO )
*
      RETURN
*
*     End of ZERRLQ
*
      END
