      SUBROUTINE ZERRQR( PATH, NUNIT )
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
*  ZERRQR tests the error exits for the COMPLEX*16 routines
*  that use the QR decomposition of a general matrix.
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
      EXTERNAL           ALAESM, CHKXER, ZGEQR2, ZGEQRF, ZUNG2R,
     $                   ZUNGQR, ZUNM2R, ZUNMQR
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
         W( J ) = 0.D0
         X( J ) = 0.D0
   20 CONTINUE
      OK = .TRUE.
*
*     Allocate HT
*
      CALL PLASMA_ALLOC_WORKSPACE_ZGEQRF( 2, 2, HT, INFO )
*
*
*     Error exits for QR factorization
*
*     ZGEQRF
*
      SRNAMT = 'ZGEQRF'
      INFOT = 1
      CALL PLASMA_ZGEQRF( -1, 0, A, 1, HT, INFO )
      CALL CHKXER( 'ZGEQRF', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_ZGEQRF( 0, -1, A, 1, HT, INFO )
      CALL CHKXER( 'ZGEQRF', INFOT, NOUT, INFO, OK )
      INFOT = 4
      CALL PLASMA_ZGEQRF( 2, 1, A, 1, HT, INFO )
      CALL CHKXER( 'ZGEQRF', INFOT, NOUT, INFO, OK )
*
*     ZGEQRS
*
      SRNAMT = 'ZGEQRS'
      INFOT = 1
      CALL PLASMA_ZGEQRS( -1, 0, 0, A, 1, X, B, 1, INFO )
      CALL CHKXER( 'ZGEQRS', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_ZGEQRS( 0, -1, 0, A, 1, X, B, 1, INFO )
      CALL CHKXER( 'ZGEQRS', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_ZGEQRS( 1, 2, 0, A, 2, X, B, 2, INFO )
      CALL CHKXER( 'ZGEQRS', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_ZGEQRS( 0, 0, -1, A, 1, X, B, 1, INFO )
      CALL CHKXER( 'ZGEQRS', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_ZGEQRS( 2, 1, 0, A, 1, X, B, 2, INFO )
      CALL CHKXER( 'ZGEQRS', INFOT, NOUT, INFO, OK )
      INFOT = 8
      CALL PLASMA_ZGEQRS( 2, 1, 0, A, 2, X, B, 1, INFO )
      CALL CHKXER( 'ZGEQRS', INFOT, NOUT, INFO, OK )
*
*     ZUNGQR
*
      SRNAMT = 'ZUNGQR'
      INFOT = 1
      CALL PLASMA_ZUNGQR( -1, 0, 0, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'ZUNGQR', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_ZUNGQR( 0, -1, 0, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'ZUNGQR', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_ZUNGQR( 1, 2, 0, A, 1, HT, W, 2, INFO )
      CALL CHKXER( 'ZUNGQR', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_ZUNGQR( 0, 0, -1, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'ZUNGQR', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_ZUNGQR( 1, 1, 2, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'ZUNGQR', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_ZUNGQR( 2, 2, 0, A, 1, HT, W, 2, INFO )
      CALL CHKXER( 'ZUNGQR', INFOT, NOUT, INFO, OK )
      INFOT = 8
      CALL PLASMA_ZUNGQR( 2, 2, 0, A, 2, HT, W, 1, INFO )
      CALL CHKXER( 'ZUNGQR', INFOT, NOUT, INFO, OK )
*
*     PLASMA_ZUNMQR
*
      SRNAMT = 'ZUNMQR'
      INFOT = 1
      CALL PLASMA_ZUNMQR( '/', PLASMACONJTRANS, 0, 0, 0, A, 1, HT, AF,
     $                   1, INFO )
      CALL CHKXER( 'ZUNMQR', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_ZUNMQR( PLASMALEFT, '/', 0, 0, 0, A, 1, HT, AF, 1,
     $                   INFO )
      CALL CHKXER( 'ZUNMQR', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_ZUNMQR( PLASMALEFT, PLASMACONJTRANS, -1, 0, 0, A, 1,
     $                   HT, AF, 1, INFO )
      CALL CHKXER( 'ZUNMQR', INFOT, NOUT, INFO, OK )
      INFOT = 4
      CALL PLASMA_ZUNMQR( PLASMALEFT, PLASMACONJTRANS, 0, -1, 0, A, 1,
     $                   HT, AF, 1, INFO )
      CALL CHKXER( 'ZUNMQR', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_ZUNMQR( PLASMALEFT, PLASMACONJTRANS, 0, 0, -1, A, 1,
     $                   HT, AF, 1, INFO )
      CALL CHKXER( 'ZUNMQR', INFOT, NOUT, INFO, OK )
*      INFOT = 5
*      CALL PLASMA_ZUNMQR( PLASMALEFT, PLASMACONJTRANS, 0, 1, 1, A, 1, HT,
*     4                   AF, 1, INFO )
*      CALL CHKXER( 'ZUNMQR', INFOT, NOUT, INFO, OK )
*      INFOT = 5
*      CALL PLASMA_ZUNMQR( PLASMALEFT, PLASMACONJTRANS, 1, 0, 1, A, 1, HT,
*     4                   AF, 1, INFO )
*      CALL CHKXER( 'ZUNMQR', INFOT, NOUT, INFO, OK )
*      INFOT = 7
*      CALL PLASMA_ZUNMQR( PLASMALEFT, PLASMACONJTRANS, 2, 1, 0, A, 1, HT,
*     4                   AF, 2, INFO )
*      CALL CHKXER( 'ZUNMQR', INFOT, NOUT, INFO, OK )
*      INFOT = 7
*      CALL PLASMA_ZUNMQR( PLASMALEFT, PLASMACONJTRANS, 1, 2, 0, A, 1, HT,
*     4                   AF, 1, INFO )
*      CALL CHKXER( 'ZUNMQR', INFOT, NOUT, INFO, OK )
*      INFOT = 10
*      CALL PLASMA_ZUNMQR( PLASMALEFT, PLASMACONJTRANS, 1, 2, 0, A, 1, HT,
*     4                   AF, 1, INFO )
*      CALL CHKXER( 'ZUNMQR', INFOT, NOUT, INFO, OK )
*      INFOT = 10
*      CALL PLASMA_ZUNMQR( PLASMALEFT, PLASMACONJTRANS, 2, 1, 0, A, 1, HT,
*     4                   AF, 2, INFO )
*      CALL CHKXER( 'ZUNMQR', INFOT, NOUT, INFO, OK )
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
*     End of ZERRQR
*
      END
