      SUBROUTINE DERRQR( PATH, NUNIT )
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
*  DERRQR tests the error exits for the DOUBLE PRECISION routines
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
      DOUBLE PRECISION   A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( NMAX ), X( NMAX )
      INTEGER            HT( 2 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, DGEQR2, DGEQRF, DORG2R,
     $                   DORGQR, DORM2R, DORMQR
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
         W( J ) = 0.D0
         X( J ) = 0.D0
   20 CONTINUE
      OK = .TRUE.
*
*     Allocate HT
*
      CALL PLASMA_ALLOC_WORKSPACE_DGEQRF( 2, 2, HT, INFO )

*
*     Error exits for QR factorization
*
*     PLASMA_DGEQRF
*
      SRNAMT = 'DGEQRF'
      INFOT = 1
      CALL PLASMA_DGEQRF( -1, 0, A, 1, HT, INFO )
      CALL CHKXER( 'DGEQRF', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_DGEQRF( 0, -1, A, 1, HT, INFO )
      CALL CHKXER( 'DGEQRF', INFOT, NOUT, INFO, OK )
      INFOT = 4
      CALL PLASMA_DGEQRF( 2, 1, A, 1, HT, INFO )
      CALL CHKXER( 'DGEQRF', INFOT, NOUT, INFO, OK )
*
*     DGEQRS
*
      SRNAMT = 'DGEQRS'
      INFOT = 1
      CALL PLASMA_DGEQRS( -1, 0, 0, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'DGEQRS', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_DGEQRS( 0, -1, 0, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'DGEQRS', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_DGEQRS( 1, 2, 0, A, 2, HT, B, 2, INFO )
      CALL CHKXER( 'DGEQRS', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_DGEQRS( 0, 0, -1, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'DGEQRS', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_DGEQRS( 2, 1, 0, A, 1, HT, B, 2, INFO )
      CALL CHKXER( 'DGEQRS', INFOT, NOUT, INFO, OK )
      INFOT = 8
      CALL PLASMA_DGEQRS( 2, 1, 0, A, 2, HT, B, 1, INFO )
      CALL CHKXER( 'DGEQRS', INFOT, NOUT, INFO, OK )
*
*     DORGQR
*
      SRNAMT = 'DORGQR'
      INFOT = 1
      CALL PLASMA_DORGQR( -1, 0, 0, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'DORGQR', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_DORGQR( 0, -1, 0, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'DORGQR', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_DORGQR( 1, 2, 0, A, 1, HT, W, 2, INFO )
      CALL CHKXER( 'DORGQR', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_DORGQR( 0, 0, -1, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'DORGQR', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_DORGQR( 1, 1, 2, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'DORGQR', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_DORGQR( 2, 2, 0, A, 1, HT, W, 2, INFO )
      CALL CHKXER( 'DORGQR', INFOT, NOUT, INFO, OK )
      INFOT = 8
      CALL PLASMA_DORGQR( 2, 2, 0, A, 2, HT, W, 1, INFO )
      CALL CHKXER( 'DORGQR', INFOT, NOUT, INFO, OK )
*
*     PLASMA_DORMQR
*
      SRNAMT = 'DORMQR'
      INFOT = 1
      CALL PLASMA_DORMQR( '/', PLASMATRANS, 0, 0, 0, A, 1, HT, AF, 1,
     $                   INFO )
      CALL CHKXER( 'DORMQR', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_DORMQR( PLASMALEFT, '/', 0, 0, 0, A, 1, HT, AF, 1,
     $                   INFO )
      CALL CHKXER( 'DORMQR', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_DORMQR( PLASMALEFT, PLASMATRANS, -1, 0, 0, A, 1, HT,
     $                   AF, 1, INFO )
      CALL CHKXER( 'DORMQR', INFOT, NOUT, INFO, OK )
      INFOT = 4
      CALL PLASMA_DORMQR( PLASMALEFT, PLASMATRANS, 0, -1, 0, A, 1, HT,
     $                   AF, 1, INFO )
      CALL CHKXER( 'DORMQR', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_DORMQR( PLASMALEFT, PLASMATRANS, 0, 0, -1, A, 1, HT,
     $                   AF, 1, INFO )
      CALL CHKXER( 'DORMQR', INFOT, NOUT, INFO, OK )
*      INFOT = 5
*      CALL PLASMA_DORMQR( PLASMALEFT, PLASMATRANS, 0, 1, 1, A, 1, HT,
*     4                   AF, 1, INFO )
*      CALL CHKXER( 'DORMQR', INFOT, NOUT, INFO, OK )
*      INFOT = 5
*      CALL PLASMA_DORMQR( PLASMALEFT, PLASMATRANS, 1, 0, 1, A, 1, HT,
*     4                   AF, 1, INFO )
*      CALL CHKXER( 'DORMQR', INFOT, NOUT, INFO, OK )
*      INFOT = 7
*      CALL PLASMA_DORMQR( PLASMALEFT, PLASMATRANS, 2, 1, 0, A, 1, HT,
*     4                   AF, 2, INFO )
*      CALL CHKXER( 'DORMQR', INFOT, NOUT, INFO, OK )
*      INFOT = 7
*      CALL PLASMA_DORMQR( PLASMALEFT, PLASMATRANS, 1, 2, 0, A, 1, HT,
*     4                   AF, 1, INFO )
*      CALL CHKXER( 'DORMQR', INFOT, NOUT, INFO, OK )
*      INFOT = 10
*      CALL PLASMA_DORMQR( PLASMALEFT, PLASMATRANS, 1, 2, 0, A, 1, HT,
*     4                   AF, 1, INFO )
*      CALL CHKXER( 'DORMQR', INFOT, NOUT, INFO, OK )
*      INFOT = 10
*      CALL PLASMA_DORMQR( PLASMALEFT, PLASMATRANS, 2, 1, 0, A, 1, HT,
*     4                   AF, 2, INFO )
*      CALL CHKXER( 'DORMQR', INFOT, NOUT, INFO, OK )
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
*     End of DERRQR
*
      END
