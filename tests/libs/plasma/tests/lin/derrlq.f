      SUBROUTINE DERRLQ( PATH, NUNIT )
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
*  DERRLQ tests the error exits for the DOUBLE PRECISION routines
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
      DOUBLE PRECISION   A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( NMAX ), X( NMAX )
      INTEGER            HT( 2 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, DGELQ2, DGELQF, DORGL2,
     $                   DORGLQ, DORML2, DORMLQ
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
      CALL PLASMA_ALLOC_WORKSPACE_DGELQF( 2, 2, HT, INFO )
*
*     Error exits for LQ factorization
*
*     PLASMA_DGELQF
*
      SRNAMT = 'DGELQF'
      INFOT = 1
      CALL PLASMA_DGELQF( -1, 0, A, 1, HT, INFO )
      CALL CHKXER( 'DGELQF', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_DGELQF( 0, -1, A, 1, HT, INFO )
      CALL CHKXER( 'DGELQF', INFOT, NOUT, INFO, OK )
      INFOT = 4
      CALL PLASMA_DGELQF( 2, 1, A, 1, HT, INFO )
      CALL CHKXER( 'DGELQF', INFOT, NOUT, INFO, OK )
*
*     DGELQS
*
      SRNAMT = 'DGELQS'
      INFOT = 1
      CALL PLASMA_DGELQS( -1, 0, 0, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'DGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_DGELQS( 0, -1, 0, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'DGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_DGELQS( 2, 1, 0, A, 2, HT, B, 1, INFO )
      CALL CHKXER( 'DGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_DGELQS( 0, 0, -1, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'DGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_DGELQS( 2, 2, 0, A, 1, HT, B, 2, INFO )
      CALL CHKXER( 'DGELQS', INFOT, NOUT, INFO, OK )
      INFOT = 8
      CALL PLASMA_DGELQS( 1, 2, 0, A, 1, HT, B, 1, INFO )
      CALL CHKXER( 'DGELQS', INFOT, NOUT, INFO, OK )
*
*     DORGLQ
*
      SRNAMT = 'DORGLQ'
      INFOT = 1
      CALL PLASMA_DORGLQ( -1, 0, 0, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'DORGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_DORGLQ( 0, -1, 0, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'DORGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_DORGLQ( 2, 1, 0, A, 2, HT, W, 2, INFO )
      CALL CHKXER( 'DORGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_DORGLQ( 0, 0, -1, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'DORGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_DORGLQ( 1, 1, 2, A, 1, HT, W, 1, INFO )
      CALL CHKXER( 'DORGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_DORGLQ( 2, 2, 0, A, 1, HT, W, 2, INFO )
      CALL CHKXER( 'DORGLQ', INFOT, NOUT, INFO, OK )
      INFOT = 8
      CALL PLASMA_DORGLQ( 2, 2, 0, A, 2, HT, W, 1, INFO )
      CALL CHKXER( 'DORGLQ', INFOT, NOUT, INFO, OK )
*
*     PLASMA_DORMLQ
*
      SRNAMT = 'DORMLQ'
      INFOT = 1
      CALL PLASMA_DORMLQ( '/', PLASMATRANS, 0, 0, 0, A, 1, HT, AF, 1,
     $                   INFO )
      CALL CHKXER( 'DORMLQ', INFOT, NOUT, INFO, OK )
      INFOT = 2
      CALL PLASMA_DORMLQ( PLASMALEFT, '/', 0, 0, 0, A, 1, HT, AF, 1,
     $                   INFO )
      CALL CHKXER( 'DORMLQ', INFOT, NOUT, INFO, OK )
      INFOT = 3
      CALL PLASMA_DORMLQ( PLASMALEFT, PLASMATRANS, -1, 0, 0, A, 1, HT,
     $                   AF, 1, INFO )
      CALL CHKXER( 'DORMLQ', INFOT, NOUT, INFO, OK )
      INFOT = 4
      CALL PLASMA_DORMLQ( PLASMALEFT, PLASMATRANS, 0, -1, 0, A, 1, HT,
     $                   AF, 1, INFO )
      CALL CHKXER( 'DORMLQ', INFOT, NOUT, INFO, OK )
      INFOT = 5
      CALL PLASMA_DORMLQ( PLASMALEFT, PLASMATRANS, 0, 0, -1, A, 1, HT,
     $                   AF, 1, INFO )
      CALL CHKXER( 'DORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 5
*      CALL PLASMA_DORMLQ( PLASMALEFT, PLASMATRANS, 0, 1, 1, A, 1, HT, AF, 1,
*     $                   INFO )
*      CALL CHKXER( 'DORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 5
*      CALL PLASMA_DORMLQ( PLASMARIGHT, PLASMATRANS, 1, 0, 1, A, 1, HT, AF, 1,
*     $                   INFO )
*      CALL CHKXER( 'DORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 7
*      CALL PLASMA_DORMLQ( PLASMALEFT, PLASMATRANS, 2, 0, 2, A, 1, HT, AF, 2,
*     $                   INFO )
*      CALL CHKXER( 'DORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 7
*      CALL PLASMA_DORMLQ( PLASMARIGHT, PLASMATRANS, 0, 2, 2, A, 1, HT, AF, 1,
*     $                   INFO )
*      CALL CHKXER( 'DORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 10
*      CALL PLASMA_DORMLQ( PLASMALEFT, PLASMATRANS, 2, 1, 0, A, 2, HT, AF, 1,
*     $                   INFO )
*      CALL CHKXER( 'DORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 12
*      CALL PLASMA_DORMLQ( PLASMALEFT, PLASMATRANS, 1, 2, 0, A, 1, HT, AF, 1,
*     $                   INFO )
*      CALL CHKXER( 'DORMLQ', INFOT, NOUT, INFO, OK )
*      INFOT = 12
*      CALL PLASMA_DORMLQ( PLASMARIGHT, PLASMATRANS, 2, 1, 0, A, 1, HT, AF, 2,
*     $                   INFO )
*      CALL CHKXER( 'DORMLQ', INFOT, NOUT, INFO, OK )
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
      RETURN
*
*     End of DERRLQ
*
      END
