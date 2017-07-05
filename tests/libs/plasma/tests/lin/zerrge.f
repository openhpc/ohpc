      SUBROUTINE ZERRGE( PATH, NUNIT )
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
*  ZERRGE tests the error exits for the COMPLEX*16 routines
*  for general matrices.
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
      DOUBLE PRECISION   ANRM
*      DOUBLE PRECISION   CCOND
      DOUBLE PRECISION   RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      INTEGER            HL( 2 ), HPIV( 2 )
*      DOUBLE PRECISION   R( NMAX )
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
      EXTERNAL           ALAESM, CHKXER, ZGBCON, ZGBEQU, ZGBRFS, ZGBTF2,
     $                   ZGBTRF, ZGBTRS, ZGECON, ZGEEQU, ZGERFS, ZGETF2,
     $                   ZGETRF, ZGETRI, ZGETRS
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
         IP( J ) = J
   20 CONTINUE
      ANRM = 1.D0
      OK = .TRUE.
*
*     Test error exits of the routines that use the LU decomposition
*     of a general matrix.
*
      IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        ZGETRF_INCPIV
*
*
*        ALLOCATE L and IPIV
*
         CALL PLASMA_ALLOC_WORKSPACE_ZGETRF_INCPIV(
     $        2, 1, HL, HPIV, INFO )
*
*        ZGETRF
*
         SRNAMT = 'ZGETRF'
         INFOT = 1
         CALL PLASMA_ZGETRF_INCPIV( -1, 0, A, 1, HL, HPIV, INFO )
         CALL CHKXER( 'ZGETRF', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_ZGETRF_INCPIV( 0, -1, A, 1, HL, HPIV, INFO )
         CALL CHKXER( 'ZGETRF', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_ZGETRF_INCPIV( 2, 1, A, 1, HL, HPIV, INFO )
         CALL CHKXER( 'ZGETRF', INFOT, NOUT, INFO, OK )
*
*        ZGETRS
*
         SRNAMT = 'ZGETRS'
         INFOT = 103
         CALL PLASMA_ZGETRS_INCPIV( '/', -1, 0, A, 1, HL, HPIV, 
     $        B, 1, INFO )
         CALL CHKXER( 'ZGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_ZGETRS_INCPIV( PLASMANOTRANS, -1, 0, A, 1, HL, 
     $        HPIV, B, 1, INFO )
         CALL CHKXER( 'ZGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_ZGETRS_INCPIV( PLASMANOTRANS, 0, -1, A, 1, HL, 
     $        HPIV, B, 1, INFO )
         CALL CHKXER( 'ZGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 5
         CALL PLASMA_ZGETRS_INCPIV( PLASMANOTRANS, 2, 1, A, 1, HL,
     $        HPIV, B, 2, INFO )
         CALL CHKXER( 'ZGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 9
         CALL PLASMA_ZGETRS_INCPIV( PLASMANOTRANS, 2, 1, A, 2, HL,
     $        HPIV, B, 1, INFO )
         CALL CHKXER( 'ZGETRS', INFOT, NOUT, INFO, OK )
*
*        DEALLOCATE L and IPIV
*
         CALL PLASMA_DEALLOC_HANDLE( HL, INFO )
         CALL PLASMA_DEALLOC_HANDLE( HPIV, INFO )
*
*        ZGETRF PP
*
         SRNAMT = 'ZGETRF'
         INFOT = 1
         CALL PLASMA_ZGETRF( -1, 0, A, 1, IP, INFO )
         CALL CHKXER( 'ZGETRF', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_ZGETRF( 0, -1, A, 1, IP, INFO )
         CALL CHKXER( 'ZGETRF', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_ZGETRF( 2, 1, A, 1, IP, INFO )
         CALL CHKXER( 'ZGETRF', INFOT, NOUT, INFO, OK )
*
*        ZGETRI
*
         SRNAMT = 'ZGETRI'
         INFOT = 1
         CALL PLASMA_ZGETRI( -1, A, 1, IP, INFO )
         CALL CHKXER( 'ZGETRI', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_ZGETRI( 2, A, 1, IP, INFO )
         CALL CHKXER( 'ZGETRI', INFOT, NOUT, INFO, OK )
*
*        ZGETRS
*
         SRNAMT = 'ZGETRS'
         INFOT = 1
         CALL PLASMA_ZGETRS( '/', 0, 0, A, 1, IP,
     $        B, 1, INFO )
         CALL CHKXER( 'ZGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_ZGETRS( PLASMANOTRANS, -1, 0, A, 1, IP,
     $        B, 1, INFO )
         CALL CHKXER( 'ZGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_ZGETRS( PLASMANOTRANS, 0, -1, A, 1, IP,
     $        B, 1, INFO )
         CALL CHKXER( 'ZGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 5
         CALL PLASMA_ZGETRS( PLASMANOTRANS, 2, 1, A, 1, IP,
     $        B, 2, INFO )
         CALL CHKXER( 'ZGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 7
         CALL PLASMA_ZGETRS( PLASMANOTRANS, 2, 1, A, 2, IP,
     $        B, 1, INFO )
         CALL CHKXER( 'ZGETRS', INFOT, NOUT, INFO, OK )
*
*        ZGECON
*
         SRNAMT = 'ZGECON'
         INFOT = 1
         CALL PLASMA_ZGECON( '/', 0, A, 1, ANRM, RCOND, INFO )
         CALL CHKXER( 'ZGECON', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_ZGECON( PLASMAONENORM, -1, A, 1, ANRM, RCOND,
     $        INFO )
         CALL CHKXER( 'ZGECON', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_ZGECON( PLASMAONENORM, 2, A, 1, ANRM, RCOND,
     $        INFO )
         CALL CHKXER( 'ZGECON', INFOT, NOUT, INFO, OK )
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
*     End of ZERRGE
*
      END
