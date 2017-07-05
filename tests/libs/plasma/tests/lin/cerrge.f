      SUBROUTINE CERRGE( PATH, NUNIT )
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
*  CERRGE tests the error exits for the COMPLEX routines
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
      REAL               ANRM
*     REAL               CCOND
      REAL               RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      INTEGER            HL( 2 ), HPIV( 2 )
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
      EXTERNAL           ALAESM, CGBCON, CGBEQU, CGBRFS, CGBTF2, CGBTRF,
     $                   CGBTRS, CGECON, CGEEQU, CGERFS, CGETF2, CGETRF,
     $                   CGETRI, CGETRS, CHKXER
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
         IP( J ) = J
   20 CONTINUE
      ANRM = 1.
      OK = .TRUE.
*
*     Test error exits of the routines that use the LU decomposition
*     of a general matrix.
*
      IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        CGETRF_INCPIV
*
*
*        ALLOCATE L and IPIV
*
         CALL PLASMA_ALLOC_WORKSPACE_CGETRF_INCPIV( 
     $        2, 1, HL, HPIV, INFO )
*
*        CGETRF
*
         SRNAMT = 'CGETRF'
         INFOT = 1
         CALL PLASMA_CGETRF_INCPIV( -1, 0, A, 1, HL, HPIV, INFO )
         CALL CHKXER( 'CGETRF', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_CGETRF_INCPIV( 0, -1, A, 1, HL, HPIV, INFO )
         CALL CHKXER( 'CGETRF', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_CGETRF_INCPIV( 2, 1, A, 1, HL, HPIV, INFO )
         CALL CHKXER( 'CGETRF', INFOT, NOUT, INFO, OK )
*
*        CGETRS
*
         SRNAMT = 'CGETRS'
         INFOT = 103
         CALL PLASMA_CGETRS_INCPIV( '/', -1, 0, A, 1, HL, HPIV, 
     $        B, 1, INFO )
         CALL CHKXER( 'CGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_CGETRS_INCPIV( PLASMANOTRANS, -1, 0, A, 1, HL, 
     $        HPIV, B, 1, INFO )
         CALL CHKXER( 'CGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_CGETRS_INCPIV( PLASMANOTRANS, 0, -1, A, 1, HL, 
     $        HPIV, B, 1, INFO )
         CALL CHKXER( 'CGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 5
         CALL PLASMA_CGETRS_INCPIV( PLASMANOTRANS, 2, 1, A, 1, HL, 
     $        HPIV, B, 2, INFO )
         CALL CHKXER( 'CGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 9
         CALL PLASMA_CGETRS_INCPIV( PLASMANOTRANS, 2, 1, A, 2, HL, 
     $        HPIV, B, 1, INFO )
         CALL CHKXER( 'CGETRS', INFOT, NOUT, INFO, OK )
*
*        DEALLOCATE L and IPIV
*
         CALL PLASMA_DEALLOC_HANDLE( HL, INFO )
         CALL PLASMA_DEALLOC_HANDLE( HPIV, INFO )
*
*        CGETRF PP
*
         SRNAMT = 'CGETRF'
         INFOT = 1
         CALL PLASMA_CGETRF( -1, 0, A, 1, IP, INFO )
         CALL CHKXER( 'CGETRF', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_CGETRF( 0, -1, A, 1, IP, INFO )
         CALL CHKXER( 'CGETRF', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_CGETRF( 2, 1, A, 1, IP, INFO )
         CALL CHKXER( 'CGETRF', INFOT, NOUT, INFO, OK )
*
*        CGETRI
*
         SRNAMT = 'CGETRI'
         INFOT = 1
         CALL PLASMA_CGETRI( -1, A, 1, IP, INFO )
         CALL CHKXER( 'CGETRI', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_CGETRI( 2, A, 1, IP, INFO )
         CALL CHKXER( 'CGETRI', INFOT, NOUT, INFO, OK )
*
*        CGETRS
*
         SRNAMT = 'CGETRS'
         INFOT = 1
         CALL PLASMA_CGETRS( '/', 0, 0, A, 1, IP,
     $        B, 1, INFO )
         CALL CHKXER( 'CGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_CGETRS( PLASMANOTRANS, -1, 0, A, 1, IP,
     $        B, 1, INFO )
         CALL CHKXER( 'CGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_CGETRS( PLASMANOTRANS, 0, -1, A, 1, IP,
     $        B, 1, INFO )
         CALL CHKXER( 'CGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 5
         CALL PLASMA_CGETRS( PLASMANOTRANS, 2, 1, A, 1, IP,
     $        B, 2, INFO )
         CALL CHKXER( 'CGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 7
         CALL PLASMA_CGETRS( PLASMANOTRANS, 2, 1, A, 2, IP,
     $        B, 1, INFO )
         CALL CHKXER( 'CGETRS', INFOT, NOUT, INFO, OK )
*
*        CGECON
*
         SRNAMT = 'CGECON'
         INFOT = 1
         CALL PLASMA_CGECON( '/', 0, A, 1, ANRM, RCOND, INFO )
         CALL CHKXER( 'CGECON', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_CGECON( PLASMAONENORM, -1, A, 1, ANRM, RCOND,
     $        INFO )
         CALL CHKXER( 'CGECON', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_CGECON( PLASMAONENORM, 2, A, 1, ANRM, RCOND,
     $        INFO )
         CALL CHKXER( 'CGECON', INFOT, NOUT, INFO, OK )
*
      ENDIF
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
*     End of CERRGE
*
      END
