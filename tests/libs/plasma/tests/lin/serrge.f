      SUBROUTINE SERRGE( PATH, NUNIT )
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
*  SERRGE tests the error exits for the REAL routines
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
      INTEGER            NMAX, LW
      PARAMETER          ( NMAX = 4, LW = 3*NMAX )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J
      REAL               ANRM
*     REAL               CCOND
      REAL               RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IW( NMAX )
      INTEGER            HL( 2 ), HPIV( 2 )
      REAL               A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   R1( NMAX ), R2( NMAX ), W( LW ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, SGBCON, SGBEQU, SGBRFS, SGBTF2,
     $                   SGBTRF, SGBTRS, SGECON, SGEEQU, SGERFS, SGETF2,
     $                   SGETRF, SGETRI, SGETRS
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
            A( I, J ) = 1. / REAL( I+J )
            AF( I, J ) = 1. / REAL( I+J )
   10    CONTINUE
         B( J ) = 0.
         R1( J ) = 0.
         R2( J ) = 0.
         W( J ) = 0.
         X( J ) = 0.
         IP( J ) = J
         IW( J ) = J
   20 CONTINUE
      ANRM = 1.
      OK = .TRUE.
*
*     Test error exits of the routines that use the LU decomposition
*     of a general matrix.
*
      IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        SGETRF_INCPIV
*
*
*        ALLOCATE L and IPIV
*
         CALL PLASMA_ALLOC_WORKSPACE_SGETRF_INCPIV(
     $        2, 1, HL, HPIV, INFO )
*
*        SGETRF
*
         SRNAMT = 'SGETRF'
         INFOT = 1
         CALL PLASMA_SGETRF_INCPIV( -1, 0, A, 1, HL, HPIV, INFO )
         CALL CHKXER( 'SGETRF', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_SGETRF_INCPIV( 0, -1, A, 1, HL, HPIV, INFO )
         CALL CHKXER( 'SGETRF', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_SGETRF_INCPIV( 2, 1, A, 1, HL, HPIV, INFO )
         CALL CHKXER( 'SGETRF', INFOT, NOUT, INFO, OK )
*
*        SGETRS
*
         SRNAMT = 'SGETRS'
         INFOT = 103
         CALL PLASMA_SGETRS_INCPIV( '/', -1, 0, A, 1, HL, HPIV, 
     $        B, 1, INFO )
         CALL CHKXER( 'SGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_SGETRS_INCPIV( PLASMANOTRANS, -1, 0, A, 1, HL, 
     $        HPIV, B, 1, INFO )
         CALL CHKXER( 'SGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_SGETRS_INCPIV( PLASMANOTRANS, 0, -1, A, 1, HL, 
     $        HPIV, B, 1, INFO )
         CALL CHKXER( 'SGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 5
         CALL PLASMA_SGETRS_INCPIV( PLASMANOTRANS, 2, 1, A, 1, HL, 
     $        HPIV, B, 2, INFO )
         CALL CHKXER( 'SGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 9
         CALL PLASMA_SGETRS_INCPIV( PLASMANOTRANS, 2, 1, A, 2, HL, 
     $        HPIV, B, 1, INFO )
         CALL CHKXER( 'SGETRS', INFOT, NOUT, INFO, OK )
*
*        DEALLOCATE L and IPIV
*
         CALL PLASMA_DEALLOC_HANDLE( HL, INFO )
         CALL PLASMA_DEALLOC_HANDLE( HPIV, INFO )
*
*        SGETRF PP
*
         SRNAMT = 'SGETRF'
         INFOT = 1
         CALL PLASMA_SGETRF( -1, 0, A, 1, IP, INFO )
         CALL CHKXER( 'SGETRF', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_SGETRF( 0, -1, A, 1, IP, INFO )
         CALL CHKXER( 'SGETRF', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_SGETRF( 2, 1, A, 1, IP, INFO )
         CALL CHKXER( 'SGETRF', INFOT, NOUT, INFO, OK )
*
*        SGETRI
*
         SRNAMT = 'SGETRI'
         INFOT = 1
         CALL PLASMA_SGETRI( -1, A, 1, IP, INFO )
         CALL CHKXER( 'SGETRI', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_SGETRI( 2, A, 1, IP, INFO )
         CALL CHKXER( 'SGETRI', INFOT, NOUT, INFO, OK )
*
*        SGETRS
*
         SRNAMT = 'SGETRS'
         INFOT = 1
         CALL PLASMA_SGETRS( '/', 0, 0, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'SGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_SGETRS( PLASMANOTRANS, -1, 0, A, 1, IP, 
     $        B, 1, INFO )
         CALL CHKXER( 'SGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 3
         CALL PLASMA_SGETRS( PLASMANOTRANS, 0, -1, A, 1, IP, 
     $        B, 1, INFO )
         CALL CHKXER( 'SGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 5
         CALL PLASMA_SGETRS( PLASMANOTRANS, 2, 1, A, 1, IP, 
     $        B, 2, INFO )
         CALL CHKXER( 'SGETRS', INFOT, NOUT, INFO, OK )
         INFOT = 7
         CALL PLASMA_SGETRS( PLASMANOTRANS, 2, 1, A, 2, IP, 
     $        B, 1, INFO )
         CALL CHKXER( 'SGETRS', INFOT, NOUT, INFO, OK )
*
*        SGECON
*
         SRNAMT = 'SGECON'
         INFOT = 1
         CALL PLASMA_SGECON( '/', 0, A, 1, ANRM, RCOND, INFO )
         CALL CHKXER( 'SGECON', INFOT, NOUT, INFO, OK )
         INFOT = 2
         CALL PLASMA_SGECON( PLASMAONENORM, -1, A, 1, ANRM, RCOND,
     $        INFO )
         CALL CHKXER( 'SGECON', INFOT, NOUT, INFO, OK )
         INFOT = 4
         CALL PLASMA_SGECON( PLASMAONENORM, 2, A, 1, ANRM, RCOND,
     $        INFO )
         CALL CHKXER( 'SGECON', INFOT, NOUT, INFO, OK )
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
*     End of SERRGE
*
      END
