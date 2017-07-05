      SUBROUTINE CGET02( TRANS, M, N, NRHS, A, LDA, X, LDX, B, LDB,
     $                   RWORK, RESID )
*
*  -- LAPACK test routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            LDA, LDB, LDX, M, N, NRHS
      REAL               RESID
*     ..
*     .. Array Arguments ..
      REAL               RWORK( * )
      COMPLEX            A( LDA, * ), B( LDB, * ), X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  CGET02 computes the residual for a solution of a system of linear
*  equations  A*x = b  or  A'*x = b:
*     RESID = norm( B - A*X ) / ( norm(A) * norm(X) + norm(RHS))* N * EPS ) .
*  where EPS is the machine epsilon.
*
*  Arguments
*  =========
*
*  TRANS   (input) CHARACTER*1
*          Specifies the form of the system of equations:
*          = 'N':  A *x = b
*          = 'T':  A^T*x = b, where A^T is the transpose of A
*          = 'C':  A^H*x = b, where A^H is the conjugate transpose of A
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of columns of B, the matrix of right hand sides.
*          NRHS >= 0.
*
*  A       (input) COMPLEX array, dimension (LDA,N)
*          The original M x N matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  X       (input) COMPLEX array, dimension (LDX,NRHS)
*          The computed solution vectors for the system of linear
*          equations.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X.  If TRANS = 'N',
*          LDX >= max(1,N); if TRANS = 'T' or 'C', LDX >= max(1,M).
*
*  B       (input/output) COMPLEX array, dimension (LDB,NRHS)
*          On entry, the right hand side vectors for the system of
*          linear equations.
*          On exit, B is overwritten with the difference B - A*X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  IF TRANS = 'N',
*          LDB >= max(1,M); if TRANS = 'T' or 'C', LDB >= max(1,N).
*
*  RWORK   (workspace) REAL array, dimension (M)
*
*  RESID   (output) REAL
*          The maximum over the number of right hand sides of
*        norm( B - A*X ) / ( norm(A) * norm(X) + norm(RHS))* N * EPS ) .
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
      COMPLEX            CONE
      PARAMETER          ( CONE = ( 1.0E+0, 0.0E+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            J, N1, N2
      REAL               ANORM, BNORM, EPS, XNORM, RHSNORM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      REAL               CLANGE, SCASUM, SLAMCH
      EXTERNAL           LSAME, CLANGE, SCASUM, SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           CGEMM
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Quick exit if M = 0 or N = 0 or NRHS = 0
*
      IF( M.LE.0 .OR. N.LE.0 .OR. NRHS.EQ.0 ) THEN
         RESID = ZERO
         RETURN
      END IF
*
      IF( LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' ) ) THEN
         N1 = N
         N2 = M
      ELSE
         N1 = M
         N2 = N
      END IF
*
*     Exit with RESID = 1/EPS if ANORM = 0.
*
      EPS = SLAMCH( 'Epsilon' )
      ANORM = CLANGE( '1', N1, N2, A, LDA, RWORK )
      RHSNORM = CLANGE( '1', N1, NRHS, B, LDB, RWORK )
      IF( ANORM.LE.ZERO ) THEN
         RESID = ONE / EPS
         RETURN
      END IF
*
*     Compute  B - A*X  (or  B - A'*X ) and store in B.
*
      CALL CGEMM( TRANS, 'No transpose', N1, NRHS, N2, -CONE, A, LDA, X,
     $            LDX, CONE, B, LDB )
*
*     Compute the maximum over the number of right hand sides of
*        norm( B - A*X ) / ( norm(A) * norm(X) + norm(RHS))* N * EPS ) .
*
      RESID = ZERO
      DO 10 J = 1, NRHS
         BNORM = SCASUM( N1, B( 1, J ), 1 )
         XNORM = SCASUM( N2, X( 1, J ), 1 )
         IF( XNORM.LE.ZERO ) THEN
            RESID = ONE / EPS
         ELSE
            RESID = MAX( RESID, ( BNORM) / ((ANORM * XNORM + RHSNORM)* 
     $   N1 *EPS ))
         END IF
   10 CONTINUE
*
      RETURN
*
*     End of CGET02
*
      END
