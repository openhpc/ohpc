      SUBROUTINE SPOT03( UPLO, N, A, LDA, AINV, LDAINV, WORK, LDWORK,
     $                   RWORK, RCOND, RESID )
*
*  -- LAPACK test routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, LDAINV, LDWORK, N
      REAL               RCOND, RESID
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * ), AINV( LDAINV, * ), RWORK( * ),
     $                   WORK( LDWORK, * )
*     ..
*
*  Purpose
*  =======
*
*  SPOT03 computes the residual for a symmetric matrix times its
*  inverse:
*     norm( I - A*AINV ) / ( N * norm(A) * norm(AINV) * EPS ),
*  where EPS is the machine epsilon.
*
*  Arguments
*  ==========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          symmetric matrix A is stored:
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The number of rows and columns of the matrix A.  N >= 0.
*
*  A       (input) REAL array, dimension (LDA,N)
*          The original symmetric matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N)
*
*  AINV    (input/output) REAL array, dimension (LDAINV,N)
*          On entry, the inverse of the matrix A, stored as a symmetric
*          matrix in the same format as A.
*          In this version, AINV is expanded into a full matrix and
*          multiplied by A, so the opposing triangle of AINV will be
*          changed; i.e., if the upper triangular part of AINV is
*          stored, the lower triangular part will be used as work space.
*
*  LDAINV  (input) INTEGER
*          The leading dimension of the array AINV.  LDAINV >= max(1,N).
*
*  WORK    (workspace) REAL array, dimension (LDWORK,N)
*
*  LDWORK  (input) INTEGER
*          The leading dimension of the array WORK.  LDWORK >= max(1,N).
*
*  RWORK   (workspace) REAL array, dimension (N)
*
*  RCOND   (output) REAL
*          The reciprocal of the condition number of A, computed as
*          ( 1/norm(A) ) / norm(AINV).
*
*  RESID   (output) REAL
*          norm(I - A*AINV) / ( N * norm(A) * norm(AINV) * EPS )
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      REAL               AINVNM, ANORM, EPS
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      REAL               SLAMCH, SLANGE, SLANSY
      EXTERNAL           LSAME, SLAMCH, SLANGE, SLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           SSYMM
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          REAL
*     ..
*     .. Executable Statements ..
*
*     Quick exit if N = 0.
*
      IF( N.LE.0 ) THEN
         RCOND = ONE
         RESID = ZERO
         RETURN
      END IF
*
*     Exit with RESID = 1/EPS if ANORM = 0 or AINVNM = 0.
*
      EPS = SLAMCH( 'Epsilon' )
      ANORM = SLANSY( '1', UPLO, N, A, LDA, RWORK )
      AINVNM = SLANSY( '1', UPLO, N, AINV, LDAINV, RWORK )
      IF( ANORM.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
         RCOND = ZERO
         RESID = ONE / EPS
         RETURN
      END IF
      RCOND = ( ONE / ANORM ) / AINVNM
*
*     Expand AINV into a full matrix and call SSYMM to multiply
*     AINV on the left by A.
*
      IF( LSAME( UPLO, 'U' ) ) THEN
         DO 20 J = 1, N
            DO 10 I = 1, J - 1
               AINV( J, I ) = AINV( I, J )
   10       CONTINUE
   20    CONTINUE
      ELSE
         DO 40 J = 1, N
            DO 30 I = J + 1, N
               AINV( J, I ) = AINV( I, J )
   30       CONTINUE
   40    CONTINUE
      END IF
      CALL SSYMM( 'Left', UPLO, N, N, -ONE, A, LDA, AINV, LDAINV, ZERO,
     $            WORK, LDWORK )
*
*     Add the identity matrix to WORK .
*
      DO 50 I = 1, N
         WORK( I, I ) = WORK( I, I ) + ONE
   50 CONTINUE
*
*     Compute norm(I - A*AINV) / (N * norm(A) * norm(AINV) * EPS)
*
      RESID = SLANGE( '1', N, N, WORK, LDWORK, RWORK )
*
      RESID = ( ( RESID*RCOND ) / EPS ) / REAL( N )
*
      RETURN
*
*     End of SPOT03
*
      END
