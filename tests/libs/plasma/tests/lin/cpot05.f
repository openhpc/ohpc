      SUBROUTINE CPOT05( UPLO, N, NRHS, A, LDA, B, LDB, X, LDX, XACT,
     $                   LDXACT, FERR, BERR, RESLTS )
*
*  -- LAPACK test routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, LDB, LDX, LDXACT, N, NRHS
*     ..
*     .. Array Arguments ..
      REAL               BERR( * ), FERR( * ), RESLTS( * )
      COMPLEX            A( LDA, * ), B( LDB, * ), X( LDX, * ),
     $                   XACT( LDXACT, * )
*     ..
*
*  Purpose
*  =======
*
*  CPOT05 tests the error bounds from iterative refinement for the
*  computed solution to a system of equations A*X = B, where A is a
*  Hermitian n by n matrix.
*
*  RESLTS(1) = test of the error bound
*            = norm(X - XACT) / ( norm(X) * FERR )
*
*  A large value is returned if this ratio is not less than one.
*
*  RESLTS(2) = residual from the iterative refinement routine
*            = the maximum of BERR / ( (n+1)*EPS + (*) ), where
*              (*) = (n+1)*UNFL / (min_i (abs(A)*abs(X) +abs(b))_i )
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          Hermitian matrix A is stored.
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The number of rows of the matrices X, B, and XACT, and the
*          order of the matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of columns of the matrices X, B, and XACT.
*          NRHS >= 0.
*
*  A       (input) COMPLEX array, dimension (LDA,N)
*          The Hermitian matrix A.  If UPLO = 'U', the leading n by n
*          upper triangular part of A contains the upper triangular part
*          of the matrix A, and the strictly lower triangular part of A
*          is not referenced.  If UPLO = 'L', the leading n by n lower
*          triangular part of A contains the lower triangular part of
*          the matrix A, and the strictly upper triangular part of A is
*          not referenced.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  B       (input) COMPLEX array, dimension (LDB,NRHS)
*          The right hand side vectors for the system of linear
*          equations.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  X       (input) COMPLEX array, dimension (LDX,NRHS)
*          The computed solution vectors.  Each vector is stored as a
*          column of the matrix X.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X.  LDX >= max(1,N).
*
*  XACT    (input) COMPLEX array, dimension (LDX,NRHS)
*          The exact solution vectors.  Each vector is stored as a
*          column of the matrix XACT.
*
*  LDXACT  (input) INTEGER
*          The leading dimension of the array XACT.  LDXACT >= max(1,N).
*
*  FERR    (input) REAL array, dimension (NRHS)
*          The estimated forward error bounds for each solution vector
*          X.  If XTRUE is the true solution, FERR bounds the magnitude
*          of the largest entry in (X - XTRUE) divided by the magnitude
*          of the largest entry in X.
*
*  BERR    (input) REAL array, dimension (NRHS)
*          The componentwise relative backward error of each solution
*          vector (i.e., the smallest relative change in any entry of A
*          or B that makes X an exact solution).
*
*  RESLTS  (output) REAL array, dimension (2)
*          The maximum over the NRHS solution vectors of the ratios:
*          RESLTS(1) = norm(X - XACT) / ( norm(X) * FERR )
*          RESLTS(2) = BERR / ( (n+1)*EPS + (*) )
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I, IMAX, J, K
      REAL               AXBI, DIFF, EPS, ERRBND, OVFL, TMP, UNFL, XNORM
      COMPLEX            ZDUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ICAMAX
      REAL               SLAMCH
      EXTERNAL           LSAME, ICAMAX, SLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, AIMAG, MAX, MIN, REAL
*     ..
*     .. Statement Functions ..
      REAL               CABS1
*     ..
*     .. Statement Function definitions ..
      CABS1( ZDUM ) = ABS( REAL( ZDUM ) ) + ABS( AIMAG( ZDUM ) )
*     ..
*     .. Executable Statements ..
*
*     Quick exit if N = 0 or NRHS = 0.
*
      IF( N.LE.0 .OR. NRHS.LE.0 ) THEN
         RESLTS( 1 ) = ZERO
         RESLTS( 2 ) = ZERO
         RETURN
      END IF
*
      EPS = SLAMCH( 'Epsilon' )
      UNFL = SLAMCH( 'Safe minimum' )
      OVFL = ONE / UNFL
      UPPER = LSAME( UPLO, 'U' )
*
*     Test 1:  Compute the maximum of
*        norm(X - XACT) / ( norm(X) * FERR )
*     over all the vectors X and XACT using the infinity-norm.
*
      ERRBND = ZERO
      DO 30 J = 1, NRHS
         IMAX = ICAMAX( N, X( 1, J ), 1 )
         XNORM = MAX( CABS1( X( IMAX, J ) ), UNFL )
         DIFF = ZERO
         DO 10 I = 1, N
            DIFF = MAX( DIFF, CABS1( X( I, J )-XACT( I, J ) ) )
   10    CONTINUE
*
         IF( XNORM.GT.ONE ) THEN
            GO TO 20
         ELSE IF( DIFF.LE.OVFL*XNORM ) THEN
            GO TO 20
         ELSE
            ERRBND = ONE / EPS
            GO TO 30
         END IF
*
   20    CONTINUE
         IF( DIFF / XNORM.LE.FERR( J ) ) THEN
            ERRBND = MAX( ERRBND, ( DIFF / XNORM ) / FERR( J ) )
         ELSE
            ERRBND = ONE / EPS
         END IF
   30 CONTINUE
      RESLTS( 1 ) = ERRBND
*
*     Test 2:  Compute the maximum of BERR / ( (n+1)*EPS + (*) ), where
*     (*) = (n+1)*UNFL / (min_i (abs(A)*abs(X) +abs(b))_i )
*
      DO 90 K = 1, NRHS
         DO 80 I = 1, N
            TMP = CABS1( B( I, K ) )
            IF( UPPER ) THEN
               DO 40 J = 1, I - 1
                  TMP = TMP + CABS1( A( J, I ) )*CABS1( X( J, K ) )
   40          CONTINUE
               TMP = TMP + ABS( REAL( A( I, I ) ) )*CABS1( X( I, K ) )
               DO 50 J = I + 1, N
                  TMP = TMP + CABS1( A( I, J ) )*CABS1( X( J, K ) )
   50          CONTINUE
            ELSE
               DO 60 J = 1, I - 1
                  TMP = TMP + CABS1( A( I, J ) )*CABS1( X( J, K ) )
   60          CONTINUE
               TMP = TMP + ABS( REAL( A( I, I ) ) )*CABS1( X( I, K ) )
               DO 70 J = I + 1, N
                  TMP = TMP + CABS1( A( J, I ) )*CABS1( X( J, K ) )
   70          CONTINUE
            END IF
            IF( I.EQ.1 ) THEN
               AXBI = TMP
            ELSE
               AXBI = MIN( AXBI, TMP )
            END IF
   80    CONTINUE
         TMP = BERR( K ) / ( ( N+1 )*EPS+( N+1 )*UNFL /
     $         MAX( AXBI, ( N+1 )*UNFL ) )
         IF( K.EQ.1 ) THEN
            RESLTS( 2 ) = TMP
         ELSE
            RESLTS( 2 ) = MAX( RESLTS( 2 ), TMP )
         END IF
   90 CONTINUE
*
      RETURN
*
*     End of CPOT05
*
      END
