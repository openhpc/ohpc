      SUBROUTINE ZQRT13( SCALE, M, N, A, LDA, NORMA, ISEED )
*
*  -- LAPACK test routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            LDA, M, N, SCALE
      DOUBLE PRECISION   NORMA
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      COMPLEX*16         A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  ZQRT13 generates a full-rank matrix that may be scaled to have large
*  or small norm.
*
*  Arguments
*  =========
*
*  SCALE   (input) INTEGER
*          SCALE = 1: normally scaled matrix
*          SCALE = 2: matrix scaled up
*          SCALE = 3: matrix scaled down
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.
*
*  N       (input) INTEGER
*          The number of columns of A.
*
*  A       (output) COMPLEX*16 array, dimension (LDA,N)
*          The M-by-N matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*
*  NORMA   (output) DOUBLE PRECISION
*          The one-norm of A.
*
*  ISEED   (input/output) integer array, dimension (4)
*          Seed for random number generator
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, J
      DOUBLE PRECISION   BIGNUM, SMLNUM
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DZASUM, ZLANGE
      EXTERNAL           DLAMCH, DZASUM, ZLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLABAD, ZLARNV, ZLASCL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX, SIGN
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   DUMMY( 1 )
*     ..
*     .. Executable Statements ..
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
*     benign matrix
*
      DO 10 J = 1, N
         CALL ZLARNV( 2, ISEED, M, A( 1, J ) )
         IF( J.LE.M ) THEN
            A( J, J ) = A( J, J ) + DCMPLX( SIGN( DZASUM( M, A( 1, J ),
     $                  1 ), DBLE( A( J, J ) ) ) )
         END IF
   10 CONTINUE
*
*     scaled versions
*
      IF( SCALE.NE.1 ) THEN
         NORMA = ZLANGE( 'Max', M, N, A, LDA, DUMMY )
         SMLNUM = DLAMCH( 'Safe minimum' )
         BIGNUM = ONE / SMLNUM
         CALL DLABAD( SMLNUM, BIGNUM )
         SMLNUM = SMLNUM / DLAMCH( 'Epsilon' )
         BIGNUM = ONE / SMLNUM
*
         IF( SCALE.EQ.2 ) THEN
*
*           matrix scaled up
*
            CALL ZLASCL( 'General', 0, 0, NORMA, BIGNUM, M, N, A, LDA,
     $                   INFO )
         ELSE IF( SCALE.EQ.3 ) THEN
*
*           matrix scaled down
*
            CALL ZLASCL( 'General', 0, 0, NORMA, SMLNUM, M, N, A, LDA,
     $                   INFO )
         END IF
      END IF
*
      NORMA = ZLANGE( 'One-norm', M, N, A, LDA, DUMMY )
      RETURN
*
*     End of ZQRT13
*
      END
