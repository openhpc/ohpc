      SUBROUTINE ZLAIPD( N, A, INDA, VINDA )
*
*  -- LAPACK test routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INDA, N, VINDA
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( * )
*     ..
*
*  Purpose
*  =======
*
*  ZLAIPD sets the imaginary part of the diagonal elements of a complex
*  matrix A to a large value.  This is used to test LAPACK routines for
*  complex Hermitian matrices, which are not supposed to access or use
*  the imaginary parts of the diagonals.
*
*  Arguments
*  =========
*
*  N      (input) INTEGER
*         The number of diagonal elements of A.
*
*  A      (input/output) COMPLEX*16 array, dimension
*                        (1+(N-1)*INDA+(N-2)*VINDA)
*         On entry, the complex (Hermitian) matrix A.
*         On exit, the imaginary parts of the diagonal elements are set
*         to BIGNUM = EPS / SAFMIN, where EPS is the machine epsilon and
*         SAFMIN is the safe minimum.
*
*  INDA   (input) INTEGER
*         The increment between A(1) and the next diagonal element of A.
*         Typical values are
*         = LDA+1:  square matrices with leading dimension LDA
*         = 2:  packed upper triangular matrix, starting at A(1,1)
*         = N:  packed lower triangular matrix, starting at A(1,1)
*
*  VINDA  (input) INTEGER
*         The change in the diagonal increment between columns of A.
*         Typical values are
*         = 0:  no change, the row and column increments in A are fixed
*         = 1:  packed upper triangular matrix
*         = -1:  packed lower triangular matrix
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, IA, IXA
      DOUBLE PRECISION   BIGNUM
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX
*     ..
*     .. Executable Statements ..
*
      BIGNUM = DLAMCH( 'Epsilon' ) / DLAMCH( 'Safe minimum' )
      IA = 1
      IXA = INDA
      DO 10 I = 1, N
         A( IA ) = DCMPLX( DBLE( A( IA ) ), BIGNUM )
         IA = IA + IXA
         IXA = IXA + VINDA
   10 CONTINUE
      RETURN
      END
