      SUBROUTINE ZQRT02( M, N, K, A, AF, Q, R, LDA, T, WORK, LWORK,
     $                   RWORK, RESULT )
*
      INCLUDE 'plasmaf.h'
*
*  -- LAPACK test routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            K, LDA, LWORK, M, N
      INTEGER            T( 2 )
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   RESULT( * ), RWORK( * )
      COMPLEX*16         A( LDA, * ), AF( LDA, * ), Q( LDA, * ),
     $                   R( LDA, * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  ZQRT02 tests ZUNGQR, which generates an m-by-n matrix Q with
*  orthonornmal columns that is defined as the product of k elementary
*  reflectors.
*
*  Given the QR factorization of an m-by-n matrix A, ZQRT02 generates
*  the orthogonal matrix Q defined by the factorization of the first k
*  columns of A; it compares R(1:n,1:k) with Q(1:m,1:n)'*A(1:m,1:k),
*  and checks that the columns of Q are orthonormal.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q to be generated.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q to be generated.
*          M >= N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. N >= K >= 0.
*
*  A       (input) COMPLEX*16 array, dimension (LDA,N)
*          The m-by-n matrix A which was factorized by ZQRT01.
*
*  AF      (input) COMPLEX*16 array, dimension (LDA,N)
*          Details of the QR factorization of A, as returned by ZGEQRF.
*          See ZGEQRF for further details.
*
*  Q       (workspace) COMPLEX*16 array, dimension (LDA,N)
*
*  R       (workspace) COMPLEX*16 array, dimension (LDA,N)
*
*  LDA     (input) INTEGER
*          The leading dimension of the arrays A, AF, Q and R. LDA >= M.
*
*  TAU     (input) COMPLEX*16 array, dimension (N)
*          The scalar factors of the elementary reflectors corresponding
*          to the QR factorization in AF.
*
*  WORK    (workspace) COMPLEX*16 array, dimension (LWORK)
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (M)
*
*  RESULT  (output) DOUBLE PRECISION array, dimension (2)
*          The test ratios:
*          RESULT(1) = norm( R - Q'*A ) / ( M * norm(A) * EPS )
*          RESULT(2) = norm( I - Q'*Q ) / ( M * EPS )
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      COMPLEX*16         ROGUE
      PARAMETER          ( ROGUE = ( -1.0D+10, -1.0D+10 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO
      DOUBLE PRECISION   ANORM, EPS, RESID
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, ZLANGE, ZLANSY
      EXTERNAL           DLAMCH, ZLANGE, ZLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZGEMM, ZHERK, ZLACPY, ZLASET, ZUNGQR
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX, MAX
*     ..
*     .. Scalars in Common ..
      CHARACTER*32       SRNAMT
*     ..
*     .. Common blocks ..
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Executable Statements ..
*
      EPS = DLAMCH( 'Epsilon' )
*
*     Copy the first k columns of the factorization to the array Q
*
      CALL ZLASET( 'Full', M, N, DCMPLX( ZERO ), DCMPLX( ONE ), Q, LDA )
*
*     Generate the first n columns of the matrix Q
*
      SRNAMT = 'ZUNGQR'
      CALL PLASMA_ZUNGQR( M, N, K, AF, LDA, T, Q, LDA, INFO )
*
*     Copy R(1:n,1:k)
*
      CALL ZLASET( 'Full', N, K, DCMPLX( ZERO ), DCMPLX( ZERO ), R,
     $             LDA )
      CALL ZLACPY( 'Upper', N, K, AF, LDA, R, LDA )
*
*     Compute R(1:n,1:k) - Q(1:m,1:n)' * A(1:m,1:k)
*
      CALL ZGEMM( 'Conjugate transpose', 'No transpose', N, K, M,
     $            DCMPLX( -ONE ), Q, LDA, A, LDA, DCMPLX( ONE ), R,
     $            LDA )
*
*     Compute norm( R - Q'*A ) / ( M * norm(A) * EPS ) .
*
      ANORM = ZLANGE( '1', M, K, A, LDA, RWORK )
      RESID = ZLANGE( '1', N, K, R, LDA, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = ( ( RESID / DBLE( MAX( 1, M ) ) ) / ANORM ) / EPS
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute I - Q'*Q
*
      CALL ZLASET( 'Full', N, N, DCMPLX( ZERO ), DCMPLX( ONE ), R, LDA )
      CALL ZHERK( 'Upper', 'Conjugate transpose', N, M, -ONE, Q, LDA,
     $            ONE, R, LDA )
*
*     Compute norm( I - Q'*Q ) / ( M * EPS ) .
*
      RESID = ZLANSY( '1', 'Upper', N, R, LDA, RWORK )
*
      RESULT( 2 ) = ( RESID / DBLE( MAX( 1, M ) ) ) / EPS
*
      RETURN
*
*     End of ZQRT02
*
      END
