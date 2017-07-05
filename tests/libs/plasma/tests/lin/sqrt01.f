      SUBROUTINE SQRT01( M, N, A, AF, Q, R, LDA, T, WORK, LWORK,
     $                   RWORK, RESULT )
*
      INCLUDE 'plasmaf.h'
*
*  -- LAPACK test routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LWORK, M, N
      INTEGER            T( 2 )
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * ), AF( LDA, * ), Q( LDA, * ),
     $                   R( LDA, * ), RESULT( * ), RWORK( * ), 
     $                   WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  SQRT01 tests SGEQRF, which computes the QR factorization of an m-by-n
*  matrix A, and partially tests SORGQR which forms the m-by-m
*  orthogonal matrix Q.
*
*  SQRT01 compares R with Q'*A, and checks that Q is orthogonal.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input) REAL array, dimension (LDA,N)
*          The m-by-n matrix A.
*
*  AF      (output) REAL array, dimension (LDA,N)
*          Details of the QR factorization of A, as returned by SGEQRF.
*          See SGEQRF for further details.
*
*  Q       (output) REAL array, dimension (LDA,M)
*          The m-by-m orthogonal matrix Q.
*
*  R       (workspace) REAL array, dimension (LDA,max(M,N))
*
*  LDA     (input) INTEGER
*          The leading dimension of the arrays A, AF, Q and R.
*          LDA >= max(M,N).
*
*  TAU     (output) REAL array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors, as returned
*          by SGEQRF.
*
*  WORK    (workspace) REAL array, dimension (LWORK)
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*
*  RWORK   (workspace) REAL array, dimension (M)
*
*  RESULT  (output) REAL array, dimension (2)
*          The test ratios:
*          RESULT(1) = norm( R - Q'*A ) / ( M * norm(A) * EPS )
*          RESULT(2) = norm( I - Q'*Q ) / ( M * EPS )
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
      REAL               ROGUE
      PARAMETER          ( ROGUE = -1.0E+10 )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, MINMN
      REAL               ANORM, EPS, RESID
*     ..
*     .. External Functions ..
      REAL               SLAMCH, SLANGE, SLANSY
      EXTERNAL           SLAMCH, SLANGE, SLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           SGEMM, SGEQRF, SLACPY, SLASET, SORGQR, SSYRK
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, REAL
*     ..
*     .. Scalars in Common ..
      CHARACTER*32       SRNAMT
*     ..
*     .. Common blocks ..
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Executable Statements ..
*
      MINMN = MIN( M, N )
      EPS = SLAMCH( 'Epsilon' )
*
*     Copy the matrix A to the array AF.
*
      CALL SLACPY( 'Full', M, N, A, LDA, AF, LDA )
*
*     Factorize the matrix A in the array AF.
*
      SRNAMT = 'SGEQRF'
      CALL PLASMA_SGEQRF( M, N, AF, LDA, T, INFO )
*
*     Copy details of Q
*
      CALL SLASET( 'Full', M, N, ZERO, ONE, Q, LDA )
*
*     Generate the m-by-m matrix Q
*
      SRNAMT = 'SORGQR'
      CALL PLASMA_SORGQR( M, N, MINMN, AF, LDA, T, Q, LDA, INFO )
*
*     Copy R
*
      CALL SLASET( 'Full', N, N, ZERO, ZERO, R, LDA )
      CALL SLACPY( 'Upper', N, N, AF, LDA, R, LDA )
*
*     Compute R - Q'*A
*
      CALL SGEMM( 'Transpose', 'No transpose', N, N, M, -ONE, Q, LDA, A,
     $            LDA, ONE, R, LDA )
*
*     Compute norm( R - Q'*A ) / ( M * norm(A) * EPS ) .
*
      ANORM = SLANGE( '1', M, N, A, LDA, RWORK )
      RESID = SLANGE( '1', N, N, R, LDA, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = ( ( RESID / REAL( MAX( 1, M ) ) ) / ANORM ) / EPS
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute I - Q'*Q
*
      CALL SLASET( 'Full', N, N, ZERO, ONE, R, LDA )
      CALL SSYRK( 'Upper', 'Transpose', N, M, ONE, Q, LDA, -ONE, R,
     $            LDA )
*
*     Compute norm( I - Q'*Q ) / ( M * EPS ) .
*
      RESID = SLANSY( '1', 'Upper', N, R, LDA, RWORK )
*
      RESULT( 2 ) = ( RESID / REAL( MAX( 1, N ) ) ) / EPS
*
      RETURN
*
*     End of SQRT01
*
      END
