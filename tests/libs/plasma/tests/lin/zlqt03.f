      SUBROUTINE ZLQT03( M, N, K, AF, C, CC, Q, LDA, T, WORK, LWORK,
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
      COMPLEX*16         AF( LDA, * ), C( LDA, * ), CC( LDA, * ),
     $                   Q( LDA, * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  ZLQT03 tests ZUNMLQ, which computes Q*C, Q'*C, C*Q or C*Q'.
*
*  ZLQT03 compares the results of a call to ZUNMLQ with the results of
*  forming Q explicitly by a call to ZUNGLQ and then performing matrix
*  multiplication by a call to ZGEMM.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows or columns of the matrix C; C is n-by-m if
*          Q is applied from the left, or m-by-n if Q is applied from
*          the right.  M >= 0.
*
*  N       (input) INTEGER
*          The order of the orthogonal matrix Q.  N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          orthogonal matrix Q.  N >= K >= 0.
*
*  AF      (input) COMPLEX*16 array, dimension (LDA,N)
*          Details of the LQ factorization of an m-by-n matrix, as
*          returned by ZGELQF. See CGELQF for further details.
*
*  C       (workspace) COMPLEX*16 array, dimension (LDA,N)
*
*  CC      (workspace) COMPLEX*16 array, dimension (LDA,N)
*
*  Q       (workspace) COMPLEX*16 array, dimension (LDA,N)
*
*  LDA     (input) INTEGER
*          The leading dimension of the arrays AF, C, CC, and Q.
*
*  TAU     (input) COMPLEX*16 array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors corresponding
*          to the LQ factorization in AF.
*
*  WORK    (workspace) COMPLEX*16 array, dimension (LWORK)
*
*  LWORK   (input) INTEGER
*          The length of WORK.  LWORK must be at least M, and should be
*          M*NB, where NB is the blocksize for this environment.
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (M)
*
*  RESULT  (output) DOUBLE PRECISION array, dimension (4)
*          The test ratios compare two techniques for multiplying a
*          random matrix C by an n-by-n orthogonal matrix Q.
*          RESULT(1) = norm( Q*C - Q*C )  / ( N * norm(C) * EPS )
*          RESULT(2) = norm( C*Q - C*Q )  / ( N * norm(C) * EPS )
*          RESULT(3) = norm( Q'*C - Q'*C )/ ( N * norm(C) * EPS )
*          RESULT(4) = norm( C*Q' - C*Q' )/ ( N * norm(C) * EPS )
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
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, ISIDE, ITRANS, J, MC, NC
      INTEGER            PLASMA_SIDE, PLASMA_TRANS
      DOUBLE PRECISION   CNORM, EPS, RESID
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, ZLANGE
      EXTERNAL           LSAME, DLAMCH, ZLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZGEMM, ZLACPY, ZLARNV, ZLASET, ZUNGLQ, ZUNMLQ
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 )
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
*     .. Data statements ..
      DATA               ISEED / 1988, 1989, 1990, 1991 /
*     ..
*     .. Executable Statements ..
*
      EPS = DLAMCH( 'Epsilon' )
*
*     Copy the first k rows of the factorization to the array Q
*
      IF ( K.EQ.0 ) THEN
          CALL ZLASET( 'Full', N, N, ROGUE, ROGUE, Q, LDA )
      ELSE
          CALL ZLASET( 'Full', N, N, DCMPLX( ZERO ), DCMPLX( ONE ),
     $                 Q, LDA )
      ENDIF
*
*     Generate the n-by-n matrix Q
*
      SRNAMT = 'ZUNGLQ'
      CALL PLASMA_ZUNGLQ( N, N, K, AF, LDA, T, Q, LDA, INFO )
*
      DO 30 ISIDE = 1, 2
         IF( ISIDE.EQ.1 ) THEN
            SIDE = 'L'
            PLASMA_SIDE = PLASMALEFT
            MC = N
            NC = M
         ELSE
            SIDE = 'R'
            PLASMA_SIDE = PLASMARIGHT
            MC = M
            NC = N
         END IF
*
*        Generate MC by NC matrix C
*
         DO 10 J = 1, NC
            CALL ZLARNV( 2, ISEED, MC, C( 1, J ) )
   10    CONTINUE
         CNORM = ZLANGE( '1', MC, NC, C, LDA, RWORK )
         IF( CNORM.EQ.ZERO )
     $      CNORM = ONE
*
         DO 20 ITRANS = 1, 2
            IF( ITRANS.EQ.1 ) THEN
               PLASMA_TRANS = PLASMANOTRANS
               TRANS = 'N'
            ELSE
               TRANS = 'C'
               PLASMA_TRANS = PLASMACONJTRANS
            END IF
*
*           Copy C
*
            CALL ZLACPY( 'Full', MC, NC, C, LDA, CC, LDA )
*
*           Apply Q or Q' to C
*
            SRNAMT = 'ZUNMLQ'
            CALL PLASMA_ZUNMLQ( PLASMA_SIDE, PLASMA_TRANS, MC, NC, K,
     $                   AF, LDA, T, CC, LDA, INFO )
*
*           Form explicit product and subtract
*
            IF ( K.EQ.0 ) THEN
               CALL ZLASET( 'Full', N, N, DCMPLX( ZERO ),
     $              DCMPLX( ONE ), Q, LDA )
            ENDIF
            IF( LSAME( SIDE, 'L' ) ) THEN
               CALL ZGEMM( TRANS, 'No transpose', MC, NC, MC,
     $                     DCMPLX( -ONE ), Q, LDA, C, LDA,
     $                     DCMPLX( ONE ), CC, LDA )
            ELSE
               CALL ZGEMM( 'No transpose', TRANS, MC, NC, NC,
     $                     DCMPLX( -ONE ), C, LDA, Q, LDA,
     $                     DCMPLX( ONE ), CC, LDA )
            END IF
*
*           Compute error in the difference
*
            RESID = ZLANGE( '1', MC, NC, CC, LDA, RWORK )
            RESULT( ( ISIDE-1 )*2+ITRANS ) = RESID /
     $         ( DBLE( MAX( 1, N ) )*CNORM*EPS )
*
   20    CONTINUE
   30 CONTINUE
*
      RETURN
*
*     End of ZLQT03
*
      END
