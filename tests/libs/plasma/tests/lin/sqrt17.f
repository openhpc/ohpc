      REAL             FUNCTION SQRT17( TRANS, IRESID, M, N, NRHS, A,
     $                 LDA, X, LDX, B, LDB, C, WORK, LWORK )
*
*  -- LAPACK test routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            IRESID, LDA, LDB, LDX, LWORK, M, N, NRHS
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * ), B( LDB, * ), C( LDB, * ),
     $                   WORK( LWORK ), X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  SQRT17 computes the ratio
*
*     || R'*op(A) ||/(||A||*alpha*max(M,N,NRHS)*eps)
*
*  where R = op(A)*X - B, op(A) is A or A', and
*
*     alpha = ||B|| if IRESID = 1 (zero-residual problem)
*     alpha = ||R|| if IRESID = 2 (otherwise).
*
*  Arguments
*  =========
*
*  TRANS   (input) CHARACTER*1
*          Specifies whether or not the transpose of A is used.
*          = 'N':  No transpose, op(A) = A.
*          = 'T':  Transpose, op(A) = A'.
*
*  IRESID  (input) INTEGER
*          IRESID = 1 indicates zero-residual problem.
*          IRESID = 2 indicates non-zero residual.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.
*          If TRANS = 'N', the number of rows of the matrix B.
*          If TRANS = 'T', the number of rows of the matrix X.
*
*  N       (input) INTEGER
*          The number of columns of the matrix  A.
*          If TRANS = 'N', the number of rows of the matrix X.
*          If TRANS = 'T', the number of rows of the matrix B.
*
*  NRHS    (input) INTEGER
*          The number of columns of the matrices X and B.
*
*  A       (input) REAL array, dimension (LDA,N)
*          The m-by-n matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= M.
*
*  X       (input) REAL array, dimension (LDX,NRHS)
*          If TRANS = 'N', the n-by-nrhs matrix X.
*          If TRANS = 'T', the m-by-nrhs matrix X.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X.
*          If TRANS = 'N', LDX >= N.
*          If TRANS = 'T', LDX >= M.
*
*  B       (input) REAL array, dimension (LDB,NRHS)
*          If TRANS = 'N', the m-by-nrhs matrix B.
*          If TRANS = 'T', the n-by-nrhs matrix B.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.
*          If TRANS = 'N', LDB >= M.
*          If TRANS = 'T', LDB >= N.
*
*  C       (workspace) REAL array, dimension (LDB,NRHS)
*
*  WORK    (workspace) REAL array, dimension (LWORK)
*
*  LWORK   (input) INTEGER
*          The length of the array WORK.  LWORK >= NRHS*(M+N).
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E0, ONE = 1.0E0 )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, ISCL, NCOLS, NROWS
      REAL               BIGNUM, ERR, NORMA, NORMB, NORMRS, NORMX,
     $                   SMLNUM
*     ..
*     .. Local Arrays ..
      REAL               RWORK( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      REAL               SLAMCH, SLANGE
      EXTERNAL           LSAME, SLAMCH, SLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           SGEMM, SLACPY, SLASCL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, REAL
*     ..
*     .. Executable Statements ..
*
      SQRT17 = ZERO
*
      IF( LSAME( TRANS, 'N' ) ) THEN
         NROWS = M
         NCOLS = N
      ELSE IF( LSAME( TRANS, 'T' ) ) THEN
         NROWS = N
         NCOLS = M
      ELSE
         CALL XERBLA( 'SQRT17', 1 )
         RETURN
      END IF
*
      IF( LWORK.LT.NCOLS*NRHS ) THEN
         CALL XERBLA( 'SQRT17', 13 )
         RETURN
      END IF
*
      IF( M.LE.0 .OR. N.LE.0 .OR. NRHS.LE.0 ) THEN
         RETURN
      END IF
*
      NORMA = SLANGE( 'One-norm', M, N, A, LDA, RWORK )
      SMLNUM = SLAMCH( 'Safe minimum' ) / SLAMCH( 'Precision' )
      BIGNUM = ONE / SMLNUM
      ISCL = 0
*
*     compute residual and scale it
*
      CALL SLACPY( 'All', NROWS, NRHS, B, LDB, C, LDB )
      CALL SGEMM( TRANS, 'No transpose', NROWS, NRHS, NCOLS, -ONE, A,
     $            LDA, X, LDX, ONE, C, LDB )
      NORMRS = SLANGE( 'Max', NROWS, NRHS, C, LDB, RWORK )
      IF( NORMRS.GT.SMLNUM ) THEN
         ISCL = 1
         CALL SLASCL( 'General', 0, 0, NORMRS, ONE, NROWS, NRHS, C, LDB,
     $                INFO )
      END IF
*
*     compute R'*A
*
      CALL SGEMM( 'Transpose', TRANS, NRHS, NCOLS, NROWS, ONE, C, LDB,
     $            A, LDA, ZERO, WORK, NRHS )
*
*     compute and properly scale error
*
      ERR = SLANGE( 'One-norm', NRHS, NCOLS, WORK, NRHS, RWORK )
      IF( NORMA.NE.ZERO )
     $   ERR = ERR / NORMA
*
      IF( ISCL.EQ.1 )
     $   ERR = ERR*NORMRS
*
      IF( IRESID.EQ.1 ) THEN
         NORMB = SLANGE( 'One-norm', NROWS, NRHS, B, LDB, RWORK )
         IF( NORMB.NE.ZERO )
     $      ERR = ERR / NORMB
      ELSE
         NORMX = SLANGE( 'One-norm', NCOLS, NRHS, X, LDX, RWORK )
         IF( NORMX.NE.ZERO )
     $      ERR = ERR / NORMX
      END IF
*
      SQRT17 = ERR / ( SLAMCH( 'Epsilon' )*REAL( MAX( M, N, NRHS ) ) )
      RETURN
*
*     End of SQRT17
*
      END
