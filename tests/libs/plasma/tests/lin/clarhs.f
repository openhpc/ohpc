      SUBROUTINE CLARHS( PATH, XTYPE, UPLO, TRANS, M, N, KL, KU, NRHS,
     $                   A, LDA, X, LDX, B, LDB, ISEED, INFO )
*
*  -- LAPACK test routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          TRANS, UPLO, XTYPE
      CHARACTER*3        PATH
      INTEGER            INFO, KL, KU, LDA, LDB, LDX, M, N, NRHS
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      COMPLEX            A( LDA, * ), B( LDB, * ), X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  CLARHS chooses a set of NRHS random solution vectors and sets
*  up the right hand sides for the linear system
*     op( A ) * X = B,
*  where op( A ) may be A, A**T (transpose of A), or A**H (conjugate
*  transpose of A).
*
*  Arguments
*  =========
*
*  PATH    (input) CHARACTER*3
*          The type of the complex matrix A.  PATH may be given in any
*          combination of upper and lower case.  Valid paths include
*             xGE:  General m x n matrix
*             xGB:  General banded matrix
*             xPO:  Hermitian positive definite, 2-D storage
*             xPP:  Hermitian positive definite packed
*             xPB:  Hermitian positive definite banded
*             xHE:  Hermitian indefinite, 2-D storage
*             xHP:  Hermitian indefinite packed
*             xHB:  Hermitian indefinite banded
*             xSY:  Symmetric indefinite, 2-D storage
*             xSP:  Symmetric indefinite packed
*             xSB:  Symmetric indefinite banded
*             xTR:  Triangular
*             xTP:  Triangular packed
*             xTB:  Triangular banded
*             xQR:  General m x n matrix
*             xLQ:  General m x n matrix
*             xQL:  General m x n matrix
*             xRQ:  General m x n matrix
*          where the leading character indicates the precision.
*
*  XTYPE   (input) CHARACTER*1
*          Specifies how the exact solution X will be determined:
*          = 'N':  New solution; generate a random X.
*          = 'C':  Computed; use value of X on entry.
*
*  UPLO    (input) CHARACTER*1
*          Used only if A is symmetric or triangular; specifies whether
*          the upper or lower triangular part of the matrix A is stored.
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  TRANS   (input) CHARACTER*1
*          Used only if A is nonsymmetric; specifies the operation
*          applied to the matrix A.
*          = 'N':  B := A    * X
*          = 'T':  B := A**T * X
*          = 'C':  B := A**H * X
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  KL      (input) INTEGER
*          Used only if A is a band matrix; specifies the number of
*          subdiagonals of A if A is a general band matrix or if A is
*          symmetric or triangular and UPLO = 'L'; specifies the number
*          of superdiagonals of A if A is symmetric or triangular and
*          UPLO = 'U'.  0 <= KL <= M-1.
*
*  KU      (input) INTEGER
*          Used only if A is a general band matrix or if A is
*          triangular.
*
*          If PATH = xGB, specifies the number of superdiagonals of A,
*          and 0 <= KU <= N-1.
*
*          If PATH = xTR, xTP, or xTB, specifies whether or not the
*          matrix has unit diagonal:
*          = 1:  matrix has non-unit diagonal (default)
*          = 2:  matrix has unit diagonal
*
*  NRHS    (input) INTEGER
*          The number of right hand side vectors in the system A*X = B.
*
*  A       (input) COMPLEX array, dimension (LDA,N)
*          The test matrix whose type is given by PATH.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*          If PATH = xGB, LDA >= KL+KU+1.
*          If PATH = xPB, xSB, xHB, or xTB, LDA >= KL+1.
*          Otherwise, LDA >= max(1,M).
*
*  X       (input or output) COMPLEX  array, dimension (LDX,NRHS)
*          On entry, if XTYPE = 'C' (for 'Computed'), then X contains
*          the exact solution to the system of linear equations.
*          On exit, if XTYPE = 'N' (for 'New'), then X is initialized
*          with random values.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X.  If TRANS = 'N',
*          LDX >= max(1,N); if TRANS = 'T', LDX >= max(1,M).
*
*  B       (output) COMPLEX  array, dimension (LDB,NRHS)
*          The right hand side vector(s) for the system of equations,
*          computed from B = op(A) * X, where op(A) is determined by
*          TRANS.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  If TRANS = 'N',
*          LDB >= max(1,M); if TRANS = 'T', LDB >= max(1,N).
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          The seed vector for the random number generator (used in
*          CLATMS).  Modified on exit.
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -k, the k-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX            ONE, ZERO
      PARAMETER          ( ONE = ( 1.0E+0, 0.0E+0 ),
     $                   ZERO = ( 0.0E+0, 0.0E+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            BAND, GEN, NOTRAN, QRS, SYM, TRAN, TRI
      CHARACTER          C1, DIAG
      CHARACTER*2        C2
      INTEGER            J, MB, NX
*     ..
*     .. External Functions ..
      LOGICAL            LSAME, LSAMEN
      EXTERNAL           LSAME, LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CGBMV, CGEMM, CHBMV, CHEMM, CHPMV, CLACPY,
     $                   CLARNV, CSBMV, CSPMV, CSYMM, CTBMV, CTPMV,
     $                   CTRMM, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      C1 = PATH( 1: 1 )
      C2 = PATH( 2: 3 )
      TRAN = LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' )
      NOTRAN = .NOT.TRAN
      GEN = LSAME( PATH( 2: 2 ), 'G' )
      QRS = LSAME( PATH( 2: 2 ), 'Q' ) .OR. LSAME( PATH( 3: 3 ), 'Q' )
      SYM = LSAME( PATH( 2: 2 ), 'P' ) .OR.
     $      LSAME( PATH( 2: 2 ), 'S' ) .OR. LSAME( PATH( 2: 2 ), 'H' )
      TRI = LSAME( PATH( 2: 2 ), 'T' )
      BAND = LSAME( PATH( 3: 3 ), 'B' )
      IF( .NOT.LSAME( C1, 'Complex precision' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LSAME( XTYPE, 'N' ) .OR. LSAME( XTYPE, 'C' ) ) )
     $          THEN
         INFO = -2
      ELSE IF( ( SYM .OR. TRI ) .AND. .NOT.
     $         ( LSAME( UPLO, 'U' ) .OR. LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -3
      ELSE IF( ( GEN.OR.QRS ) .AND.
     $   .NOT.( TRAN .OR. LSAME( TRANS, 'N' ) ) ) THEN
         INFO = -4
      ELSE IF( M.LT.0 ) THEN
         INFO = -5
      ELSE IF( N.LT.0 ) THEN
         INFO = -6
      ELSE IF( BAND .AND. KL.LT.0 ) THEN
         INFO = -7
      ELSE IF( BAND .AND. KU.LT.0 ) THEN
         INFO = -8
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -9
      ELSE IF( ( .NOT.BAND .AND. LDA.LT.MAX( 1, M ) ) .OR.
     $         ( BAND .AND. ( SYM .OR. TRI ) .AND. LDA.LT.KL+1 ) .OR.
     $         ( BAND .AND. GEN .AND. LDA.LT.KL+KU+1 ) ) THEN
         INFO = -11
      ELSE IF( ( NOTRAN .AND. LDX.LT.MAX( 1, N ) ) .OR.
     $         ( TRAN .AND. LDX.LT.MAX( 1, M ) ) ) THEN
         INFO = -13
      ELSE IF( ( NOTRAN .AND. LDB.LT.MAX( 1, M ) ) .OR.
     $         ( TRAN .AND. LDB.LT.MAX( 1, N ) ) ) THEN
         INFO = -15
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'CLARHS', -INFO )
         RETURN
      END IF
*
*     Initialize X to NRHS random vectors unless XTYPE = 'C'.
*
      IF( TRAN ) THEN
         NX = M
         MB = N
      ELSE
         NX = N
         MB = M
      END IF
      IF( .NOT.LSAME( XTYPE, 'C' ) ) THEN
         DO 10 J = 1, NRHS
            CALL CLARNV( 2, ISEED, N, X( 1, J ) )
   10    CONTINUE
      END IF
*
*     Multiply X by op( A ) using an appropriate
*     matrix multiply routine.
*
      IF( LSAMEN( 2, C2, 'GE' ) .OR. LSAMEN( 2, C2, 'QR' ) .OR.
     $    LSAMEN( 2, C2, 'LQ' ) .OR. LSAMEN( 2, C2, 'QL' ) .OR.
     $    LSAMEN( 2, C2, 'RQ' ) ) THEN
*
*        General matrix
*
         CALL CGEMM( TRANS, 'N', MB, NRHS, NX, ONE, A, LDA, X, LDX,
     $               ZERO, B, LDB )
*
      ELSE IF( LSAMEN( 2, C2, 'PO' ) .OR. LSAMEN( 2, C2, 'HE' ) ) THEN
*
*        Hermitian matrix, 2-D storage
*
         CALL CHEMM( 'Left', UPLO, N, NRHS, ONE, A, LDA, X, LDX, ZERO,
     $               B, LDB )
*
      ELSE IF( LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        Symmetric matrix, 2-D storage
*
         CALL CSYMM( 'Left', UPLO, N, NRHS, ONE, A, LDA, X, LDX, ZERO,
     $               B, LDB )
*
      ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        General matrix, band storage
*
         DO 20 J = 1, NRHS
            CALL CGBMV( TRANS, M, N, KL, KU, ONE, A, LDA, X( 1, J ), 1,
     $                  ZERO, B( 1, J ), 1 )
   20    CONTINUE
*
      ELSE IF( LSAMEN( 2, C2, 'PB' ) .OR. LSAMEN( 2, C2, 'HB' ) ) THEN
*
*        Hermitian matrix, band storage
*
         DO 30 J = 1, NRHS
            CALL CHBMV( UPLO, N, KL, ONE, A, LDA, X( 1, J ), 1, ZERO,
     $                  B( 1, J ), 1 )
   30    CONTINUE
*
      ELSE IF( LSAMEN( 2, C2, 'SB' ) ) THEN
*
*        Symmetric matrix, band storage
*
         DO 40 J = 1, NRHS
            CALL CSBMV( UPLO, N, KL, ONE, A, LDA, X( 1, J ), 1, ZERO,
     $                  B( 1, J ), 1 )
   40    CONTINUE
*
      ELSE IF( LSAMEN( 2, C2, 'PP' ) .OR. LSAMEN( 2, C2, 'HP' ) ) THEN
*
*        Hermitian matrix, packed storage
*
         DO 50 J = 1, NRHS
            CALL CHPMV( UPLO, N, ONE, A, X( 1, J ), 1, ZERO, B( 1, J ),
     $                  1 )
   50    CONTINUE
*
      ELSE IF( LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        Symmetric matrix, packed storage
*
         DO 60 J = 1, NRHS
            CALL CSPMV( UPLO, N, ONE, A, X( 1, J ), 1, ZERO, B( 1, J ),
     $                  1 )
   60    CONTINUE
*
      ELSE IF( LSAMEN( 2, C2, 'TR' ) ) THEN
*
*        Triangular matrix.  Note that for triangular matrices,
*           KU = 1 => non-unit triangular
*           KU = 2 => unit triangular
*
         CALL CLACPY( 'Full', N, NRHS, X, LDX, B, LDB )
         IF( KU.EQ.2 ) THEN
            DIAG = 'U'
         ELSE
            DIAG = 'N'
         END IF
         CALL CTRMM( 'Left', UPLO, TRANS, DIAG, N, NRHS, ONE, A, LDA, B,
     $               LDB )
*
      ELSE IF( LSAMEN( 2, C2, 'TP' ) ) THEN
*
*        Triangular matrix, packed storage
*
         CALL CLACPY( 'Full', N, NRHS, X, LDX, B, LDB )
         IF( KU.EQ.2 ) THEN
            DIAG = 'U'
         ELSE
            DIAG = 'N'
         END IF
         DO 70 J = 1, NRHS
            CALL CTPMV( UPLO, TRANS, DIAG, N, A, B( 1, J ), 1 )
   70    CONTINUE
*
      ELSE IF( LSAMEN( 2, C2, 'TB' ) ) THEN
*
*        Triangular matrix, banded storage
*
         CALL CLACPY( 'Full', N, NRHS, X, LDX, B, LDB )
         IF( KU.EQ.2 ) THEN
            DIAG = 'U'
         ELSE
            DIAG = 'N'
         END IF
         DO 80 J = 1, NRHS
            CALL CTBMV( UPLO, TRANS, DIAG, N, KL, A, LDA, B( 1, J ), 1 )
   80    CONTINUE
*
      ELSE
*
*        If none of the above, set INFO = -1 and return
*
         INFO = -1
         CALL XERBLA( 'CLARHS', -INFO )
      END IF
*
      RETURN
*
*     End of CLARHS
*
      END
