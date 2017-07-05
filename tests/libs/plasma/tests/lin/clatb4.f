      SUBROUTINE CLATB4( PATH, IMAT, M, N, TYPE, KL, KU, ANORM, MODE,
     $                   CNDNUM, DIST )
*
*  -- LAPACK test routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          DIST, TYPE
      CHARACTER*3        PATH
      INTEGER            IMAT, KL, KU, M, MODE, N
      REAL               ANORM, CNDNUM
*     ..
*
*  Purpose
*  =======
*
*  CLATB4 sets parameters for the matrix generator based on the type of
*  matrix to be generated.
*
*  Arguments
*  =========
*
*  PATH    (input) CHARACTER*3
*          The LAPACK path name.
*
*  IMAT    (input) INTEGER
*          An integer key describing which matrix to generate for this
*          path.
*
*  M       (input) INTEGER
*          The number of rows in the matrix to be generated.
*
*  N       (input) INTEGER
*          The number of columns in the matrix to be generated.
*
*  TYPE    (output) CHARACTER*1
*          The type of the matrix to be generated:
*          = 'S':  symmetric matrix
*          = 'P':  symmetric positive (semi)definite matrix
*          = 'N':  nonsymmetric matrix
*
*  KL      (output) INTEGER
*          The lower band width of the matrix to be generated.
*
*  KU      (output) INTEGER
*          The upper band width of the matrix to be generated.
*
*  ANORM   (output) REAL
*          The desired norm of the matrix to be generated.  The diagonal
*          matrix of singular values or eigenvalues is scaled by this
*          value.
*
*  MODE    (output) INTEGER
*          A key indicating how to choose the vector of eigenvalues.
*
*  CNDNUM  (output) REAL
*          The desired condition number.
*
*  DIST    (output) CHARACTER*1
*          The type of distribution to be used by the random number
*          generator.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               SHRINK, TENTH
      PARAMETER          ( SHRINK = 0.25E0, TENTH = 0.1E+0 )
      REAL               ONE
      PARAMETER          ( ONE = 1.0E+0 )
      REAL               TWO
      PARAMETER          ( TWO = 2.0E+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FIRST
      CHARACTER*2        C2
      INTEGER            MAT
      REAL               BADC1, BADC2, EPS, LARGE, SMALL
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      REAL               SLAMCH
      EXTERNAL           LSAMEN, SLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. External Subroutines ..
      EXTERNAL           SLABAD
*     ..
*     .. Save statement ..
      SAVE               EPS, SMALL, LARGE, BADC1, BADC2, FIRST
*     ..
*     .. Data statements ..
      DATA               FIRST / .TRUE. /
*     ..
*     .. Executable Statements ..
*
*     Set some constants for use in the subroutine.
*
      IF( FIRST ) THEN
         FIRST = .FALSE.
         EPS = SLAMCH( 'Precision' )
         BADC2 = TENTH / EPS
         BADC1 = SQRT( BADC2 )
         SMALL = SLAMCH( 'Safe minimum' )
         LARGE = ONE / SMALL
*
*        If it looks like we're on a Cray, take the square root of
*        SMALL and LARGE to avoid overflow and underflow problems.
*
         CALL SLABAD( SMALL, LARGE )
         SMALL = SHRINK*( SMALL / EPS )
         LARGE = ONE / SMALL
      END IF
*
      C2 = PATH( 2: 3 )
*
*     Set some parameters we don't plan to change.
*
      DIST = 'S'
      MODE = 3
*
*     xQR, xLQ, xQL, xRQ:  Set parameters to generate a general
*                          M x N matrix.
*
      IF( LSAMEN( 2, C2, 'QR' ) .OR. LSAMEN( 2, C2, 'LQ' ) .OR.
     $    LSAMEN( 2, C2, 'QL' ) .OR. LSAMEN( 2, C2, 'RQ' ) ) THEN
*
*        Set TYPE, the type of matrix to be generated.
*
         TYPE = 'N'
*
*        Set the lower and upper bandwidths.
*
         IF( IMAT.EQ.1 ) THEN
            KL = 0
            KU = 0
         ELSE IF( IMAT.EQ.2 ) THEN
            KL = 0
            KU = MAX( N-1, 0 )
         ELSE IF( IMAT.EQ.3 ) THEN
            KL = MAX( M-1, 0 )
            KU = 0
         ELSE
            KL = MAX( M-1, 0 )
            KU = MAX( N-1, 0 )
         END IF
*
*        Set the condition number and norm.
*
         IF( IMAT.EQ.5 ) THEN
            CNDNUM = BADC1
         ELSE IF( IMAT.EQ.6 ) THEN
            CNDNUM = BADC2
         ELSE
            CNDNUM = TWO
         END IF
*
         IF( IMAT.EQ.7 ) THEN
            ANORM = SMALL
         ELSE IF( IMAT.EQ.8 ) THEN
            ANORM = LARGE
         ELSE
            ANORM = ONE
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        xGE:  Set parameters to generate a general M x N matrix.
*
*        Set TYPE, the type of matrix to be generated.
*
         TYPE = 'N'
*
*        Set the lower and upper bandwidths.
*
         IF( IMAT.EQ.1 ) THEN
            KL = 0
            KU = 0
         ELSE IF( IMAT.EQ.2 ) THEN
            KL = 0
            KU = MAX( N-1, 0 )
         ELSE IF( IMAT.EQ.3 ) THEN
            KL = MAX( M-1, 0 )
            KU = 0
         ELSE
            KL = MAX( M-1, 0 )
            KU = MAX( N-1, 0 )
         END IF
*
*        Set the condition number and norm.
*
         IF( IMAT.EQ.8 ) THEN
            CNDNUM = BADC1
         ELSE IF( IMAT.EQ.9 ) THEN
            CNDNUM = BADC2
         ELSE
            CNDNUM = TWO
         END IF
*
         IF( IMAT.EQ.10 ) THEN
            ANORM = SMALL
         ELSE IF( IMAT.EQ.11 ) THEN
            ANORM = LARGE
         ELSE
            ANORM = ONE
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        xGB:  Set parameters to generate a general banded matrix.
*
*        Set TYPE, the type of matrix to be generated.
*
         TYPE = 'N'
*
*        Set the condition number and norm.
*
         IF( IMAT.EQ.5 ) THEN
            CNDNUM = BADC1
         ELSE IF( IMAT.EQ.6 ) THEN
            CNDNUM = TENTH*BADC2
         ELSE
            CNDNUM = TWO
         END IF
*
         IF( IMAT.EQ.7 ) THEN
            ANORM = SMALL
         ELSE IF( IMAT.EQ.8 ) THEN
            ANORM = LARGE
         ELSE
            ANORM = ONE
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        xGT:  Set parameters to generate a general tridiagonal matrix.
*
*        Set TYPE, the type of matrix to be generated.
*
         TYPE = 'N'
*
*        Set the lower and upper bandwidths.
*
         IF( IMAT.EQ.1 ) THEN
            KL = 0
         ELSE
            KL = 1
         END IF
         KU = KL
*
*        Set the condition number and norm.
*
         IF( IMAT.EQ.3 ) THEN
            CNDNUM = BADC1
         ELSE IF( IMAT.EQ.4 ) THEN
            CNDNUM = BADC2
         ELSE
            CNDNUM = TWO
         END IF
*
         IF( IMAT.EQ.5 .OR. IMAT.EQ.11 ) THEN
            ANORM = SMALL
         ELSE IF( IMAT.EQ.6 .OR. IMAT.EQ.12 ) THEN
            ANORM = LARGE
         ELSE
            ANORM = ONE
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'PO' ) .OR. LSAMEN( 2, C2, 'PP' ) .OR.
     $         LSAMEN( 2, C2, 'HE' ) .OR. LSAMEN( 2, C2, 'HP' ) .OR.
     $         LSAMEN( 2, C2, 'SY' ) .OR. LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        xPO, xPP, xHE, xHP, xSY, xSP: Set parameters to generate a
*        symmetric or Hermitian matrix.
*
*        Set TYPE, the type of matrix to be generated.
*
         TYPE = C2( 1: 1 )
*
*        Set the lower and upper bandwidths.
*
         IF( IMAT.EQ.1 ) THEN
            KL = 0
         ELSE
            KL = MAX( N-1, 0 )
         END IF
         KU = KL
*
*        Set the condition number and norm.
*
         IF( IMAT.EQ.6 ) THEN
            CNDNUM = BADC1
         ELSE IF( IMAT.EQ.7 ) THEN
            CNDNUM = BADC2
         ELSE
            CNDNUM = TWO
         END IF
*
         IF( IMAT.EQ.8 ) THEN
            ANORM = SMALL
         ELSE IF( IMAT.EQ.9 ) THEN
            ANORM = LARGE
         ELSE
            ANORM = ONE
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        xPB:  Set parameters to generate a symmetric band matrix.
*
*        Set TYPE, the type of matrix to be generated.
*
         TYPE = 'P'
*
*        Set the norm and condition number.
*
         IF( IMAT.EQ.5 ) THEN
            CNDNUM = BADC1
         ELSE IF( IMAT.EQ.6 ) THEN
            CNDNUM = BADC2
         ELSE
            CNDNUM = TWO
         END IF
*
         IF( IMAT.EQ.7 ) THEN
            ANORM = SMALL
         ELSE IF( IMAT.EQ.8 ) THEN
            ANORM = LARGE
         ELSE
            ANORM = ONE
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        xPT:  Set parameters to generate a symmetric positive definite
*        tridiagonal matrix.
*
         TYPE = 'P'
         IF( IMAT.EQ.1 ) THEN
            KL = 0
         ELSE
            KL = 1
         END IF
         KU = KL
*
*        Set the condition number and norm.
*
         IF( IMAT.EQ.3 ) THEN
            CNDNUM = BADC1
         ELSE IF( IMAT.EQ.4 ) THEN
            CNDNUM = BADC2
         ELSE
            CNDNUM = TWO
         END IF
*
         IF( IMAT.EQ.5 .OR. IMAT.EQ.11 ) THEN
            ANORM = SMALL
         ELSE IF( IMAT.EQ.6 .OR. IMAT.EQ.12 ) THEN
            ANORM = LARGE
         ELSE
            ANORM = ONE
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'TR' ) .OR. LSAMEN( 2, C2, 'TP' ) ) THEN
*
*        xTR, xTP:  Set parameters to generate a triangular matrix
*
*        Set TYPE, the type of matrix to be generated.
*
         TYPE = 'N'
*
*        Set the lower and upper bandwidths.
*
         MAT = ABS( IMAT )
         IF( MAT.EQ.1 .OR. MAT.EQ.7 ) THEN
            KL = 0
            KU = 0
         ELSE IF( IMAT.LT.0 ) THEN
            KL = MAX( N-1, 0 )
            KU = 0
         ELSE
            KL = 0
            KU = MAX( N-1, 0 )
         END IF
*
*        Set the condition number and norm.
*
         IF( MAT.EQ.3 .OR. MAT.EQ.9 ) THEN
            CNDNUM = BADC1
         ELSE IF( MAT.EQ.4 .OR. MAT.EQ.10 ) THEN
            CNDNUM = BADC2
         ELSE
            CNDNUM = TWO
         END IF
*
         IF( MAT.EQ.5 ) THEN
            ANORM = SMALL
         ELSE IF( MAT.EQ.6 ) THEN
            ANORM = LARGE
         ELSE
            ANORM = ONE
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'TB' ) ) THEN
*
*        xTB:  Set parameters to generate a triangular band matrix.
*
*        Set TYPE, the type of matrix to be generated.
*
         TYPE = 'N'
*
*        Set the norm and condition number.
*
         IF( IMAT.EQ.2 .OR. IMAT.EQ.8 ) THEN
            CNDNUM = BADC1
         ELSE IF( IMAT.EQ.3 .OR. IMAT.EQ.9 ) THEN
            CNDNUM = BADC2
         ELSE
            CNDNUM = TWO
         END IF
*
         IF( IMAT.EQ.4 ) THEN
            ANORM = SMALL
         ELSE IF( IMAT.EQ.5 ) THEN
            ANORM = LARGE
         ELSE
            ANORM = ONE
         END IF
      END IF
      IF( N.LE.1 )
     $   CNDNUM = ONE
*
      RETURN
*
*     End of CLATB4
*
      END
