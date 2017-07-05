      SUBROUTINE SCHKLQ( DOTYPE, NM, MVAL, NN, NVAL, NNB, NBVAL, NXVAL,
     $                   IBVAL, NRHS, THRESH, TSTERR, NMAX, A, AF, AQ,
     $                   AL, AC, B, X, XACT, TAU, WORK, RWORK, IWORK,
     $                   NOUT )
*
      INCLUDE 'plasmaf.h'
*
*  -- LAPACK test routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NM, NMAX, NN, NNB, NOUT, NRHS
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), MVAL( * ), NBVAL( * ), NVAL( * ),
     $                   NXVAL( * ), IBVAL( * )
      REAL               A( * ), AC( * ), AF( * ), AL( * ), AQ( * ),
     $                   B( * ), RWORK( * ), TAU( * ), WORK( * ),
     $                   X( * ), XACT( * )
*     ..
*
*  Purpose
*  =======
*
*  SCHKLQ tests SGELQF, SORGLQ and SORMLQ.
*
*  Arguments
*  =========
*
*  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
*          The matrix types to be used for testing.  Matrices of type j
*          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
*          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix column dimension N.
*
*  NNB     (input) INTEGER
*          The number of values of NB and NX contained in the
*          vectors NBVAL and NXVAL.  The blocking parameters are used
*          in pairs (NB,NX).
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NRHS    (input) INTEGER
*          The number of right hand side vectors to be generated for
*          each linear system.
*
*  THRESH  (input) REAL
*          The threshold value for the test ratios.  A result is
*          included in the output file if RESULT >= THRESH.  To have
*          every test ratio printed, use THRESH = 0.
*
*  TSTERR  (input) LOGICAL
*          Flag that indicates whether error exits are to be tested.
*
*  NMAX    (input) INTEGER
*          The maximum value permitted for M or N, used in dimensioning
*          the work arrays.
*
*  A       (workspace) REAL array, dimension (NMAX*NMAX)
*
*  AF      (workspace) REAL array, dimension (NMAX*NMAX)
*
*  AQ      (workspace) REAL array, dimension (NMAX*NMAX)
*
*  AL      (workspace) REAL array, dimension (NMAX*NMAX)
*
*  AC      (workspace) REAL array, dimension (NMAX*NMAX)
*
*  B       (workspace) REAL array, dimension (NMAX*NRHS)
*
*  X       (workspace) REAL array, dimension (NMAX*NRHS)
*
*  XACT    (workspace) REAL array, dimension (NMAX*NRHS)
*
*  TAU     (workspace) REAL array, dimension (NMAX)
*
*  WORK    (workspace) REAL array, dimension (NMAX*NMAX)
*
*  RWORK   (workspace) REAL array, dimension (NMAX)
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 7 )
      INTEGER            NTYPES
      PARAMETER          ( NTYPES = 8 )
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          DIST, TYPE
      CHARACTER*3        PATH
      INTEGER            I, IK, IM, IMAT, IN, INB, INFO, K, KL, KU, LDA,
     $                   LWORK, M, MINMN, MODE, N, NB, NERRS, NFAIL, NK,
     $                   NRUN, NT, NX, IB, IRH, RHBLK
      REAL               ANORM, CNDNUM
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 ), ISEEDY( 4 ), KVAL( 4 )
      REAL               RESULT( NTESTS )
      INTEGER            HT( 2 )
*     ..
*     .. External Functions ..
      LOGICAL            SGENND
      EXTERNAL           SGENND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAERH, ALAHD, ALASUM, SERRLQ, SGET02,
     $                   SLACPY, SLARHS, SLATB4, SLATMS, SLQT01, SLQT02,
     $                   SLQT03, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*32       SRNAMT
      INTEGER            INFOT, NUNIT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Data statements ..
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      RHBLK = 4
      PATH( 1: 1 ) = 'Single precision'
      PATH( 2: 3 ) = 'LQ'
      NRUN = 0
      NFAIL = 0
      NERRS = 0
      DO 10 I = 1, 4
         ISEED( I ) = ISEEDY( I )
   10 CONTINUE
*
*     Test the error exits
*
      IF( TSTERR )
     $   CALL SERRLQ( PATH, NOUT )
      INFOT = 0
      CALL XLAENV( 2, 2 )
*
      LDA = NMAX
      LWORK = NMAX*MAX( NMAX, NRHS )
*
*     Do for each value of M in MVAL.
*
      DO 70 IM = 1, NM
         M = MVAL( IM )
*
*        Do for each value of N in NVAL.
*
         DO 60 IN = 1, NN
            N = NVAL( IN )
            IF ( N.LT.M )
     $           GO TO 60
            MINMN = MIN( M, N )
            DO 50 IMAT = 1, NTYPES
*
*              Do the tests only if DOTYPE( IMAT ) is true.
*
               IF( .NOT.DOTYPE( IMAT ) )
     $            GO TO 50
*
*              Set up parameters with SLATB4 and generate a test matrix
*              with SLATMS.
*
               CALL SLATB4( PATH, IMAT, M, N, TYPE, KL, KU, ANORM, MODE,
     $                      CNDNUM, DIST )
*
               SRNAMT = 'SLATMS'
               CALL SLATMS( M, N, DIST, ISEED, TYPE, RWORK, MODE,
     $                      CNDNUM, ANORM, KL, KU, 'No packing', A, LDA,
     $                      WORK, INFO )
*
*              Check error code from SLATMS.
*
               IF( INFO.NE.0 ) THEN
                  CALL ALAERH( PATH, 'SLATMS', INFO, 0, ' ', M, N, -1,
     $                         -1, -1, IMAT, NFAIL, NERRS, NOUT )
                  GO TO 50
               END IF
*
*              Set some values for K: the first value must be MINMN,
*              corresponding to the call of SLQT01; other values are
*              used in the calls of SLQT02, and must not exceed MINMN.
*
               KVAL( 1 ) = MINMN
               KVAL( 2 ) = 0
               KVAL( 3 ) = 1
               KVAL( 4 ) = MINMN / 2
               IF( MINMN.EQ.0 ) THEN
                  NK = 1
               ELSE IF( MINMN.EQ.1 ) THEN
                  NK = 2
               ELSE IF( MINMN.LE.3 ) THEN
                  NK = 3
               ELSE
                  NK = 4
               END IF
*     
*     Set Householder mode (tree or flat)
*
               DO 45 IRH = 0, 1
                  IF (IRH .EQ. 0) THEN
                     CALL PLASMA_SET(PLASMA_HOUSEHOLDER_MODE,
     $                    PLASMA_FLAT_HOUSEHOLDER, INFO )
                  ELSE
                     CALL PLASMA_SET(PLASMA_HOUSEHOLDER_MODE,
     $                    PLASMA_TREE_HOUSEHOLDER, INFO )
                     CALL PLASMA_SET(PLASMA_HOUSEHOLDER_SIZE, 
     $                    RHBLK, INFO)
                     END IF
*
*              Do for each value of K in KVAL
*
               DO 40 IK = 1, 2
                  K = KVAL( IK )
*
*                 Do for each pair of values (NB,NX) in NBVAL and NXVAL.
*
                  DO 30 INB = 1, NNB
                     NB = NBVAL( INB )
                     IB = IBVAL( INB )
                     CALL XLAENV( 1, NB )
                     NX = NXVAL( INB )
                     CALL XLAENV( 3, NX )
                     IF ( (MAX(M, N) / 10) .GT. NB ) THEN
                        GOTO 30
                     END IF
                     CALL PLASMA_SET( PLASMA_TILE_SIZE, NB, INFO )
                     CALL PLASMA_SET( PLASMA_INNER_BLOCK_SIZE, IB, INFO)
*
*                    Allocate HT
*
                     CALL PLASMA_ALLOC_WORKSPACE_SGELQF( M, N, HT,
     $                                                  INFO )
*
                     DO I = 1, NTESTS
                        RESULT( I ) = ZERO
                     END DO
                     NT = 2
                     IF( IK.EQ.1 ) THEN
*
*                       Test SGELQF
*
                        CALL SLQT01( M, N, A, AF, AQ, AL, LDA, HT,
     $                               WORK, LWORK, RWORK, RESULT( 1 ) )
*                        IF( .NOT.SGENND( M, N, AF, LDA ) )
*     $                       RESULT( 8 ) = 2*THRESH
*                        NT = NT + 1
                     ELSE IF( M.LE.N ) THEN
*
*                       Test SORGLQ, using factorization
*                       returned by SLQT01
*
                        CALL SLQT02( M, N, K, A, AF, AQ, AL, LDA, HT,
     $                               WORK, LWORK, RWORK, RESULT( 1 ) )
                     END IF
                     IF( M.GE.K ) THEN
*
*                       Test SORMLQ, using factorization returned
*                       by SLQT01
*
                        CALL SLQT03( M, N, K, AF, AC, AL, AQ, LDA, HT,
     $                               WORK, LWORK, RWORK, RESULT( 3 ) )
                        NT = NT + 4
*
*                       If M>=N and K=N, call SGELQS to solve a system
*                       with NRHS right hand sides and compute the
*                       residual.
*
                        IF( K.EQ.M .AND. INB.EQ.1 ) THEN
*
*                          Generate a solution and set the right
*                          hand side.
*
                           SRNAMT = 'SLARHS'
                           CALL SLARHS( PATH, 'New', 'Full',
     $                                  'No transpose', M, N, 0, 0,
     $                                  NRHS, A, LDA, XACT, LDA, B, LDA,
     $                                  ISEED, INFO )
*
                           CALL SLACPY( 'Full', M, NRHS, B, LDA, X,
     $                                  LDA )
                           SRNAMT = 'SGELQS'
                           CALL PLASMA_SGELQS( M, N, NRHS, AF, LDA, HT,
     $                                  X, LDA, INFO )
*
*                          Check error code from SGELQS.
*
                           IF( INFO.NE.0 )
     $                        CALL ALAERH( PATH, 'SGELQS', INFO, 0, ' ',
     $                                     M, N, NRHS, -1, NB, IMAT,
     $                                     NFAIL, NERRS, NOUT )
*
                           CALL SGET02( 'No transpose', M, N, NRHS, A,
     $                                  LDA, X, LDA, B, LDA, RWORK,
     $                                  RESULT( 7 ) )
                           NT = NT + 1
                        END IF
                     END IF
*
*                    Print information about the tests that did not
*                    pass the threshold.
*
                     DO 20 I = 1, NTESTS
                        IF( RESULT( I ).GE.THRESH ) THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL ALAHD( NOUT, PATH )
                           WRITE( NOUT, FMT = 9999 )M, N, K, NB, NX,
     $                        IMAT, I, RESULT( I )
                           NFAIL = NFAIL + 1
                        END IF
   20                CONTINUE
                     NRUN = NRUN + NT
*
*                    Deallocate T
*
                     CALL PLASMA_DEALLOC_HANDLE( HT, INFO )
   30             CONTINUE
   40          CONTINUE
   45         CONTINUE
   50       CONTINUE
   60    CONTINUE
   70 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASUM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' M=', I5, ', N=', I5, ', K=', I5, ', NB=', I4, ', NX=',
     $      I5, ', type ', I2, ', test(', I2, ')=', G12.5 )
      RETURN
*
*     End of SCHKLQ
*
      END
