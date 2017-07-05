      SUBROUTINE CDRVLS( DOTYPE, NM, MVAL, NN, NVAL, NNS, NSVAL, NNB,
     $                   IBVAL, NBVAL, NXVAL, THRESH, TSTERR, A, COPYA,
     $                   B, COPYB, C, S, COPYS, WORK, RWORK, IWORK,
     $                   NOUT )
*
      INCLUDE 'plasmaf.h'
*
*  -- LAPACK test routine (version 3.1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     January 2007
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NM, NN, NNB, NNS, NOUT
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), MVAL( * ), NBVAL( * ), NSVAL( * ),
     $                   NVAL( * ), NXVAL( * ), IBVAL( * )
      REAL               COPYS( * ), RWORK( * ), S( * )
      COMPLEX            A( * ), B( * ), C( * ), COPYA( * ), COPYB( * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  CDRVLS tests the least squares driver routines CGELS, CGELSX, CGELSS,
*  CGELSY and CGELSD.
*
*  Arguments
*  =========
*
*  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
*          The matrix types to be used for testing.  Matrices of type j
*          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
*          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
*          The matrix of type j is generated as follows:
*          j=1: A = U*D*V where U and V are random unitary matrices
*               and D has random entries (> 0.1) taken from a uniform
*               distribution (0,1). A is full rank.
*          j=2: The same of 1, but A is scaled up.
*          j=3: The same of 1, but A is scaled down.
*          j=4: A = U*D*V where U and V are random unitary matrices
*               and D has 3*min(M,N)/4 random entries (> 0.1) taken
*               from a uniform distribution (0,1) and the remaining
*               entries set to 0. A is rank-deficient.
*          j=5: The same of 4, but A is scaled up.
*          j=6: The same of 5, but A is scaled down.
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
*  IBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the inner block size IB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  THRESH  (input) REAL
*          The threshold value for the test ratios.  A result is
*          included in the output file if RESULT >= THRESH.  To have
*          every test ratio printed, use THRESH = 0.
*
*  TSTERR  (input) LOGICAL
*          Flag that indicates whether error exits are to be tested.
*
*  A       (workspace) COMPLEX array, dimension (MMAX*NMAX)
*          where MMAX is the maximum value of M in MVAL and NMAX is the
*          maximum value of N in NVAL.
*
*  COPYA   (workspace) COMPLEX array, dimension (MMAX*NMAX)
*
*  B       (workspace) COMPLEX array, dimension (MMAX*NSMAX)
*          where MMAX is the maximum value of M in MVAL and NSMAX is the
*          maximum value of NRHS in NSVAL.
*
*  COPYB   (workspace) COMPLEX array, dimension (MMAX*NSMAX)
*
*  C       (workspace) COMPLEX array, dimension (MMAX*NSMAX)
*
*  S       (workspace) REAL array, dimension
*                      (min(MMAX,NMAX))
*
*  COPYS   (workspace) REAL array, dimension
*                      (min(MMAX,NMAX))
*
*  WORK    (workspace) COMPLEX array, dimension
*                      (MMAX*NMAX + 4*NMAX + MMAX).
*
*  RWORK   (workspace) REAL array, dimension (5*NMAX-1)
*
*  IWORK   (workspace) INTEGER array, dimension (15*NMAX)
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 18 )
      INTEGER            SMLSIZ
      PARAMETER          ( SMLSIZ = 25 )
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
      COMPLEX            CONE, CZERO
      PARAMETER          ( CONE = ( 1.0E+0, 0.0E+0 ),
     $                   CZERO = ( 0.0E+0, 0.0E+0 ) )
*     ..
*     .. Local Scalars ..
      CHARACTER          TRANS
      CHARACTER*3        PATH
      INTEGER            CRANK, I, IM, IN, INB, INFO, INS, IRANK,
     $                   ISCALE, ITRAN, ITYPE, J, K, LDA, LDB, LDWORK,
     $                   LWLSY, LWORK, M, MNMIN, N, NB, NCOLS, NERRS,
     $                   NFAIL, NRHS, NROWS, NRUN, RANK, IB,
     $                   PLASMA_TRANS
      INTEGER            HT( 2 )
      REAL               EPS, NORMA, NORMB, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL               RESULT( NTESTS )
*     ..
*     .. External Functions ..
      REAL               CQRT14, CQRT17, SASUM, SLAMCH
      EXTERNAL           CQRT14, CQRT17, SASUM, SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAERH, ALAHD, ALASVM, CERRLS, CGELS, CGELSD,
     $                   CGELSS, CGELSX, CGELSY, CGEMM, CLACPY, CLARNV,
     $                   CQRT13, CQRT15, CQRT16, CSSCAL, SAXPY, 
     $                   XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, REAL, SQRT
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*32       SRNAMT
      INTEGER            INFOT, IOUNIT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, IOUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Data statements ..
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'Complex precision'
      PATH( 2: 3 ) = 'LS'
      NRUN = 0
      NFAIL = 0
      NERRS = 0
      DO 10 I = 1, 4
         ISEED( I ) = ISEEDY( I )
   10 CONTINUE
      EPS = SLAMCH( 'Epsilon' )
*
*     Threshold for rank estimation
*
      RCOND = SQRT( EPS ) - ( SQRT( EPS )-EPS ) / 2
*
*     Test the error exits
*
      CALL XLAENV( 9, SMLSIZ )
      IF( TSTERR )
     $   CALL CERRLS( PATH, NOUT )
*
*     Print the header if NM = 0 or NN = 0 and THRESH = 0.
*
      IF( ( NM.EQ.0 .OR. NN.EQ.0 ) .AND. THRESH.EQ.ZERO )
     $   CALL ALAHD( NOUT, PATH )
      INFOT = 0
*
      DO 140 IM = 1, NM
         M = MVAL( IM )
         LDA = MAX( 1, M )
*
         DO 130 IN = 1, NN
            N = NVAL( IN )
            MNMIN = MIN( M, N )
            LDB = MAX( 1, M, N )
*
            DO 120 INS = 1, NNS
               NRHS = NSVAL( INS )
               LWORK = MAX( 1, ( M+NRHS )*( N+2 ), ( N+NRHS )*( M+2 ),
     $                 M*N+4*MNMIN+MAX( M, N ), 2*N+M )
*
               DO 110 IRANK = 1, 2
                  DO 100 ISCALE = 1, 3
                     ITYPE = ( IRANK-1 )*3 + ISCALE
                     IF( .NOT.DOTYPE( ITYPE ) )
     $                  GO TO 100
*
                     IF( IRANK.EQ.1 ) THEN
*
*                       Test CGELS
*
*                       Generate a matrix of scaling type ISCALE
*
                        CALL CQRT13( ISCALE, M, N, COPYA, LDA, NORMA,
     $                               ISEED )
                        DO 40 INB = 1, NNB
                           NB = NBVAL( INB )
                           IB = IBVAL( INB )
                           CALL XLAENV( 1, NB )
                           CALL XLAENV( 3, NXVAL( INB ) )
                           IF ( (MAX(M, N) / 25) .GT. NB ) THEN
                              GOTO 40
                           END IF
                           CALL PLASMA_SET( PLASMA_TILE_SIZE, NB, INFO )
                           CALL PLASMA_SET( PLASMA_INNER_BLOCK_SIZE, IB,
     $                                      INFO )
*
*                          Allocate T
*
                           CALL PLASMA_ALLOC_WORKSPACE_CGELS( M, N , HT,
     $                                                        INFO )
*
*                           DO 30 ITRAN = 1, 2
                           DO 30 ITRAN = 1, 1
                              IF( ITRAN.EQ.1 ) THEN
                                 TRANS = 'N'
                                 PLASMA_TRANS = PLASMANOTRANS 
                                 NROWS = M
                                 NCOLS = N
                              ELSE
                                 TRANS = 'C'
                                 PLASMA_TRANS = PLASMACONJTRANS 
                                 NROWS = N
                                 NCOLS = M
                              END IF
                              LDWORK = MAX( 1, NCOLS )
*
*                             Set up a consistent rhs
*
                              IF( NCOLS.GT.0 ) THEN
                                 CALL CLARNV( 2, ISEED, NCOLS*NRHS,
     $                                        WORK )
                                 CALL CSSCAL( NCOLS*NRHS,
     $                                        ONE / REAL( NCOLS ), WORK,
     $                                        1 )
                              END IF
                              CALL CGEMM( TRANS, 'No transpose', NROWS,
     $                                    NRHS, NCOLS, CONE, COPYA, LDA,
     $                                    WORK, LDWORK, CZERO, B, LDB )
                              CALL CLACPY( 'Full', NROWS, NRHS, B, LDB,
     $                                     COPYB, LDB )
*
*                             Solve LS or overdetermined system
*
                              IF( M.GT.0 .AND. N.GT.0 ) THEN
                                 CALL CLACPY( 'Full', M, N, COPYA, LDA,
     $                                        A, LDA )
                                 CALL CLACPY( 'Full', NROWS, NRHS,
     $                                        COPYB, LDB, B, LDB )
                              END IF
                              SRNAMT = 'CGELS '
                              CALL PLASMA_CGELS( PLASMA_TRANS,
     $                                           M, N, NRHS,
     $                                           A, LDA, HT, B, LDB,
     $                                           INFO )
*
                              IF( INFO.NE.0 )
     $                           CALL ALAERH( PATH, 'CGELS ', INFO, 0,
     $                                        TRANS, M, N, NRHS, -1, NB,
     $                                        ITYPE, NFAIL, NERRS,
     $                                        NOUT )
*
*                             Check correctness of results
*
                              LDWORK = MAX( 1, NROWS )
                              IF( NROWS.GT.0 .AND. NRHS.GT.0 )
     $                           CALL CLACPY( 'Full', NROWS, NRHS,
     $                                        COPYB, LDB, C, LDB )
                              CALL CQRT16( TRANS, M, N, NRHS, COPYA,
     $                                     LDA, B, LDB, C, LDB, RWORK,
     $                                     RESULT( 1 ) )
*
                              IF( ( ITRAN.EQ.1 .AND. M.GE.N ) .OR.
     $                            ( ITRAN.EQ.2 .AND. M.LT.N ) ) THEN
*
*                                Solving LS system
*
                                 RESULT( 2 ) = CQRT17( TRANS, 1, M, N,
     $                                         NRHS, COPYA, LDA, B, LDB,
     $                                         COPYB, LDB, C, WORK,
     $                                         LWORK )
                              ELSE
*
*                                Solving overdetermined system
*
                                 RESULT( 2 ) = CQRT14( TRANS, M, N,
     $                                         NRHS, COPYA, LDA, B, LDB,
     $                                         WORK, LWORK )
                              END IF
*
*                             Print information about the tests that
*                             did not pass the threshold.
*
                              DO 20 K = 1, 2
                                 IF( RESULT( K ).GE.THRESH ) THEN
                                    IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                                 CALL ALAHD( NOUT, PATH )
                                    WRITE( NOUT, FMT = 9999 )TRANS, M,
     $                                 N, NRHS, NB, ITYPE, K,
     $                                 RESULT( K )
                                    NFAIL = NFAIL + 1
                                 END IF
   20                         CONTINUE
                              NRUN = NRUN + 2
   30                      CONTINUE
*
*                          Deallocate T
*
                           CALL PLASMA_DEALLOC_HANDLE( HT, INFO )
   40                   CONTINUE
                     END IF
*
  100             CONTINUE
  110          CONTINUE
  120       CONTINUE
  130    CONTINUE
  140 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASVM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' TRANS=''', A1, ''', M=', I5, ', N=', I5, ', NRHS=', I4,
     $      ', NB=', I4, ', type', I2, ', test(', I2, ')=', G12.5 )
 9998 FORMAT( ' M=', I5, ', N=', I5, ', NRHS=', I4, ', NB=', I4,
     $      ', type', I2, ', test(', I2, ')=', G12.5 )
      RETURN
*
*     End of CDRVLS
*
      END
