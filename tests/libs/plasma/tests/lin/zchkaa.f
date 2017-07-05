      PROGRAM ZCHKAA
*
      INCLUDE 'plasmaf.h'
*
*  -- PLASMA test routine (From LAPACK version 3.1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     January 2007
*
*  Purpose
*  =======
*
*  ZCHKAA is the main test program for the COMPLEX*16 linear equation
*  routines.
*
*  The program must be driven by a short data file. The first 14 records
*  specify problem dimensions and program options using list-directed
*  input.  The remaining lines specify the PLASMA test paths and the
*  number of matrix types to use in testing.  An annotated example of a
*  data file can be obtained by deleting the first 3 characters from the
*  following 38 lines:
*  Data file for testing COMPLEX*16 PLASMA linear equation routines
*  1                      Number of values of NP
*  16                     Values of NP (number of cores)
*  1                      Values of SCHED (0: STATIC, 1:DYNAMIC)
*  7                      Number of values of M
*  0 1 2 3 5 10 16        Values of M (row dimension)
*  7                      Number of values of N
*  0 1 2 3 5 10 16        Values of N (column dimension)
*  1                      Number of values of NRHS
*  2                      Values of NRHS (number of right hand sides)
*  5                      Number of values of NB
*  1 3 3 3 20             Values of NB (the blocksize)
*  1 0 5 9 1              Values of NX (crossover point)
*  3                      Number of values of RANK
*  30 50 90               Values of rank (as a % of N)
*  30.0                   Threshold value of test ratio
*  T                      Put T to test the PLASMA routines
*  T                      Put T to test the driver routines
*  T                      Put T to test the error exits
*  ZGE   11               List types on next line if 0 < NTYPES < 11
*  ZGB    8               List types on next line if 0 < NTYPES <  8
*  ZGT   12               List types on next line if 0 < NTYPES < 12
*  ZPO    9               List types on next line if 0 < NTYPES <  9
*  ZPS    9               List types on next line if 0 < NTYPES <  9
*  ZPP    9               List types on next line if 0 < NTYPES <  9
*  ZPB    8               List types on next line if 0 < NTYPES <  8
*  ZPT   12               List types on next line if 0 < NTYPES < 12
*  ZHE   10               List types on next line if 0 < NTYPES < 10
*  ZHP   10               List types on next line if 0 < NTYPES < 10
*  ZSY   11               List types on next line if 0 < NTYPES < 11
*  ZSP   11               List types on next line if 0 < NTYPES < 11
*  ZTR   18               List types on next line if 0 < NTYPES < 18
*  ZTP   18               List types on next line if 0 < NTYPES < 18
*  ZTB   17               List types on next line if 0 < NTYPES < 17
*  ZQR    8               List types on next line if 0 < NTYPES <  8
*  ZRQ    8               List types on next line if 0 < NTYPES <  8
*  ZLQ    8               List types on next line if 0 < NTYPES <  8
*  ZQL    8               List types on next line if 0 < NTYPES <  8
*  ZQP    6               List types on next line if 0 < NTYPES <  6
*  ZTZ    3               List types on next line if 0 < NTYPES <  3
*  ZLS    6               List types on next line if 0 < NTYPES <  6
*  ZEQ
*
*  Internal Parameters
*  ===================
*
*  NMAX    INTEGER
*          The maximum allowable value for N.
*
*  MAXIN   INTEGER
*          The number of different values that can be used for each of
*          M, N, or NB
*
*  MAXRHS  INTEGER
*          The maximum number of right hand sides
*
*  NIN     INTEGER
*          The unit number for input
*
*  NOUT    INTEGER
*          The unit number for output
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NPMAX
      PARAMETER          ( NPMAX = 16 )
      INTEGER            NMAX
      PARAMETER          ( NMAX = 1000 )
      INTEGER            MAXIN
      PARAMETER          ( MAXIN = 12 )
      INTEGER            MAXRHS
      PARAMETER          ( MAXRHS = 16 )
      INTEGER            MATMAX
      PARAMETER          ( MATMAX = 30 )
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            KDMAX
      PARAMETER          ( KDMAX = NMAX+( NMAX+1 ) / 4 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FATAL, TSTCHK, TSTDRV, TSTERR
      CHARACTER          C1
      CHARACTER*2        C2
      CHARACTER*3        PATH
      CHARACTER*10       INTSTR
      CHARACTER*72       ALINE
      INTEGER            I, IB, IC, J, K, LA, LAFAC, LDA, NB, NM, NMATS,
     $                   NN, NNB, NNB2, NP, NNP, SCHED, NNS, NRHS, 
     $                   NTYPES, NRANK, VERS_MAJOR, VERS_MINOR, 
     $                   VERS_PATCH, INFO
      DOUBLE PRECISION   EPS, S1, S2, THREQ, THRESH
*     ..
*     .. Local Arrays ..
      LOGICAL            DOTYPE( MATMAX )
      INTEGER            IBVAL(MAXIN ), IWORK( 25*NMAX ), MVAL( MAXIN ),
     $                   NBVAL( MAXIN ), NBVAL2( MAXIN ),
     $                   NPVAL( MAXIN ), NSVAL( MAXIN ), 
     $                   NVAL( MAXIN ), NXVAL( MAXIN ),
     $                   RANKVAL( MAXIN ), PIV( NMAX )
      DOUBLE PRECISION   RWORK( 150*NMAX+2*MAXRHS ), S( 2*NMAX )
      COMPLEX*16         A( ( KDMAX+1 )*NMAX, 7 ), B( NMAX*MAXRHS, 4 ),
     $                   WORK( NMAX, NMAX+MAXRHS+10 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME, LSAMEN
      DOUBLE PRECISION   DLAMCH, DSECND
      EXTERNAL           LSAME, LSAMEN, DLAMCH, DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAREQ, ZCHKGE, 
     $                   ZCHKLQ, ZCHKPO, 
     $                   ZCHKQ3,
     $                   ZCHKQR, 
     $                   ZDRVLS,
     $                   ZDRVPO,
     $                   ILAVER
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*32       SRNAMT
      INTEGER            INFOT, NUNIT
*     ..
*     .. Arrays in Common ..
      INTEGER            IPARMS( 100 )
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
      COMMON             / CLAENV / IPARMS
*     ..
*     .. Data statements ..
      DATA               THREQ / 2.0D0 / , INTSTR / '0123456789' /
*     ..
*     .. Executable Statements ..
*
*      S1 = DSECND( )
      LDA = NMAX
      FATAL = .FALSE.
*
*     Report values of parameters.
*
      CALL PLASMA_VERSION( VERS_MAJOR, VERS_MINOR, VERS_PATCH, INFO)
      WRITE( NOUT, FMT = 9994 ) VERS_MAJOR, VERS_MINOR, VERS_PATCH
*
*
*     Read a dummy line.
*
      READ( NIN, FMT = * )
*
*     Read the values of NP
*
      READ( NIN, FMT = * )NNP
      IF( NNP.LT.1 ) THEN
         WRITE( NOUT, FMT = 9996 )' NNP ', NNP, 1
         NNP = 0
         FATAL = .TRUE.
      ELSE IF( NNP.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9995 )' NNP ', NNP, MAXIN
         NNP = 0
         FATAL = .TRUE.
      END IF
      READ( NIN, FMT = * )( NPVAL( I ), I = 1, NNP )
      DO 01 I = 1, NNP
         IF( NPVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )' NP  ', NPVAL( I ), 0
            FATAL = .TRUE.
         ELSE IF( NPVAL( I ).GT.NPMAX ) THEN
            WRITE( NOUT, FMT = 9995 )' NP  ', NPVAL( I ), NPMAX
            FATAL = .TRUE.
         END IF
   01 CONTINUE
      IF( NNP.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'NP   ', ( NPVAL( I ), I = 1, NNP )
*
*     Read the values of SCHED
*
      READ( NIN, FMT = * )SCHED
      IF (( SCHED .LT. 0 ) .OR. (SCHED .GT. 1)) THEN
         WRITE( NOUT, FMT = 9987 )' SCHED ', SCHED
         SCHED = 0
         FATAL = .TRUE.
      END IF       
*
*     Read the values of M
*
      READ( NIN, FMT = * )NM
      IF( NM.LT.1 ) THEN
         WRITE( NOUT, FMT = 9996 )' NM ', NM, 1
         NM = 0
         FATAL = .TRUE.
      ELSE IF( NM.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9995 )' NM ', NM, MAXIN
         NM = 0
         FATAL = .TRUE.
      END IF
      READ( NIN, FMT = * )( MVAL( I ), I = 1, NM )
      DO 10 I = 1, NM
         IF( MVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )' M  ', MVAL( I ), 0
            FATAL = .TRUE.
         ELSE IF( MVAL( I ).GT.NMAX ) THEN
            WRITE( NOUT, FMT = 9995 )' M  ', MVAL( I ), NMAX
            FATAL = .TRUE.
         END IF
   10 CONTINUE
      IF( NM.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'M   ', ( MVAL( I ), I = 1, NM )
*
*     Read the values of N
*
      READ( NIN, FMT = * ) NN
      IF( NN.LT.1 ) THEN
         WRITE( NOUT, FMT = 9996 )' NN ', NN, 1
         NN = 0
         FATAL = .TRUE.
      ELSE IF( NN.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9995 )' NN ', NN, MAXIN
         NN = 0
         FATAL = .TRUE.
      END IF
      READ( NIN, FMT = * )( NVAL( I ), I = 1, NN )
      DO 20 I = 1, NN
         IF( NVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )' N  ', NVAL( I ), 0
            FATAL = .TRUE.
         ELSE IF( NVAL( I ).GT.NMAX ) THEN
            WRITE( NOUT, FMT = 9995 )' N  ', NVAL( I ), NMAX
            FATAL = .TRUE.
         END IF
   20 CONTINUE
      IF( NN.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'N   ', ( NVAL( I ), I = 1, NN )
*
*     Read the values of NRHS
*
      READ( NIN, FMT = * )NNS
      IF( NNS.LT.1 ) THEN
         WRITE( NOUT, FMT = 9996 )' NNS', NNS, 1
         NNS = 0
         FATAL = .TRUE.
      ELSE IF( NNS.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9995 )' NNS', NNS, MAXIN
         NNS = 0
         FATAL = .TRUE.
      END IF
      READ( NIN, FMT = * )( NSVAL( I ), I = 1, NNS )
      DO 30 I = 1, NNS
         IF( NSVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )'NRHS', NSVAL( I ), 0
            FATAL = .TRUE.
         ELSE IF( NSVAL( I ).GT.MAXRHS ) THEN
            WRITE( NOUT, FMT = 9995 )'NRHS', NSVAL( I ), MAXRHS
            FATAL = .TRUE.
         END IF
   30 CONTINUE
      IF( NNS.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'NRHS', ( NSVAL( I ), I = 1, NNS )
*
*     Read the values of NB
*
      READ( NIN, FMT = * )NNB
      IF( NNB.LT.1 ) THEN
         WRITE( NOUT, FMT = 9996 )'NNB ', NNB, 1
         NNB = 0
         FATAL = .TRUE.
      ELSE IF( NNB.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9995 )'NNB ', NNB, MAXIN
         NNB = 0
         FATAL = .TRUE.
      END IF
      READ( NIN, FMT = * )( NBVAL( I ), I = 1, NNB )
      DO 40 I = 1, NNB
         IF( NBVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )' NB ', NBVAL( I ), 0
            FATAL = .TRUE.
         END IF
   40 CONTINUE
      IF( NNB.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'NB  ', ( NBVAL( I ), I = 1, NNB )
*
*     Read the values of IB
*
      READ( NIN, FMT = * )( IBVAL( I ), I = 1, NNB )
      DO 41 I = 1, NNB
         IF( IBVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )' NB ', IBVAL( I ), 0
            FATAL = .TRUE.
         END IF
   41 CONTINUE
      IF( NNB.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'IB  ', ( IBVAL( I ), I = 1, NNB )
*
*     Set NBVAL2 to be the set of unique values of NB
*
      NNB2 = 0
      DO 60 I = 1, NNB
         NB = NBVAL( I )
         DO 50 J = 1, NNB2
            IF( NB.EQ.NBVAL2( J ) )
     $         GO TO 60
   50    CONTINUE
         NNB2 = NNB2 + 1
         NBVAL2( NNB2 ) = NB
   60 CONTINUE
*
*     Read the values of NX
*
      READ( NIN, FMT = * )( NXVAL( I ), I = 1, NNB )
      DO 70 I = 1, NNB
         IF( NXVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )' NX ', NXVAL( I ), 0
            FATAL = .TRUE.
         END IF
   70 CONTINUE
      IF( NNB.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'NX  ', ( NXVAL( I ), I = 1, NNB )
*
*     Read the values of RANKVAL
*
      READ( NIN, FMT = * )NRANK
      IF( NN.LT.1 ) THEN
         WRITE( NOUT, FMT = 9996 )' NRANK ', NRANK, 1
         NRANK = 0
         FATAL = .TRUE.
      ELSE IF( NN.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9995 )' NRANK ', NRANK, MAXIN
         NRANK = 0
         FATAL = .TRUE.
      END IF
      READ( NIN, FMT = * )( RANKVAL( I ), I = 1, NRANK )
      DO I = 1, NRANK
         IF( RANKVAL( I ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9996 )' RANK  ', RANKVAL( I ), 0
            FATAL = .TRUE.
         ELSE IF( RANKVAL( I ).GT.100 ) THEN
            WRITE( NOUT, FMT = 9995 )' RANK  ', RANKVAL( I ), 100
            FATAL = .TRUE.
         END IF
      END DO
      IF( NRANK.GT.0 )
     $   WRITE( NOUT, FMT = 9993 )'RANK % OF N',
     $   ( RANKVAL( I ), I = 1, NRANK )
*
*     Read the threshold value for the test ratios.
*
      READ( NIN, FMT = * )THRESH
      WRITE( NOUT, FMT = 9992 )THRESH
*
*     Read the flag that indicates whether to test the PLASMA routines.
*
      READ( NIN, FMT = * )TSTCHK
*
*     Read the flag that indicates whether to test the driver routines.
*
      READ( NIN, FMT = * )TSTDRV
*
*     Read the flag that indicates whether to test the error exits.
*
      READ( NIN, FMT = * )TSTERR
*
      IF( FATAL ) THEN
         WRITE( NOUT, FMT = 9999 )
         STOP
      END IF
*
*     Calculate and print the machine dependent constants.
*
      EPS = DLAMCH( 'Underflow threshold' )
      WRITE( NOUT, FMT = 9991 )'underflow', EPS
      EPS = DLAMCH( 'Overflow threshold' )
      WRITE( NOUT, FMT = 9991 )'overflow ', EPS
      EPS = DLAMCH( 'Epsilon' )
      WRITE( NOUT, FMT = 9991 )'precision', EPS
      WRITE( NOUT, FMT = * )
      NRHS = NSVAL( 1 )
*
*     Initialize PLASMA
*
      CALL PLASMA_INIT( NPVAL(NNP), INFO )
*
      IF( SCHED .EQ. 1 ) THEN
         CALL PLASMA_SET(PLASMA_SCHEDULING_MODE, 
     $        PLASMA_DYNAMIC_SCHEDULING, INFO )
      ELSE
         CALL PLASMA_SET(PLASMA_SCHEDULING_MODE, 
     $        PLASMA_STATIC_SCHEDULING, INFO )
      ENDIF      
*
      CALL PLASMA_DISABLE( PLASMA_AUTOTUNING, INFO )
*
*      
   80 CONTINUE
*
*     Read a test path and the number of matrix types to use.
*
      READ( NIN, FMT = '(A72)', END = 140 )ALINE
      PATH = ALINE( 1: 3 )
      NMATS = MATMAX
      I = 3
   90 CONTINUE
      I = I + 1
      IF( I.GT.72 )
     $   GO TO 130
      IF( ALINE( I: I ).EQ.' ' )
     $   GO TO 90
      NMATS = 0
  100 CONTINUE
      C1 = ALINE( I: I )
      DO 110 K = 1, 10
         IF( C1.EQ.INTSTR( K: K ) ) THEN
            IC = K - 1
            GO TO 120
         END IF
  110 CONTINUE
      GO TO 130
  120 CONTINUE
      NMATS = NMATS*10 + IC
      I = I + 1
      IF( I.GT.72 )
     $   GO TO 130
      GO TO 100
  130 CONTINUE
      C1 = PATH( 1: 1 )
      C2 = PATH( 2: 3 )
*
*     Check first character for correct precision.
*
      IF( .NOT.LSAME( C1, 'Zomplex precision' ) ) THEN
         WRITE( NOUT, FMT = 9990 )PATH
*
      ELSE IF( NMATS.LE.0 ) THEN
*
*        Check for a positive number of tests requested.
*
         WRITE( NOUT, FMT = 9989 )PATH
*
      ELSE IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        GE:  general matrices
*
         NTYPES = 11
         CALL ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL ZCHKGE( DOTYPE, NM, MVAL, NN, NVAL, NNB2, NBVAL2, NNS,
     $                   IBVAL, NSVAL, THRESH, TSTERR, LDA, A( 1, 1 ),
     $                   A( 1, 2 ), A( 1, 3 ), B( 1, 1 ), B( 1, 2 ),
     $                   B( 1, 3 ), WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL ZDRVGE( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, LDA,
     $                   A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B( 1, 1 ),
     $                   B( 1, 2 ), B( 1, 3 ), B( 1, 4 ), S, WORK,
     $                   RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        PO:  positive definite matrices
*
         NTYPES = 9
         CALL ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL ZCHKPO( DOTYPE, NN, NVAL, NNB2, NBVAL2, NNS, NSVAL,
     $                   THRESH, TSTERR, LDA, A( 1, 1 ), A( 1, 2 ),
     $                   A( 1, 3 ), B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                   WORK, RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
         IF( TSTDRV ) THEN
            CALL ZDRVPO( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, LDA,
     $                   A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B( 1, 1 ),
     $                   B( 1, 2 ), B( 1, 3 ), B( 1, 4 ), S, WORK,
     $                   RWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9988 )PATH
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'QR' ) ) THEN
*
*        QR:  QR factorization
*
         NTYPES = 8
         CALL ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL ZCHKQR( DOTYPE, NM, MVAL, NN, NVAL, NNB, NBVAL, NXVAL,
     $                   IBVAL, NRHS, THRESH, TSTERR, NMAX, A( 1, 1 ),
     $                   A( 1, 2 ), A( 1, 3 ), A( 1, 4 ), A( 1, 5 ),
     $                   B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), B( 1, 4 ),
     $                   WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'LQ' ) ) THEN
*
*        LQ:  LQ factorization
*
         NTYPES = 8
         CALL ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL ZCHKLQ( DOTYPE, NM, MVAL, NN, NVAL, NNB, NBVAL, NXVAL,
     $                   IBVAL, NRHS, THRESH, TSTERR, NMAX, A( 1, 1 ),
     $                   A( 1, 2 ), A( 1, 3 ), A( 1, 4 ), A( 1, 5 ),
     $                   B( 1, 1 ), B( 1, 2 ), B( 1, 3 ), B( 1, 4 ),
     $                   WORK, RWORK, IWORK, NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'QP' ) ) THEN
*
*        QP:  QR factorization with pivoting
*
         NTYPES = 3
         CALL ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTCHK ) THEN
            CALL ZCHKQ3( DOTYPE, NM, MVAL, NN, NVAL, NNB, NBVAL, NXVAL,
     $                   THRESH, A( 1, 1 ), A( 1, 2 ), S( 1 ),
     $                   B( 1, 1 ), WORK, RWORK, IWORK,
     $                   NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE IF( LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        LS:  Least squares drivers
*
         NTYPES = 6
         CALL ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
*
         IF( TSTDRV ) THEN
            CALL ZDRVLS( DOTYPE, NM, MVAL, NN, NVAL, NNS, NSVAL, NNB,
     $                   IBVAL, NBVAL, NXVAL, THRESH, TSTERR, A( 1, 1 ),
     $                   A( 1, 2 ), A( 1, 3 ), A( 1, 4 ), A( 1, 5 ),
     $                   S( 1 ), S( NMAX+1 ), WORK, RWORK, IWORK,
     $                   NOUT )
         ELSE
            WRITE( NOUT, FMT = 9989 )PATH
         END IF
*
      ELSE
*
         WRITE( NOUT, FMT = 9990 )PATH
      END IF
*
*     Go back to get another input line.
*
      GO TO 80
*
*     Branch to this line when the last record is read.
*
  140 CONTINUE
      CLOSE ( NIN )
*
*     Finalize PLASMA
*
      CALL PLASMA_FINALIZE( INFO )
*      S2 = DSECND( )
      WRITE( NOUT, FMT = 9998 )
*      WRITE( NOUT, FMT = 9997 )S2 - S1
*
 9999 FORMAT( / ' Execution not attempted due to input errors' )
 9998 FORMAT( / ' End of tests' )
 9997 FORMAT( ' Total time used = ', F12.2, ' seconds', / )
 9996 FORMAT( ' Invalid input value: ', A4, '=', I6, '; must be >=',
     $      I6 )
 9995 FORMAT( ' Invalid input value: ', A4, '=', I6, '; must be <=',
     $      I6 )
 9994 FORMAT( ' Tests of the COMPLEX*16 PLASMA routines ',
     $      / ' PLASMA VERSION ', I1, '.', I1, '.', I1,
     $      / / ' The following parameter values will be used:' )
 9993 FORMAT( 4X, A4, ':  ', 10I6, / 11X, 10I6 )
 9992 FORMAT( / ' Routines pass computational tests if test ratio is ',
     $      'less than', F8.2, / )
 9991 FORMAT( ' Relative machine ', A, ' is taken to be', D16.6 )
 9990 FORMAT( / 1X, A3, ':  Unrecognized path name' )
 9989 FORMAT( / 1X, A3, ' routines were not tested' )
 9988 FORMAT( / 1X, A3, ' driver routines were not tested' )
 9987 FORMAT( ' Invalid input value: ', A6, '=', I6, '; must be 0 or 1')
*
*     End of ZCHKAA
*
      END
