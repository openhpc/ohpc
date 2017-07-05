      SUBROUTINE ZDRVGE( DOTYPE, NN, NVAL, NRHS, THRESH, TSTERR, NMAX,
     $                   A, AFAC, ASAV, B, BSAV, X, XACT, S, WORK,
     $                   RWORK, IWORK, NOUT )
*
      INCLUDE 'plasmaf.h'
*
*  -- LAPACK test routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NMAX, NN, NOUT, NRHS
      DOUBLE PRECISION   THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), NVAL( * )
      DOUBLE PRECISION   RWORK( * ), S( * )
      COMPLEX*16         A( * ), AFAC( * ), ASAV( * ), B( * ),
     $                   BSAV( * ), WORK( * ), X( * ), XACT( * )
*     ..
*
*  Purpose
*  =======
*
*  ZDRVGE tests the driver routines ZGESV and -SVX.
*
*  Arguments
*  =========
*
*  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
*          The matrix types to be used for testing.  Matrices of type j
*          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
*          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix column dimension N.
*
*  NRHS    (input) INTEGER
*          The number of right hand side vectors to be generated for
*          each linear system.
*
*  THRESH  (input) DOUBLE PRECISION
*          The threshold value for the test ratios.  A result is
*          included in the output file if RESULT >= THRESH.  To have
*          every test ratio printed, use THRESH = 0.
*
*  TSTERR  (input) LOGICAL
*          Flag that indicates whether error exits are to be tested.
*
*  NMAX    (input) INTEGER
*          The maximum value permitted for N, used in dimensioning the
*          work arrays.
*
*  A       (workspace) COMPLEX*16 array, dimension (NMAX*NMAX)
*
*  AFAC    (workspace) COMPLEX*16 array, dimension (NMAX*NMAX)
*
*  ASAV    (workspace) COMPLEX*16 array, dimension (NMAX*NMAX)
*
*  B       (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)
*
*  BSAV    (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)
*
*  X       (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)
*
*  XACT    (workspace) COMPLEX*16 array, dimension (NMAX*NRHS)
*
*  S       (workspace) DOUBLE PRECISION array, dimension (2*NMAX)
*
*  WORK    (workspace) COMPLEX*16 array, dimension
*                      (NMAX*max(3,NRHS))
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (2*NRHS+NMAX)
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            NTYPES
      PARAMETER          ( NTYPES = 11 )
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 7 )
      INTEGER            NTRAN
      PARAMETER          ( NTRAN = 1 )
*     ..
*     .. Local Scalars ..
      LOGICAL            EQUIL, NOFACT, PREFAC, TRFCON, ZEROT
      CHARACTER          DIST, EQUED, FACT, TRANS, TYPE, XTYPE
      CHARACTER*3        PATH
      INTEGER            HL( 2 ), HPIV( 2 ), IB, PLASMA_TRANS
      INTEGER            I, IEQUED, IFACT, IMAT, IN, INFO, IOFF, ITRAN,
     $                   IZERO, K, K1, KL, KU, LDA, LWORK, MODE, N, NB,
     $                   NBMIN, NERRS, NFACT, NFAIL, NIMAT, NRUN, NT
      DOUBLE PRECISION   AINVNM, AMAX, ANORM, ANORMI, ANORMO, CNDNUM,
     $                   COLCND, RCOND, RCONDC, RCONDI, RCONDO, ROLDC,
     $                   ROLDI, ROLDO, ROWCND, RPVGRW
*     ..
*     .. Local Arrays ..
      CHARACTER          EQUEDS( 4 ), FACTS( 3 ), TRANSS( NTRAN )
      INTEGER            ISEED( 4 ), ISEEDY( 4 ), PLASMA_TRANSS( NTRAN)
      DOUBLE PRECISION   RDUM( 1 ), RESULT( NTESTS )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DGET06, DLAMCH, ZLANGE, ZLANTR
      EXTERNAL           LSAME, DGET06, DLAMCH, ZLANGE, ZLANTR
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALADHD, ALAERH, ALASVM, XLAENV, ZERRVX, ZGEEQU,
     $                   ZGESV, ZGESVX, ZGET02, ZGET04,
     $                   ZGETRF, ZGETRI, ZLACPY, ZLAQGE, ZLARHS, ZLASET,
     $                   ZLATB4, ZLATMS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DCMPLX, MAX
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
*      DATA               TRANSS / 'N', 'T', 'C' /
      DATA               TRANSS / 'N' /
      DATA               PLASMA_TRANSS / PLASMANOTRANS /
      DATA               FACTS / 'F', 'N', 'E' /
      DATA               EQUEDS / 'N', 'R', 'C', 'B' /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'Zomplex precision'
      PATH( 2: 3 ) = 'GE'
      RCONDO = ZERO
      RCONDI = ZERO
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
     $   CALL ZERRVX( PATH, NOUT )
      INFOT = 0
*
*     Set the block size and minimum block size for testing.
*
      NB = 128
      IB = 32
      NBMIN = 32
      CALL XLAENV( 1, NB )
      CALL XLAENV( 2, NBMIN )
      CALL PLASMA_SET( PLASMA_TILE_SIZE, NB, INFO )
      CALL PLASMA_SET( PLASMA_INNER_BLOCK_SIZE, IB, INFO )
*
*     Do for each value of N in NVAL
*
      DO 90 IN = 1, NN
         N = NVAL( IN )
         LDA = MAX( N, 1 )
         XTYPE = 'N'
         NIMAT = NTYPES
         IF( N.LE.0 )
     $      NIMAT = 1
*
*        ALLOCATE L and IPIV
*
c$$$         CALL PLASMA_ALLOC_WORKSPACE_ZGETRF_INCPIV(
c$$$     $        N, N, HL, HPIV, INFO )
*
*
         DO 80 IMAT = 1, NIMAT
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 80
*
*           Skip types 5, 6, or 7 if the matrix size is too small.
*
            ZEROT = IMAT.GE.5 .AND. IMAT.LE.7
            IF( ZEROT .AND. N.LT.IMAT-4 )
     $         GO TO 80
*
*           Set up parameters with ZLATB4 and generate a test matrix
*           with ZLATMS.
*
            CALL ZLATB4( PATH, IMAT, N, N, TYPE, KL, KU, ANORM, MODE,
     $                   CNDNUM, DIST )
            RCONDC = ONE / CNDNUM
*
            SRNAMT = 'ZLATMS'
            CALL ZLATMS( N, N, DIST, ISEED, TYPE, RWORK, MODE, CNDNUM,
     $                   ANORM, KL, KU, 'No packing', A, LDA, WORK,
     $                   INFO )
*
*           Check error code from ZLATMS.
*
            IF( INFO.NE.0 ) THEN
               CALL ALAERH( PATH, 'ZLATMS', INFO, 0, ' ', N, N, -1, -1,
     $                      -1, IMAT, NFAIL, NERRS, NOUT )
               GO TO 80
            END IF
*
*           For types 5-7, zero one or more columns of the matrix to
*           test that INFO is returned correctly.
*
            IF( ZEROT ) THEN
               IF( IMAT.EQ.5 ) THEN
                  IZERO = 1
               ELSE IF( IMAT.EQ.6 ) THEN
                  IZERO = N
               ELSE
                  IZERO = N / 2 + 1
               END IF
               IOFF = ( IZERO-1 )*LDA
               IF( IMAT.LT.7 ) THEN
                  DO 20 I = 1, N
                     A( IOFF+I ) = ZERO
   20             CONTINUE
               ELSE
                  CALL ZLASET( 'Full', N, N-IZERO+1, DCMPLX( ZERO ),
     $                         DCMPLX( ZERO ), A( IOFF+1 ), LDA )
               END IF
            ELSE
               IZERO = 0
            END IF
*
*           Save a copy of the matrix A in ASAV.
*
            CALL ZLACPY( 'Full', N, N, A, LDA, ASAV, LDA )
*
            DO 70 IEQUED = 1, 4
               EQUED = EQUEDS( IEQUED )
               IF( IEQUED.EQ.1 ) THEN
                  NFACT = 3
               ELSE
                  NFACT = 1
               END IF
*
               DO 60 IFACT = 1, NFACT
                  FACT = FACTS( IFACT )
                  PREFAC = LSAME( FACT, 'F' )
                  NOFACT = LSAME( FACT, 'N' )
                  EQUIL = LSAME( FACT, 'E' )
*
                  IF( ZEROT ) THEN
                     IF( PREFAC )
     $                  GO TO 60
                     RCONDO = ZERO
                     RCONDI = ZERO
*
                  ELSE IF( .NOT.NOFACT ) THEN
*
*                    Compute the condition number for comparison with
*                    the value returned by ZGESVX (FACT = 'N' reuses
*                    the condition number from the previous iteration
*                    with FACT = 'F').
*
                     CALL ZLACPY( 'Full', N, N, ASAV, LDA, AFAC, LDA )
                     IF( EQUIL .OR. IEQUED.GT.1 ) THEN
*
*                       Compute row and column scale factors to
*                       equilibrate the matrix A.
*
                        CALL ZGEEQU( N, N, AFAC, LDA, S, S( N+1 ),
     $                               ROWCND, COLCND, AMAX, INFO )
                        IF( INFO.EQ.0 .AND. N.GT.0 ) THEN
                           IF( LSAME( EQUED, 'R' ) ) THEN
                              ROWCND = ZERO
                              COLCND = ONE
                           ELSE IF( LSAME( EQUED, 'C' ) ) THEN
                              ROWCND = ONE
                              COLCND = ZERO
                           ELSE IF( LSAME( EQUED, 'B' ) ) THEN
                              ROWCND = ZERO
                              COLCND = ZERO
                           END IF
*
*                          Equilibrate the matrix.
*
                           CALL ZLAQGE( N, N, AFAC, LDA, S, S( N+1 ),
     $                                  ROWCND, COLCND, AMAX, EQUED )
                        END IF
                     END IF
*
*                    Save the condition number of the non-equilibrated
*                    system for use in ZGET04.
*
                     IF( EQUIL ) THEN
                        ROLDO = RCONDO
                        ROLDI = RCONDI
                     END IF
*
*                    Compute the 1-norm and infinity-norm of A.
*
                     ANORMO = ZLANGE( '1', N, N, AFAC, LDA, RWORK )
                     ANORMI = ZLANGE( 'I', N, N, AFAC, LDA, RWORK )
*
*                    Factor the matrix A.
*
c$$$                     CALL PLASMA_ZGETRF_INCPIV( N, N, AFAC, LDA, 
c$$$     $                                   HL, HPIV, INFO )
                     CALL PLASMA_ZGETRF( N, N, AFAC, LDA, 
     $                                   IWORK, INFO )
*
                  END IF
*
                  DO 50 ITRAN = 1, NTRAN
*
*                    Do for each value of TRANS.
*
                     TRANS = TRANSS( ITRAN )
                     PLASMA_TRANS = PLASMA_TRANSS( ITRAN )
                     IF( ITRAN.EQ.1 ) THEN
                        RCONDC = RCONDO
                     ELSE
                        RCONDC = RCONDI
                     END IF
*
*                    Restore the matrix A.
*
                     CALL ZLACPY( 'Full', N, N, ASAV, LDA, A, LDA )
*
*                    Form an exact solution and set the right hand side.
*
                     SRNAMT = 'ZLARHS'
                     CALL ZLARHS( PATH, XTYPE, 'Full', TRANS, N, N, KL,
     $                            KU, NRHS, A, LDA, XACT, LDA, B, LDA,
     $                            ISEED, INFO )
                     XTYPE = 'C'
                     CALL ZLACPY( 'Full', N, NRHS, B, LDA, BSAV, LDA )
*
                     IF( NOFACT .AND. ITRAN.EQ.1 ) THEN
*
*                       --- Test ZGESV  ---
*
*                       Compute the LU factorization of the matrix and
*                       solve the system.
*
                        CALL ZLACPY( 'Full', N, N, A, LDA, AFAC, LDA )
                        CALL ZLACPY( 'Full', N, NRHS, B, LDA, X, LDA )
*
                        SRNAMT = 'ZGESV '
c$$$                        CALL PLASMA_ZGESV_INCPIV( N, NRHS, AFAC, LDA, 
c$$$     $                       HL, HPIV, X, LDA, INFO )
                        CALL PLASMA_ZGESV( N, NRHS, AFAC, LDA, 
     $                       IWORK, X, LDA, INFO )
*
*                       Check error code from ZGESV .
*
                        IF( INFO.NE.IZERO )
     $                     CALL ALAERH( PATH, 'ZGESV ', INFO, IZERO,
     $                                  ' ', N, N, -1, -1, NRHS, IMAT,
     $                                  NFAIL, NERRS, NOUT )
*
                        IF( IZERO.EQ.0 ) THEN
*
*                          Compute residual of the computed solution.
*
                           CALL ZLACPY( 'Full', N, NRHS, B, LDA, WORK,
     $                                  LDA )
                           CALL ZGET02( 'No transpose', N, N, NRHS, A,
     $                                  LDA, X, LDA, WORK, LDA, RWORK,
     $                                  RESULT( 1 ) )
*
*                          Check solution from generated exact solution.
*
                           CALL ZGET04( N, NRHS, X, LDA, XACT, LDA,
     $                                  RCONDC, RESULT( 2 ) )
                           NT = 2
                        END IF
*
*                       Print information about the tests that did not
*                       pass the threshold.
*
                        DO 30 K = 1, NT
                           IF( RESULT( K ).GE.THRESH ) THEN
                              IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                           CALL ALADHD( NOUT, PATH )
                              WRITE( NOUT, FMT = 9999 )'ZGESV ', N,
     $                           IMAT, K, RESULT( K )
                              NFAIL = NFAIL + 1
                           END IF
   30                   CONTINUE
                        NRUN = NRUN + NT
                     END IF
*
   50             CONTINUE
   60          CONTINUE
   70       CONTINUE
   80    CONTINUE
*
*        DEALLOCATE HL and HPIV
*
c$$$         CALL PLASMA_DEALLOC_HANDLE( HL, INFO )
c$$$         CALL PLASMA_DEALLOC_HANDLE( HPIV, INFO )
   90 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASVM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( 1X, A, ', N =', I5, ', type ', I2, ', test(', I2, ') =',
     $      G12.5 )
 9998 FORMAT( 1X, A, ', FACT=''', A1, ''', TRANS=''', A1, ''', N=', I5,
     $      ', type ', I2, ', test(', I1, ')=', G12.5 )
 9997 FORMAT( 1X, A, ', FACT=''', A1, ''', TRANS=''', A1, ''', N=', I5,
     $      ', EQUED=''', A1, ''', type ', I2, ', test(', I1, ')=',
     $      G12.5 )
      RETURN
*
*     End of ZDRVGE
*
      END
