      SUBROUTINE CHKXER( SRNAMT, INFOT, NOUT, INFO, OK )
*
*  Tests whether XERBLA has detected an error when it should.
*
*  Auxiliary routine for test program for Level 2 Blas.
*
*  -- Written on 10-August-1987. 
*     Richard Hanson, Sandia National Labs.
*     Jeremy Du Croz, NAG Central Office.
*
*  =====================================================================
*
*     .. Scalar Arguments ..
      LOGICAL            OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, INFO, NOUT
*     ..
*     .. Executable Statements ..
      IF( INFOT.NE.ABS( INFO )  ) THEN
         WRITE( NOUT, FMT = 9999 ) INFOT, SRNAMT
         OK = .FALSE.
      END IF
      RETURN
*
 9999 FORMAT( ' *** Illegal value of parameter number ', I2,
     $      ' not detected by ', A6, ' ***' )
*
*     End of CHKXER.
*
      END
