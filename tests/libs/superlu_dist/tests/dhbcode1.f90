!> @file
!! \brief Fortran code for reading a sparse matrix in Harwell-Boeing format.
!!
!
      subroutine dhbcode1(nrow, ncol, nnzero, values, rowind, colptr)

!     ================================================================
!     ... SAMPLE CODE FOR READING A SPARSE MATRIX IN STANDARD FORMAT
!     ================================================================

      CHARACTER      TITLE*72, KEY*8, MXTYPE*3, PTRFMT*16, &
                     INDFMT*16, VALFMT*20, RHSFMT*20

      INTEGER        TOTCRD, PTRCRD, INDCRD, VALCRD, RHSCRD,  NROW,  &
                     NCOL  , NNZERO, NELTVL

      INTEGER        COLPTR (*), ROWIND (*)

      REAL*8         VALUES (*)

!    ------------------------
!     ... READ IN HEADER BLOCK
!     ------------------------

      READ ( 5, 1000 ) TITLE , KEY , TOTCRD, PTRCRD, INDCRD, VALCRD, &
                       RHSCRD, MXTYPE, NROW  , NCOL  , NNZERO, NELTVL, &
                       PTRFMT, INDFMT, VALFMT, RHSFMT
 1000 FORMAT ( A72, A8 / 5I14 / A3, 11X, 4I14 / 2A16, 2A20 )

!     -------------------------
!     ... READ MATRIX STRUCTURE
!     -------------------------

      READ ( 5, PTRFMT ) ( COLPTR (I), I = 1, NCOL+1 )

      READ ( 5, INDFMT ) ( ROWIND (I), I = 1, NNZERO )

      IF  ( VALCRD .GT. 0 )  THEN

!         ----------------------
!         ... READ MATRIX VALUES
!         ----------------------

          READ ( 5, VALFMT ) ( VALUES (I), I = 1, NNZERO )

      ENDIF

      return
      end

