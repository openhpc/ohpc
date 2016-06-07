! Copyright (C) 2007 Intel Corporation. All Rights Reserved. 
!
! The source code contained or described herein and all documents related to the source code 
! ("Material") are owned by Intel Corporation or its suppliers or licensors. Title to the 
! Material remains with Intel Corporation or its suppliers and licensors.  The Material is 
! protected by worldwide copyright laws and treaty provisions. No part of the Material may be 
! used, copied, reproduced, modified, published, uploaded, posted, transmitted, distributed, 
! or disclosed in any way except as expressly provided in the license provided with the 
! Materials.  No license under any patent, copyright, trade secret or other intellectual 
! property right is granted to or conferred upon you by disclosure or delivery of the 
! Materials, either expressly, by implication, inducement, estoppel or otherwise, except as 
! expressly provided in the license provided with the Materials.

!**********************************************************************
!
! SciGraph -- Scientific Graphs for Intel Fortran
!
!

MODULE SGDRAW

USE SGLOWLVL
IMPLICIT NONE

CONTAINS

!   Calculates the offset into a data array of a specified x,y pair based on
!     xfirst from the specified data settings.  1=X, 2=Y, 3=E

      INTEGER   FUNCTION SciGetDataOffset(dSettings, &
                                setNum,pointNum,pointWhich)

          RECORD /DataSettings/   dSettings       ! input
          INTEGER                 setNum          ! input
          INTEGER                 pointNum        ! input
          INTEGER                 pointWhich      ! input

          INTEGER                 pw
          INTEGER                 off

          IF (pointNum <= 0  .OR.  pointWhich <= 0       &
                  .OR.  pointNum > dSettings.numPoints   &
                  .OR.  pointWhich > dSettings.numElements) THEN
              SciGetDataOffset=-1     ! error condition
              RETURN
          END IF

          pw=pointWhich
          IF (.NOT. dSettings.xFirst) THEN
              pw=dSettings.numElements-pw+1
          END IF

          off=((setNum-1)*dSettings.numPoints+(pointNum-1))* &
              dSettings.numElements+pw

          SciGetDataOffset=off
          RETURN
      END FUNCTION

!   Sets fill style (for bars)

      SUBROUTINE SciSetFillType(fillType)

          INTEGER                  fillType

!     Bar values for use in data settings structures
      INTEGER(1), PARAMETER :: $BVEMPTY(8) = &
           (/ #00, #00, #00, #00, #00, #00, #00, #00 /) ! draw outline of bars
      INTEGER(1), PARAMETER :: $BVSOLID(8) = &
           (/ #FF, #FF, #FF, #FF, #FF, #FF, #FF, #FF /) ! draw solid filled bars
      INTEGER(1), PARAMETER :: $BVHASHLEFT(8) = &
           (/ #02, #04, #08, #10, #20, #40, #80, #01 /) ! left hashing  (/ /)
      INTEGER(1), PARAMETER :: $BVHASHRIGHT(8) = &
           (/ #40, #20, #10, #08, #04, #02, #01, #80 /) ! right hashing  (\ \)
      INTEGER(1), PARAMETER :: $BVHEAVYHASHLEFT(8) = &
           (/ #28, #50, #A0, #41, #82, #05, #0A, #14 /) ! heavy left hashing  (// //)
      INTEGER(1), PARAMETER :: $BVHEAVYHASHRIGHT(8) = &
           (/ #14, #0A, #05, #82, #41, #A0, #50, #28 /) ! heavy right hashing  (\\ \\)

          RETURN
      END SUBROUTINE



!   Responsible for drawing all bars on bar graphs
      SUBROUTINE SciDrawBar(x,y,width,height,barType,barColor)

          REAL(4)                x,y,width,height
          INTEGER(2)             barType,barColor

          INTEGER                retv

          IF (barType .NE. $BTSOLID) THEN
              CALL SciSetFillType($BTSOLID)
          END IF

          CALL SciSetFillType(INT4(barType))
          RETURN
      END SUBROUTINE


!   Responsible for drawing all markers on all graphs

      SUBROUTINE SciDrawMarker(x,y,markerType,markerColor)

          REAL(4)                 x,y
          INTEGER(2)              markerType,markerColor

          INTEGER*4               retv
          LOGICAL                 lretv
          REAL(8)                 a,b

          lretv=SciSetLineType($LTSOLID)
          CALL SciSetFillType($BTSOLID)
          RETURN
      END SUBROUTINE


!   Responsible for drawing all error bars on all graphs

      SUBROUTINE SciDrawErrorBars(x,y,yeLow,yeHigh, &
                                     errorbarType,errorbarColor)

          REAL                      x,y,yeLow,yeHigh
          INTEGER(2)                errorbarType,errorbarColor

          INTEGER*4                retv
          SELECT CASE (errorbarType)
          CASE ($EBTHIN)
              CALL SciLine_w($LTSOLID,DBLE(x),DBLE(y),DBLE(x),DBLE(yeHigh))
              CALL SciLine_w($LTSOLID,DBLE(x-80),DBLE(yeHigh),DBLE(x+80),DBLE(yeHigh))
              CALL SciLine_w($LTSOLID,DBLE(x),DBLE(y),DBLE(x),DBLE(yeLow))
              CALL SciLine_w($LTSOLID,DBLE(x-80),DBLE(yeLow),DBLE(x+80),DBLE(yeLow))
          CASE ($EBTHICK)
              CALL SciLine_w($LTTHICKSOLID,DBLE(x),DBLE(y),DBLE(x),DBLE(yeHigh))
              CALL SciLine_w($LTTHICKSOLID,DBLE(x-80),DBLE(yeHigh),DBLE(x+80),DBLE(yeHigh))
              CALL SciLine_w($LTTHICKSOLID,DBLE(x),DBLE(y),DBLE(x),DBLE(yeLow))
              CALL SciLine_w($LTTHICKSOLID,DBLE(x-80),DBLE(yeLow),DBLE(x+80),DBLE(yeLow))
          END SELECT
          RETURN
      END SUBROUTINE


!   SciDrawLegend draws the text, lines, markers, and shading

      SUBROUTINE SciDrawLegend(xwpr,ywpr,setNum,numSets,   &
                     lineType,lineColor,markerType,markerColor,  &
                     barType,barColor, &
                     titleColor,title,boxColor)

          REAL         xwpr,ywpr
          INTEGER      setNum,numSets
          INTEGER(2)   lineType,lineColor,markerType,markerColor
          INTEGER(2)   barType,barColor,titleColor,boxColor
          CHARACTER*20 title

          INTEGER      retv
          REAL         xp,yp,xb,yb,xw,yw
          INTEGER      numRows,numCols,currRow,currCol
          INTEGER      exw,eyw

          IF (setNum > numSets .OR. setNum <= 0 .OR. numSets > 8) THEN
              RETURN
          END IF


          numRows= INT((numSets-1)/4.0)+1
          numCols= INT((numSets-1)/numRows)+1
          currRow= INT((setNum-1)/numCols)+1
          currCol= MOD(setNum-1_2,numCols)+1

          exw=2000
          eyw=320
          xw=exw*numCols
          yw=eyw*numRows
          xb=$GXMIN+($GX-exw*0.5-xw)/2+exw*0.25
          yb=$GYMAX+16*ywpr+eyw*2

          xp=xb+160+(currCol-1)*exw
          yp=yb+(currRow-0.5)*eyw

          IF (barType .EQ. $BTNONE) THEN
              CALL SciLine_w(lineType,DBLE(xp),           &
                     DBLE(yp),DBLE(xp+240),DBLE(yp))
              CALL SciDrawMarker(REAL(xp+240.0/2),        &
                     REAL(yp),markerType,markerColor)
              xp=xp+272
          ELSE
              CALL SciDrawBar(xp,yp-0.25*eyw,             &
                     160.0,160.0,barType,barColor)
              xp=xp+272
          END IF

      END SUBROUTINE

END MODULE SGDRAW
