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
! 6/2001 Replace calls to setgtextvector to calls to setgtextrotation
! 6/2001 Add EOL check in loops removing leading blanks
!

MODULE SGLOWLVL

USE SGDATA
IMPLICIT NONE

CONTAINS

! This function makes sure we are in the correct graphics mode, and then
!    sets all the graphics library settings to values that are good as
!    defaults

   LOGICAL FUNCTION SciGraphicsMode(graph)
          logical lretval

          RECORD /GraphSettings/ graph
          INTEGER retVal
	  INTEGER solidi
    INTEGER*1, PARAMETER :: SOLID(8) = (/ (#FF,solidi=1,8) /)   ! solid fill

          RECORD /PrivateGraphSettings/ pgraph   ! private graph
          RECORD /GraphSettings/ cgraph          ! copy of private graph
          EQUIVALENCE(cgraph,pgraph)

          cgraph=graph
          IF (graph.setGraphMode .AND. pgraph.didPlotGraph .EQ. 0) THEN
              lretval = SGSetMaxRes()
              IF (.not. lretVal) THEN
                  SciGraphicsMode=.FALSE.
                  RETURN
              END IF
          END IF


          IF (graph.x1 < 0  .OR.  graph.y1 < 0)  THEN 
              SciGraphicsMode=.FALSE.
              RETURN
          END IF

          SciGraphicsMode=.TRUE.
          RETURN
      END FUNCTION

!   Does a MOVETO_W, but allows us to not clutter up the code with lots of temp wxycoords

      SUBROUTINE SciMoveto_w(x,y)
          REAL*8 x,y
      END SUBROUTINE

!   Draws a line from one (x,y) to another.  Draws in the specified line type
!     from the library.  Here so we don't have to keep doing MOVETO_W, LINETO_W
!     in the code

      SUBROUTINE SciLine_w(lt,x1,y1,x2,y2)

          INTEGER(2) lt
          REAL*8 x1,y1,x2,y2

          INTEGER retv
          LOGICAL thick
          REAL*8 dx,dy,adx,ady

          thick=SciSetLineType(lt)

          IF (thick) THEN
                  dy=(y2-y1)/16         ! number of pixels rise
                  dx=(x2-x1)/16         !  and run

                  IF (abs(dy) > abs(dx)) THEN
                      adx=SIGN(1.0D0,dy)
                      ady=0
                  ELSE
                      ady=-SIGN(1.0D0,dx)
                      adx=0
                  END IF

          END IF

          RETURN
      END SUBROUTINE


!   Draws an circle with a given center and radius.  Draws with the specified line type
!     from the library and should be filled in if desired

      SUBROUTINE SciCircle_w(x,y,r,lt,ft)

          REAL*8 x,y,r
          INTEGER(2) lt
          INTEGER*2 ft

          LOGICAL thick
          REAL*8 x1,y1,x2,y2
          INTEGER retv

          x1=x-r
          y1=y-r
          x2=x+r
          y2=y+r

          thick=SciSetLineType(lt)

          RETURN
      END SUBROUTINE

!   Sets the line type to a library specified value and returns whether it is
!     thick or not

      LOGICAL FUNCTION SciSetLineType(lt)

          INTEGER(2) lt

          LOGICAL thick

          thick=.FALSE.

          SciSetLineType=thick
          RETURN
      END FUNCTION



!   Sets the font to one of the library defined values

      INTEGER FUNCTION SciSetFont(fontType)

          INTEGER(2) fontType

          INTEGER retv /-1/

          SciSetFont=retv
          RETURN
      END FUNCTION


!   Get font heigth
      INTEGER FUNCTION SciGetTextHeight()

          INTEGER retv,th

              th=16
          SciGetTextHeight=th
          RETURN
      END FUNCTION


!   Get text length
      INTEGER FUNCTION SciGetTextLength(text,rotated,column)

          CHARACTER*(*) text
          LOGICAL rotated,column            ! if text rotated or in a column

          INTEGER retv,tl


          IF (.NOT. rotated .AND. .NOT. column) THEN      ! normal text
              tl=12
              SciGetTextLength=tl
              RETURN
          END IF

          IF (rotated) THEN
              IF (column) THEN
                  tl=5
                  SciGetTextLength=tl
                  RETURN
              ELSE
                  tl=0
                  SciGetTextLength=tl
                  RETURN
              END IF
          END IF
      END FUNCTION

!   Centers a given string between two x and between two y

      SUBROUTINE SciCenter(xwpr,ywpr,xp1,xp2,yp1,yp2,text)

          REAL*4 xwpr,ywpr
          REAL*8 xp1,xp2,yp1,yp2
          CHARACTER*(*) text

          INTEGER nsend,nsstart

          REAL*8 xsp,ysp
          INTEGER tl,th

          nsstart=1          ! trash leading blanks
          DO WHILE (text(nsstart:nsstart) .EQ. ' ')
              nsstart=nsstart+1
	      if (nsstart > len(text)) exit
          END DO

          nsend=nsstart+len_trim(text(nsstart:))-1
          tl=22
          xsp=xp1+(xp2-xp1-tl*xwpr)/2.0D0

          th=SciGetTextHeight()
          ysp=yp1+(yp2-yp1-th*ywpr)/2.0D0 ! text draw starts from upper left

      END SUBROUTINE


!   Centers a given string starting or ending at an x and between two y

      SUBROUTINE SciEndCenter(xwpr,ywpr,xp1,xEnd,yp1,yp2,text)

          REAL*4 xwpr,ywpr
          REAL*8 xp1,yp1,yp2
          LOGICAL xEnd
          CHARACTER*(*) text

          INTEGER nsend,nsstart

          REAL*8 xsp,ysp
          INTEGER tl,th

          nsstart=1          ! trash leading blanks
          DO WHILE (text(nsstart:nsstart) .EQ. ' ')
              nsstart=nsstart+1
	      if (nsstart > len(text)) exit
          END DO

          nsend=nsstart+len_trim(text(nsstart:))-1
          tl=10
          IF (xEnd) THEN
              xsp=xp1-tl*xwpr
          ELSE
              xsp=xp1
          END IF

          th=SciGetTextHeight()
          ysp=yp1+(yp2-yp1-th*ywpr)/2.0D0 ! text draw starts from upper left

      END SUBROUTINE

!   Centers a given line of rotated text at a given x and between two y

      SUBROUTINE SciRotCenter(xwpr,ywpr,xp1,xp2,yp1,yp2,text)

          REAL*4 xwpr,ywpr
          REAL*8 xp1,xp2,yp1,yp2
          CHARACTER*(*) text

          INTEGER nsstart,nsend
          REAL*8 ysp,xsp
          INTEGER tl,th
          LOGICAL column

          nsstart=1          ! trash leading blanks
          DO WHILE (text(nsstart:nsstart) .EQ. ' ')
              nsstart=nsstart+1
	      if (nsstart > len(text)) exit
          END DO
          nsend=nsstart+len_trim(text(nsstart:))-1

          tl=SciGetTextLength(text(nsstart:nsend),.true.,column)
          ysp=yp1+(yp2-yp1+tl*ywpr)/2.0D0

          th=SciGetTextHeight()
          xsp=xp1+(xp2-xp1-th*xwpr)/2.0D0

          IF (column) THEN
              ysp=ysp-tl*ywpr
              CALL SciVertText(text(nsstart:nsend))
          END IF

      END SUBROUTINE

!   Centers a given line of rotated text at a given x and between two y

      SUBROUTINE SciRotEndCenter(xwpr,ywpr,xp1,xBottom,yp1,yp2,text)

          REAL*4 xwpr,ywpr
          REAL*8 xp1,yp1,yp2
          LOGICAL xBottom
          CHARACTER*(*) text

          INTEGER nsstart,nsend
          REAL*8 ysp,xsp
          INTEGER tl,th
          LOGICAL column

          nsstart=1          ! trash leading blanks
          DO WHILE (text(nsstart:nsstart) .EQ. ' ')
              nsstart=nsstart+1
	      if (nsstart > len(text)) exit
          END DO
          nsend=nsstart+len_trim(text(nsstart:))-1

          tl=SciGetTextLength(text(nsstart:nsend),.true.,column)
          ysp=yp1+(yp2-yp1+tl*ywpr)/2.0D0

          th=SciGetTextHeight()
          IF (xBottom) THEN
              xsp=xp1-th*xwpr
          ELSE
              xsp=xp1
          END IF

          IF (column) THEN
              ysp=ysp-tl*ywpr
              CALL SciVertText(text(nsstart:nsend))
          END IF

      END SUBROUTINE

      Logical Function SGSetMaxRes
       SGSetMaxRes = .true.
      end function

!
!  Vertical graphics output of a text string (from the current position)
!

      SUBROUTINE SciVertText (text)

      CHARACTER*(*)  text
      CHARACTER*1    ch
      INTEGER        status, loop, ipos, jpos, indent

!     Output one char of the string at a time, in an descending column
      do loop = 1, len(text)
        ch = text(loop:loop)
      end do

      END SUBROUTINE

END MODULE SGLOWLVL
