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
! 6/2001 - replace calls to setgtextvector to calls to setgtextrotation
!

MODULE SGPLOT

USE SGDRAW
IMPLICIT NONE

CONTAINS

!   PlotGraph begins the graphing process.  It draws the graph border, the
!     graphing area boundary, the graph title, the axes, the tick marks,
!     the grid, and the axis titles.  numSettings is the total number of
!     data ranges that will be plotted on this graph.

      INTEGER FUNCTION PlotGraph(graph,numAxes,axisArray,numSettings)

          RECORD /GraphSettings/  graph               ! input
          INTEGER                 numAxes             ! input
          RECORD /AxisSettings/   axisArray(numAxes)  ! input
          INTEGER                 numSettings         ! input

          RECORD /PrivateGraphSettings/ pgraph   ! private graph
          RECORD /GraphSettings/ cgraph          ! copy of private graph
          EQUIVALENCE(cgraph,pgraph)

          LOGICAL                 errBar,polar,xlabels,xy,isLog
          INTEGER                 axesWanted($APCOUNT)

          RECORD /AxisSettings/   ca,ra,ta         ! current axis,raxis,theta axis
          DOUBLE PRECISION        xp,yp,xb,yb,tval,tlen,toff
          DOUBLE PRECISION        lowVal,highVal,increment
          DOUBLE PRECISION        logLowVal,loghighVal,logBase,logMul

          INTEGER                 xd,yd
          INTEGER                 stc,btc,ttc,maxtc,numTicks
          INTEGER                 retv
          LOGICAL                 top,left,smt
          CHARACTER*16            tempstr,fms
          INTEGER                 i,drawAxes
          REAL*4                  xwpr,ywpr
          DOUBLE PRECISION        xc,yc,xo,yo,r
          DOUBLE PRECISION        tadd,tang,tanginc,ctang,stang
          DOUBLE PRECISION        x0,y0,x1,y1,x2,y2,x3,y3

          cgraph=graph

          IF (numAxes .LT. 1  .OR.  numSettings .LT. 1) THEN
              PlotGraph=$GEPARAMS
              RETURN
          END IF

          IF (cgraph.graphType .EQ. $GTLINEWERRBAR .OR.   &
              cgraph.graphType .EQ. $GTXYWERRBAR) THEN
              errBar=.TRUE.
          ELSE
              errBar=.FALSE.
          ENDIF

          IF (cgraph.graphType .EQ. $GTBAR .OR.           &
              cgraph.graphType .EQ. $GTLINE .OR.         &
              cgraph.graphType .EQ. $GTLINEWERRBAR) THEN
              xlabels=.TRUE.
              xy=.FALSE.
          ELSE
              xlabels=.FALSE.
              xy=.TRUE.
          ENDIF

          IF (cgraph.graphType .EQ. $GTPOLAR) THEN
              polar=.TRUE.
          ELSE
              polar=.FALSE.
          ENDIF

          IF (.NOT. SciGraphicsMode(cgraph)) THEN
              PlotGraph=$GEGRAPHMODE
              RETURN
          ENDIF

          xwpr=$WX/(cgraph.x2-cgraph.x1+1)
          ywpr=$WY/(cgraph.y2-cgraph.y1+1)

!     Print graph title
          IF (SciSetFont(graph.titleFont) .LE. 0) THEN
              PlotGraph=$GEFONT
              RETURN
          END IF
          toff=SciGetTextHeight()                 ! save text height

          CALL SciCenter(xwpr,ywpr,$WXMIN,$WXMAX,   &
             $WYMIN+192,$WYMIN+192,graph.title)      ! print graph title


!     Print graph title, line 2
          IF (SciSetFont(graph.title2Font) .LE. 0) THEN
              PlotGraph=$GEFONT
              RETURN
          END IF

          toff=$WYMIN+192+toff*8.0+128
          CALL SciCenter(xwpr,ywpr,$WXMIN,$WXMAX,  &
             toff,toff,graph.title2)                ! print graph second title

          IF (.NOT. polar) THEN           ! all this code is for rect. graphs
              DO i=1,$APCOUNT             ! zero out the axis counts
                  axesWanted(i)=0
              END DO

              IF (numAxes .GT. 4 .OR. numAxes .LT. 2) THEN    ! 1 per side at max
                  PlotGraph=$GEAXIS
                  RETURN
              END IF

              DO drawAxes = 1,numAxes     ! count up the axis position usage, check
                  ca=AxisArray(drawAxes)
                  ! only allow valid position/type pairs
                  IF ( (ca.axisType .EQ. $ATX .AND.           &
                     (ca.axisPos .NE. $APTOP .AND.           &
                     ca.axisPos .NE. $APBOTTOM)) .OR.        &
                     ( (ca.axisType .EQ. $ATY .AND.          &
                     ca.axisPos .NE. $APLEFT .AND.           &
                     ca.axisPos .NE. $APRIGHT)) ) THEN
                          PlotGraph=$GEAXIS
                          RETURN
                  END IF
                  axesWanted(ca.axisPos)=axesWanted(ca.axisPos)+1

                  IF (ca.highVal .LE. ca.lowVal .OR.   &
                         ca.increment .LE. 0) THEN
                      PlotGraph=$GEAXIS
                      RETURN
                  END IF

                  IF (axesWanted(ca.axisPos) .GT. 1) THEN
                      PlotGraph=$GEAXIS
                      RETURN
                  END IF
              END DO

              ! draw the axes: only 1 axis per side
              DO drawAxes = 1,numAxes
                  ca=AxisArray(drawAxes)
!                  retv=SETCOLOR(ca.axisColor)

                  SELECT CASE (ca.axisPos)
                  CASE ($APBOTTOM)
                      CALL SciLine_w($LTSOLID,$GXMIN,$GYMAX,$GXMAX,$GYMAX)
                      IF (axesWanted($APTOP) .EQ. 0) THEN
                          CALL SciLine_w($LTSOLID,         &
                             $GXMIN,$GYMIN,$GXMAX,$GYMIN)
                      END IF
                  CASE ($APTOP)
                      CALL SciLine_w($LTSOLID,             &
                         $GXMIN,$GYMIN,$GXMAX,$GYMIN)
                      IF (axesWanted($APBOTTOM) .EQ. 0) THEN
                          CALL SciLine_w($LTSOLID,         &
                             $GXMIN,$GYMAX,$GXMAX,$GYMAX)
                      END IF
                  CASE ($APLEFT)
                      CALL SciLine_w($LTSOLID,             &
                         $GXMIN,$GYMAX,$GXMIN,$GYMIN)
                      IF (axesWanted($APRIGHT) .EQ. 0) THEN
                          CALL SciLine_w($LTSOLID,         &
                             $GXMAX,$GYMIN,$GXMAX,$GYMAX)
                      END IF
                  CASE ($APRIGHT)
                      CALL SciLine_w($LTSOLID,             &
                         $GXMAX,$GYMIN,$GXMAX,$GYMAX)
                      IF (axesWanted($APLEFT) .EQ. 0) THEN
                          CALL SciLine_w($LTSOLID,         &
                             $GXMIN,$GYMAX,$GXMIN,$GYMIN)
                      END IF
                  END SELECT
              END DO

              ! now draw the tick marks and grids and the zero line (if any)
              DO drawAxes = 1,numAxes
                  ca=AxisArray(drawAxes)

                  lowVal=ca.lowVal
                  highVal=ca.highVal
                  increment=ca.increment

                  isLog=ca.axisFunc .NE. $AFLINEAR
                  IF (isLog) THEN     ! must check log low/high/inc
                      IF (lowVal .LE. 0) THEN
                          PlotGraph=$GEAXIS
                          RETURN
                      END IF

                      SELECT CASE (ca.axisFunc)
                      CASE ($AFLOG10)
                          logBase=10.0
                      CASE ($AFLOG)
                          logBase=EXP(1.0)
                      CASE ($AFLOG2)
                          logBase=2.0
                      END SELECT

                      logMul=1.0D0/DLOG(logBase)

                      logLowVal=NINT(DLOG(lowVal)/DLOG(logBase))
                      lowVal=logBase**logLowVal
                      logHighVal=NINT(DLOG(highVal)/DLOG(logBase))
                      highVal=logBase**logHighVal

                      numTicks=ca.tickRatio

                      increment=(lowVal*logBase-lowVal)/numTicks
                  ELSE
                      ! axis setup for normal rect. graph
                      IF (xlabels .AND. (ca.axisType .EQ. $ATX    &
                             .OR. ca.axisType .EQ. $ATR)) THEN
                          numTicks=1
                      ELSE
                          numTicks=ca.tickRatio
                      END IF

                      increment=increment/numTicks
                  END IF

                  ! draw zero line(s) if requested
                  IF (ca.gridStyle .EQ. $GSZERO                  &
                         .OR. ca.gridStyle .EQ. $GSZEROMAJOR     &
                         .OR. ca.gridStyle .EQ. $GSZEROBOTH) THEN
                      IF (lowVal .LE. 0) THEN
!                          retv=SETCOLOR(ca.tickColor)
                          IF (ca.axisType .EQ. $ATX) THEN
                              xp=$GXMIN-lowVal/(highVal-lowVal)*$GX
                              CALL SciLine_w(ca.gridLineType,xp,$GYMIN,xp,$GYMAX)
                          END IF

                          IF (ca.axisType .EQ. $ATY) THEN
                              yp=$GYMAX+lowVal/(highVal-lowVal)*$GY
                              CALL SciLine_w(ca.gridLineType,     &
                                     $GXMIN,yp,$GXMAX,yp)
                          END IF
                      END IF
                  END IF

                  IF (ca.axisType .EQ. $ATX .AND. xlabels) THEN   ! label x axis
                      ! just draw major ticks for each label position

                      IF (SciSetFont(ca.axisFont) .LT. 0) THEN
                          PlotGraph=$GEFONT
                          RETURN
                      END IF

                      ! set xp and yp based on axisPos
                      xb=$GXMIN
                      xd=1.0

                      top=ca.axisPos .EQ. $APTOP  ! top or bottom
                      IF (top) THEN
                          yb=$GYMIN
                          yd=1
                      ELSE
                          yb=$GYMAX
                          yd=-1
                      ENDIF

                      ttc=0            ! tick count
                      tval=lowVal      ! current tick value

                      maxtc=NINT((highVal-lowVal)/increment)-1
                      tlen=240.0
                      DO WHILE (ttc .LE. maxtc)
                          xp=xb+(ttc+0.5D0)*increment/(highVal-lowVal)*$GX

                          IF (ca.gridStyle .NE. $GSNONE .AND.      &
                                 ca.gridStyle .NE. $GSZERO ) THEN
                              CALL SciLine_w(ca.gridLineType,xp,   &
                                 $GYMIN,xp,$GYMAX)
                          END IF

                          SELECT CASE (ca.tickType)
                          CASE ($TTNONE)
                                      ! Do nothing
                          CASE ($TTINSIDE)
                              CALL SciLine_w($LTSOLID,xp,yb+yd*tlen,xp,yb)
                              yp=yb-yd*(tlen+144)   ! since labels still below us

                          CASE ($TTOUTSIDE)
                              CALL SciLine_w($LTSOLID,xp,yb-yd*tlen,xp,yb)
                              yp=yb-yd*(tlen+144)
                          CASE ($TTBOTH)
                              CALL SciLine_w($LTSOLID,xp,yb+yd*tlen,xp,yb-yd*tlen)
                              yp=yb-yd*(tlen+144)
                          END SELECT

                          ttc=ttc+1
                          tval=tval+increment
                      END DO

                      ! Axis label
                      retv=SciSetFont(ca.titleFont)

                      CALL SciCenter(xwpr,ywpr,$GXMIN,$GXMAX,   &
                             yp-yd*288,yp-yd*288,ca.title)

                  ELSE IF (ca.axisType .EQ. $ATX) THEN        ! numeric x-axis

                      IF (SciSetFont(ca.axisFont) .LT. 0) THEN
                          PlotGraph=$GEFONT
                          RETURN
                      END IF

                      ! set xp and yp based on axisPos
                      xb=$GXMIN
                      xd=1.0

                      top=ca.axisPos .EQ. $APTOP  ! top or bottom
                      IF (top) THEN
                          yb=$GYMIN
                          yd=1
                      ELSE
                          yb=$GYMAX
                          yd=-1
                      ENDIF

                      stc=0               ! small tick count
                      btc=0               ! big tick count
                      ttc=0               ! total tick count
                      tval=lowVal         ! current tick value to draw

                      IF (isLog) THEN
                          maxtc=(logHighVal-logLowVal+1)*numTicks
                      ELSE
                          maxtc=NINT((highVal-lowVal)/increment)
                      ENDIF

                      DO WHILE (ttc .LE. maxtc)
                          IF (stc .EQ. 0) THEN
                              smt=.FALSE.
                          ELSE
                              smt=.TRUE.
                          END IF

                          IF (isLog) THEN
                              toff=DLOG(tval)*logMul
                              xp=xb+(toff-logLowVal)/(logHighVal-logLowVal)*$GX
                          ELSE
                              xp=xb+(tval-lowVal)/(highVal-lowVal)*$GX
                          END IF

                          IF (smt) THEN
                              IF (ca.gridStyle .EQ. $GSBOTH .OR.     &
                                   ca.gridStyle .EQ. $GSZEROBOTH) THEN
                                  CALL SciLine_w(ca.gridLineType,   &
                                     xp,$GYMIN,xp,$GYMAX)
                              END IF
                          ELSE
                              IF (ttc .NE. 0 .AND. ttc .NE. maxtc) THEN
                                  IF (ca.gridStyle .NE. $GSNONE .AND.  &
                                     ca.gridStyle .NE. $GSZERO ) THEN
                                      CALL SciLine_w(ca.gridLineType,xp, &
                                         $GYMIN,xp,$GYMAX)
                                  END IF
                              END IF
                          END IF

                          IF (smt) THEN
                              tlen=80.0
                          ELSE
                              tlen=240.0
                          END IF

                          ! Now convert the current val to a string to print it
                          WRITE(fms,'(A5, I2, A1)') '(F10.',ca.numDigits,')'
                          WRITE(tempStr,fms) tval

                          SELECT CASE (ca.tickType)
                          CASE ($TTNONE)
                                      ! Do nothing
                          CASE ($TTINSIDE)
                              CALL SciLine_w($LTSOLID,xp,yb+yd*tlen,xp,yb)
                              yp=yb-yd*144

                          CASE ($TTOUTSIDE)
                              CALL SciLine_w($LTSOLID,xp,yb-yd*tlen,xp,yb)
                              yp=yb-yd*(tlen+144)
                          CASE ($TTBOTH)
                              CALL SciLine_w($LTSOLID,xp,yb+yd*tlen,xp,yb-yd*tlen)
                              yp=yb-yd*(tlen+144)
                          END SELECT

                          IF (.NOT. smt) THEN         ! write numbers on axis
                              CALL SciCenter(xwpr,ywpr,xp,xp,yp,yp,tempStr)
                          END IF

                          ttc=ttc+1
                          stc=stc+1
                          IF (stc .EQ. numTicks) THEN
                              stc=0
                              btc=btc+1
                              IF (isLog) THEN         ! reset incr
                                  tval=logBase**(btc+logLowVal)
                                  increment=(tval*logBase-tval)/numTicks
                              ELSE
                                  tval=tval+increment
                              END IF
                          ELSE
                              tval=tval+increment
                          END IF
                      END DO

                      ! Axis label
                      retv=SciSetFont(ca.titleFont)

                      CALL SciCenter(xwpr,ywpr,$GXMIN,$GXMAX,   &
                             yp-yd*288,yp-yd*288,ca.title)

                  ELSE        ! numeric y-axis

                      IF (SciSetFont(ca.axisFont) .LT. 0) THEN
                          PlotGraph=$GEFONT
                          RETURN
                      END IF

                      ! set xp and yp based on axisPos
                      yb=$GYMAX
                      yd=1

                      left=ca.axisPos .EQ. $APLEFT  ! left or right
                      IF (left) THEN
                          xb=$GXMIN
                          xd=1
                      ELSE
                          xb=$GXMAX
                          xd=-1
                      ENDIF

                      stc=0               ! small tick count
                      btc=0               ! big tick count
                      ttc=0               ! total tick count
                      tval=lowVal         ! current tick value to draw

                      IF (isLog) THEN
                          maxtc=(logHighVal-logLowVal)*numTicks
                      ELSE
                          maxtc=NINT((highVal-lowVal)/increment)
                      ENDIF

                      DO WHILE (ttc .LE. maxtc)
                          IF (stc .EQ. 0) THEN
                              smt=.FALSE.
                          ELSE
                              smt=.TRUE.
                          END IF

                          IF (isLog) THEN
                              toff=DLOG(tval)*logMul
                              yp=yb-(toff-logLowVal)/(logHighVal-logLowVal)*$GY
                          ELSE
                              yp=yb-(tval-lowVal)/(highVal-lowVal)*$GY
                          END IF

                          IF (smt) THEN
                              IF (ca.gridStyle .EQ. $GSBOTH .OR.     &
                                   ca.gridStyle .EQ. $GSZEROBOTH) THEN
                                  CALL SciLine_w(ca.gridLineType,  &
                                         $GXMIN,yp,$GXMAX,yp)
                              END IF
                          ELSE
                              IF (ttc .NE. 0 .AND. ttc .NE. maxtc) THEN
                                  IF (ca.gridStyle .NE. $GSNONE .AND.   &
                                     ca.gridStyle .NE. $GSZERO ) THEN
                                      CALL SciLine_w(ca.gridLineType, &
                                            $GXMIN,yp,$GXMAX,yp)
                                  END IF
                              END IF
                          END IF

                          IF (smt) THEN
                              tlen=80.0
                          ELSE
                              tlen=240.0
                          END IF

                          ! Now convert the current val to a string to print it
                          WRITE(fms,'(A5, I2, A1)') '(F10.',ca.numDigits,')'
                          WRITE(tempStr,fms) tval

                          SELECT CASE (ca.tickType)
                          CASE ($TTNONE)
                                      ! Do nothing
                          CASE ($TTINSIDE)
                              CALL SciLine_w($LTSOLID,xb,yp,xb+xd*tlen,yp)
                              xp=xb-xd*64
                          CASE ($TTOUTSIDE)
                              CALL SciLine_w($LTSOLID,xb,yp,xb-xd*tlen,yp)
                              xp=xb-xd*(tlen+64)
                          CASE ($TTBOTH)
                              CALL SciLine_w($LTSOLID,xb-xd*tlen,yp,xb+xd*tlen,yp)
                              xp=xb-xd*(tlen+64)
                          END SELECT

                          IF (.NOT. smt) THEN             ! write numbers on axis
                              IF (ca.AxisFontRotated) THEN
                                   CALL SciRotEndCenter(xwpr,ywpr, &
                                                 xp,left,yp,yp,tempStr)
                              ELSE
                                  CALL SciEndCenter(xwpr,ywpr,xp, &
                                            left,yp,yp,tempStr)
                              END IF
                          END IF

                          ttc=ttc+1
                          stc=stc+1
                          IF (stc .EQ. numTicks) THEN
                              stc=0
                              btc=btc+1
                              IF (isLog) THEN         ! reset incr
                                  tval=logBase**(btc+logLowVal)
                                  increment=(tval*logBase-tval)/numTicks
                              ELSE
                                  tval=tval+increment
                              END IF
                          ELSE
                              tval=tval+increment
                          END IF
                      END DO

                      ! Axis label
                      retv=SciSetFont(ca.titleFont)

                      IF (ca.AxisFontRotated) THEN
                          CALL SciRotCenter(xwpr,ywpr,xp-xd*352,    &
                                 xp-xd*352,$GYMIN,$GYMAX,ca.title)
                      ELSE
                          CALL SciRotCenter(xwpr,ywpr,xp-xd*704,    &
                                 xp-xd*704,$GYMIN,$GYMAX,ca.title)
                      END IF
                  END IF
              END DO

          ELSE
!             Draw polar graph

              IF (numAxes .NE. 2) THEN    ! only allow 2 axes exactly
                  PlotGraph=$GEAXIS
                  RETURN
              END IF

              IF (.NOT. ( ( AxisArray(1).axisType .EQ. $ATTHETA .AND. &
                     AxisArray(2).axisType .EQ. $ATR ) .OR.           &
                     ( AxisArray(1).axisType .EQ. $ATR .AND.          &
                     AxisArray(2).axisType .EQ. $ATTHETA ) ) ) THEN
                  PlotGraph=$GEAXIS       ! two of the same or some invalid
                  RETURN
              END IF


              IF (AxisArray(1).axisType .EQ. $ATR) THEN
                  ra=AxisArray(1)
                  ta=AxisArray(2)
              ELSE
                  ra=AxisArray(2)
                  ta=AxisArray(1)
              END IF

              IF (ta.gridStyle .EQ. $GSZERO  .OR.          &
                     ta.gridStyle .EQ. $GSZEROBOTH  .OR.   &
                     ra.gridStyle .EQ. $GSZERO .OR.        &
                     ra.gridStyle .EQ. $GSZEROBOTH) THEN
                  PlotGraph=$GEAXIS       ! don't allow zero lines on polar
                  RETURN
              END IF

              xc=$GXC
              yc=$GYC
              r=$PGR

              CALL SciLine_w($LTSOLID,xc-r,yc,xc+r,yc)
              CALL SciLine_w($LTSOLID,xc,yc-r,xc,yc+r)

!             First draw the R-axes
              ca=ra
              ! draw x part of r-axes
              ! r axis ignores axispos and axiscw, does not support log

              lowVal=ca.lowVal
              highVal=ca.highVal
              numTicks=ca.tickRatio
              increment=ca.increment/numTicks

              IF (SciSetFont(ca.axisFont) .LT. 0) THEN
                  PlotGraph=$GEFONT
                  RETURN
              END IF

              ! set xp and yp based on axisPos
              xb=$GXC
              xd=1.0

              yb=$GYC

              top=ca.axisPos .EQ. $APTOP  ! top or bottom
              IF (top) THEN
                  yd=1
              ELSE
                  yd=-1
              ENDIF

              stc=0               ! small tick count
              btc=0               ! big tick count
              ttc=0               ! total tick count
              tval=lowVal         ! current tick value to draw

              maxtc=NINT((highVal-lowVal)/increment)

              DO WHILE (ttc .LE. maxtc)
                  IF (stc .EQ. 0) THEN
                      smt=.FALSE.
                  ELSE
                      smt=.TRUE.
                  END IF

                  xo=(tval-lowVal)/(highVal-lowVal)*r

                  IF (smt) THEN
                      tlen=80.0
                  ELSE
                      tlen=240.0
                  END IF

                  ! Now convert the current val to a string to print it
                  WRITE(fms,'(A5, I2, A1)') '(F10.',ca.numDigits,')'
                  WRITE(tempStr,fms) tval

                  SELECT CASE (ca.tickType)
                  CASE ($TTNONE)
                              ! Do nothing
                  CASE ($TTINSIDE)
                      CALL SciLine_w($LTSOLID,xb+xo,yb+yd*tlen,xb+xo,yb)
                      CALL SciLine_w($LTSOLID,xb-xo,yb+yd*tlen,xb-xo,yb)
                      yp=yb-yd*144

                  CASE ($TTOUTSIDE)
                      CALL SciLine_w($LTSOLID,xb+xo,yb-yd*tlen,xb+xo,yb)
                      CALL SciLine_w($LTSOLID,xb-xo,yb-yd*tlen,xb-xo,yb)
                      yp=yb-yd*(tlen+144)
                  CASE ($TTBOTH)
                      CALL SciLine_w($LTSOLID,xb+xo,yb+yd*tlen,xb+xo,yb-yd*tlen)
                      CALL SciLine_w($LTSOLID,xb-xo,yb+yd*tlen,xb-xo,yb-yd*tlen)
                      yp=yb-yd*(tlen+144)
                  END SELECT

                  IF (.NOT. smt) THEN         ! write numbers on axis
                      CALL SciCenter(xwpr,ywpr,xb+xo,xb+xo,yp,yp,tempStr)
                  END IF

                  ttc=ttc+1
                  stc=stc+1
                  IF (stc .EQ. numTicks) THEN
                      stc=0
                      btc=btc+1
                      tval=tval+increment
                  ELSE
                      tval=tval+increment
                  END IF
              END DO

              ! Axis label
              retv=SciSetFont(ca.titleFont)

              CALL SciCenter(xwpr,ywpr,$GXC,$GXC+r,yp-yd*288,yp-yd*288,ca.title)

              ! draw y part of r-axis

              IF (SciSetFont(ca.axisFont) .LT. 0) THEN
                  PlotGraph=$GEFONT
                  RETURN
              END IF

              ! set xp and yp based on axisPos
              xb=$GXC

              left=ca.axisPos .EQ. $APTOP  ! left or right
              IF (left) THEN
                  xd=1
              ELSE
                  xd=-1
              ENDIF

              yb=$GYC
              yd=1

              stc=0               ! small tick count
              btc=0               ! big tick count
              ttc=0               ! total tick count
              tval=lowVal         ! current tick value to draw

              maxtc=NINT((highVal-lowVal)/increment)

              DO WHILE (ttc .LE. maxtc)
                  IF (stc .EQ. 0) THEN
                      smt=.FALSE.
                  ELSE
                      smt=.TRUE.
                  END IF

                  yo=(tval-lowVal)/(highVal-lowVal)*r

                  IF (smt) THEN
                      tlen=80.0
                  ELSE
                      tlen=240.0
                  END IF


                  SELECT CASE (ca.tickType)
                  CASE ($TTNONE)
                              ! Do nothing
                  CASE ($TTINSIDE)
                      CALL SciLine_w($LTSOLID,xb,yb+yo,xb+xd*tlen,yb+yo)
                      CALL SciLine_w($LTSOLID,xb,yb-yo,xb+xd*tlen,yb-yo)
                      xp=xb-xd*64
                  CASE ($TTOUTSIDE)
                      CALL SciLine_w($LTSOLID,xb,yb+yo,xb-xd*tlen,yb+yo)
                      CALL SciLine_w($LTSOLID,xb,yb-yo,xb-xd*tlen,yb-yo)
                      xp=xb-xd*(tlen+64)
                  CASE ($TTBOTH)
                      CALL SciLine_w($LTSOLID,xb-xd*tlen,yb+yo,xb+xd*tlen,yb+yo)
                      CALL SciLine_w($LTSOLID,xb-xd*tlen,yb-yo,xb+xd*tlen,yb-yo)
                      xp=xb-xd*(tlen+64)
                  END SELECT

                  ttc=ttc+1
                  stc=stc+1
                  IF (stc .EQ. numTicks) THEN
                      stc=0
                      btc=btc+1
                      tval=tval+increment
                  ELSE
                      tval=tval+increment
                  END IF
              END DO


!             Now draw the theta axis
              ca=ta

              IF (SciSetFont(ca.axisFont) .LT. 0) THEN
                  PlotGraph=$GEFONT
                  RETURN
              END IF


              lowVal=ca.lowVal
              highVal=ca.highVal
              numTicks=ca.tickRatio
              increment=ca.increment/numTicks

              IF (SciSetFont(ca.axisFont) .LT. 0) THEN
                  PlotGraph=$GEFONT
                  RETURN
              END IF

              ! set xp and yp based on axisPos
              xb=$GXC
              xd=1.0

              yb=$GYC
              yd=1

              ! set tadd based on axisPos
              SELECT CASE (ca.axisPos)
              CASE ($APRIGHT)
                  tadd=0.0
              CASE ($APTOP)
                  tadd=$PI/2.0
              CASE ($APLEFT)
                  tadd=$PI
              CASE ($APBOTTOM)
                  tadd=3.0*$PI/2.0
              END SELECT

              stc=0               ! small tick count
              btc=0               ! big tick count
              ttc=0               ! total tick count
              tval=lowVal         ! current tick value to draw
              tanginc=2.0D0*$PI/( (DBLE(highVal)-DBLE(lowVal))/DBLE(increment) )

              IF (.NOT. ca.axisCW) THEN ! go the other way around the circle
                  tanginc=-tanginc
                  tadd=-tadd
              END IF
              tang=tadd

              maxtc=NINT((highVal-lowVal)/increment)

              DO WHILE (ttc .LT. maxtc)
                  IF (stc .EQ. 0) THEN
                      smt=.FALSE.
                  ELSE
                      smt=.TRUE.
                  END IF

                  IF (smt) THEN
                      tlen=80.0
                  ELSE
                      tlen=240.0
                  END IF

                  ctang=DCOS(tang)
                  stang=DSIN(tang)

                  IF (DABS(ctang) .LT. $EPREAL8) THEN     ! near miss should be 0
                      ctang=0.0D0
                  END IF
                  IF (DABS(stang) .LT. $EPREAL8) THEN     ! near miss should be 0
                      stang=0.0D0
                  END IF

                  IF (1-DABS(ctang) .LT. $EPREAL8) THEN    ! near miss should be 1
                      ctang=SIGN(1.0D0,ctang)
                  END IF
                  IF (1-DABS(stang) .LT. $EPREAL8) THEN    ! near miss should be 1
                      stang=SIGN(1.0D0,stang)
                  END IF

                  x0=xc+(r-tlen)*ctang
                  y0=yc+(r-tlen)*stang

                  x1=xc+(r)*ctang
                  y1=yc+(r)*stang

                  x2=xc+(r+tlen)*ctang
                  y2=yc+(r+tlen)*stang

                  x3=xc+(r+2.0*tlen)*ctang
                  y3=yc+(r+2.0*tlen)*stang

                  IF (smt) THEN
                      IF (ca.gridStyle .EQ. $GSBOTH .OR.  &
                                ca.gridStyle .EQ. $GSZEROBOTH) THEN
                          CALL SciLine_w(ca.gridLineType,xc,yc,x1,y1)
                      END IF
                  ELSE
                      IF (ttc .NE. maxtc) THEN
                          IF (ca.gridStyle .NE. $GSNONE .AND.   &
                                  ca.gridStyle .NE. $GSZERO ) THEN
                              CALL SciLine_w(ca.gridLineType,xc,yc,x1,y1)
                          END IF
                      END IF
                  END IF


                  ! Now convert the current val to a string to print it
                  WRITE(fms,'(A5, I2, A1)') '(F10.',ca.numDigits,')'
                  WRITE(tempStr,fms) tval

                  SELECT CASE (ca.tickType)
                  CASE ($TTNONE)
                              ! Do nothing
                  CASE ($TTINSIDE)
                      CALL SciLine_w($LTSOLID,x0,y0,x1,y1)
                      xp=x2
                      yp=y2
                  CASE ($TTOUTSIDE)
                      CALL SciLine_w($LTSOLID,x1,y1,x2,y2)
                      xp=x3
                      yp=y3
                  CASE ($TTBOTH)
                      CALL SciLine_w($LTSOLID,x0,y0,x2,y2)
                      xp=x3
                      yp=y3
                  END SELECT

                  IF (.NOT. smt) THEN         ! write numbers on axis
                      CALL SciCenter(xwpr,ywpr,xp,xp,yp,yp,tempStr)
                  END IF

                  ttc=ttc+1
                  stc=stc+1
                  tang=tang+tanginc
                  IF (stc .EQ. numTicks) THEN
                      stc=0
                      btc=btc+1
                      tang=tadd+DBLE(ttc)*tanginc
                      tval=tval+increment
                  ELSE
                      tval=tval+increment
                  END IF
              END DO


          END IF

          pgraph.didPlotGraph=pgraph.didPlotGraph+1
          pgraph.numSets=pgraph.numSets+numSettings

          graph=cgraph
          PlotGraph=$GEOK
          RETURN
      END FUNCTION


!   Plots a numeric data set.  Must happen after PlotGraph

      INTEGER FUNCTION PlotData(graph,data,dSettings, axis1,axis2)

          RECORD /GraphSettings/   graph         ! input
          REAL*4                   data(1)       ! input: actual data
          RECORD /DataSettings/    dSettings     ! input
          RECORD /AxisSettings/    axis1         ! input
          RECORD /AxisSettings/    axis2         ! input

          PlotData = PlotMultiData(graph,data,1,(/dSettings/),axis1,axis2)
      END FUNCTION

!   Plots multiple numeric data sets.  Must happen after PlotGraph

      INTEGER FUNCTION PlotMultiData(graph,data,         &
                                         numSettings,    &
                                         dSettingsArray, &
                                         axis1,axis2)

          RECORD /GraphSettings/   graph          ! input
          REAL*4                   data(1)        ! input: actual data
          INTEGER                  numSettings    ! input
          RECORD /DataSettings/    dSettingsArray(numSettings)    ! input
          RECORD /AxisSettings/    axis1          ! input
          RECORD /AxisSettings/    axis2          ! input


          RECORD /PrivateGraphSettings/ pgraph   ! private graph
          RECORD /GraphSettings/ cgraph          ! copy of private graph
          EQUIVALENCE(cgraph,pgraph)

          DOUBLE PRECISION x1,x2,y1,y2,ngxw,ngyw,xwidth,ywidth
          DOUBLE PRECISION xLogBase,xLogMul,yLogBase,yLogMul
          DOUBLE PRECISION xPolarMul,yPolarMul
          DOUBLE PRECISION xLogLowVal,xLogHighVal,yLogLowVal,yLogHighVal
          DOUBLE PRECISION r,th,tadd
          INTEGER retv
          RECORD /AxisSettings/ xa,ya
          INTEGER plotSets,checkSets,plotPoints,drawLegends
          LOGICAL first,errBars,polar
          REAL*4 xdata,ydata,ulydata,edata,edataLow,edataHigh
          REAL*4 lxdata,lydata
          REAL*4 xwm,ywm
          REAL*4 xwpr,ywpr

          cgraph=graph
          xa=axis1
          ya=axis2

          IF (pgraph.didPlotGraph .LE. 0) THEN
              PlotMultiData=$GENOPLOTGRAPH
              RETURN
          END IF


          IF (SciSetFont(graph.titleFont) .LE. 0) THEN
              PlotMultiData=$GEFONT
              RETURN
          END IF

          DO checkSets = 1,numSettings
              IF (dSettingsArray(checkSets).numElements .LT. 2  .OR.    &
                     dSettingsArray(checkSets).numElements .GT. 3  .OR. &
                     dSettingsArray(checkSets).numPoints .LT. 1) THEN
                  PlotMultiData=$GEDIMS
                  RETURN
              END IF
          END DO


          IF (xa.axisFunc .NE. $AFLINEAR) THEN
              SELECT CASE (xa.axisFunc)
              CASE ($AFLOG10)
                  xLogBase=10.0
              CASE ($AFLOG)
                  xLogBase=EXP(1.0)
              CASE ($AFLOG2)
                  xLogBase=2.0
              END SELECT
              xLogMul=1.0D0/DLOG(xLogBase)
          END IF

          IF (ya.axisFunc .NE. $AFLINEAR) THEN
              SELECT CASE (ya.axisFunc)
              CASE ($AFLOG10)
                  yLogBase=10.0
              CASE ($AFLOG)
                  ylogBase=EXP(1.0)
              CASE ($AFLOG2)
                  ylogBase=2.0
              END SELECT
              yLogMul=1.0D0/DLOG(ylogBase)
          END IF

          IF (cgraph.graphType .EQ. $GTPOLAR) THEN
              polar=.TRUE.
          ELSE
              polar=.FALSE.
          ENDIF

          IF (xa.axisFunc .NE. $AFLINEAR) THEN
              xLogLowVal=NINT(DLOG(xa.lowVal)*xLogMul)
              xLogHighVal=NINT(DLOG(xa.highVal)*xLogMul)

              xwidth=(xLogHighVal-xLogLowVal)*$WX/$GX  ! how wide in graphing units
              ngxw=xwidth-(xLogHighVal-xLogLowVal)
              x1=xLogLowVal-ngxw*($WXMAX-$GXMAX)/($WX-$GX)
              x2=xLogHighVal+ngxw*($GXMIN-$WXMIN)/($WX-$GX)
          ELSE
              xwidth=(xa.highVal-xa.lowVal)*$WX/$GX  ! how wide in graphing units
              ngxw=xwidth-(xa.highVal-xa.lowVal)
              x1=xa.lowVal-ngxw*($WXMAX-$GXMAX)/($WX-$GX)
              x2=xa.highVal+ngxw*($GXMIN-$WXMIN)/($WX-$GX)
          END IF

          IF (ya.axisFunc .NE. $AFLINEAR) THEN
              yLogLowVal=NINT(DLOG(ya.lowVal)*yLogMul)
              yLogHighVal=NINT(DLOG(ya.highVal)*yLogMul)

              ywidth=(yLogHighVal-yLogLowVal)*$WY/$GY  ! how wide screen in graph units
              ngyw=ywidth-(yLogHighVal-yLogLowVal)     ! how much not in graphing region
              y1=yLogLowVal-ngyw*($WYMAX-$GYMAX)/($WY-$GY)
              y2=yLogHighVal+ngyw*($GYMIN-$WYMIN)/($WY-$GY)
          ELSE
              ywidth=(ya.highVal-ya.lowVal)*$WY/$GY  ! how wide screen in graph units
              ngyw=ywidth-(ya.highVal-ya.lowVal)     ! how much not in graphing region
              y1=ya.lowVal-ngyw*($WYMAX-$GYMAX)/($WY-$GY)
              y2=ya.highVal+ngyw*($GYMIN-$WYMIN)/($WY-$GY)
          END IF

          IF (polar) THEN
              xPolarMul=$PGR/(xa.highVal-xa.lowVal)
              yPolarMul=2.0*$PI/(ya.highVal-ya.lowVal)
          END IF

          xwm=$WX/(x2-x1)
          ywm=$WY/(y2-y1)

          xwpr=$WX/(cgraph.x2-cgraph.x1+1)
          ywpr=$WY/(cgraph.y2-cgraph.y1+1)

          ! set tadd based on axisPos
          SELECT CASE (ya.axisPos)
          CASE ($APRIGHT)
              tadd=0.0
          CASE ($APTOP)
              tadd=$PI/2.0
          CASE ($APLEFT)
              tadd=$PI
          CASE ($APBOTTOM)
              tadd=3.0*$PI/2.0
          END SELECT

          IF (.NOT. ya.axisCW) THEN ! go the other way around the circle
              yPolarMul=-yPolarMul
              tadd=-tadd
          END IF

          ! draw lines first
          DO plotSets = 1,numSettings

              first=.TRUE.

              IF (dSettingsArray(plotSets).lineType .NE. $LTNONE) THEN

                  DO plotPoints=1,dSettingsArray(plotSets).numPoints
                      xdata=data(SciGetDataOffset(dSettingsArray(plotSets), &
                             plotSets,plotPoints,1) )
                      ydata=data(SciGetDataOffset(dSettingsArray(plotSets), &
                             plotSets,plotPoints,2) )
                      IF (xa.axisFunc .NE. $AFLINEAR) THEN
                          xdata=LOG(xdata)*xLogMul
                      END IF
                      IF (ya.axisFunc .NE. $AFLINEAR) THEN
                          ydata=LOG(ydata)*yLogMul
                      END IF

                      IF (polar) THEN
                          r=xdata*xPolarMul
                          th=ydata*yPolarMul +tadd
                          xdata=$GXC+r*COS(th)
                          ydata=$GYC+r*SIN(th)
                      ELSE
                          xdata=$WXMIN+(xdata-x1)*xwm
                          ydata=$WYMAX-(ydata-y1)*ywm
                      END IF

                      IF (.NOT. first) THEN
                          CALL SciLine_w(dSettingsArray(plotSets).lineType, &
                             DBLE(lxdata),DBLE(lydata),                     &
                             DBLE(xdata),DBLE(ydata))
                      ELSE
                          first=.FALSE.
                      END IF

                      lxdata=xdata    ! store current point for next time through
                      lydata=ydata
                  END DO
              END IF
          END DO

          ! draw point markers with error bars if desired
          DO plotSets = 1,numSettings
              errBars= cgraph.graphType .EQ. $GTXYWERRBAR   &
                     .OR. cgraph.graphType .EQ. $GTLINEWERRBAR

              IF (dSettingsArray(plotSets).markerType .EQ. $MKNONE) THEN
                  CYCLE           ! no need to do the rest of this
              END IF

              DO plotPoints=1,dSettingsArray(plotSets).numPoints
                  xdata=data(SciGetDataOffset(dSettingsArray(plotSets), &
                     plotSets,plotPoints,1))
                  ydata=data(SciGetDataOffset(dSettingsArray(plotSets), &
                     plotSets,plotPoints,2))

                  IF (xa.axisFunc .NE. $AFLINEAR) THEN
                      xdata=LOG(xdata)*xLogMul
                  END IF
                  IF (ya.axisFunc .NE. $AFLINEAR) THEN
                      ulydata=ydata
                      ydata=LOG(ydata)*yLogMul
                  END IF

                  IF (errBars) THEN
                      edata=data(SciGetDataOffset(  &
                         dSettingsArray(plotSets),  &
                         plotSets,plotPoints,3))
                      IF (ya.axisFunc .NE. $AFLINEAR) THEN
                          edataLow=LOG(ulydata-edata)*yLogMul
                          edataHigh=LOG(ulydata+edata)*yLogMul
                      ELSE
                          edataLow=ydata-edata
                          edataHigh=ydata+edata
                      END IF

                      xdata=$WXMIN+(xdata-x1)*xwm
                      ydata=$WYMAX-(ydata-y1)*ywm
                      edataLow=$WYMAX-(edataLow-y1)*ywm
                      edataHigh=$WYMAX-(edataHigh-y1)*ywm

                      CALL SciDrawErrorBars(xdata,ydata,          &
                          edataLow,edataHigh,                     &
                          dSettingsArray(plotSets).errorbarType,  &
                          dSettingsArray(plotSets).errorbarColor)
                  ELSE
                      IF (polar) THEN
                          r=xdata*xPolarMul
                          th=ydata*yPolarMul+tadd
                          xdata=$GXC+r*COS(th)
                          ydata=$GYC+r*SIN(th)
                      ELSE
                          xdata=$WXMIN+(xdata-x1)*xwm
                          ydata=$WYMAX-(ydata-y1)*ywm
                      END IF
                  END IF

                  CALL SciDrawMarker(xdata,ydata,           &
                     dSettingsArray(plotSets).markerType,   &
                     dSettingsArray(plotSets).markerColor)

              END DO
          END DO


          DO drawLegends= 1,numSettings
              pgraph.numSetsDone=pgraph.numSetsDone+1

              IF (pgraph.numSetsDone .GT. pgraph.numSets) THEN
                  PlotMultiData=$GEGRAPH
                  RETURN
              END IF

              IF (SciSetFont(dSettingsArray(drawLegends).titleFont).LE. 0) THEN
                  PlotMultiData=$GEFONT
                  RETURN
              END IF

              CALL SciDrawLegend(xwpr,ywpr,                   &
                     pgraph.numSetsDone,pgraph.numSets,       &
                     dSettingsArray(drawLegends).lineType,    &
                     dSettingsArray(drawLegends).lineColor,   &
                     dSettingsArray(drawLegends).markerType,  &
                     dSettingsArray(drawLegends).markerColor, &
                     dSettingsArray(drawLegends).barType,     &
                     dSettingsArray(drawLegends).barColor,    &
                     dSettingsArray(drawLegends).titleColor,  &
                     dSettingsArray(drawLegends).title,       &
                     graph.graphColor)
          END DO

          graph=cgraph
          PlotMultiData=$GEOK
          RETURN
      END FUNCTION


!   Plots a single label/numeric data sets.  Must happen after PlotGraph

      INTEGER FUNCTION PlotLabelData(graph,labels,data,dSettings,axis1,axis2)

          RECORD /GraphSettings/   graph         ! input
          CHARACTER*(*)            labels(1)     ! input
          REAL*4                   data(1)       ! input: actual data
          RECORD /DataSettings/    dSettings     ! input
          RECORD /AxisSettings/    axis1         ! input
          RECORD /AxisSettings/    axis2         ! input

          PlotLabelData=PlotLabelMultiData(graph,labels,data, &
                 1,(/dSettings/),axis1,axis2)
          RETURN
      END FUNCTION


!   Plots multiple label/numeric data sets.  Must happen after PlotGraph

      INTEGER FUNCTION PlotLabelMultiData(graph,labels,data,          &
                                         numSettings,dSettingsArray,  &
                                         axis1,axis2)

          RECORD /GraphSettings/   graph         ! input
          INTEGER                  numSettings   ! input
          RECORD /DataSettings/    dSettingsArray(numSettings)    ! input
          CHARACTER*(*)            labels(1)     ! input
          REAL*4                   data(1)       ! input: actual data
          RECORD /AxisSettings/    axis1         ! input
          RECORD /AxisSettings/    axis2         ! input

          RECORD /PrivateGraphSettings/ pgraph   ! private graph
          RECORD /GraphSettings/ cgraph          ! copy of private graph
          EQUIVALENCE(cgraph,pgraph)

          DOUBLE PRECISION x1,x2,y1,y2,ngxw,ngyw,xwidth,ywidth
          DOUBLE PRECISION yLogBase,yLogMul
          DOUBLE PRECISION yLogLowVal,yLogHighVal
          INTEGER retv
          RECORD /AxisSettings/ xa,ya
          INTEGER checkSet,plotGroup,plotSets,yd
          INTEGER drawLegends
          LOGICAL bar,first,errBars
          REAL*4 ydata,ulydata,edata,edataLow,edataHigh,lxdata,lydata
          REAL*4 barWidth,tickSpacing,xb,xp,yb
          REAL*4 xwm,ywm
          REAL*4 xwpr,ywpr

          cgraph=graph
          xa=axis1
          ya=axis2

          IF (numSettings .LT. 1) THEN
              PlotLabelMultiData=$GEPARAMS
              RETURN
          END IF

          IF (pgraph.didPlotGraph .LE. 0) THEN
              PlotLabelMultiData=$GENOPLOTGRAPH
              RETURN
          END IF


          IF (SciSetFont(graph.titleFont) .LE. 0) THEN
              PlotLabelMultiData=$GEFONT
              RETURN
          END IF

          DO checkSet = 1,numSettings      ! check the bounds settings
              IF (dSettingsArray(checkSet).numElements .LT. 1  .OR.       &
                     dSettingsArray(checkSet) .numElements .GT. 3  .OR.   &
                     dSettingsArray(checkSet) .numPoints .LT. 1) THEN
                  PlotLabelMultiData=$GEDIMS
                  RETURN
              END IF
          END DO

          IF (xa.axisFunc .NE. $AFLINEAR) THEN
              PlotLabelMultiData=$GEAXIS
              RETURN
          END IF

          IF (ya.axisFunc .NE. $AFLINEAR) THEN
              SELECT CASE (ya.axisFunc)
              CASE ($AFLOG10)
                  yLogBase=10.0
              CASE ($AFLOG)
                  ylogBase=EXP(1.0)
              CASE ($AFLOG2)
                  ylogBase=2.0
              END SELECT
              yLogMul=1.0D0/DLOG(ylogBase)
          END IF

          IF (xa.lowVal .NE. 0  .OR. xa.increment .NE. 1 .OR.    &
             xa.highVal .NE. dSettingsArray(1).numPoints) THEN
              PlotLabelMultiData=$GEAXIS
              RETURN
          END IF

          xwidth=(xa.highVal-xa.lowVal)*$WX/$GX  ! how wide in graphing units
          ngxw=xwidth-(xa.highVal-xa.lowVal)
          x1=xa.lowVal-ngxw*($WXMAX-$GXMAX)/($WX-$GX)
          x2=xa.highVal+ngxw*($GXMIN-$WXMIN)/($WX-$GX)

          IF (ya.axisFunc .NE. $AFLINEAR) THEN
              yLogLowVal=NINT(DLOG(ya.lowVal)*yLogMul)
              yLogHighVal=NINT(DLOG(ya.highVal)*yLogMul)

              ywidth=(yLogHighVal-yLogLowVal)*$WY/$GY  ! how wide screen in graph units
              ngyw=ywidth-(yLogHighVal-yLogLowVal)     ! how much not in graphing region
              y1=yLogLowVal-ngyw*($WYMAX-$GYMAX)/($WY-$GY)
              y2=yLogHighVal+ngyw*($GYMIN-$WYMIN)/($WY-$GY)
          ELSE
              ywidth=(ya.highVal-ya.lowVal)*$WY/$GY  ! how wide screen in graph units
              ngyw=ywidth-(ya.highVal-ya.lowVal)     ! how much not in graphing region
              y1=ya.lowVal-ngyw*($WYMAX-$GYMAX)/($WY-$GY)
              y2=ya.highVal+ngyw*($GYMIN-$WYMIN)/($WY-$GY)
          END IF

          xwm=$WX/(x2-x1)
          ywm=$WY/(y2-y1)

          xwpr=$WX/(cgraph.x2-cgraph.x1+1)
          ywpr=$WY/(cgraph.y2-cgraph.y1+1)

          bar=cgraph.graphType .EQ. $GTBAR

          IF (bar) THEN       ! bars can only be in bar graphs
              barWidth=1.0D0/(numSettings+0.5D0) ! leave room for spacing

              IF (ya.lowVal .LT. 0.0D0) THEN
                  yb=REAL($WYMAX-(0.0-y1)*ywm)
              ELSE
                  yb=$GYMAX
              END IF

              ! Draw the bars
              DO plotGroup = 1,dSettingsArray(1).numPoints
                  xb=(plotGroup-1)+barWidth*0.25D0
                  DO plotSets = 1,numSettings
                      xp=xb+(plotSets-1)*barWidth
                      ydata=data(SciGetDataOffset(   &
                         dSettingsArray(plotSets),   &
                         plotSets,plotGroup,1))
                      IF (ya.axisFunc .NE. $AFLINEAR) THEN
                          ydata=LOG(ydata)*yLogMul
                      END IF

                      xp=$WXMIN+(xp-x1)*xwm
                      ydata=$WYMAX-(ydata-y1)*ywm-yb

                      CALL SciDrawBar(xp,yb,                &
                         barWidth*xwm-16,ydata,             &
                         dSettingsArray(plotSets).barType,  &
                         dSettingsArray(plotSets).barColor)
                  END DO
              END DO
          END IF  ! bars

          IF (.NOT. bar) THEN   ! lines, markers, error bars only on a line graph
              tickSpacing=1.0D0

              !   Draw the lines
              DO plotSets = 1,numSettings
                  first=.TRUE.
                  DO plotGroup = 1,dSettingsArray(plotSets).numPoints
                      xb=(plotGroup-1)+tickSpacing*0.50D0
                      ydata=data(SciGetDataOffset(  &
                         dSettingsArray(plotSets),  &
                         plotSets,plotGroup,1))
                      IF (ya.axisFunc .NE. $AFLINEAR) THEN
                          ydata=LOG(ydata)*yLogMul
                      END IF

                      xb=$WXMIN+(xb-x1)*xwm
                      ydata=$WYMAX-(ydata-y1)*ywm

                      IF (.NOT. first) THEN
                          CALL SciLine_w(                        &
                             dSettingsArray(plotSets).lineType,  &
                             DBLE(lxdata),DBLE(lydata),          &
                             DBLE(xb),DBLE(ydata))
                      ELSE
                          first=.FALSE.
                      END IF
                      lxdata=xb       ! store current point for next time through
                      lydata=ydata
                  END DO
              END DO

              errBars= cgraph.graphType .EQ. $GTLINEWERRBAR

              !     Draw the markers and error bars
              DO plotSets = 1,numSettings
                  DO plotGroup = 1,dSettingsArray(plotSets).numPoints
                      xb=(plotGroup-1)+tickSpacing*0.50D0
                      ydata=data(SciGetDataOffset(   &
                         dSettingsArray(plotSets),   &
                         plotSets,plotGroup,1))
                      IF (ya.axisFunc .NE. $AFLINEAR) THEN
                          ulydata=ydata
                          ydata=LOG(ydata)*yLogMul
                      END IF

                      IF (errBars) THEN
                          edata=data(SciGetDataOffset(  &
                             dSettingsArray(plotSets),  &
                             plotSets,plotGroup,2))
                          IF (ya.axisFunc .NE. $AFLINEAR) THEN
                              edataLow=LOG(ulydata-edata)*yLogMul
                              edataHigh=LOG(ulydata+edata)*yLogMul
                          ELSE
                              edataLow=ydata-edata
                              edataHigh=ydata+edata
                          END IF

                          xb=$WXMIN+(xb-x1)*xwm
                          ydata=$WYMAX-(ydata-y1)*ywm
                          edataLow=$WYMAX-(edataLow-y1)*ywm
                          edataHigh=$WYMAX-(edataHigh-y1)*ywm

                          CALL SciDrawErrorBars(xb,ydata,            &
                             edataLow,edataHigh,                     &
                             dSettingsArray(plotSets).errorbarType,  &
                             dSettingsArray(plotSets).errorbarColor)
                      ELSE
                          xb=$WXMIN+(xb-x1)*xwm
                          ydata=$WYMAX-(ydata-y1)*ywm
                      END IF

                      CALL SciDrawMarker(xb,ydata,             &
                         dSettingsArray(plotSets).markerType,  &
                         dSettingsArray(plotSets).markerColor)
                  END DO
              END DO
          END IF

          ! Put up labels
          tickSpacing=1.0D0
          IF (SciSetFont(xa.axisFont) .LE. 0) THEN
              PlotLabelMultiData=$GEFONT
              RETURN
          END IF

          IF (xa.axisPos .EQ. $APTOP) THEN
              yb=$GYMIN
              yd=-1
          ELSE
              yb=$GYMAX
              yd=1
          END IF

          DO plotGroup = 1,dSettingsArray(1).numPoints
              xb=(plotGroup-1)+tickSpacing*0.50D0
              xb=$WXMIN+(xb-x1)*xwm
              CALL SciCenter(xwpr,ywpr,DBLE(xb),DBLE(xb),  &
                     DBLE(yb+yd*342),DBLE(yb+yd*342),      &
                     labels(plotGroup))                    ! print range labels
          END DO


          DO drawLegends= 1,numSettings
              pgraph.numSetsDone=pgraph.numSetsDone+1

              IF (pgraph.numSetsDone .GT. pgraph.numSets) THEN
                  PlotLabelMultiData=$GEGRAPH
                  RETURN
              END IF

              IF (SciSetFont(dSettingsArray(drawLegends).titleFont).LE. 0) THEN
                  PlotLabelMultiData=$GEFONT
                  RETURN
              END IF

              CALL SciDrawLegend(xwpr,ywpr,                   &
                     pgraph.numSetsDone,pgraph.numSets,       &
                     dSettingsArray(drawLegends).lineType,    &
                     dSettingsArray(drawLegends).lineColor,   &
                     dSettingsArray(drawLegends).markerType,  &
                     dSettingsArray(drawLegends).markerColor, &
                     dSettingsArray(drawLegends).barType,     &
                     dSettingsArray(drawLegends).barColor,    &
                     dSettingsArray(drawLegends).titleColor,  &
                     dSettingsArray(drawLegends).title,       &
                     graph.graphColor)
          END DO

          graph=cgraph
          PlotLabelMultiData=$GEOK
          RETURN
      END FUNCTION

END MODULE SGPLOT
