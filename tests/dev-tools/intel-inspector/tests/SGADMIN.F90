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

MODULE SGADMIN

USE SGDRAW
IMPLICIT NONE

CONTAINS

!     Gets the graph defaults for a given graph type

      INTEGER   FUNCTION GetGraphDefaults(graphType,graph)

          INTEGER                 graphType    ! input
          RECORD /GraphSettings/  graph        ! output

          RECORD /GraphSettings/ cDefs        ! where to put the defaults
          RECORD /PrivateGraphSettings/ pDefs ! private version of defs
          EQUIVALENCE(cDefs,pDefs)

          DATA cDefs.title,cDefs.titleFont,cDefs.titleColor,           &
             cDefs.title2,cDefs.title2Font,cDefs.title2Color,          &
             cDefs.graphColor,cDefs.graphBgColor,                      &
             cDefs.x1,pDefs.y1,cDefs.x2,cDefs.y2,cDefs.setGraphMode,   &
             pDefs.numXAxesReq,pDefs.numYAxesReq,                      &
             pDefs.numSetsReq,pDefs.didPlotGraph,pDefs.numSets,        &
             pDefs.numSetsDone                                         &
             /'Graph',$FTCOUR,$CIYELLOW,' ',$FTCOURSM,$CIWHITE,        &
             $CIBRIGHTWHITE,$CIBLUE,0,0,639,479,.TRUE.,6*0/

          IF (graphType .LT. 1  .OR. graphType .GT. $GTCOUNT) THEN
              GetGraphDefaults=$GEGRAPH
          END IF

          cDefs.graphType=graphType

          graph=cDefs
          GetGraphDefaults=$GEOK
          RETURN
      END FUNCTION

!     Gets data settings defaults and stores them in dSettings based on the graph
!       settings and the data values

      INTEGER   FUNCTION GetDataDefaults(graph,numPoints,data,dSettings)

          RECORD /GraphSettings/   graph         ! input/output
          INTEGER                  numPoints     ! input: number of data points
          REAL*4                   data(1)       ! input: actual data
          RECORD /DataSettings/    dSettings     ! output
	  RECORD /DataSettings/    dSettingsArray(1)
	  pointer( dSettingsArrayPtr, dSettingsArray )

          dSettingsArrayPtr = loc(dSettings)
          GetDataDefaults=GetMultiDataDefaults(graph,numPoints,data, &
             1,dSettingsArray)
          RETURN
      END FUNCTION


!    Gets multiple data settings defaults and stores them in dSettings based on
!       the graph settings and the data values

      INTEGER   FUNCTION GetMultiDataDefaults(graph,numPoints,data,  &
                                              numSettings,dSettingsArray)

          RECORD /GraphSettings/   graph         ! input
          INTEGER                  numPoints     ! input: number of data points
          REAL*4                   data(1)       ! input: actual data
          INTEGER                  numSettings   ! input
          RECORD /DataSettings/    dSettingsArray(numSettings)    ! output

          RECORD /PrivateGraphSettings/ pgraph   ! private graph
          RECORD /GraphSettings/ cgraph          ! copy of private graph
          EQUIVALENCE(cgraph,pgraph)

          RECORD /DataSettings/ dDefs

          CHARACTER*2 tempStr
          INTEGER, PARAMETER :: markers($MKCOUNT) = &
               (/ $MKSQUARE,$MKTRIANGLE,$MKDIAMOND,$MKCIRCLE,  &
                  $MKPLUS,$MKX,$MKFISQUARE,$MKFITRIANGLE,      &
                  $MKFIDIAMOND,$MKFICIRCLE /)
          INTEGER, PARAMETER :: colors(16) = &
               (/ $CIBRIGHTWHITE,$CIYELLOW,$CILIGHTMAGENTA,        &
                  $CILIGHTCYAN,$CIWHITE,$CILIGHTGREEN,$CILIGHTRED, &
                  $CILIGHTBLUE,$CIBROWN,$CIMAGENTA,$CICYAN,        &
                  $CIGRAY,$CIGREEN,$CIRED,$CIBLACK,$CIBLUE /)
          INTEGER     errBar,d1
          REAL*4      xval,yval,eval
          INTEGER     getSets

          cgraph=graph

          IF (numPoints .LT. 1 .OR. numSettings .LT. 1) THEN
              GetMultiDataDefaults=$GEPARAMS
              RETURN
          END IF

          IF (cgraph.graphType .EQ. $GTBAR .OR. cgraph.graphType   &
                 .EQ. $GTLINE .OR. cgraph.graphType                &
                 .EQ. $GTLINEWERRBAR) THEN
              GetMultiDataDefaults=$GEGRAPH
              RETURN
          END IF

          IF (cgraph.graphType .EQ. $GTXYWERRBAR) THEN
              errBar=1
          ELSE
              errBar=0
          ENDIF

          DO getSets = 1,numSettings
              pgraph.numSetsReq=pgraph.numSetsReq+1
              WRITE(tempStr,'(I2)') pgraph.numSetsReq
              dDefs.title="Data Range"//tempStr
              dDefs.titleColor=colors(MOD(pgraph.numSetsReq-1,16)+1)
              dDefs.titleFont=$FTCOURSM
              dDefs.xFirst=.TRUE.
              dDefs.numPoints=numPoints
              dDefs.numElements=2+errBar
              IF (dDefs.numPoints .LT. 1) THEN
                  GetMultiDataDefaults=$GEDIMS
                  RETURN
              END IF

              dDefs.dataType=$DTNUM
              dDefs.markerType=markers(MOD(pgraph.numSetsReq-1_2,$MKCOUNT)+1)
              dDefs.markerColor=dDefs.titleColor
              dDefs.lineType=$LTSOLID
              dDefs.lineColor=dDefs.titleColor
              dDefs.barType=$BTNONE
              dDefs.barColor=0
              dDefs.errorBarType=$EBNONE
              dDefs.errorBarColor=0

              IF (cgraph.graphType .EQ. $GTXYWERRBAR  &
                     .OR. cgraph.graphType .EQ. $GTLINEWERRBAR) THEN
                  dDefs.errorBarType=$EBTHIN
                  dDefs.errorBarColor=dDefs.markerColor
              END IF

              eval=0.0
              DO d1= 1,dDefs.numPoints           ! scan data for high/low
                  xval=data(SciGetDataOffset(dDefs,getSets,d1,1))
                  IF (dDefs.xLowVal .GT. xval .OR. d1 .EQ. 1)  &
                         dDefs.xLowVal=xval
                  IF (dDefs.xHighVal .LT. xval .OR. d1 .EQ. 1)  &
                         dDefs.xHighVal=xval

                  yval=data(SciGetDataOffset(dDefs,getSets,d1,2))
                  IF (errBar .EQ. 1)  &
                         eval=data(SciGetDataOffset(dDefs,getSets,d1,3))
                  IF (dDefs.yLowVal .GT. yval-eval .OR. d1 .EQ. 1)  &
                         dDefs.yLowVal=yval-eval
                  IF (dDefs.yHighVal .LT. yval+eval .OR. d1 .EQ. 1)  &
                         dDefs.yHighVal=yval+eval
              END DO
              dSettingsArray(getSets)=dDefs
          END DO ! settings loop
          graph=cgraph
          GetMultiDataDefaults=$GEOK
          RETURN
      END FUNCTION



!   GetLabelDataDefaults sets a DataSettings structure to good defaults based
!         on the given graph settings, labels, and data

      INTEGER   FUNCTION GetLabelDataDefaults(graph,numLabels,labels, &
                                         data,dSettings)
          RECORD /GraphSettings/   graph              ! input
          INTEGER                  numLabels          ! input
          CHARACTER*(*)            labels(numLabels)  ! input
          REAL*4                   data(1)            ! input: actual data
          RECORD /DataSettings/    dSettings          ! output
	  RECORD /DataSettings/    dSettingsArray(1)
          pointer( dSettingsArrayPtr, dSettingsArray )

	  dSettingsArrayPtr = loc(dSettings)
          GetLabelDataDefaults=GetLabelMultiDataDefaults(graph, &
                 numLabels,labels,data,1,dSettingsArray)
          RETURN
      END FUNCTION


!   GetMultiLabelDataDefaults sets DataSettings structures to good defaults based
!         on the given graph settings, labels, and data

      INTEGER FUNCTION GetLabelMultiDataDefaults(graph,numLabels,labels,data, &
                                         numSettings,dSettingsArray)

          RECORD /GraphSettings/   graph              ! input
          INTEGER                  numLabels          ! input
          CHARACTER*(*)            labels(numLabels)  ! input
          REAL*4                   data(1)            ! input: actual data
          INTEGER                  numSettings        ! input
          RECORD /DataSettings/    dSettingsArray(numSettings)    ! output

          RECORD /PrivateGraphSettings/ pgraph   ! private graph
          RECORD /GraphSettings/ cgraph          ! copy of private graph
          EQUIVALENCE(cgraph,pgraph)

          RECORD /DataSettings/ dDefs

          CHARACTER*2 tempStr
          INTEGER, PARAMETER :: bars($BTCOUNT) = &
           (/ $BTHASHLEFT,$BTHASHRIGHT,  &
              $BTHEAVYHASHLEFT,$BTHEAVYHASHRIGHT,$BTSOLID,$BTEMPTY /)
          INTEGER, PARAMETER :: markers($MKCOUNT) = &
           (/ $MKSQUARE,$MKTRIANGLE,$MKDIAMOND,$MKCIRCLE, &
              $MKPLUS,$MKX,$MKFISQUARE,$MKFITRIANGLE, &
              $MKFIDIAMOND,$MKFICIRCLE /)
          INTEGER, PARAMETER :: colors(16) = &
           (/ $CIBRIGHTWHITE,$CIYELLOW,$CILIGHTMAGENTA, &
              $CILIGHTCYAN,$CIWHITE,$CILIGHTGREEN,$CILIGHTRED, &
              $CILIGHTBLUE,$CIBROWN,$CIMAGENTA,$CICYAN, &
              $CIGRAY,$CIGREEN,$CIRED,$CIBLACK,$CIBLUE /)
          REAL*4      yval,eval
          INTEGER     errBar,d1
          INTEGER     getSets
          INTEGER     dummy

          cgraph=graph

          IF (numLabels .LT. 1 .OR. numSettings .LT. 1) THEN
              GetLabelMultiDataDefaults=$GEPARAMS
              RETURN
          END IF

          IF (cgraph.graphType .NE. $GTBAR .AND. cgraph.graphType  &
                 .NE. $GTLINE .AND. cgraph.graphType               &
                 .NE. $GTLINEWERRBAR) THEN
              GetLabelMultiDataDefaults=$GEGRAPH
              RETURN
          END IF

          IF (cgraph.graphType .EQ. $GTLINEWERRBAR) THEN
              errBar=1
          ELSE
              errBar=0
          ENDIF


          DO getSets = 1,numSettings
              pgraph.numSetsReq=pgraph.numSetsReq+1
              WRITE(tempStr,'(I2)') pgraph.numSetsReq
              dDefs.title="Data Range"//tempStr
              dDefs.titleColor=colors(MOD(pgraph.numSetsReq-1,16)+1)
              dDefs.titleFont=$FTCOURSM
              dDefs.xFirst=.TRUE.

              dDefs.numPoints=numLabels
              dDefs.numElements=1+errBar
              IF (dDefs.numPoints .LT. 1) THEN
                  GetLabelMultiDataDefaults=$GEDIMS
                  RETURN
              END IF

              dDefs.dataType=$DTTEXT

              ! could check to see what lengths strings are here
              dummy=LEN(labels(1))

              IF (cgraph.graphType .EQ. $GTBAR) THEN
                  dDefs.markerType=$MKNONE
                  dDefs.markerColor=0
                  dDefs.lineType=$LTNONE
                  dDefs.lineColor=0
                  dDefs.barType=bars(MOD(pgraph.numSetsReq-1_2,$BTCOUNT)+1)
                  dDefs.barColor=dDefs.titleColor
                  dDefs.errorBarType=$EBNONE
                  dDefs.errorBarColor=0
              ELSE            ! one of the line graphs
                  dDefs.lineType=$LTSOLID
                  dDefs.lineColor=dDefs.titleColor
                  dDefs.markerType=markers(MOD(pgraph.numSetsReq-1_2,$MKCOUNT)+1)
                  dDefs.markerColor=dDefs.lineColor
                  dDefs.barType=$BTNONE
                  dDefs.barColor=0
                  IF (errBar .EQ. 1) THEN
                      dDefs.errorBarType=$EBTHIN
                      dDefs.errorBarColor=dDefs.markerColor
                  ELSE
                      dDefs.errorBarType=$EBNONE
                      dDefs.errorBarColor=0
                  END IF
              END IF

              dDefs.xLowVal=0
              dDefs.xHighVal=numLabels

              eval=0.0
              DO d1= 1,dDefs.numPoints                 ! scan data for high/low
                  yval=data(SciGetDataOffset(dDefs,getSets,d1,1))
                  IF (errBar .EQ. 1)   &
                         eval=data(SciGetDataOffset(dDefs,getSets,d1,2))
                  IF (dDefs.yLowVal .GT. yval-eval .OR. d1 .EQ. 1)  &
                         dDefs.yLowVal=yval-eval
                  IF (dDefs.yHighVal .LT. yval+eval .OR. d1 .EQ. 1)  &
                         dDefs.yHighVal=yval+eval
              END DO

              dSettingsArray(getSets)=dDefs
          END DO ! settings loop

          graph=cgraph
          GetLabelMultiDataDefaults=$GEOK
          RETURN
      END FUNCTION



!     Gets axis defaults based on a data settings structure and a graph settings
!     structure

      INTEGER   FUNCTION GetAxisDefaults(graph,dSettings,axisType,axisFunc,axis)

          RECORD /GraphSettings/   graph              ! input
          RECORD /DataSettings/    dSettings          ! input
          RECORD /DataSettings/    dSettingsArray(1)
          pointer( dSettingsArrayPtr, dSettingsArray )
          INTEGER                  axisType           ! input: $ATX,$ATY
          INTEGER                  axisFunc           ! input: $AFNONE,$AFLOG,...
          RECORD /AxisSettings/    axis               ! output

	  dSettingsArrayPtr = loc(dSettings)
          GetAxisDefaults=GetAxisMultiDefaults(graph,1,  &
                 dSettingsArray,axisType,axisFunc,axis)
          RETURN
      END FUNCTION


!   GetAxisMultiDefautls sets axis to good defaults based of the given graph,
!     and data settings.  It tries to give a good scale.
      INTEGER   FUNCTION GetAxisMultiDefaults(graph,numSettings,  &
                                     dSettingsArray,axisType,axisFunc,axis)

          RECORD /GraphSettings/   graph              ! input
          INTEGER                  numSettings        ! input
          RECORD /DataSettings/    dSettingsArray(numSettings)    ! input
          INTEGER                  axisType           ! input: $ATX,$ATY
          INTEGER                  axisFunc           ! input: $AFNONE,$AFLOG,...
          RECORD /AxisSettings/    axis               ! output

          RECORD /PrivateGraphSettings/ pgraph   ! private graph
          RECORD /GraphSettings/ cgraph          ! copy of private graph
          EQUIVALENCE(cgraph,pgraph)

          RECORD /AxisSettings/   aDefs
          INTEGER                 getExtremes
          DOUBLE PRECISION        lowi,highi,diffi
          INTEGER                 numTicks,helpScale
          REAL*8                  logBase,rmag,temp
          REAL*8                  cLow,cHigh
          LOGICAL                 isLog

          cgraph=graph

          IF (numSettings .LT. 1) THEN
              GetAxisMultiDefaults=$GEPARAMS
              RETURN
          END IF

          IF (axisType .EQ. $ATX .OR. axisType .EQ. $ATR) THEN
              IF (axisType .EQ. $ATX) THEN
                  aDefs.title='X Axis'
              ELSE
                  aDefs.title='R Axis'
              END IF
              IF (pgraph.numXAxesReq .GT. 0) THEN
                  aDefs.axisPos=$APTOP
              ELSE
                  aDefs.axisPos=$APBOTTOM
              END IF
              pgraph.numXAxesReq=pgraph.numXAxesReq+1
          ELSE IF (axisType .EQ. $ATY .OR. axisType .EQ. $ATTHETA) THEN
              IF (axisType .EQ. $ATY) THEN
                  aDefs.title='Y Axis'
              ELSE
                  aDefs.title='Theta Axis'
              END IF
              IF (pgraph.numYAxesReq .GT. 0) THEN
                  aDefs.axisPos=$APRIGHT
              ELSE
                  aDefs.axisPos=$APLEFT
              END IF
              IF (axisType .EQ. $ATTHETA) THEN
                  aDefs.axisPos=$APRIGHT
              END IF
              pgraph.numYAxesReq=pgraph.numYAxesReq+1
          ELSE
              GetAxisMultiDefaults=$GEAXIS    ! not a valid axis type
              RETURN
          ENDIF

          IF ( ( (axisType .EQ. $ATX .OR. axisType .EQ. $ATY)       &	
                 .AND. graph.graphType .EQ. $GTPOLAR)  .OR.         &
                 ( (axisType .EQ. $ATR .OR. axisType .EQ. $ATTHETA) &
                 .AND. graph.graphType .NE. $GTPOLAR) ) THEN
              GetAxisMultiDefaults=$GEAXIS    ! not a valid axis func
              RETURN
          ENDIF

          IF (axisFunc .LT. 0  .OR. axisFunc .GT. $AFCOUNT) THEN
              GetAxisMultiDefaults=$GEAXIS    ! not a valid axis func
              RETURN
          ENDIF

          aDefs.axisType=axisType
          aDefs.axisColor=cgraph.graphColor
          aDefs.axisFont=$FTSANSSERIFSM
          aDefs.axisFontRotated=.FALSE.
          aDefs.titleFont=cgraph.titleFont
          aDefs.titleColor=cgraph.titleColor

          IF (.NOT. (dSettingsArray(1).dataType .EQ. $DTTEXT   .AND.  &
                 (axisType .EQ. $ATX  .OR. axisType .EQ. $ATR ))) THEN  ! only do scaling for numeric

              DO getExtremes = 1,numSettings
                  IF (axisType .EQ. $ATX  .OR. axisType .EQ. $ATR ) THEN
                      cLow=dSettingsArray(getExtremes).xLowVal
                      cHigh=dSettingsArray(getExtremes).xHighVal
                  ELSE
                      cLow=dSettingsArray(getExtremes).yLowVal
                      cHigh=dSettingsArray(getExtremes).yHighVal
                  END IF

                  IF (aDefs.lowVal .GT. cLow .OR. getExtremes .EQ. 1) THEN
                      aDefs.lowVal=cLow
                  END IF

                  IF (aDefs.highVal .LT. cHigh .OR. getExtremes .EQ. 1) THEN
                      aDefs.highVal=cHigh
                  END IF
              END DO

              IF (axisType .EQ. $ATR) THEN        ! don't let min r-polar be < 0
                  IF (aDefs.lowVal .LT. 0) THEN
                      IF (-aDefs.lowVal .GT. aDefs.highVal) THEN
                          aDefs.highVal=-aDefs.lowVal
                          aDefs.lowVal=0
                      ELSE
                          aDefs.lowVal=0
                      END IF
                  END IF
              END IF

              IF (aDefs.lowVal .GE. aDefs.highVal) THEN
                  GetAxisMultiDefaults=$GEDATA
                  RETURN
              END IF

              isLog=axisFunc .NE. $AFNONE
              IF (isLog) THEN     ! log axis graph
                  IF (aDefs.lowVal .LE. 0) THEN
                      GetAxisMultiDefaults=$GEDATA
                      RETURN
                  END IF
                  SELECT CASE (axisFunc)
                  CASE ($AFLOG10)
                      aDefs.tickRatio=9   ! 10,20,30,40,50,60,70,80,90,100
                      logBase=10.0
                  CASE ($AFLOG)
                      logBase=EXP(1.0)
                      aDefs.tickRatio=9   ! e**2..e**4 10 values
                  CASE ($AFLOG2)
                      logBase=2.0
                      aDefs.tickRatio=8   ! 2, 2.25, 2.5, 2.75, 3.0, 3.25, 3.5, 3.75, 4.0
                  END SELECT

                  temp=DLOG(aDefs.lowVal)/DLOG(logBase)
                  lowi=INT(temp)                      ! calc lowi
                  IF (temp .NE. lowi .AND. temp .LT. 0) THEN
                          lowi=lowi-1
                  ENDIF
                  IF (lowi .LT. 0) THEN
                      aDefs.numDigits=-lowi
                  ELSE
                      aDefs.numDigits=2
                  END IF
                  lowi=logBase**lowi

                  temp=DLOG(aDefs.highVal)/DLOG(logBase)
                  highi=INT(temp)                      ! calc lowi
                  IF (temp .NE. highi .AND. temp .GT. 0) THEN
                          highi=highi+1
                  END IF
                  highi=logBase**highi


                  aDefs.lowVal=lowi
                  aDefs.highVal=highi
                  aDefs.increment=1           ! increment gets fixed up later

              ELSE  ! non-log graphs  (rect and polar)
                  IF (axisType .NE. $ATTHETA) THEN
                      lowi=aDefs.lowVal                   ! will be left bounding int
                      highi=aDefs.highVal                 ! will be right bounding int

                      diffi=highi-lowi
                      rmag=10.D0**INT(DLOG10(diffi))         ! 10s, 100s, 1000s, ...

                      temp=lowi/rmag                      ! calc lowi
                      lowi=INT(temp)
                      IF (ABS(temp-lowi) .GE. $EPREAL4 .AND. temp .LT. 0) THEN
                          lowi=lowi-1
                      ENDIF
                      lowi=lowi*rmag

                      temp=highi/rmag                     ! calc highi
                      highi=INT(temp)
                      IF (ABS(temp-highi) .GE. $EPREAL4 .AND. temp .GT. 0) THEN
                          highi=highi+1
                      ENDIF
                      highi=highi*rmag

                      aDefs.lowVal=lowi
                      aDefs.highVal=highi
                      helpScale= (highi-lowi)/rmag
                      IF (helpScale .GT. 5) THEN
                          numTicks=helpScale
                      ELSE
                          SELECT CASE (helpScale)
                            CASE (5)
                                numTicks=10
                            CASE (4)
                                numTicks=8
                            CASE (3)
                                numTicks=6
                            CASE (2)
                                numTicks=8
                            CASE (1)
                                numTicks=10
                          END SELECT
                      END IF

                      aDefs.tickRatio=5
                      aDefs.increment=(highi-lowi)/numTicks
                      aDefs.numDigits=3
                      IF (graph.graphType .EQ. $GTPOLAR) THEN ! polar r-axis
                          aDefs.increment=aDefs.increment*2.0
                      END IF
                  ELSE  ! must be a polar theta axis, do special scaling
                      lowi=aDefs.lowVal                   ! will be left bounding int
                      highi=aDefs.highVal                 ! will be right bounding int

                      diffi=highi-lowi

                      ! assume it is a degree graph
                      IF (diffi .GT. 4*$PI) THEN
                          rmag=90.0
                          aDefs.numDigits=2
                      ELSE
                          rmag=$PI/2.0D0
                          aDefs.numDigits=3
                      END IF


                      temp=lowi/rmag                      ! calc lowi
                      lowi=INT(temp)
                      IF (ABS(temp-lowi) .GE. $EPREAL4 .AND. temp .LT. 0) THEN
                          lowi=lowi-1
                      ENDIF
                      lowi=lowi*rmag

                      temp=highi/rmag                     ! calc highi
                      highi=INT(temp)
                      IF (ABS(temp-highi) .GE. $EPREAL4 .AND. temp .GT. 0) THEN
                          highi=highi+1
                      ENDIF
                      highi=highi*rmag

                      aDefs.lowVal=lowi
                      aDefs.highVal=highi
                      helpScale= (highi-lowi)/rmag
                      IF (helpScale .GE. 5) THEN
                          numTicks=helpScale
                      ELSE
                          SELECT CASE (helpScale)
                            CASE (4)
                                numTicks=4
                            CASE (3)
                                numTicks=6
                            CASE (2)
                                numTicks=4
                            CASE (1)
                                numTicks=4
                          END SELECT
                      END IF

                      aDefs.tickRatio=5
                      aDefs.increment=(highi-lowi)/numTicks
                  END IF
              END IF
          ELSE    ! this must be X axis labels
              aDefs.lowVal=0    ! must be like this (0..n)
              aDefs.highVal=dSettingsArray(1).numPoints
              aDefs.tickRatio=1
              aDefs.increment=1
              aDefs.numDigits=0
          END IF

          aDefs.axisFunc=axisFunc
          aDefs.axisCW=.FALSE.
          aDefs.tickType=$TTBOTH
          aDefs.tickColor=aDefs.axisColor
          aDefs.minorTickColor=MOD(aDefs.tickColor,8_2)

          aDefs.gridStyle=$GSMAJOR
          aDefs.gridLineType=$LTDOT
          aDefs.gridColor=aDefs.tickColor

          graph=cgraph
          axis=aDefs
          GetAxisMultiDefaults=$GEOK
          RETURN
      END FUNCTION

END MODULE SGADMIN
