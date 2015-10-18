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

!
!   sgdemo.for - Demo of the SciGraph graphing library
!
!   DESCRIPTION
!       This program draws an example of each type of
!       graph available with SciGraph.
!

MODULE SciGraphDemo

USE SCIGRAPH

CONTAINS
	
SUBROUTINE SciGraphDemoStart

      integer i

      open(10,file='user')
      call LineDemo

      open(20,file='user')
      call BarDemo

      open(30,file='user')
      call XYDemo

      open(40,file='user')
      call LogDemo

      open(50,file='user')
      call PolarDemo

      open(60,file='user')
      call XYErrorBarDemo

      open(70,file='user')
      call LineErrorBarDemo

END SUBROUTINE

!
! LineDemo -- Draws a simple line graph
!
      SUBROUTINE LineDemo

      RECORD /GraphSettings/ lineGraph
      RECORD /DataSettings/ lineDataSets(4)    ! 4 data sets (ranges)
      RECORD /AxisSettings/ lineAxes(2)        ! 2 axes

      REAL*4      lineData(6,4)                ! 4 data sets each with 6 (y)
      CHARACTER*20 lineLabels(6)               ! the x labels for the line  graph
      CHARACTER*20 lineDataLegends(4)          ! 4 data range legends

      INTEGER   retcode
      INTEGER   setLegends

      DATA lineData /                                     &
             316.5, 317.8, 320.1, 318.3, 314.2, 315.1,    &
             324.6, 326.6, 327.8, 326.3, 323.1, 324.0,    &
             337.8, 339.9, 341.2, 339.3, 335.7, 336.7,    &
             349.9, 351.9, 353.9, 352.1, 348.5, 349.8 /

      DATA lineLabels / 'January','March','May','July','September','November' /

      DATA lineDataLegends / '1960','1970','1980','1988'/


          retcode=GetGraphDefaults($GTLINE,lineGraph)
          lineGraph.setGraphMode=.FALSE.
          lineGraph.title='Atmospheric CO2'
          lineGraph.title2='Parts Per Million by Volume'

          retcode=GetLabelMultiDataDefaults(lineGraph,6,lineLabels, &
                 lineData,4,lineDataSets)
          DO setLegends=1,4
              lineDataSets(setLegends).title=lineDataLegends(setLegends)
          END DO

          retcode=GetAxisMultiDefaults(lineGraph,4,lineDataSets, &
             $ATX,$AFLINEAR,lineAxes(1))
          lineAxes(1).title='Month'

          retcode=GetAxisMultiDefaults(lineGraph,4,lineDataSets, &
             $ATY,$AFLINEAR,lineAxes(2))
          lineAxes(2).title='Concentration of CO2'
          lineAxes(2).tickType=$TTOUTSIDE
          lineAxes(2).numDigits=1

          retcode=PlotGraph(lineGraph,2,lineAxes,4)

          retcode=PlotLabelMultiData(lineGraph,lineLabels,lineData, &
             4,lineDataSets,lineAxes(1),lineAxes(2))
      END SUBROUTINE

!
! BarDemo -- Draws a simple Bar Graph
!
      SUBROUTINE BarDemo

      RECORD /GraphSettings/ barGraph
      RECORD /DataSettings/ barDataSets(6)    ! 6 data sets (ranges)
      RECORD /AxisSettings/ barAxes(2)        ! 2 axes

      REAL*4      barData(6,6)                ! 6 data sets each with 6 (y)
      CHARACTER*20 barLabels(6)               ! the x labels for the bar  graph
      CHARACTER*20 barDataLegends(6)          ! 6 data range legends

      INTEGER     retcode
      INTEGER     setLegends

      DATA barData /                               &
             6.2, 3.3, 9.8, 23.3, 9.3, 2.0,        &
             12.4, 6.4, 18.7, 37.6, 18.4, 3.6,     &
             17.9, 9.5, 26.0, 49.8, 26.3, 5.5,     &
             22.6, 12.3, 32.3, 60.3, 33.7, 7.3,    &
             26.8, 14.8, 38.3, 69.9, 41.0, 8.9,    &
             30.6, 17.1, 44.0, 78.7, 48.1, 10.4 /

      DATA barLabels / 'Argon','Krypton','Nitrogen','Neon','Oxygen','Xenon'  /

      DATA barDataLegends / '100 K', '200 K', '300 K','400 K','500 K','600 K' /

          retcode=GetGraphDefaults($GTBAR,barGraph)
          barGraph.setGraphMode=.FALSE.
          barGraph.title='Conductivity of Gases'
          barGraph.title2='Conductivity at different Temps'

          retcode=GetLabelMultiDataDefaults(barGraph,6,barLabels,  &
                 barData,6,barDataSets)
          DO setLegends=1,6
              barDataSets(setLegends).title=barDataLegends(setLegends)
          END DO
          barDataSets(6).barType=$BTHASHLEFT

          retcode=GetAxisMultiDefaults(barGraph,6,barDataSets,  &
                 $ATX,$AFLINEAR,barAxes(1))
          barAxes(1).title='Gases'

          retcode=GetAxisMultiDefaults(barGraph,6,barDataSets,  &
                 $ATY,$AFLINEAR,barAxes(2))
          barAxes(2).title='Conductivity in mW/MK'
          barAxes(2).gridStyle=$GSBOTH
          barAxes(2).gridLineType=$LTDOT
          barAxes(2).gridColor=$CILIGHTBLUE
          barAxes(2).numDigits=1
          retcode=PlotGraph(barGraph,2,barAxes,6)

          retcode=PlotLabelMultiData(barGraph,barLabels,barData,  &
                 6,barDataSets,barAxes(1),barAxes(2))

END SUBROUTINE

!
! XYDemo -- Draws a simple XY Graph
!
SUBROUTINE XYDemo

      RECORD /GraphSettings/ xyGraph
      RECORD /DataSettings/ xyDataSets(5)    ! 5 data sets (ranges)
      RECORD /AxisSettings/ xyAxes(4)        ! 4 axes: 2 y, 2 x

      REAL*4      xyData(2,7,5)              ! 5 data sets each with 7 (x,y)
      CHARACTER*20 xyDataLegends(5)          ! 5 data range legends

      INTEGER     retcode
      INTEGER     setLegends

      DATA xyData /                                                            &
        640,0.52,  660,0.54, 680,0.57, 720,0.59, 740,0.60, 760,0.62, 770,0.63, &
        630,1.03, 640,1.04, 660,1.08, 700,1.14, 730,1.19, 740,1.21, 770, 1.24, &
        650,1.27, 660,1.29, 680,1.33, 700,1.37, 720,1.41, 730,1.43, 760,1.49,  &
        640,1.56, 660,1.61, 670,1.64, 690, 1.69, 710,1.74, 730,1.78, 750,1.83, &
        630,2.05, 640,2.08, 670,2.18, 700, 2.28, 730, 2.38, 740,2.41, 760,2.47 /

      DATA xyDataLegends / '5 °C', '10 °C', '12 °C', '15 °C', '20 °C' /

          retcode=GetGraphDefaults($GTXY,xyGraph)
          xyGraph.setGraphMode=.FALSE.
          xyGraph.title='Corrections for Barometer Readings'

          retcode=GetMultiDataDefaults(xyGraph,7,xyData,5,xyDataSets)
          DO setLegends=1,5
              xyDataSets(setLegends).title=xyDataLegends(setLegends)
          END DO

          retcode=GetAxisMultiDefaults(xyGraph,5,xyDataSets,  &
                 $ATX,$AFLINEAR,xyAxes(1))
          xyAxes(1).title='Barometric Pressure (mm)'

          retcode=GetAxisMultiDefaults(xyGraph,5,xyDataSets,  &
                 $ATY,$AFLINEAR,xyAxes(2))
          xyAxes(2).title='Correction (mm)'
          xyAxes(2).gridStyle=$GSBOTH
          xyAxes(2).gridLineType=$LTDOT
          xyAxes(2).gridColor=$CILIGHTBLUE

          retcode=GetAxisMultiDefaults(xyGraph,5,xyDataSets,  &
                 $ATX,$AFLINEAR,xyAxes(3))
          xyAxes(3).title='Barometric Pressure (in)'
          xyAxes(3).lowVal=xyAxes(3).lowVal/2.54
          xyAxes(3).highVal=xyAxes(3).highVal/2.54
          xyAxes(3).increment=xyAxes(3).increment/2.54
          xyAxes(3).tickColor=$CIWHITE
          xyAxes(3).minorTickColor=$CIGRAY
          xyAxes(3).titleColor=$CIWHITE

          retcode=GetAxisMultiDefaults(xyGraph,5,xyDataSets,  &
                 $ATY,$AFLINEAR,xyAxes(4))
          xyAxes(4).title='Correction (in)'
          xyAxes(4).lowVal=xyAxes(4).lowVal/2.54
          xyAxes(4).highVal=xyAxes(4).highVal/2.54
          xyAxes(4).increment=xyAxes(4).increment/2.54
          xyAxes(4).tickColor=$CIWHITE
          xyAxes(4).minorTickColor=$CIGRAY
          xyAxes(4).titleColor=$CIWHITE


          retcode=PlotGraph(xyGraph,4,xyAxes,5)

          retcode=PlotMultiData(xyGraph,xyData,5,xyDataSets,xyAxes(1),xyAxes(2))

      END SUBROUTINE

!
! LogDemo -- Draws a logarithmic graph
!
      SUBROUTINE LogDemo

      RECORD /GraphSettings/ loggraph
      RECORD /DataSettings/ logdsets         ! 1 data sets (ranges)
      RECORD /AxisSettings/ logaxes(2)       ! 2 axes

      REAL*4      logdata(2,7)               ! data set with 7 (x,y)

      INTEGER     retcode

      DATA logdata / 1.0,20, 1.2,185, 1.3,345, 1.4,672, &
                       1.5,1024, 1.9,2919, 1.95,8210 /

      retcode=GetGraphDefaults($GTXY,loggraph)
      loggraph.setGraphMode = .FALSE.
      loggraph.title='Log Graph'

      retcode=GetDataDefaults(loggraph,7,logdata,logdsets)
      logdsets.markerColor=$CILIGHTGREEN
      logdsets.lineType=$LTTHICKDOT
      logdsets.lineColor=$CIYELLOW

      retcode=GetAxisDefaults(loggraph,logdsets,$ATX,$AFLINEAR,logaxes(1))
      logaxes(1).tickType=$TTOUTSIDE
      logaxes(1).gridStyle=$GSMAJOR
      logaxes(1).gridLineType=$LTSOLID
      logaxes(1).gridColor=$CILIGHTCYAN

      retcode=GetAxisDefaults(loggraph,logdsets,$ATY,$AFLOG10,logaxes(2))
      logaxes(2).titleColor=$CICYAN
      logaxes(2).axisColor=$CICYAN
      logaxes(2).minorTickColor=$CILIGHTMAGENTA
      logaxes(2).tickType=$TTOUTSIDE
      logaxes(2).gridStyle=$GSBOTH
      logaxes(2).gridLineType=$LTDOT
      logaxes(2).gridColor=$CILIGHTBLUE

      retcode=PlotGraph(loggraph,2,logaxes,1)
      retcode=PlotData(loggraph,logdata,logdsets, logaxes(1),logaxes(2))

      END SUBROUTINE

!
! PolarDemo -- Draws a simple polar plot
!
      SUBROUTINE PolarDemo

      RECORD /GraphSettings/ polarGraph
      RECORD /DataSettings/ polarDataSets(2)    ! 2 data sets (ranges)
      RECORD /AxisSettings/ polarAxes(2)        ! 2 axes: 2 y, 1 x

      INTEGER   nump
      PARAMETER (NUMP=200)

      REAL*4      polarData(2,NUMP,2)           ! 2 data set each with NUMP (r,th)
      CHARACTER*20 polarDataLegends(2)          ! 2 data range legends

      INTEGER     retcode
      INTEGER     index
      REAL*4      theta

      DATA polarDataLegends / 'r=2Sin(3@)','r=1-2cos(@)'/

          ! set up data range 1
          DO index=1,nump
              theta=$PI*(index-1.0D0)/(nump-1.0D0)
              polarData(1,index,1)=2*sin(3*theta)
              polarData(2,index,1)=theta
          END DO

          ! set up data range 2
          DO index=1,nump
              theta=2*$PI*(index-1.0)/(nump-1.0)
              polarData(1,index,2)=1-2*cos(theta)
              polarData(2,index,2)=theta
          END DO

          retcode=GetGraphDefaults($GTPOLAR,polarGraph)
          polarGraph.setGraphMode=.FALSE.
          polarGraph.title='Polar Graph'

          retcode=GetMultiDataDefaults(polarGraph,nump, &
                 polarData,2,polarDataSets)
          polarDataSets(1).title=polarDataLegends(1)
          polarDataSets(1).lineColor=$CIYELLOW
          polarDataSets(1).markerType=$MKNONE

          polarDataSets(2).title=polarDataLegends(2)
          polarDataSets(2).lineColor=$CILIGHTCYAN
          polarDataSets(2).markerType=$MKNONE

          retcode=GetAxisMultiDefaults(polarGraph,2,polarDataSets, &
                 $ATR,$AFLINEAR,polarAxes(1))

          retcode=GetAxisMultiDefaults(polarGraph,2,polarDataSets, &
                 $ATTHETA,$AFLINEAR,polarAxes(2))
         polarAxes(2).gridStyle=$GSBOTH
         polarAxes(2).gridLineType=$LTDOT
         polarAxes(2).gridColor=$CILIGHTBLUE

          retcode=PlotGraph(polarGraph,2,polarAxes,2)

          retcode=PlotMultiData(polarGraph,polarData,         &
                 2,polarDataSets,polarAxes(1),polarAxes(2))

      END SUBROUTINE

!
! XYErrorBarDemo -- Draws a simple XY graph with error bars
!
      SUBROUTINE XYErrorBarDemo

      RECORD /GraphSettings/ simplegraph
      RECORD /DataSettings/ simpledsets         ! 1 data sets (ranges)
      RECORD /AxisSettings/ simpleaxes(2)       ! 2 axes

      REAL*4      simpledata(3,7)               ! data set with 7 (x,y,e)

      INTEGER     retcode

      DATA simpledata / 1.0,2,0.15, 1.2,4,0.2, 1.3,6,0.3, 1.4,7,0,  &
                        1.5,8,0.4, 1.9,9,0.125, 1.95,10,0.2 /

      retcode=GetGraphDefaults($GTXYWERRBAR,simplegraph)
      simplegraph.setGraphMode = .FALSE.

      retcode=GetDataDefaults(simplegraph,7,simpledata,simpledsets)
      simpledsets.markerColor=$CIYELLOW
      simpledsets.lineType=$LTTHICKDASH
      simpledsets.lineColor=$CILIGHTGREEN

      retcode=GetAxisDefaults(simplegraph,simpledsets,$ATX,  &
             $AFLINEAR,simpleaxes(1))
      simpleaxes(1).axisPos=$APTOP
      simpleaxes(1).titleColor=$CIWHITE
      simpleaxes(1).axisColor=$CIWHITE
      simpleaxes(1).tickColor=$CIBRIGHTWHITE
      simpleaxes(1).minorTickColor=$CIGREEN
      simpleaxes(1).tickType=$TTOUTSIDE
      simpleaxes(1).gridStyle=$GSMAJOR
      simpleaxes(1).gridLineType=$LTSOLID
      simpleaxes(1).gridColor=$CILIGHTCYAN

      retcode=GetAxisDefaults(simplegraph,simpledsets,$ATY,  &
             $AFLINEAR,simpleaxes(2))
      simpleaxes(2).titleColor=$CICYAN
      simpleaxes(2).axisColor=$CICYAN
      simpleaxes(2).tickColor=$CILIGHTCYAN
      simpleaxes(2).minorTickColor=$CIMAGENTA
      simpleaxes(2).tickType=$TTOUTSIDE
      simpleaxes(2).gridStyle=$GSBOTH
      simpleaxes(2).gridLineType=$LTDOT
      simpleaxes(2).gridColor=$CILIGHTBLUE

      retcode=PlotGraph(simplegraph,2,simpleaxes,1)
      retcode=PlotData(simplegraph,simpledata,simpledsets,  &
             simpleaxes(1),simpleaxes(2))

      END SUBROUTINE

!
! LineErrorBarDemo -- Draws a simple line graph with Error Bars
!
      SUBROUTINE LineErrorBarDemo

      RECORD /GraphSettings/ lineGraph
      RECORD /DataSettings/ lineDataSets(4)    ! 4 data sets (ranges)
      RECORD /AxisSettings/ lineAxes(2)        ! 2 axes

      REAL*4      lineData(6,4, 2)             ! 4 data sets each with 6 (y)
      CHARACTER*20 lineLabels(6)               ! the x labels for the line graph
      CHARACTER*20 lineDataLegends(4)          ! 4 data range legends

      INTEGER     retcode
      INTEGER     setLegends

      DATA lineData /                                                    &
       316.5, 2, 317.8, 1, 320.1, 3, 318.3, 2.4, 314.2, 3, 315.1, 2.2,   &
       324.6, 0, 326.6, 9, 327.8, 3, 326.3, 1.2, 323.1, 5, 324.0, 2,     &
       337.8, 5, 339.9, 2, 341.2, 2, 339.3, .8,  335.7, 4, 336.7, 3,     &
       349.9, 1, 351.9, 4, 353.9, 5, 352.1, 1,   348.5, 2, 349.8, 1.1 /

      DATA lineLabels / 'January','March','May','July','September','November' /

      DATA lineDataLegends / '1960','1970','1980','1988'/

          retcode=GetGraphDefaults($GTLINEWERRBAR,lineGraph)
          lineGraph.setGraphMode=.FALSE.
          lineGraph.title='Atmospheric CO2'
          lineGraph.title2='Parts Per Million by Volume'

          retcode=GetLabelMultiDataDefaults(lineGraph,6,lineLabels,  &
                  lineData,4,lineDataSets)
          DO setLegends=1,4
              lineDataSets(setLegends).title=lineDataLegends(setLegends)
          END DO

          retcode=GetAxisMultiDefaults(lineGraph,4,lineDataSets,  &
             $ATX,$AFLINEAR,lineAxes(1))
          lineAxes(1).title='Month'

          retcode=GetAxisMultiDefaults(lineGraph,4,lineDataSets,  &
             $ATY,$AFLINEAR,lineAxes(2))
          lineAxes(2).title='Concentration of CO2'
          lineAxes(2).tickType=$TTOUTSIDE
          lineAxes(2).numDigits=1

          retcode=PlotGraph(lineGraph,2,lineAxes,4)

          retcode=PlotLabelMultiData(lineGraph,lineLabels,lineData,  &
             4,lineDataSets,lineAxes(1),lineAxes(2))
      END SUBROUTINE

END MODULE

PROGRAM SGDemo
USE SciGraphDemo
CALL SciGraphDemoStart()
END
