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

!======================================================================
!
!   DESCRIPTION
!       Contains the type definitions and parameters (constants) used in the
!             SciGraph graphing library.
!
! 6/2001 Remove 'noisy' compiler directives

MODULE SGDATA

      IMPLICIT NONE

      STRUCTURE /GraphSettings/
          CHARACTER*40    title           ! title for graph
          INTEGER(2)      titleFont       ! title font: $FTCOUR,$FTCOURSM,...
          INTEGER(2)      titleColor      ! title color (0..numcolors)
          CHARACTER*40    title2          ! second title for graph
          INTEGER(2)         title2Font      ! second title font: $FTCOUR,$FTCOURSM,...
          INTEGER(2)      title2Color     ! second title color (0..numcolors)
          INTEGER(2)      graphType       ! what kind of graph: $GTBAR,$GTLINE,...
          INTEGER(2)      graphColor      ! what color for graph border
          INTEGER(2)      graphBgColor    ! what color for graph background
          INTEGER(2)      x1,y1,x2,y2     ! physical screen boundaries for graph
          LOGICAL         setGraphMode    ! if SciGraph should set the graphics mode
          INTEGER         sgprivate(8)    ! data PRIVATE to SciGraph: don't change
      END STRUCTURE

      STRUCTURE /PrivateGraphSettings/
          CHARACTER*40    title           ! title for graph
          INTEGER(2)      titleFont       ! title font: $FTCOUR,$FTCOURSM,...
          INTEGER(2)      titleColor      ! title color (0..numcolors)
          CHARACTER*40    title2          ! second title for graph
          INTEGER(2)      title2Font      ! second title font: $FTCOUR,$FTCOURSM,...
          INTEGER(2)      title2Color     ! second title color (0..numcolors)
          INTEGER(2)      graphType       ! what kind of graph: $GTBAR,$GTLINE,...
          INTEGER(2)      graphColor      ! what color for graph border
          INTEGER(2)      graphBgColor    ! what color for graph background
          INTEGER(2)      x1,y1,x2,y2     ! physical screen boundaries for graph
          LOGICAL         setGraphMode    ! if SciGraph should set the graphics mode
!   The following 3 will only get set if the user calls the Get*Default routines
          INTEGER         numSetsReq      ! number of data settings requested (for auto color)
          INTEGER         numXAxesReq     ! number of X axes requested
          INTEGER         numYAxesReq     ! number of Y axes requested
!   The following 4 will always get set by the PlotGraph and Plot*Data routines
          INTEGER         didPlotGraph    ! 1 if did plot graph successfully, else 0
          INTEGER         numSets         ! number of data sets to plot
          INTEGER         numSetsDone     ! number of data sets plotted so far
      END STRUCTURE

      REAL(8), PARAMETER :: $WXMIN           = 0.0D0
      REAL(8), PARAMETER :: $WXMAX           = 10240.0D0
      REAL(8), PARAMETER :: $WX              = $WXMAX-$WXMIN
      REAL(8), PARAMETER :: $WXC             = $WXMIN+$WX/2.0D0
      REAL(8), PARAMETER :: $WYMIN           = 0.0D0
      REAL(8), PARAMETER :: $WYMAX           = 7680.0D0
      REAL(8), PARAMETER :: $WY              = $WYMAX-$WYMIN
      REAL(8), PARAMETER :: $WYC             = $WYMIN+$WY/2.0D0
      REAL(8), PARAMETER :: $GXMIN           = 1300.0D0
      REAL(8), PARAMETER :: $GXMAX           = 8940.0D0
      REAL(8), PARAMETER :: $GX              = $GXMAX-$GXMIN
      REAL(8), PARAMETER :: $GXC             = $GXMIN+$GX/2.0D0
      REAL(8), PARAMETER :: $GYMIN           = 1248.0D0
      REAL(8), PARAMETER :: $GYMAX           = 6032.0D0
      REAL(8), PARAMETER :: $GY              = $GYMAX-$GYMIN
      REAL(8), PARAMETER :: $GYC             = $GYMIN+$GY/2.0D0
      REAL(8), PARAMETER :: $PGR             = 2560.0

      STRUCTURE /DataSettings/
          CHARACTER*20    title           ! title for data range (legend)
          INTEGER(2)      titleFont       ! which font: $FTCOUR,$FTCOURSM,...
          INTEGER(2)      titleColor      ! title color (0..numcolors)

          INTEGER(2)      markerType      ! marker style: $MKNONE,$MKSQUARE,...
          INTEGER(2)      markerColor     ! marker color (0..numcolors)
          INTEGER(2)      lineType        ! line type: $LTNONE, $LTSOLID,...
          INTEGER(2)      lineColor       ! line color
          INTEGER(2)      barType         ! bar type: $BTNONE,$BTSOLID,..
          INTEGER(2)      barColor        ! bar color
          INTEGER(2)      errorbarType    ! error bar style: $EBNONE, $EBTHIN,...
          INTEGER(2)      errorbarColor   ! error bar color

          INTEGER         dataType        ! #,# or text,# ?  $DTTEXT, $DTNUM
          LOGICAL         xFirst          ! x stored before y in a given row
          INTEGER         numPoints       ! number of data points or labels (bar/line)
          INTEGER         numElements     ! number of elements per data point (2/3)

          REAL*8          xLowVal         ! x values>= this value
          REAL*8          xHighVal        ! x values<= this value

          REAL*8          yLowVal         ! y values>= this value
          REAL*8          yHighVal        ! y values<= this value
      END STRUCTURE


      STRUCTURE /AxisSettings/
          CHARACTER*25    title           ! title for axis
          INTEGER(2)      titleFont       ! which font: $FTCOUR,$FTCOURSM,...
          INTEGER(2)      titleColor      ! title color (0..numcolors)

          INTEGER(2)      axisFont        ! what font for the axis numbers/labels
          LOGICAL(2)      axisFontRotated ! .TRUE. if axis numbers rotated (y only)
          INTEGER(2)      axisPos         ! where to put axis: $APBOTTOM,$APTOP,...
          INTEGER(2)      axisType        ! what kind of axis: $ATX, $ATY,...
          INTEGER(2)      axisColor       ! what color the line of the axis is
          INTEGER(2)      axisFunc        ! axis function: $AFNONE,$AFLOG10,...
          LOGICAL         axisCW          ! if polar axis is clockwise
          REAL*8          lowVal          ! axis starts at this value
          REAL*8          highVal         ! axis ends at this value
          REAL*8          increment       ! axis increment for major ticks
          INTEGER         numDigits       ! number of decimal digits to print

          INTEGER(2)      tickType        ! tick type: $TTNONE,$TTINSIDE,...
          INTEGER(2)      tickColor       ! color of the major (big) ticks
          INTEGER(2)      minorTickColor  ! color of the minor (small) ticks
          INTEGER(2)      tickRatio       ! number of minor ticks from one major
                                           !  tick to the next:
                                           !  1= all major ticks(||||),
                                           !  2=(|.|), 3=(|..|),4= |...|, etc
          INTEGER(2)      gridStyle       ! grid style: $GSNONE,$GSMAJOR,...
          INTEGER(2)      gridLineType    ! grid line type: $LTNONE, $LTSOLID,...
          INTEGER(2)      gridColor       ! color of grid lines
      END STRUCTURE

!   Include the actual structure definitions which are stored in separate files
!   Logical   constants for Fortran type checking
!      LOGICAL   $TRUE2,$FALSE2
!      PARAMETER ($TRUE2           =.TRUE.)
!      PARAMETER ($FALSE2          =.FALSE.)

!   Numeric constants need in SciGraph
      REAL(8), PARAMETER :: $PI          = 3.141592653589793D0
      REAL(8), PARAMETER :: $EPREAL4     = 1.192092895507813D-07
      REAL(8), PARAMETER :: $EPREAL8     = 2.220446049250313D-016

!   Color indices for default 16 color palette
      INTEGER(2), PARAMETER :: $CIBLACK         = 0
      INTEGER(2), PARAMETER :: $CIBLUE          = 1
      INTEGER(2), PARAMETER :: $CIGREEN         = 2
      INTEGER(2), PARAMETER :: $CICYAN          = 3
      INTEGER(2), PARAMETER :: $CIRED           = 4
      INTEGER(2), PARAMETER :: $CIMAGENTA       = 5
      INTEGER(2), PARAMETER :: $CIBROWN         = 6
      INTEGER(2), PARAMETER :: $CIWHITE         = 7
      INTEGER(2), PARAMETER :: $CIGRAY          = 8
      INTEGER(2), PARAMETER :: $CILIGHTBLUE     = 9
      INTEGER(2), PARAMETER :: $CILIGHTGREEN    = 10
      INTEGER(2), PARAMETER :: $CILIGHTCYAN     = 11
      INTEGER(2), PARAMETER :: $CILIGHTRED      = 12
      INTEGER(2), PARAMETER :: $CILIGHTMAGENTA  = 13
      INTEGER(2), PARAMETER :: $CIYELLOW        = 14
      INTEGER(2), PARAMETER :: $CIBRIGHTWHITE   = 15

!   Graph types for use in graph settings structure
      INTEGER, PARAMETER :: $GTBAR           = 1     ! bar graph (x=text labels, y=num)
      INTEGER, PARAMETER :: $GTLINE          = 2     ! line graph (x=text labels, y=num)
      INTEGER, PARAMETER :: $GTLINEWERRBAR   = 3     ! line graph with error bars (same)
      INTEGER, PARAMETER :: $GTXY            = 4     ! xy graph (x=num, y=num)
      INTEGER, PARAMETER :: $GTXYWERRBAR     = 5     ! xy graph with error bars
      INTEGER, PARAMETER :: $GTPOLAR         = 6     ! polar coordinate graph
      INTEGER, PARAMETER :: $GTCOUNT         = 6     ! how many total GTs

!   Font types for use in settings structures
      INTEGER(2), PARAMETER :: $FTCOUR          = 1     ! courier font
      INTEGER(2), PARAMETER :: $FTCOURSM        = 2     ! small courier font
      INTEGER(2), PARAMETER :: $FTTROMAN        = 3     ! roman font
      INTEGER(2), PARAMETER :: $FTTROMANSM      = 4     ! small roman font
      INTEGER(2), PARAMETER :: $FTSANSSERIF     = 5     ! SANSSERIF font
      INTEGER(2), PARAMETER :: $FTSANSSERIFSM   = 6     ! small SANSSERIF font
      INTEGER(2), PARAMETER :: $FTCOUNT         = 6     ! how many total FTs

      INTEGER(2), PARAMETER :: $FTDEF           = 1000  ! default font

!   Font "values"
      CHARACTER*50, PARAMETER :: $FVCOUR      = "t'Courier New'h20w10b"
      CHARACTER*50, PARAMETER :: $FVCOURSM    = "t'Courier New'h20w9b"
      CHARACTER*50, PARAMETER :: $FVTROMAN    = "t'Times New Roman'h26w13b"
      CHARACTER*50, PARAMETER :: $FVTROMANSM  = "t'Times New Roman'h16w8b"
      CHARACTER*50, PARAMETER :: $FVSANSSERIF = "t'MS Sans Serif'h24w12b"
      CHARACTER*50, PARAMETER :: $FVSANSSERIFSM = "t'MS Sans Serif'h12w6b"
      CHARACTER*50, PARAMETER :: $FVDEF       = $FVCOUR


!   Data types for use in data settings structure
      INTEGER(2), PARAMETER :: $DTNUM           = 1     ! data is numeric,numeric
      INTEGER(2), PARAMETER :: $DTTEXT          = 2     ! data is text,numeric
      INTEGER(2), PARAMETER :: $DTCOUNT         = 2     ! how many total DTs


!   Marker types for use in data settings structure
      INTEGER(2), PARAMETER :: $MKNONE          = 0     ! no markers at points
      INTEGER(2), PARAMETER :: $MKSQUARE        = 1     ! draw squares at points
      INTEGER(2), PARAMETER :: $MKTRIANGLE      = 2     ! triangles
      INTEGER(2), PARAMETER :: $MKDIAMOND       = 3     ! diamonds
      INTEGER(2), PARAMETER :: $MKCIRCLE        = 4     ! circles
      INTEGER(2), PARAMETER :: $MKPLUS          = 5     ! plus signs
      INTEGER(2), PARAMETER :: $MKX             = 6     ! X marks
      INTEGER(2), PARAMETER :: $MKFISQUARE      = 7     ! filled in squares
      INTEGER(2), PARAMETER :: $MKFITRIANGLE    = 8     ! filled triangles
      INTEGER(2), PARAMETER :: $MKFIDIAMOND     = 9     ! filled diamonds
      INTEGER(2), PARAMETER :: $MKFICIRCLE      = 10    ! filled circles
      INTEGER(2), PARAMETER :: $MKCOUNT         = 10    ! how many total MKs

!   Line types for use in settings structures
      INTEGER(2), PARAMETER :: $LTNONE          = 0     ! no lines between points
      INTEGER(2), PARAMETER :: $LTSOLID         = 1     ! draw solid lines between points
      INTEGER(2), PARAMETER :: $LTDASH          = 2     ! dash
      INTEGER(2), PARAMETER :: $LTDASHDOT       = 3     ! dash-dot
      INTEGER(2), PARAMETER :: $LTDASHDOTDOT    = 4     ! dash-dot-dot
      INTEGER(2), PARAMETER :: $LTDOT           = 5     ! dot
      INTEGER(2), PARAMETER :: $LTTHICKSOLID    = 6     ! thick solid lines
      INTEGER(2), PARAMETER :: $LTTHICKDASH     = 7     ! thick dash
      INTEGER(2), PARAMETER :: $LTTHICKDASHDOT  = 8     ! thick dash-dot
      INTEGER(2), PARAMETER :: $LTTHICKDASHDOTDOT= 9    ! thick dash-dot-dot
      INTEGER(2), PARAMETER :: $LTTHICKDOT      = 10    ! thick dot
      INTEGER(2), PARAMETER :: $LTCOUNT         = 10    ! how many total LTs

      INTEGER(2), PARAMETER :: $LTDEF           = 1000  ! default line type

!   Line "values"

      INTEGER(2), PARAMETER :: $LVNONE          = 0     ! no lines between points
      INTEGER(2), PARAMETER :: $LVSOLID         = #FFFF ! draw solid lines between points
      INTEGER(2), PARAMETER :: $LVDASH          = #EEEE ! dash
      INTEGER(2), PARAMETER :: $LVDASHDOT       = #ECEC ! dash-dot
      INTEGER(2), PARAMETER :: $LVDASHDOTDOT    = #ECCC ! dash-dot-dot
      INTEGER(2), PARAMETER :: $LVDOT           = #AAAA ! dot
      INTEGER(2), PARAMETER :: $LVTHICKSOLID    = #FFFF ! thick solid lines
      INTEGER(2), PARAMETER :: $LVTHICKDASH     = #EEEE ! thick dash
      INTEGER(2), PARAMETER :: $LVTHICKDASHDOT  = #ECEC ! thick dash-dot
      INTEGER(2), PARAMETER :: $LVTHICKDASHDOTDOT= #ECCC! thick dash-dot-dot
      INTEGER(2), PARAMETER :: $LVTHICKDOT      = #AAAA ! thick dot

      INTEGER(2), PARAMETER :: $LVDEF           = $LVSOLID ! default line type

!   Bar types for use in data settings structures.  $BV* are in SCIGRAPH.FOR
!     function SciDrawBar
      INTEGER, PARAMETER :: $BTNONE          = 0     ! don't draw bars
      INTEGER, PARAMETER :: $BTEMPTY         = 1     ! draw outline of bars
      INTEGER, PARAMETER :: $BTSOLID         = 2     ! draw bars filled in with solid color
      INTEGER, PARAMETER :: $BTHASHLEFT      = 3     ! left hashing  (/ /)
      INTEGER, PARAMETER :: $BTHASHRIGHT     = 4     ! right hashing  (\ \)
      INTEGER, PARAMETER :: $BTHEAVYHASHLEFT = 5     ! heavy left hashing  (// //)
      INTEGER, PARAMETER :: $BTHEAVYHASHRIGHT= 6     ! heavy right hashing  (\\ \\)
      INTEGER, PARAMETER :: $BTCOUNT         = 6     ! how many total BTs

!   Error bar types for use in data settings structures
      INTEGER, PARAMETER :: $EBNONE          = 0     ! no error bars
      INTEGER, PARAMETER :: $EBTHIN          = 1     ! thin y error bars
      INTEGER, PARAMETER :: $EBTHICK         = 2     ! thick y error bars

!   Axis Functions for use in axis settings
      INTEGER, PARAMETER :: $AFNONE          = 0     ! no transform function: leave data as is
      INTEGER, PARAMETER :: $AFLINEAR        = 0     ! no transform function: leave data as is
      INTEGER, PARAMETER :: $AFLOG10         = 1     ! take log base 10 of each data point
      INTEGER, PARAMETER :: $AFLOG           = 2     ! take log base e (natural log)
      INTEGER, PARAMETER :: $AFLOG2          = 3     ! take log base 2
      INTEGER, PARAMETER :: $AFCOUNT         = 3     ! how many total AFs

!   Axis Positions for use in axis setting structure
      INTEGER, PARAMETER :: $APNONE          = 0     ! don't display an axis
      INTEGER, PARAMETER :: $APBOTTOM        = 1     ! display axis at bottom
      INTEGER, PARAMETER :: $APTOP           = 2     ! top
      INTEGER, PARAMETER :: $APLEFT          = 3     ! left
      INTEGER, PARAMETER :: $APRIGHT         = 4     ! right
      INTEGER, PARAMETER :: $APCOUNT         = 4     ! how many total APs

!   Axis Types for use in axis settings structure
      INTEGER, PARAMETER :: $ATX             = 0     ! x-axis
      INTEGER, PARAMETER :: $ATY             = 1     ! y-axis
      INTEGER, PARAMETER :: $ATR             = 2     ! r-axis
      INTEGER, PARAMETER :: $ATTHETA         = 3     ! theta-axis
      INTEGER, PARAMETER :: $ATCOUNT         = 4     ! how many total ATs

!   Tick Types for use in axis settings structure
      INTEGER, PARAMETER :: $TTNONE          = 0     ! no ticks on axis
      INTEGER, PARAMETER :: $TTINSIDE        = 1     ! ticks "inside" axis
      INTEGER, PARAMETER :: $TTOUTSIDE       = 2     ! ticks "outside" axis
      INTEGER, PARAMETER :: $TTBOTH          = 3     ! ticks on both sides of axis
      INTEGER, PARAMETER :: $TTCOUNT         = 3     ! how many total TTs

!   Grid Style for use in axis settings structure
      INTEGER, PARAMETER :: $GSNONE          = 0     ! no grid
      INTEGER, PARAMETER :: $GSMAJOR         = 1     ! grid on major ticks
      INTEGER, PARAMETER :: $GSBOTH          = 2     ! grid on major and minor ticks
      INTEGER, PARAMETER :: $GSZERO          = 3     ! grid line on zero
      INTEGER, PARAMETER :: $GSZEROMAJOR     = 4     ! grid on zero and major ticks
      INTEGER, PARAMETER :: $GSZEROBOTH      = 5     ! grid on zero and major and minor ticks
      INTEGER, PARAMETER :: $GSCOUNT         = 5     ! how many total GSs

!   Error codes for return from all SciGraph functions
      INTEGER, PARAMETER :: $GEOK            = 0     ! no error
      INTEGER, PARAMETER :: $GEGRAPHMODE     = 1     ! couldn't enter graphics mode
      INTEGER, PARAMETER :: $GEFONT          = 2     ! couldn't set fonts
      INTEGER, PARAMETER :: $GEGRAPH         = 3     ! the graph info or type was bad
      INTEGER, PARAMETER :: $GEAXIS          = 4     ! the axis info, type, or func was bad
      INTEGER, PARAMETER :: $GEDATA          = 5     ! invalid data or data settings info
      INTEGER, PARAMETER :: $GEDIMS          = 6     ! one or more of the dimensions was bad
      INTEGER, PARAMETER :: $GEPARAMS        = 7     ! one or more of the params was bad
      INTEGER, PARAMETER :: $GENOPLOTGRAPH   = 8     ! didn't do plot graph before plot data

END MODULE SGDATA

