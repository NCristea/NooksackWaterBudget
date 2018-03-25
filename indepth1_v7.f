C This version, V2, has been adjusted for inclusion of lake modelling
C
*$debug
C INDEPTH1 IS JUST LIKE INDEPTH, BUT HAS SP(10 INSTEAD OF SP(11
C RP(4 INSTEAD OF RP(5, AND ASSUMES ALL VALUES OF BIGN ARE THE SAME
C
      SUBROUTINE INDEPTH(NRCH,N0RCH,LL,LINKR,SP,RP,SI,BIGN,HI,QI,SQI,
     *      LAKE_REACH,LZERO,LAKE_AREAS,LAKE_BEACH_SLPS,LK_LINE,
     *	NUM_RAT_VALS,LHEADS,LOFLOWS,INI_LEVELS,MAX_LAKES,
     *	MAX_LHEADS,MAXCHN,MAXSLP)
C *********************************************************************
C  This subroutine calculates initial depths for all reaches and gutters
C  based on the initial flows out of the saturated face of each
C  hillslope.  The scheme assumes a steady uniform flow from the
C  slopes into the gutters before the start of the simulation. This flow
C  is calculated as the product of the horizontal saturated hydraulic
C  conductivity, the slope of the hillslope, and the depth of saturation
C  This product is treated as a lateral inflow to the gutters, and it is
C  assumed that the overland flow plane is initially in steady-state
C  with a downslope flow depth of SI(3,JS) -- NOT NECESSARILY DRY SLOPE
C  ( Adjustment must be made for the difference between the width of the
C  hillslope and the length of the gutter. )
C
C      Variable list :-
C  a)  Variables and arrays received
C      -----------------------------
C  NRCH -- actual number of gutters + higher order reaches.
C  N0RCH -- actual number of gutters.
C  LL(MAXCHN) -- pointer giving the order for processing all reaches.
C  LINKR(4,MAXCHN) -- array containing reach network information ,
C    i.e. LINKR(1,I) = arbitrary numbering (must be greater than NS).
C         LINKR(2,I) = slope or upstream reach feeding into this reach.
C         LINKR(3,I) = second slope or upstream reach feeding into this
C                      reach.
C         LINKR(4,I) = number of slopes or upstream reaches feeding into
C                      this reach.
C
C  SP(NSP,MAXSLP) -- slope properties ,
C  where SP(1,I) = PP(1) or area of map unit (mm**2)
C
C  RP(NRP,MAXCHN) -- reach properties ,
C    i.e. RP(1,I) = slope.
C         RP(2,I) = Manning's n.
C         RP(3,I) = width (mm).
C         RP(4,I) = length (mm).
C
C  SI(NSI,MAXSLP) -- slope initial conditions
C    i.e. SI(3,JS) = Initial downslope depth of overland flow
C                    ( introduced for use in forecasting )
C  BIGN -- A value of N for use in ALL flow routing (usually 5/3)
C
C  Q1(MAXSLP) -- Initial flows from each subbasin
C
C  SQI(MAX_LAKES) -- Initial flow from each reach (RPI for Lakes)
C
C  b)  Array returned
C      --------------
C  HI(MAXCHN) -- Array of calculated initial depths.
C
C              Author : D.C.K. Leong
C              Date : 25th October 1988.
C             Altered by R A Woods 24/11/88 to use initial hillslope
C             conditions instead of an initial flow rate
C                   RAW (1/2/89) was here to allow for
C                       non-zero initial overland flow depths
C
C  Passed variables
C  ****************
      IMPLICIT NONE ! 30/03/2004 ITB
      INCLUDE  'Tdims_v7.inc'
	INCLUDE 'maxvariables.inc'


      INTEGER NRCH,N0RCH
      INTEGER LL(MAXCHN),LINKR(4,MAXCHN)

      REAL*8 SP(NSP,MAXSLP), QI(MAXSLP)
      REAL*8 RP(NRP,MAXCHN),HI(MAXCHN)
      REAL*8 SI(NSI,MAXSLP)
	REAL*8  BIGN
	REAL*8 SQI(MAX_LAKES)
C  Local variables
C  ***************
      INTEGER I,J,II,JJ,IPOSN,IR
      REAL*8    ALPHA,ALPHA1,WIDTH,WIDTH1,SUMQ
C
C INTRODUCED FOR LAKE ROUTING BY RPI 16/8/00
C
C Lake variables
	INTEGER NLAKES
	INTEGER LAKE_REACH(MAX_LAKES),LZERO(MAX_LAKES),LK_LINE(MAX_LAKES)
	INTEGER LAKE_BEACH_SLPS(MAX_LAKES),NUM_RAT_VALS(MAX_LAKES)
	INTEGER LHEADS(MAX_LAKES,MAX_LHEADS),LOFLOWS(MAX_LAKES,MAX_LHEADS)
      INTEGER LAKE  ! 30/03/2004 ITB

	REAL*4 LAKE_AREAS(MAX_LAKES),INI_LEVELS(MAX_LAKES)  ! RPI 17/5/2002 added ini_levels
	COMMON /LAKES1/ NLAKES !,LAKE_REACH,LZERO,LAKE_AREAS,
C     *           LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,
C     *           INI_LEVELS ! RPI 17/5/2002 added ini_levels
      COMMON /LAKES2/ LAKE
	LOGICAL OD
C
C  Start calculating initial depths from the upstreamest to the
C  downstreamest reach.

      DO 50 I=1,NRCH
        II = LL(I)
        IF(I.LE.N0RCH) THEN
        SUMQ=0.
        DO 20 J=1,LINKR(4,II)
        JJ=LINKR(J+1,II)
        SUMQ=SUMQ+QI(JJ)
   20   CONTINUE
        HI(II)=SUMQ
        ELSE
        ALPHA1 = DSQRT(RP(1,II))/RP(2,II)
        WIDTH1 = RP(3,II)
C  Enter here if this reach is a higher order reach
          SUMQ = 0.D0
          DO 30 J=1,LINKR(4,II)
          IR = IPOSN(NRCH,LINKR,LINKR(J+1,II))
               IF(IR.GT.NRCH-N0RCH) THEN
C Have a gutter as input - artificial gutters are after the channels
C Artificial gutters are used to link sub-basins to channels
               SUMQ=SUMQ+HI(IR)
               ELSE
               ALPHA = DSQRT(RP(1,IR))/RP(2,IR)
               WIDTH = RP(3,IR)
               SUMQ = SUMQ + HI(IR)**BIGN * WIDTH * ALPHA
               ENDIF
   30     CONTINUE
        HI(II) = ( SUMQ/(ALPHA1*WIDTH1) )**(1.0D0/BIGN)
        ENDIF
c RPI 16/5/2002 thinks there will be a problem here when nlakes=0
      if(nlakes.gt.0) then
		DO 60 J=1,NLAKES
		IF(LAKE_REACH(J).EQ.LINKR(1,II)) THEN
	       SQI(J)=SUMQ
		ENDIF
   60		CONTINUE
      endif

C  Fill in the calculated depth here
   50 CONTINUE
C  End of loop calculations
C  ************************
C
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER FUNCTION IPOSN(NRCH,LINKR,II)
C  Function that gives the position of the array element which has the
C  reach number II.
C
C  D.C.K. Leong / 25th October 1988
C
      INTEGER NRCH
      INTEGER LINKR(4,NRCH),II,I
      IPOSN = 0
      DO 10 I=1,NRCH
        IF(LINKR(1,I).EQ.II) THEN
          IPOSN = I
          RETURN
        ENDIF
   10 CONTINUE
      RETURN
      END
