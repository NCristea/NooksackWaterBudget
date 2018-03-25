c warning, Ross has subverted the meaning of qinst for nooksack
c
c raw 12-jan-2005 removed many of the (ISUB) subscripts (e.g. ZBAR, SR, CV, ...)
C This version, V2, has been corrected for -ve seepage under
C saturated conditions.
*$debug
*******************************************************************
*
*  Program to use discrete distribution Z-based TOPMODEL
*
********************************************************************
c RPI 22/3/2004 - a lot of these variables in V6 were automatic arrays, i.e.
c the space allocated to them disappeared when the subroutine was exited.
c This did not matter in V6 as they were not required outside this routine.
c However, in V7 they have to retain their values between calls and so must
c be made explicit shape arrays. One way to do this is to put them in the
c calling sequence. Also there is scope for avoiding repeated calculations
c at the start of the routine - this will become a big problem on long runs
c as many unnecessary calculations will end up being made. To overcome this problem
      SUBROUTINE TOPMOD(SI,SP,ISUB,NKA,lambda,ATB,PKA,ND,CL,PD,UNITS,
     1           IRR,MODWRT,IPSUB,IPATB,STIM,SPEAK,R,PET,interv,
     *		   art_drainage,rate_irrig,IMONTH,
     *		   NDATA,
     2		   MPS,MPE,QINST_OUT,DR_OUT,ndump,ntdh,ISTEP,MAXC,ZBM,
     3		   MAXA,MAXSLP,MAXINT,SUMR,SUMQ,SUMAE,S0,Q0,SR,CV,ACIEM,
     4           ACSEM,SUMPE,SUMIE,SUMQB,SUMCE,SUMSLE,SUMR1,QB,QINST,
     5           DR,SUMQV,SUMSE,ZBAR,zbar_in,TDH,
     6		   ZR,ak0fzrdt,logoqm,qvmin,dth,sumad,evap_mm,
     7		   qlat_mm,ipflag,rirr)  ! DGT 8/17/05 added evap_mm

c         reversed the  order of DR and QINST back to what they were 25/9/98
c   DGT had problems with qinst flows not preserving mass balance so resorted
c   to routing the time step averages DR - simply by interchanging arguments
c   because the DR passed back to CALCTS was not used for anything.
c
c     2            R,PET,TIMES,NDATA,MPS,MPE,NGAUGE,DR,QINST,MP1)
c   Understanding of Arguments DGT March 1998
C   SI - Array of subbasin initil conditions - array with column for each subbasin
c       3 entries in each column SRO, ZBAR0, CV0
c   SP - Array of subbasin parameters - array with column for each subbasin
c       11 entries
c        1 - AREA - Subbasin Area
c        2 - SZF   - exponential decay parameter
c        3 - AKO  - vertical surface conductivity
c        4 - DTH1 - difference between saturation and field capacity
c        5 - DTH2 - difference between field capacity and wilting point
c        6 - soilc - This is the maximum storage capacity (m) of the root zone.
c            P-E enters the root zone and only exits if soilc is satisfied
c        7 - C  Brooks Corey / Clapp Hornberger parameter
c        8 - PSIf - Green - Ampt wetting front suction parameter
c        9 - CHV - Channel velocity
c        10 - CC  Interception capacity
c        11 - CR  Drying et enhancement ratio for drying rate of intercepted water.
c
c    ISUB - active subbasin number
c    NKA - Array of number of ln(a/tan b) classes for each subbasin
c    lambda - lambda for active subbasin
c    ATB - two dimensional array with columns the ln(a/tan b) distribution for
c          each subbasin
c    PKA - two dimensional array with columns the proportion of basin corresponding
c          to ln(a/tan b) values
c    ND - array of number of flow distances for each subbasin
c    CL - two dimensional array with columns giving flow distances for each subbasin
c    PD - two dimensional array with columns giving cumulative flow distance
c         probabilities for each subbasin
c    UNITS - a units conversion parameter
c    IRR - A 2 dimensional output array of various diagnostic quantities
C    modwrt - logical variable to control I/O
c    IPSUB - Subbasin that output will be for - no longer used
c    IPATB - not used
C    STIM - Base time for Tideda output
C    SPEAK - logical to control screen i/o - not used
c    R - rainfall data - row for each raingage
c    PET - potential et array
c    interv - time step associated with each input value (seconds).  Assumed all equal -
c        only first one is used.
c    ndata - number of time steps
c    ngauge - raingauge number being used for this subbasin
c    dr - array that is direct runoff returned for this subbasin
c    qinst - time series of outflow rates returned
c    mp1 -not used
c
*
*
      IMPLICIT NONE  ! 30/03/2004  ITB
C	implicit real*8 (a-h,o-z) ! 30/03/2004 ITB CHANGED THIS TO COMMENT
	save ! RPI 15/7/2003 added this to ensure local variables are saved
      INCLUDE 'Tdims_v7.inc'
      INCLUDE 'Luns.inc'
	INCLUDE 'maxvariables.inc'
C
C
      REAL*8 SIF(NSI,MAXSLP) ! RPI 25/7/02 added SIF for neq=0 work
      REAL*8 SI(NSI,MAXSLP),SP(NSP,MAXSLP)
      INTEGER NKA(MAXSLP),ND(MAXSLP)
      REAL*8 ATB(MAXA,MAXSLP),PKA(MAXA,MAXSLP)
      REAL*8 PD(MAXC,MAXSLP),CL(MAXC,MAXSLP)
C
      LOGICAL*1 MODWRT,SPEAK
      INTEGER NDATA,MPS,MPE,IPSUB,IPATB,ISUB,NGAUGE
	INTEGER SKIP,MAXA2,MAXA1,MAXC1,MAXC2

      CHARACTER(LEN=200) TEXT
	LOGICAL OD
      INTEGER*4 STIM

      REAL*8 R                       !  (MAXSLP)
      INTEGER*8 interv  ! DGT 5/14/12
	real*8 art_drainage,rate_irrig,evap_mm
	integer IMONTH

      REAL*8 PET
c      REAL*8 DR(MAXC,MAXSLP),QINST(MAXC+1,MAXSLP)
      REAL*8 DR(MAX_NTDH),QINST(MAX_NTDH) ! RPI 19/2/2004
	REAL*8 QINST_OUT, DR_OUT   !  DGT 6/10/05  DR_OUT added
      REAL*8 BAL,UNITS,lambda,RTEMP
C
      INTEGER*4 IRR(NIP1,1) !RAW 26-jan-2005 MAXSLP)  ! RPI 15/7/2003 MAXSLP only for testing
	real*8 rirr(nip1,1)  ! dgt real array in parallel to IRR.  At some point remove IRR
	INTEGER*4 ipflag
	integer*8 tim
C
C LOCAL TOPMODEL VARIABLES
c      REAL*8 Z(MAXA),EX(MAXA),REX(MAXA)

      INTEGER IT,I,NTDH(MAXSLP),ITDH,IND,IC,KYY
      REAL*8 SB,SUM,SZF,soilc,DALPHA,BETA,DTH1,DTH2,AK0
      REAL*8 TE,CHV,SR0,PMAC,TDH1,TDH2,AE,RIT,QV,P,EA,QUZ
      REAL*8 ROF,ROFF,QLAT,S1,qlat_mm
      REAL*8 ROFEX,ROFREX,ACIE,SRZMEAN,SUZMEAN
      REAL*8 DTH,THR,HF,DT,TEMP,TEMP1,SUZN,ZBARN,ZBM
      REAL*8 ACSE,ZBAR0,RZDRAIN,QLATIN
C
      REAL*8 SRZ(MAXA),SUZ(MAXA)
C
      real*8 i4max
      DATA I4MAX/2147000000/
C RPI 14/7/03 - turn scalars into arrays for removal of time loop
C RPI 14/7/03 - the array dimension is MAXSLP
	REAL*8 Q0,ZBAR,SR,CV,S0,zbar_in
	REAL*8 TDH(MAX_NTDH,MAXSLP)
	REAL*8 ACIEM,ACSEM,SUMR,SUMAE,
     +       SUMPE,SUMQ,SUMIE,SUMSE,
     +	   SUMQB,SUMCE,SUMSLE,SUMR1,
     +       SUMQV,SUMI,SUME
	REAL*8 QB
	REAL*8 ZR,ak0fzrdt,logoqm,qvmin
CRAW Landcare
      integer*4 ymd,hms,surfrunoff(MAXA)
	real*4 soilwetness(MAXA)
C RPI 20/6/2002 introduced these 2 variables to reduce printout
	DATA ISR0msg/0/,ICV0msg/0/
C
      INTEGER*4 IDEBUG, NITEM, ISR0MSG, ICV0MSG,ISTEP, JT   ! 30/03/2004 itb
      INTEGER*4 NMIN, NDUMP,JTDH !  30/03/2004  ITB
      REAL*8 AREA,C,PSIF,CC,CR,T0,CV0,CVS,R1,UD,R2 ! 30/03/2004 itb
      REAL*8 R3,RIE,RD,SRN,ATBSAT,ATBINF,RIEI,RDI,RSEI,ETS,R3I,ASATL ! 30/03/2004 itb
	REAL*8 AINFL, AMIDL ! 30/03/2004 ITB ITB
      REAL*8 PUNINF,PINF,PSAT,ZI, SRI, R3ITEMP,RDTEMP,SRNI,BALCV ! 30/03/2004 ITB
      REAL*8 BALSAT, BALSOIL ! 30/03/2004 ITB
	REAL*8 FI,crop_coeff,fsprinkler !impervious fraction, crop coefficient is Kc
c
c  DGT 6/28/05   Artificial drainage variables
      real*8 adi,ad_cap,ad_now,ad_now_i,sumad
C
*
***********************************************************************
*
C Calculate the number of items to be put into the model's tideda file
c      OPEN(UNIT=30,FILE='testclpd.txt',STATUS='unknown')
	idebug=0 ! put this > 0 to write water balance data to toperror.txt
c   unravelling basin properties
      AREA=SP(1,ISUB)
      SZF=SP(2,ISUB)
	AK0 = sp(3,isub)
	DTH1=SP(4,ISUB)
      DTH2=SP(5,ISUB)
      soilc=SP(6,ISUB)*(DTH1+DTH2)
	C = SP(7, ISUB)
	PSIf = SP(8, ISUB)
	CHV=SP(9,ISUB)
	CC=SP(10,ISUB)
	cr=sp(11,isub)   !  Ratio by which evaporation from interception
	FI = SP(15, ISUB) ! raw 12-jan-2005 fraction impervious
	fsprinkler = SP(21,ISUB)
	crop_coeff= SP(25+IMONTH,ISUB) ! raw 12-jan-2005
	T0=SP(25+12+1,ISUB)
C    Derived parameters
!      T0 = AK0/SZF RAW special for nooksack 21-March-2005
	TE = T0   !  No spatial variability in Hyd conductivity yet

C The following steps should only be executed at the start of a data sequence
	time0: if(istep.eq.0) then
		NITEM=NIP1-1    ! RPI & RAW removed the comment status 18/9/98
c    is enhanced over the reference potential rate which is for unlimited
c    transpiration from the vegetation
		DALPHA=1D0
		PMAC=0D0
		dt = interv/3600.  !  Time step in hours - Assumed equal interval time steps
		TIM = STIM
		Q0=DT*TE*DEXP(-lambda)
		DTH=dth1+dth2
!RAW 29/10/2002 No storage! need to prevent divide by zero
		if (soilc.le. 0. .and. DTH.le.0.) then
			DTH=1.
		endif
		ZR = soilc/DTH
		ak0fzrdt = dt* ak0*exp(-szf*ZR) ! parameter used in infiltration excess
c   Parmeters for analytic integration of sat zone store - See notebook analysis
c   DGT April 1,1998
		logoqm = - log(szf/dth1) - log(1d-9*dt)
c   The 1d-9 above is a m/hr tolerance on numerical estimation of changes in zbar
		qvmin=1d-7*dth1/szf

c   The 1d-7 is a tolerance relating the analytic solution with qv not equal to
c    zero to the solution with qv = 0.
C Calculate Time delay histogram given CHV (Distance/Time Step)
C By always calculating this each time we enter this subroutine we
C save a lot of space but at the price of time.
		NTDH(ISUB)=INT(CL(ND(ISUB),ISUB)/(CHV*DT))+1
		IF (NTDH(ISUB).EQ.1)THEN
			TDH(1,ISUB)=1.
		ELSE
			IF(NTDH(ISUB).GT.MAX_NTDH) NTDH(ISUB)=MAX_NTDH   ! RPI 29/5/03 150 to 450 ! RPI 19/2/2004
			TDH1=0.
			SUM=0.
			DO 13 ITDH=1,NTDH(ISUB)-1
				DO 14 IND=2,ND(ISUB)
				IF(CHV*DT*ITDH.GT.CL(IND,ISUB)) GOTO 14
				TDH2= PD(IND-1,ISUB) + (PD(IND,ISUB)-PD(IND-1,ISUB)) *
     1	(ITDH*CHV*DT - CL(IND-1,ISUB)) / (CL(IND,ISUB)-CL(IND-1,ISUB))
				TDH(ITDH,ISUB)=TDH2-TDH1
				SUM=SUM+TDH(ITDH,ISUB)
				TDH1=TDH2
				GOTO 13
   14				CONTINUE
   13			CONTINUE
			tdh(NTDH(ISUB),ISUB)=0.            ! rpi
			do 1314 itdh=1,NTDH(ISUB)      !rpi
 1314			tdh(itdh,ISUB)=tdh(itdh,ISUB)/sum  !rpi
c rpi 960306 removed this to prevent all routing being dumped into the
c last tdh position by default. Instead spread residual error
c proportionately across all values
c      TDH(NTDH)=1-SUM          !rpi
		ENDIF
c    Unravelling initial conditions
		SR0=SI(1,ISUB)
		if(sr0 .gt. soilc) then
			if(ISR0msg .le. 10)then ! RPI 20/6/2002 introduced ISR0msg
	 ! write(21,*)'Initial Sr0',sr0,' too big, reduced to soilc',soilc
	write(21,10)'Initial Sr0 ',sr0,' too big, reduced to soilc ',soilc
10				format(A,d12.5,a,d12.5)
	  		endif
			ISR0msg=ISR0msg+1
			sr0=soilc
		endif
		ZBAR0=SI(2,ISUB)
		CV0 = SI(3, ISUB)
		if(cv0 .gt. cc)then
			if(ICV0msg .le. 10)then ! RPI 20/6/2002 introduced ICV0msg
	  write(21,10)'Initial cv0 ',cv0,' too big, reduced to cc ',cc
	  		endif
			ICV0msg=ICV0msg+1
			cv0=cc
		endif
		IRR(:,1)=0  ! RPI 15/7/2003 - second subscript is time - do we need this?
	    rirr(:,1)=0.
		ZBAR = ZBAR0
		SR = SR0
		CV = CV0
*  Initial storage for mass balance check
		S0 = - (ZBAR)*DTH1 + SR + CV
*
* Reset maximum contributing area variables for new storm period
* RPI reintroduced the next  1 lines for infil. excess mechanism 13/1/94
c      write(30,9955) ntdh,cl,pd,tdh
c 9955 format(1x,'ntdh,cl,pd,tdh',i4/(1x,5g14.3))
		ACIEM=0.    ! Max area contributing by infiltration excess
		ACSEM=0.    ! Max area contributing by saturation excess
		ZBM=ZBAR    ! Max Zbar
		SUMR=0.    ! Sum of rainfall
		SUMAE=0.   ! Sum of actual evapotranspiration
		SUMPE=0.   ! Sum of potential evapotranspiration
		SUMQ=0.    ! Sum of all basin outflow - before routing
		SUMIE=0.   ! Sum of infiltration excess runoff - before routing
		SUMSE=0.   ! Sum of saturation excess runoff - before routing
		SUMQB=0.   ! Sum of baseflow - before routing
c		SUMQM=0.   ! Sum of measured flows
		sumce=0.   ! sum of canopy actual e/t
		sumsle=0.  ! sum of soil actual e/t (ie when r3<0)
		sumr1=0.   ! sum of throughfall
C		sumr2=0.   ! sum of net forcing at soil surface
		sumqv=0.   ! sum of soil drainage to sat zone
C		suminf=0.  !sum of infilt at soil surface (ie when r3>0)
	    TEMP = -SZF*ZBAR
	    IF ( TEMP .LT. -100D0 ) THEN
			QB = 0.D0
	    ELSE
			QB=Q0*DEXP(TEMP)
	    ENDIF
*  Initialise output array
		qinst(:)=0.
		dr(:)=0.
      	QINST(1)=QB/UNITS*AREA/interv  ! DGT converts m/ts to mm^3/sec
		QINST_OUT=QINST(1) ! rpi 16/7/2003 - needed to get at QINST
		DR_OUT=DR(1)   ! DGT 6/10/05 - To capture DR out rather than use IRR which suffers from rounding
	! and does not occur if printing is off
		DO 498 IT=1,min(NTDH(ISUB)-1,ndata)   ! DGT 3/16/98  just in case ndth > ndata
C Added by RPI to transfer out initial baseflow for INDEPTH to use
	  	TEMP=1.0
	  		DO 496 JT=1,IT   ! DGT 5/26/98   changed to go to IT so as not to double
c                          count first step
  496     	TEMP=TEMP-TDH(JT,ISUB)
        	temp=max(0.,temp)  ! DGT 5/26/98   Guard against small negative
	  	QINST(IT+1)=QINST(1)*TEMP
C RPI 17/7/2003 DR(1,1) gets over-written below so value has to go into IT+1
  498 	DR(IT+1)=QINST(1)*TEMP     ! Mass conservative time step integrated flow mm^3/ts
		GO TO 4992
	ENDIF time0
*
*
***********************************************************************
*
*  Start loop on time steps
c      DO 50 IT=1,NDATA
*
*
*  Convert inputs to m per time step
	IT=ISTEP
      AE=PET*UNITS*crop_coeff   !raw 12-jan-2005 crop_coeff is crop factor (Kc)
      SUMpE=SUMpE+ae
      RIT=(R+rate_irrig)*UNITS   !(isub) !raw 12-jan-2005
	ad_cap = art_drainage*dt   ! DGT 6/28/05   Artificial drainage capacity in units per time step (m/day)
*
      QV=0.
	sumi=0.  ! RPI 22/3/2004 removed the subscript as unnecessary
C	srav=0.
c
c   Interception layer   DGT 3/98
!raw 12-jan-2005 of the irrig water, a fraction 1-fsprinkler bypasses the canopy and reaches the ground directly
!so don't apply all the water to the canopy top. add some of it to the thrufall instead
	cvs=cv
      call intercept(CV,RIT-(1-fsprinkler)*rate_irrig*UNITS,AE,DT,CC,
     +   CR,r1,UD)  ! DGT 8/18/05  multiplied rate_irrig by UNITS
	r1=r1+(1-fsprinkler)*rate_irrig*UNITS
	sumce=sumce+ae
	sumr1=sumr1+r1
c	write(21,*)'it,balcv,rit,cv,cvs,r1,ae', it,balcv,rit,cv,cvs,r1,ae
c  cv is interception state variable [m] (input and returned)
c  RIT is precipitation amount [m] (input)
c  AE is reference evaporation amount [m] on input, and upon return is net evap
c     from interception
c  DT is time step [hr] (input)
c  CC is interception capacity parameter [m] (input)
c  CR is interception evaporation enhancement factor parameter (input)
c  r1 net precipitation (throughfall + stemflow)
c  ud is unsatisfied transpiration demand that goes through to soil.
      r2 = r1 - ud   ! Net forcing on the soil zone.
C      sumr2=sumr2+r2
cRAW thinks some ET is missing from the above
c since r2=r1-ud, then the min(r1,ud) was already removed before soil
      sumsle=sumsle+dmin1(r1,ud)
	sume = ae+dmin1(r1,ud) ! RPI 22/3/2004 removed the subscript as unnecessary
c
c   basinwide soil zone calculations
      call soil(sr,r2,r3,rie,rd,srn,ZR,dth1,dth2,szf,
     &   ak0fzrdt,c,soilc,psif,.true.,ad_cap,ad_now)  ! DGT 6/28/05 last two arguments
c   calculate saturation thresholds
      atbsat = szf*zbar + lambda   !  Saturation threshold
c       Points with ln(a/tan b) greater than this are saturated
	atbinf = atbsat - szf * ZR   !  points with ln(a/tan b) greater than this
c       are 'influenced' by shallow water table

	nmin=2
c
c    Start loop on a/TtanB increments at 2 because each increment represents a bin from i-1 to i
c    This code assumes ln(a/tan b) values are in increasing order and NOT repeated
       ACIE=0.      !   Infiltration Excess Area
       ACSE=0.      !   Saturation Excess Area
       ROFEX=0.     !   Runoff Saturation Excess
	 ROFREX=0.    !   Runoff Infiltration Excess
c raw 12-jan-2005 impervious area
	  ROF = max(r2,0.)*FI
	  ROFREX=ROFREX+ROF
	  SUMIE = SUMIE + ROF
	  ACIE  = ACIE  + FI
	  sumad=0.   !  DGT 6/28/05 This is a variable to accumulate artificial drainage across classes

      DO 2020 I=nmin,NKA(ISUB)
c       Check for saturation
        if(atb(i-1,isub) .ge. atbsat)then
c          Saturated
          riei = 0.   !  No infiltration excess runoff
          IF(r2 .le. 0.)THEN   !  Potential ET greater than precip
c  To preserve mass balance since the soil is saturated any change in sr needs
c  to be added to saturated zone store.  Do this as a component (which may be negative)
c  of the soil zone drainage.
	      rdi=r2+sr-srn   ! ET demand taken from sat zone
	      rsei = 0.   !  No runoff
	      ets = -r2   !  ET from soil satisfies all ET demand
		  r3i=r2  ! RPI 3/5/01
	    else
	      rsei = r2
c		  rdi=sr-srn   ! CX
c RPI and RAW 24/4/01 prevent -ve seepage in this case, i.e., there is
c rainfall but no infiltration because saturated conditions
		  rdi = max(0.,sr-srn)   ! For mass balance change in soil moisture taken from sat zone

	      r3i=0.   ! No infiltration
	      ets = 0.
c      no actual evap this case because it is already taken care of in interception
 	    endif
!  DGT 6/28/05 Artificial drainage from saturated soil store is at capacity
          adi=ad_cap
		rdi=rdi-ad_cap   !  Keep mass balance right
c      Accumulate saturated area
		ACSE = ACSE + (1-FI)*PKA(I,ISUB)
	  else if(atb(i,isub) .le. atbinf) then
c        Not influenced by water table at all
c        Use deep water table values
	    r3i = r3
		riei = rie
		rdi = rd
	    rsei = 0.
	    ets = 0.
	    if(r3i .lt. 0.)ets = -r3i   !  ET
!  DGT 6/28/05 Artificial drainage at uninfluenced soil value
          adi=ad_now   ! No need to adjust rd because soil did it
	  else
c
c      Here part of the zone is either saturated or influenced by the water table
c
c        compute ln(a/tan b) limits
          asatl = min(atbsat,atb(i,isub))
	    ainfl = max(atbinf,atb(i-1,isub))
	    amidl = (asatl+ainfl)*0.5   !  Use midpoint a/tan b as representative
c        compute proportions in each category
          temp = 1./(atb(i,isub)-atb(i-1,isub))
          puninf = (ainfl-atb(i-1,isub))*temp
		pinf = (asatl - ainfl)*temp
		psat = (atb(i,isub) - asatl)*temp
c
c       Saturated part
c
          IF(r2 .le. 0.)THEN   !  Potential ET greater than precip
c  To preserve mass balance since the soil is saturated any change in sr needs
c  to be added to saturated zone store.  Do this as a component (which may be negative)
c  of the soil zone drainage.
	      rdi= psat *(r2+sr-srn)   ! ET demand in excess of soil zone demand taken from sat zone
	      rsei = 0.   !  No runoff
	      ets = -psat*r2   !  ET from soil satisfies all ET demand
		  r3i=r2*psat
	    else
	      rsei = psat*r2
c RPI and RAW 24/4/01 prevent -ve seepage in this case, i.e., there is
c rainfall but no infiltration because saturated conditions
		  rdi = max(0.,psat*(sr-srn))
c	      rdi = psat*(sr-srn)  !CX
	      ets = 0.
		  r3i=0.
c      no actual evap this case because it is already taken care of in interception
 	    endif
		ACSE = ACSE + psat*(1-FI)*PKA(I,ISUB)
!  DGT 6/28/05   Artificial drainage
          adi=ad_cap*psat
		rdi=rdi-ad_cap*psat
c
c    Influenced part
c    Calculate influenced category zi
          zi=ZBAR+(lambda-amidl)/SZF
          sri = sr + (ZR-zi)*(1.-sr/soilc)*DTH ! local enhancement of
c                soil moisture
	    call soil(sri,r2,r3itemp,riei,rdtemp,srni,ZR,dth1,dth2,szf,
     &   ak0fzrdt,c,soilc,psif, .false.,ad_cap,ad_now_i)  ! RPI 6/5/01 introduced r3ie
c     6/28/05   DGT introduced last two arguments for artificial drainage
c
c          The main use of the above call is to get
c           local infiltration or ET.  This is r3i, which is then used
c           with the global storage change to infer local sat zone addition
c           or withdrawal.  The concept of flux to a sat zone is suspended
c           here because the sat zone and soil zone have overlapped.  This
c           is for mass balance.
c           The rd returned from soil is not used.  Instead mass balance
c           is used to infer the local sat zone storage reallocation.
c RPI and RAW 24/4/01 prevent -ve seepage in this case, i.e., there is
c rainfall but no infiltration because saturated conditions
		rdtemp=pinf*(r3itemp+sr-srn)
		if(r2.gt.0.) rdtemp=max(0.,rdtemp)
c          rdi= rdi + pinf*(r3i+sr-srn)
          rdi= rdi + rdtemp
	    if(r3itemp .lt. 0.)ets = ets -pinf* r3itemp
		r3i=r3i+r3itemp*pinf  ! RPI 3/5/01 get weighted contribution
	    riei = pinf * riei
!  DGT 6/28/05 artificial drainage
          adi=ad_now_i*pinf
c
c    Uninfluenced part
c        Use deep water table values
		riei = riei+puninf*rie
		rdi = rdi+puninf*rd
	    if(r3 .lt. 0.)ets = ets -puninf*r3   !  ET
	    r3i = r3i+r3*puninf  ! RPI 3/5/01 get weighted contribution
        endif
c
c   summing contributions
c
	qv = qv + rdi * (1-FI)*PKA(i,isub)
	sumi = sumi + r3i * (1-FI)*PKA(i,isub) ! RPI 22/3/2004 removed the subscript as unnecessary
	sume = sume + ets *(1-FI)*PKA(i,isub)  ! RPI 22/3/2004 removed the subscript as unnecessary
	sumsle = sumsle + ets *(1-FI)*PKA(i,isub)
      IF(rsei.GE.0.)THEN
	  ROF=rsei*(1-FI)*PKA(I,ISUB)
	  ROFEX=ROFEX+ROF
	  SUMSE=SUMSE+ROF
	else
	write(21,*)'Saturation Excess less than 0 error, rsei&psat=',
     +  rsei,psat
      stop
      ENDIF
*
*  Case of infiltration excess
      IF(riei.ge.0.0) THEN
	  ROF = riei * (1-FI)*PKA(I,ISUB)
	  ROFREX=ROFREX+ROF
	  SUMIE = SUMIE + ROF
* RPI reintroduced the next  1 lines for infil. excess mechanism 13/1/94
	  if(riei .gt. 0.)ACIE  = ACIE  + (1-FI)*PKA(I,ISUB)
	else
	  write(21,*)'Infiltration excess less than 0 error'
      ENDIF
!  DGT 6/28/05   Artificial drainage
      sumad=sumad+adi*(1-FI)*PKA(I,ISUB)

      IF ( (MODWRT) .AND. ((IT.GE.MPS) .AND. (IT.LE.MPE))) THEN
cRAW 17/4/98 we don;t have z(i) anymore, so just calculate at midpoint
      WRITE(LUNDAT,5000) ISUB,IT,I,ATB(I,ISUB),
     1  ZBAR+(lambda-(ATB(I-1,ISUB)+atb(i,isub))*.5)/SZF,rsei,riei
 5000 FORMAT(1X,3I4,5D13.3)
      ENDIF
CRAW Landcare, surfrunoff in um/timestep (rsei is in m),
CRAW soilwetness is saturated depth as a fraction of soil depth = (ZR-ZI)/ZR = 1-ZI/ZR
		surfrunoff(i)=min((rsei+riei)*1.d6,I4MAX)+.5
		soilwetness(i)=min(1.,max(0.,1. - (ZBAR
     *	+(lambda-(ATB(I-1,ISUB)+atb(i,isub))*.5)/SZF)/ZR))
cDEBUG
c      if(isub .eq. 87 .and. ipflag .eq. 1 .and. istep .eq. 245)then
c	  write(3284,*)i,r3i,ets
c	endif
 2020 CONTINUE
      if (modwrt .and. mpe.eq.-1) then
		if (it .eq. 1) write(lundat,*)isub,NDATA,nka !write a header for this subcatchment
		call td81micdh(ymd,hms,tim)
            write(lundat,'(1x)')
            do 2000 i=1,1+nka(isub)
 2000           write(lundat,'(i8)') ymd
            do 2001 i=1,nka(isub)
 2001           write(lundat,'(f6.2)') surfrunoff(i)
            do 2002 i=1,nka(isub)
 2002           write(lundat,'(f6.2)') soilwetness(i)
		if(isub.eq.1)	tim=tim+interv
     	endif
c RPI and RAW 24/4/01 have prevented -ve seepage so now
c we must calculate the correct soil moisture value to conserve mass
	srn=sr+sumi-qv-sumad !  DGT 6/28/05 added artificial drainage effect
CDEBUG
c      if(ipflag .eq. 1)then
c	   write(3284,'(1x,i5,7e14.5)')istep,r2,sumi,sume,sr,srn,qv,sumad
c	endif
c	srn=sr+sumi-qv ! RPI 22/3/2004 removed the subscript as unnecessary
c
c    update soil zone store for next iteration
c
      sr = srn
	ROFF=ROFEX+ROFREX+sumad   !  dgt 6/28/05  Adding artificial drainage to runoff
c	ROFF=ROFEX+ROFREX   !  Total runoff
*      IF ( MODWRT .AND. (ISUB .EQ. IPSUB) ) THEN
	IF ( MODWRT ) THEN
		  IRR(7,1) = MIN( ROFF*1D6, I4MAX ) +.5 !um/timestep
		  rirr(7,1)=roff*1000.   ! units for rirr are mm wherever possible
      ENDIF

C   What follows is an analytic integral of
C     d(DTH1*ZBAR)/dt = -QV + Q0 exp(-SZF * Zbar)
c    over a unit time step (because QV and Q0 are both per time step
c    Then base flow qb is obtained from the implied storage difference
c    In this derivation zbar can be negative - therefore it makes no
c    physical sense to try interpret it in terms of the soil zone issues.
c    dth1 is simply a multiplying factor that scales saturated zone storage.
c
CRAW  QB=Q0*DEXP(-SZF*ZBAR)
CRAW  ZBAR=ZBAR-(QV-QB)/DTH1

c      TEMP=DEXP( MIN(SZF*ZBAR, 100D0 ) )
c      IF ( QV .GT. Q0*1D-6 ) THEN
c	    TEMP1=DEXP( DMAX1(-QV*SZF/DTH1, -100D0 ) )
c	    ZBARN = LOG( Q0/QV + TEMP1*(TEMP-Q0/QV) ) / SZF
c      ELSE
c	    ZBARN = LOG( TEMP + Q0*SZF/DTH1 ) / SZF
c      ENDIF
c
c   DGT revised following careful analysis in notebook 4-1-98
	if(max(zbar*szf,zbar*szf-qv/dth1) .gt. logoqm)then
c  Forward difference
	  zbarn=zbar+(-qv+q0*exp(-szf*zbar))/dth1
	else
	  if(abs(qv) .gt. qvmin)then
c   Use non zero qv solution
	    zbarn=log(q0/qv+exp(-qv*szf/dth1)*(exp(szf*zbar)-
     +                                                 q0/qv))/szf
	  else
c    Use zero qv solution
		 zbarn=log(exp(szf*zbar)+q0*szf/dth1)/szf
	  endif
	endif
	zbarn = zbar_in
	sumqv=sumqv+qv
      QB = QV + (ZBARN-ZBAR)*DTH1
c       write(51,592) it,isub,zbar,zbarn,dth1,qb,qv
c 592   format(1x,2i3,5f12.6)
      ZBAR = ZBARN
C
      SUMQ=SUMQ+QB+ROFF
      SUMR=SUMR+RIT
      SUMQB=SUMQB+QB
      qlat_mm=(qb+roff)/units
C  DGT changed below to get units right
*  Convert runoff back to input data units
      QLAT=(QB+ROFF)/UNITS*AREA/interv   ! mm^3/sec
C Calculate instantaneous flow for reach routing - RPI
!!      QLATIN=(Q0*DEXP(-SZF*ZBARN)+ROFF)/UNITS*AREA/interv   ! mm^3/sec
      QLATIN=(QB)/UNITS*AREA/interv   ! mm^3/sec
	sumad=sumad/units*area/interv   ! DGT 6/28/05  - put in vol units

*
*  Use time delay histogram to route runoff (assumed known)
C RPI 31/7/2002 for neq=0 purposes we have to let this convolution go
C beyond M values to M+NTDH values
      DO 499 ITDH=1,NTDH(ISUB)
      	DR(ITDH)=DR(ITDH)+QLAT*TDH(ITDH,ISUB)
     		QINST(ITDH)=QINST(ITDH)+QLATIN*TDH(ITDH,ISUB)
  499 CONTINUE
	QINST_OUT=QINST(1) ! rpi 16/7/2003 - needed to get at QINST
	DR_OUT=DR(1)  ! DGT 6/10/05 to get at DR
*
      SUMAE=SUMAE+SUME
*
* RPI reintroduced the next  1 lines for infil. excess mechanism 13/1/94
       IF(ACIE.GT.ACIEM)ACIEM=ACIE
       IF(ACSE.GT.ACSEM)ACSEM=ACSE
       IF(ZBM.GT.ZBAR) ZBM=ZBAR
C
*
*  End of time step loop
C
C
!   DGT 8/17/05  Add basin_evap so as not to have to use irr for this
      evap_mm=sume*1.d3   ! This is in mm
c      IF ( MODWRT .AND. (ISUB .EQ. IPSUB)) THEN
	IF ( MODWRT ) THEN
C RPI 20/6/2002 added the following messages - RPI 16/7/2003 thinks this needs altering to
c		IF(ISR0msg.gt.10 .and. (it.eq.ndata.and.isub.eq.maxslp)) - ditto CV0 meesge
		IF(ISR0msg.gt.10 .and. (it.eq.1.and.isub.eq.1))
     +       write(21,113) ISR0msg
 113	FORMAT(' ***** Message about SR0 exceeding SOILC exceeded a
     +     total',' of ',I6,' times')
		IF(ICV0msg.gt.10.and. (it.eq.1.and.isub.eq.1))
     +       write(21,112) ICV0msg
 112	FORMAT(' ***** Message about CV0 exceeding CC exceeded a
     +      total',' of ',I6,' times')
		IRR(1,1)=IT
CRAW Fluxes to go into IRR as um/hr
CRAW need 1d3/AREA for IRR(2,) since DR is in mm^3/int, not m/int like QV
cDGT DR is now in mm^3/sec therefore need interv which is time step in seconds
          IRR(2,1)=DMIN1( DR(1)*1d3/(AREA)*interv,
     1      I4MAX ) +.5  ! RPI 16/7/2003 changed IT to 1 in DR() um/timestep
	    rirr(2,1)=dr(1)*interv/area   ! in mm
CRPI ADDED THE FOLLOWING 3 LINES - SEE ABOVE
	    IRR(3,1)=MIN( QB*1.D6, I4MAX ) +.5 !um/timestep
		rirr(3,1)=qb*1000.  ! mm
	    IRR(4,1)=MIN( QV*1.D6, I4MAX) +.5 !um/timestep
		rirr(4,1)=qv*1000. ! mm
	    IRR(5,1)=MIN( ROFEX*1.D6, I4MAX ) +.5 !um/timestep
	    rirr(5,1)=rofex*1000. ! mm
	    IRR(6,1)=MIN( ROFREX*1.D6, I4MAX ) +.5 !um/timestep
		rirr(6,1)=rofrex*1000. !mm
	    IRR(8,1)=ACIE*100. !percentage
		rirr(8,1)=acie*100.
	    IRR(9,1)=ACSE*100. !percentage
		rirr(9,1)=acse*100.
	    IRR(11,1)=MIN( ZBAR*1.D6, I4MAX) +.5 !micrometre
		rirr(11,1)=zbar*1000.  !mm
CRAW revised next two lines, since CV and SR replace SRZ and SUZ 17/4/98
	    IRR(10,1)=MIN( CV*1.D6, I4MAX) +.5 !micrometre
		rirr(10,1)=cv*1000.  !mm
	    IRR(12,1)=MIN( SR*1.D6, I4MAX) +.5 !micrometre
		rirr(12,1)=sr*1000. !mm
		irr(13,1)=min(PET*units*1.D6, I4MAX) +.5   ! DGT added PET !um/timestep
		rirr(13,1)=pet   ! mm
		irr(14,1)=min(sume*1.d6, i4max)+.5   ! DGT added actual ET ! RPI 22/3/2004 removed the subscript as unnecessary
		rirr(14,1)=sume*1000. ! mm
	       S1 = - ZBAR*DTH1 + SR + CV
             BAL = SUMR - SUMQ - SUMAE+(S0 - S1)
	    irr(15,1)=min(bal*1.d9,i4max)+.5 !DGT mass balance discrepancy
c            in units of m x e-9, i.e. 1000 ths of mm.
		rirr(15,1)=bal*1000. !mm

		if ( idebug .gt. 0 ) then
	       WRITE(21,*)'it=',it,'WATER BALANCE: BAL=',BAL,
     +                                            ' SUMR=',SUMR
             write(21,*)'ISUB,BAL,SUMR , s1, s0, SUMQ , SUMAE '
             write(21,*) ISUB,BAL,SUMR , s1, s0,
     +									 SUMQ , SUMAE
	       balcv=sumr-((cv-cv0)+sumr1+sumce)
	       WRITE(21,*)'ISUB,balcv,sumr,cv,cv0,sumr1,sumce'
             write(21,*) ISUB,balcv,sumr,cv,cv0,
     +										sumr1,sumce
	       balsoil=sumr1-((sr-sr0)+sumqv+
     +							sumse+sumie+sumsle)
	       WRITE(21,*)'ISUB,balsoil,sumr1,sr,sr0,sumsle'
             write(21,*) ISUB,balsoil,sumr1,sr,
     +							sr0,sumsle
	       balsat=sumqv-(-(zbar-zbar0)*dth1+sumqb)
	       WRITE(21,*)'balsat,sumqv,-(zbar-zbar0)*dth1,sumqb'
	       WRITE(21,*) balsat,sumqv,-(zbar-zbar0)*dth1,
     +												sumqb
             write(21,*) ' '
	    IF ( DABS(BAL) .GT. (SUMR+SUMQ)*1D-3 ) THEN
c can't write to lunco1, so put it in toperror.txt
	       WRITE(21,*)'WATER BALANCE: BAL=',BAL,' SUMR=',SUMR,
     +					' SUMQ=',SUMQ,' IT=',IT,' ISUB=',ISUB   ! RPI 2/5/03 added ISUB
		ENDIF
	    endif
      ENDIF
C RPI 16/7/2003 put in this test for the re-arranged version of TOPNET
C These stmts "push down" the contents of the DR and QINST arrays so that
C they don't grow beyond the size of the Time-Delay-Histogram.
4992	IF (NTDH(ISUB).GT.1) THEN
		DO JTDH=1,NTDH(ISUB)-1
			DR(JTDH)=DR(JTDH+1)
			QINST(JTDH)=QINST(JTDH+1)
		ENDDO
		DR(NTDH(ISUB))=0.
		QINST(NTDH(ISUB))=0.
	ELSE
			DR(1)=0.
			QINST(1)=0.
	ENDIF
c   50 CONTINUE
 990  format(1x,i5,i14/1x,5d13.6/1x,4d13.6/1x,11i7)
!RAW 23-Mar-2005 now we do this back in CALCTS
cDGT changed info output from lunmod to lunp
!	finish: IF(IT.EQ.NDATA) THEN
!      IF(MODWRT) WRITE(LUNp,*)'MIN ZBAR=',ZBM,
!     !+                               ' MAX CONT AREA=',ACSEM
!      IF(MODWRT) WRITE(LUNp,*)'LAMBDA=',lambda
!*
!*
!*-------------------------------------------------------------
!*  Water Balance calculations
!*  Storage at the end
!      S1 = - ZBAR*DTH1 + SR + CV
!      BAL = SUMR - SUMQ - SUMAE + ( S0 - S1 )
!      IF ( MODWRT ) THEN
!	    WRITE(LUNp,*)'WATER BALANCE: BAL=',BAL,' SUMR=',SUMR
!	    IF ( DABS(BAL) .GT. (SUMR+SUMQ)*1D-3 ) THEN
!c can't write to lunco1, so put it in toperror.txt
!	       WRITE(21,*)'WATER BALANCE: BAL=',BAL,' SUMR=',SUMR,
!     +					' SUMQ=',SUMQ,' IT=',IT
!	    ENDIF
!	    WRITE(LUNp,'()')
!	    WRITE(LUNp,110)SUMR/UNITS,SUMAE/UNITS
!110       FORMAT(' Rainfall    =',G12.5,' Total Evap  =',G12.5/)
!	    WRITE(LUNp,115)SUMQ/UNITS,sumpe/units
!115       FORMAT(' Model Runoff=',G12.5,' Total PET =',g12.5/)
!	    WRITE(LUNp,120)(S1-S0)/UNITS
!120       FORMAT(' Incr.Storage=',G12.5/)
!      ENDIF
!	ENDIF finish
*-------------------------------------------------------------
      RETURN
      END
*
c**********************************************************************
c   DGT's subroutine for infiltration.
c **********************************************************************
      subroutine soil(sr,r2,r3,rie,rd,srn,zr,dth1,dth2,szf,
     &   ak0fzrdt,c,soilc,psif,global,ad_cap,ad_now)   ! DGT 6/28/05  added last two arguments
c  Inputs are
c    sr  soil storage state variable
c    r2  surface forcing (precip-ET)
c    ad_cap   artificial drainage capacity
c  Outputs are
c    r3  surface exchange that occurs limited by soil moisture and infiltration capacity
c    rie infiltration excess runoff
c    rd  recharge to saturated zone
c    srn  updated soil storage
c    ad_now    artificial drainage that occurs limited by availability above field capacity
c  Other arguments are parameters

      IMPLICIT NONE

	REAL*8 TEMP,SR,ZR,DTH2,SRD,DTH1,RD,AK0FZRDT,C
	REAL*8 RDMAX,R2,F,SOILC,ZF,SZF,PSIF,R3,RIE,SRE,SRN,X1
	REAL*8 CV,R1,RIT,UD,AE,S,DT,DELT,P,X2,CR,E,A,B,TT,SN,R
	real*8 ad_cap,ad_now

      logical global

	temp=sr-zr*dth2
c	write(21,*)temp,sr
	if(temp .gt. 0D0)then
	  srd=temp/(zr*dth1)
        rd = ak0fzrdt * srd **c !  + art_drainage !raw 12-jan-2005 DGT 6/28/05  Drainage does not go to sat store
c   Do not drain more than is in drainable capacity + input
        rdmax = temp + max(r2,0.0)
	  rd=min(rdmax,rd)
!  DGT 6/28/05
        ad_now=min(ad_cap,rdmax-rd)  ! Drainage is limited by capacity or supply
	else
c	  srd=0.   ! not used below
	  rd=0.
	  ad_now=0.   !  DGT 6/28/05
	endif
	if(r2 .gt. 0.)then
	  f = soilc -sr+rd+ad_now   !  DGT 6/28/05
	  zf = sr/(dth1+dth2)
	  if(zf .gt. 0.)
     &     f = min(ak0fzrdt*exp(szf*(zr-zf))*(zf+psif)/zf,f)  !  max depth that can
c            be added to root zone
        r3 = min(f,r2)   !  actual depth added
	  rie = r2 - r3    ! infiltration excess runoff generated
	else
	  sre = min(1.,sr/(zr*dth2))  !  relative plant available saturation
	  r3 = sre*r2
	  rie = 0.
	  if(global .and. r3 .lt. -sr)r3 = -sr  !  smallest negative number.
c      cannot evap more than is there in global case
c      In local case, i.e. where water table is near to the surface we assume
c      that due to lateral interflow that sr is not limiting ET.
	endif
	srn = sr + r3 - rd-ad_now  !  DGT 6/28/05
c   checks to correct for overshooting.  Reduce ad_now first
      if(srn .lt. 0.)then
		if(-srn .gt. ad_now)then
		  ad_now=0.
		else
			ad_now=ad_now+srn
		endif
	endif
	srn = sr + r3 - rd-ad_now
	if(srn .lt. 0)then   ! may go negative due to big rd.
	  rd=rd+srn   !  reduce rd accordingly
	  srn = 0.
	endif
	return
	end
***********************************************************************
*     SUBROUTINE INTERCEPT
************************************************************************
      subroutine intercept(cv,RIT,AE,DelT,x1,CR,r1,UD)
c	implicit real*8 (a-h,o-z)
      IMPLICIT NONE  ! 30/03/2003  ITB
      real *8 ii

      REAL*8 X1,CV,R1,RIT,UD,AE,S,DT,DELT,P,X2 ! 30/03/2004 ITB
	REAL*8 CR,E,A,B,TT,SN,R ! 30/03/2004 ITB

c  Subroutine to implement nonlinear interception model
c
c  cv is interception state variable [m] (input and returned)
c  RIT is precipitation amount [m] (input)
c  AE is reference evaporation amount [m] on input, and upon return is evap
c     from interception accumulated over time step
c  DT is time step [hr] (input)
c  CC = x1 is interception capacity parameter [m] (input)
c  CR is interception evaporation enhancement factor parameter (input)
c  r1 net precipitation (throughfall + stemflow)
c  ud is unsatisfied transpiration demand that goes through to soil.

!  For the theory see TopNetInterceptionDerivation.docx last saved in
!   C:\Users\dtarb\Dave\Projects\WRIA1_Water_Budget_Christina\TopNetModel\Documents
!   DGT 5/28/12
      if(x1 .le. 0.) then   ! case of no interception capacity
	  cv=0.
	  r1=rit
	  ud=ae
	  ae=0.
	return
	endif
c  Normalize
	s = cv/x1
	dt=delt
	p = rit/(x1*dt)
c RPI 20/6/2002 put in the "check" on CR to avoid generation of NaNs
	x2=x1/max(cr,1.e-8)
	e = ae/(x2*dt)
	if((p + e)*dt .gt. 10d-9) then
		dt = dt*(p + e)
		if(e .gt. p*10d-8) then
c General case
c Compute roots
			a = 1. + sqrt(e/(p + e))
			b = 2. - a
c			b = 1 - sqrt(e/(p + e))
c  The choice of a as larger root >1 is important for
c  next step to prevent possibility
c  of divide by zero error when s = a.
			tt = (s - b)/(a - s) * exp( - dt*(a - b))
			sn = (b + a * tt)/(1 + tt)
		else
c Special case for e=0
			sn = ((1 - s) * dt + s)/((1 - s) * dt + 1)
		endif
		dt = dt / (p + e)
! Now unnormalize
		sn = sn * x1
		s = s * x1
		p = p * x1
		e = e * x2
! Integral of f(S) by mass balance
		ii = (s + p * dt - sn)/(p + x1/x2 * e)
		r = p * ii
		ae = x1/x2 * e * ii
		ud = e * (dt - ii)
	else
c The case when p and e are both 0 is trivial
c  no change in s
		sn = s*x1
		r = 0.
		ae = 0.
		ud = 0.
	endif
c   Values to return
	cv=sn
	r1= r
	return
      end

	subroutine irrigation(SR,SP,JS,INTERV,MAXSLP,Dapp)
	INCLUDE 'maxvariables.inc'
      INCLUDE 'Tdims_v7.inc'
	REAL*8 SP(NSP,MAXSLP)
	REAL*8 SR,Dapp
	REAL*8 dthres, dgoal,ieff,zmax
	INTEGER JS
	integer*8 INTERV ! DGT 5/14/12
c
c  DGT  8/16/05  Decided to try to reinterpret dthres and dgoal as threshold soil moisture fractions
c  of the plant available water.  So when soil moisture content is less than dthresh*DTH2 irrigation starts
c  Irrigation does not apply water that would make the soil wetter than dgoal*dth2.
      ZR=SP(6,JS) !metres
c	DTH1=SP(4,js)
      DTH2=SP(5,js)
c	DTH=dth1+dth2

	dthres= SP(23,JS)  ! 0.7  !  min(SP(23,JS),soilc) !metres   DGT 8/16/05 changed to fraction of plant available soil water
	dgoal= SP(25,JS)   ! 0.95  !  min(SP(25,JS),soilc) !metres  DGT 8/16/05 changed to fraction
	ieff=SP(22,JS) !dimensionless
	zmax=SP(24,JS) !metres/day
c		 21 - SprinklerFractionOfIrrigation
c		 22 - IrrigationEfficiency
c		 23 - D_Thresh
c		 24 - Z_Max
c		 25 - D_Goal
c		 26-37 - Kc_Jan through to Kc_Dec
c	if((soilc-sr).gt.dthres) then
c		Dapp=min( ((soilc-sr)-dgoal)/ieff,
c	1				zmax*float(interv)/(3600.*24.)) !m/timestep = m/d * s/timestep / (s/day)
c      if(sr .lt. dthres*zr*dth2)then   ! DTH2 is the plant available water
c	    Dapp=min((dgoal*zr*dth2-sr)/ieff,
c     1                zmax*float(interv)/(3600.*24.))  !m/timestep = m/d * s/timestep / (s/day)
c   DGT 8/19/05 Gradual approach more commensurate with irrigation of larger areas
c   Demand 'Dapp' increases linearly from 0 at dgoal to zmax at dt
      theta=sr/zr  ! soil moisture content
	thetagoal=dgoal*dth2
	thetathresh=dthres*dth2
	if(theta .lt. thetagoal)then
	    Dapp=min((thetagoal-theta)/(max(thetagoal-thetathresh,1e-6))      ! the 1e-6 included to avoid divide by 0
	1                                                            *zmax,
     1                zmax*float(interv)/(3600.*24.))  !m/timestep = m/d * s/timestep / (s/day)
	else
		Dapp=0
	endif

	return
	end
