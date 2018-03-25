C This version, V8, has had extra checking of rainfall sites added plus,
C input for lakes.
C V9 also has checking/matching of the flow sites with reaches - RPI 17/9/01
C RPI added code to do with ishift0 on 16/7/2002
C
C
      SUBROUTINE MDDATA(NGAUGE,NS,NRCH,NKA,TL,ATB,PKA,ND,CL2,PD2,UNITS,
     *         LL,NTR,NTS,LINKS,LINKR,SI,SP,RP,IRET,pmap,npar,lrg,wrg,
     *         iex,llout,neq,nout,nbout,ibout,nrchsav,qmap,rel,relflag,
     *         minsp,maxsp,minsi,maxsi,minrp,maxrp,limitc,clinkr,kllout,
     *		 ishift0,MAXINT,MAXGAUGE,MAXSLP,MAXRESPONSE,MAXA,MAXC,
     *		 MAXCHN,MAXRCHAREAS,MAXSITES,bp,
     *         bxlat,bxlon,wrg1)  ! 7/23/05  DGT added bxlat, bxlon, wrg1 ! DGT 7/24/05 WRG1 Interpolation weights
C RPI 12/8/02 changed limits to limitc to preserve setting for model constraints
C *********************************************************************
C  This subroutine represents an amalgamation of subroutines READPS and
C  SET_LINKR . It reads the disk file containing the hydrological data
C  and returns the data in several arrays and variables.
C
C      Variable list :-
C  a)  Variables received ( via the include file 'Tdims_v7.inc' )
C      ------------------
C  MAXSLP -- allowable number of subbasins
C  MAXCHN --   "      "    "  reaches.
C
C  b)  Variables and arrays returned
C      -----------------------------
C  NGAUGE -- number of raingauges used.
C  NS -- actual number of subbasins.
C  NRCH --  "     "    "  reaches.
C  LL(MAXCHN) -- pointer giving the order for processing all reaches.
C  NTR(MAXCHN) -- no. of times this reach is used as input to another
c                 reach.
C  NTS(MAXSLP) -- no. of times this basin is used
C  LINKS(2,MAXSLP) -- array containing basin-raingauge link data ,
C    i.e. LINKS(1,I) = arbitrary numbering (must be between 1 and NS).
C         LINKS(2,I) = no. of the raingauge to be used for this basin.
C                       (must be between 1 and NGAUGE)
C
C  LINKR(4,MAXCHN) -- array containing reach network information ,
C    i.e. LINKR(1,I) = arbitrary numbering (must be greater than NS).
C         LINKR(2,I) = basin or upstream reach feeding into this reach.
C         LINKR(3,I) = second basin or upstream reach feeding into this
C                      reach.
C         LINKR(4,I) = number of basins or upstream reaches feeding into
C                      this reach.
C
C  SI(NSI,MAXSLP) -- basin initial conditions ,
C  minsi(NSI) -- lower limit of basin initial condition
C  maxsi(NSI) -- upper limit of basin initial condition
C
C  SP(NSP,MAXSLP) -- basin properties , Only ISP actually used.
C  minsp(NSP) -- lower limit of basin properties
C  maxsp(NSP) -- upper limit of basin properties
C
C  RP(NSP,MAXCHN) -- reach properties ,
C    i.e. RP(1,I) = basin.
C         RP(2,I) = Manning's n.
C         RP(3,I) = width .
C         RP(4,I) = length
C  minrp(NRP) -- lower limit of reach properties
C  maxrp(NRP) -- upper limit of reach properties
c .
C  PAR(4,dpm) -- Parameter map.  Only npar actually used.
c   This controls the mapping of subbasin properties that may be
c   controlled by an optimizer.
c
c  LRG(MAXSLP,MAXGAUGE), WRG(MAXSLP,MAXGAUGE)
c             -- Extended basin-raingauge link data
C    i.e. LRG(1,I) = raingauge numbers to be used for sub-basin I.
C         WRG(1,I) = raingauge weight to be used for
c                      raingauge LRG(1,I) on sub-basin I
C limitc = true if a file of constraint values is to be used, false otherwise
c
c DGT additions 5/26/98.
c   llresponse(maxchn) - vector of reaches whose flow will be used as response time series
c   llout(maxchn) - vector of reaches whose flow is to be output

C
C              Author : D.C.K. Leong
C              Date : 15th November 1988.
C
C  IRET -- This is 4 if user presses ESCAPE
C *********************************************************************
C
C  Passed variables
C  ****************
      IMPLICIT NONE ! 30/03/2004 ITB


	INCLUDE 'maxvariables.inc'
      INCLUDE  'Luns.inc'
      INCLUDE  'Tdims_v7.inc'

      INTEGER NGAUGE,NRCH,NS,N0RCH,NRCHSAV
      INTEGER LL(MAXCHN),NTR(MAXCHN),llout(MAXCHN)
	INTEGER NTS(MAXSLP),ireach(MAXSLP)
	INTEGER KLLOUT(maxresponse) ! RPI 4/10/00
      INTEGER LINKR(4,MAXCHN)
	INTEGER pmap(4,dpm)
	INTEGER LINKS(2,MAXSLP)
	integer clinkr(3,maxchn) ! a copy of linkr before DGT gets at it
      INTEGER IRET,nbout
	INTEGER LRG(MAXSLP,MAXGAUGE),ibout(MAXSLP)

      REAL*8 SI(NSI,MAXSLP),SP(NSP,MAXSLP)
	REAL*8 RP(NRP,maxchn)
	REAL*8 minsp(nsp),maxsp(nsp),minsi(nsi),maxsi(nsi)
	REAL*8 minrp(nrp),maxrp(nrp)
	real*8 bp(num_basinpars,MAXSLP)

      INTEGER NKA(MAXSLP),ND(MAXSLP),IMATCH

      REAL*8 ATB(MAXA,MAXSLP),PKA(MAXA,MAXSLP),TL(MAXSLP),TEMP
      REAL*8 PD2(MAXC,MAXSLP),CL2(MAXC,MAXSLP)
	REAL*8 rtemp1,rtemp2,ptemp1,ptemp2,dummy,UNITS
	REAL*8 WRG(MAXSLP,MAXGAUGE), WRG1(MAXSLP,MAXGAUGE)  ! DGT 7/24/05 WRG1 Interpolation weights
	INTEGER rchno(maxrchareas)
! RPI 21/11/2002 changed dimension of qsite1 from maxresponse to match rel,ishift0 etc.
	INTEGER qsite1(maxrchareas),qsite2(maxresponse)   ! RPI 18/9/01
	INTEGER qeast(maxrchareas),qnorth(maxrchareas),qsite(maxrchareas)  ! RPI 18/9/01
	INTEGER relflag ! RPI 19/9/01 for use in forecast updating
! RPI 19/9/01 REL are the weight values for forecasting
! RPI 26/6/2002 - need a temporary variable
	real*4 rel(maxrchareas),reltemp(maxrchareas)
	integer ishift0(maxrchareas),ishift0temp(maxrchareas)
! DGT 7/23/05  Latitude and longitude done here rather than Hydata because here we have basin coords
	real*8 bxlat(MAXSLP),bxlon(MAXSLP),flat,clat,flong,clong

C  Local variables
C  ***************
C     INTEGER I,J,II,JJ,LINE,MAXG,N1RCH,ICHECK,LF,LEN
      INTEGER I,J,II,JJ,LINE,MAXG,N1RCH,ICHECK,npar
	INTEGER qmap(maxresponse)
      INTEGER LLN(MAXCHN)
	INTEGER NTSG(MAXSLP,MAXCHN),itbout(maxslp)

	integer iex,neq,nout
	CHARACTER*4 verno,ver_msp
C      INTEGER ILIN,LANS
CNE                                             /BYTE/CHARACTER/
c      CHARACTER QN(80),DF(32),FIL(32),STR(80)
c      CHARACTER EQQN*80,EQDF*32,EQFIL*32,EQSTR*80
	CHARACTER*20 FNAME
      LOGICAL*4 limits, rchareas  ! RPI 2/11/01 added rchareas
      LOGICAL*4 limitc ! RPI 12/8/2002 introduced this to avoid confusion with "limits"
      LOGICAL*1 ALLZER,US1,US2,HIGH,OK
	INTEGER*4 SITES(maxsites)  ! RPI 7/8/02 changed this to MAXSITES from MAX_NTRI
	REAL*4 RAIN_FACTOR           ! RPI 8/7/2002

      INTEGER*4  ISP,ISUM,NTRI,K  ! 30/03/2004 ITB
      INTEGER*4 MAXRNO, IERROR,NRCHNO, KRCH,NEQ_TEMP, NEQ_CHK ! 30/03/2004 ITB
      INTEGER*4 NGBASE ! 30/03/2004 ITB

      COMMON /RAIN1/ RAIN_FACTOR   ! RPI 8/7/2002
C RPI added this because we now need the value of MAXRNO in CALCTS
	COMMON /MDDATA1/ MAXRNO

C  ************************************************
	OPEN(UNIT=LUNRD,FILE='modelspc.dat',STATUS='OLD',ERR=280)
	open(unit=73,FILE='interpweight.dat',STATUS='OLD',ERR=280)  ! DGT 7/24/05 Interpolation weights
	READ(73,'()')            ! Comment line

c   We need a separate grid of interpolation weights for temperature etc because precip weights
c   do not work for temperature

	READ(LUNRD,*) ver_msp   ! RPI added 29/8/00

      READ(LUNRD,'()')            ! Comment line
      LINE = 1
C  Read no. of raingauges , basins and reaches used
C RPI 8/7/2002 RAIN_FACTOR is the multiplier to be applied to meso-scale rainfall
	RAIN_FACTOR=1.0   ! Set default						  RPI 8/7/2002
	IF(ver_msp.eq.'Ver2' .or. ver_msp.eq.'ver2') then   ! RPI 8/7/2002
		READ(LUNRD,*,ERR=280) NGAUGE,NS,NRCH,RAIN_FACTOR    ! RPI 8/7/2002
C check RAIN_FACTOR for reasonableness						! RPI 8/7/2002
      IF(RAIN_FACTOR.LT.0.5 .or. RAIN_FACTOR.GT.4.0) THEN ! RPI 8/7/2002 !23/1/2003 changed 2 to 4
		WRITE(21,275) RAIN_FACTOR							! RPI 8/7/2002
275	FORMAT(' ***** Your rainfall multiplier, 4th number on'/ ! RPI 8/7/2002
     &       'the 3rd line of MODELSPC.DAT, is unreasonable.'/ ! RPI 8/7/2002
     &	   'Its value is', F10.2)							! RPI 8/7/2002
	    STOP
		ENDIF												! RPI 8/7/2002
	ELSE												! RPI 8/7/2002
		READ(LUNRD,*,ERR=280) NGAUGE,NS,NRCH
	ENDIF
c	write(6,5090)NGAUGE,NS,NRCH,NKA,ND,LL,NTR,NTS,LINKS,LINKR
      IF(NGAUGE.GT.MAXGAUGE) THEN
	WRITE(21,22)MAXGAUGE
   22   FORMAT(' Number of raingauges exceeds maximum of ',I3)
	GOTO 290
      ENDIF
      IF( NRCH .GT. MAXCHN ) THEN
	WRITE(21,24)MAXCHN
   24   FORMAT(' Number of reaches exceeds maximum of ',I3)
	GOTO 290
      ENDIF
      IF ( NS .GT. MAXSLP ) THEN
	WRITE(21,26)MAXSLP
   26   FORMAT(' Number of basins exceeds maximum of ',I3)
	GOTO 290
      ENDIF
c   DGT read number of basin properties and parameters
	READ(LUNRD,*)    !  Comment line
	read(lunrd,*)isp,npar    !  isp is number of slope properties
	if (isp .gt. nsp) then
	  write(21,*)'Number of basin properties exceeds maximum of ',nsp
	  go to 290
	endif
	if (npar .gt. dpm) then
	  write(21,*)'Number of parameters exceeds maximum of ',dpm
	  go to 290
	endif
c   DGT moved two lines below from later to be logically before
c   all element properties repeat
      READ(LUNRD,'()')               ! Comment line
	READ(LUNRD,*,ERR=280) UNITS
c   DGT Read parameter map here before all element properties repeat
c DGT modified below to include variable length parameter map
	do 7711 i=1,6
 7711	  read(LUNRD,*)    ! 6 comment lines
	do 7722 i=1,npar
	read(lunrd,*,err=296,end=296) (pmap(j,i),j=1,4)
c      checks to ensure valid parameter map values
	  if(pmap(1,i) .eq. 1)then
!	    if(pmap(2,i) .le. 0 .or. pmap(2,i) .gt. isp)go to 296
	  elseif(pmap(1,i) .eq. 2)then
	    if(pmap(2,i) .le. 0 .or. pmap(2,i) .gt. nrp)go to 296
	  elseif(pmap(1,i) .eq. 3)then
	    if(pmap(2,i) .le. 0 .or. pmap(2,i) .gt. nsi)go to 296
	  else
	    go to 296
	  endif
c	write(6,7711) (pmap(j,i),j=1,4)
c 7711	format(' pmap', 4i10)
 7722	continue
c check that sufficient and correct values given for pmap
	isum=0
	do 7733 i=1,npar
	isum=isum+pmap(4,i)
 7733	continue
	if(isum.ne.(npar*(npar+1)/2)) go to 296
c   DGT  modified above for general number of parameters
c
C  Read and check basin-raingauge data
C  ***********************************
      READ(LUNRD,'()')               ! Comment line
c   end DGT modifications
C USE LLN TEMPORARILY TO CHECK THAT ALL BASINS HAVE GOT A RAINGAUGE
      DO 28 I=1,NS
  28    LLN(I)=0
C
      MAXG = 0
C get raingauge numbers from rain.dat and set up mapping to column #s RPI & RAW 25/8/00
	INQUIRE(FILE='rain.dat',EXIST=limits)
C Open constraints file if it exists
	if(.not.limits) then
		write(21,*) ' No rain.dat'
		stop
	endif
	close (lunopt)
      OPEN(UNIT=LUNopt,FILE='rain.dat',STATUS='OLD',ERR=299)
	read(LUNopt,*)
	read(LUNopt,*)
	read(LUNopt,*,err=299,end=299) verno
	close (LUNopt)
      OPEN(UNIT=LUNopt,FILE='rain.dat',STATUS='OLD',ERR=299)
c	if( verno.eq.'Ver1') then RPI replaced 18/9/01
C RPI 8/7/2002 expanded the commented out if stmt below for ver2 modelspc.dats
	if(verno.eq.'Ver1'.or.verno.eq.'ver1') then
           if(ver_msp.ne.'Ver1'.and.ver_msp.ne.'ver1'.and.
     +		ver_msp.ne.'Ver2'.and.ver_msp.ne.'ver2') then
			write(21,*) ' Incompatible MODELSPC.DAT and RAIN.DAT'
			stop
	endif
      else
!           if(ver_msp.eq.'Ver1'.or.ver_msp.eq.'ver1'.or.
!     +		ver_msp.eq.'Ver2'.or.ver_msp.eq.'ver2') then
           if(ver_msp.eq.'Ver1'.or.ver_msp.eq.'ver1') then
c			write(21,*) '??? Incompatible MODELSPC.DAT and RAIN.DAT'
cRAW 21/10/02			stop
		 endif
	endif
c	if( ( (verno.eq.'Ver1'.or.verno.eq.'ver1').and.
c     +      (ver_msp.ne.'Ver1'.and.ver_msp.ne.'ver1')) .or.
c     +	( (verno.ne.'Ver1'.and.verno.ne.'ver1').and.
c     +      (ver_msp.eq.'Ver1'.or.ver_msp.eq.'ver1'))) then
c	    if(verno.ne.ver_msp) then ! RPI replaced 18/9/01
c			write(21,*) ' Incompatible MODELSPC.DAT and RAIN.DAT'
c			stop
c		endif
c	endif
C RPI 8/7/2002 expanded the commented out if stmt below fro ver2 modelspc.dats
	if((verno.eq.'Ver1'.or.verno.eq.'ver1'.or.
     +    verno.eq.'Ver2'.or.verno.eq.'ver2') .and.
     +   (ver_msp.eq.'Ver1'.or.ver_msp.eq.'ver1'.or.
     +    ver_msp.eq.'Ver2'.or.ver_msp.eq.'ver2')) then
c	if((verno.eq.'Ver1'.or.verno.eq.'ver1') .and.
c     +   (ver_msp.eq.'Ver1'.or.ver_msp.eq.'ver1')) then
		read(LUNopt,*)
		read(LUNopt,*)
		read(LUNopt,*,err=299,end=299) verno,ntri,(sites(i),i=1,ntri)
c If both verno and ver_msp are .ne. Ver1 then assume old files
c and it is the users responsibility to get things right!!!
	else
		read(LUNopt,*)
		read(LUNopt,*)
		read(LUNopt,*) verno
	endif
	close (LUNopt)
      DO 30 I=1,NS
	LINE = 2
c RAW allow >1 raingauge per sub-basin 4-May-98
c if LINKS(2,I)<0, it means there is a list of -LINKS(2,I) pairs
c of (raingauge #, weight) which are to be used for that sub-basin
c So if the line in the file contains '1 2', then we have usual case:
c LINKS(1:2,I) is [1 2], and we use gauge 2 for basin 1 as usual.
c
c BUT if the line in the file contains '1 -2   3 0.3   4 0.5', then
c the '-2' indicates two pairs of (gauge#,weight), which are taken
c to mean 'subbasin 1 rainfall = gauge#3*0.3 + gauge#4*0.5
c
		 do 29 k=1,maxgauge
			lrg(i,k)=0
			wrg(i,k)=0.
			wrg1(i,k)=0.    ! DGT 7/24/05 WRG1 Interpolation weights
   29		 continue
           READ(73,*,ERR=280) (LINKS(J,I),J=1,2),
	1                     (LRG(I,K),WRG1(I,K),K=1,-LINKS(2,I))  ! DGT 7/24/05 WRG1 Interpolation weights
           READ(LUNRD,*,ERR=280) (LINKS(J,I),J=1,2),
	1                     (LRG(I,K),WRG(I,K),K=1,-LINKS(2,I))
!		if(verno.eq.'Ver1'.or.verno.eq.'ver1') then    ! we have new format rain.dat
		if(verno.eq.'Ver1'.or.verno.eq.'ver1'.or.
     +		verno.eq.'Ver2'.or.verno.eq.'ver2') then
C In a new modelspc.dat the sub-basin raingauge relationship is based
C on raingauge numbers, not the column numbers of data. We need to find
C column positions in rain.dat corresponding to each raingauge number
C to be compatible with early code.
		do 751 k=1,-LINKS(2,I)
! RPI 23/1/03 - set IMATCH to 0 toshow that not yet found a matching site
		IMATCH=0
		do 750 j=1,ntri
c RPI 28/9/00 introduced the following code to allow for meso-scale
c rainfall grid variations
C
C 6/27/07.  DGT Introduced the if(imatch .eq. 0) below to make sure that the chk_sites is only used
c to find a match once, because upon finding a match the value lrg(j,k) is changed and an
c error can occur if after it being changed the changed value matches to a different site.
          if(IMATCH .eq. 0)then
			call chk_sites(lrg(i,k),sites(j),j,IMATCH)
		endif
c		if(lrg(i,k).eq.sites(j)) lrg(i,k)=j
750		continue
		if(IMATCH.eq.0) then
	WRITE(21,32) MAXG
   32   FORMAT(
     1  ' Number of raingauges used for the basins is inconsistent ',
     2  'with (more than)'/' that specified in the first data line'/
     3  'This may arise because of a mismatch in the grid, check ',
     4  ' around site number ',I9)
	GOTO 290
		endif
751		continue
c RPI 5/6/2003 moved this			endif
c translate lrgs which are site #s to column position in rain.dat RPI & RAW 25/8/00
				if (links(2,i) .gt. 0) then
					lrg(i,1)=links(2,i)
					wrg(i,1)=1.
					wrg1(i,1)=1.	! DGT 7/24/05 WRG1 Interpolation weights
				endif
			else
C RPI 5/6/2003 added this else clause for compatibility with old rain.dat files
C Old rain.dat files assume column 1 is for the first mentioned site etc.
C Therefore need to set up  a correct set of lrg values, & discard site #s
				do k=1,-links(2,i)
					lrg(i,k)=k
				enddo
			endif

c translate lrgs which are site #s to column position in rain.dat RPI & RAW 25/8/00
		 if (links(2,i) .gt. 0) then
			lrg(i,1)=links(2,i)
			wrg(i,1)=1.
			wrg1(i,1)=1.  	! DGT 7/24/05 WRG1 Interpolation weights
		 endif
c	write(6,5090)NGAUGE,NS,NRCH,NKA,ND,LL,NTR,NTS,LINKS,LINKR
C       IF(LINKS(1,I).NE.I) THEN
C         WRITE(21,28)
C  28     FORMAT(' Slope numbering out of sequence.')
C         GOTO 290
C       ENDIF
C USE LLN TEMPORARILY TO CHECK THAT ALL BASINS HAVE GOT A RAINGAUGE
	LLN(LINKS(1,I)) = LLN(LINKS(1,I)) + 1
c RAW altered following test to allow -ve raingauge#, if done properly
	IF(LINKS(2,I).LE.0 .and. lrg(i,1) .eq. 0) GOTO 280
c RAW altered following test to allow -ve raingauge#, if done properly
CRAW	MAXG = MAX0(LINKS(2,JS),MAXG)
!	do 291 k=1,maxgauge  RPI 23/1/03 removed this test and replaced it with the one below
!  291	   MAXG = MAX0(lrg(i,k),MAXG)
   30 CONTINUE
CDGT altered to allow use of fewer raingauges than are in the data file
! RPI 23/1/03 removed this test as no longer needed - see chk_sites call above
!      IF(MAXG.GT.NGAUGE) THEN  ! RPI 23/1/03 changes stop here
!	WRITE(21,32) MAXG
!   32   FORMAT(
!     1  ' Number of raingauges used for the basins is inconsistent ',
!     2  'with (more than)'/' that specified in the first data line'/
!     3  'This may arise because of a mismatch in the grid, check ',
!     4  ' around site number ',I9)
!	GOTO 290
!      ENDIF
C CHECK THAT ALL BASINS HAVE GOT A RAINGAUGE
C WE SHOULD HAVE SEEN EACH  BASIN EXACTLY ONCE
      OK = .TRUE.
      DO 36 I = 1,NS
	  IF ( LLN(I) .NE. 1 ) THEN
	      WRITE(21,34)I
   34         FORMAT(' No raingauge specified for basin ',I3)
	      OK = .FALSE.
	  ENDIF
   36 CONTINUE
      IF ( .NOT. OK ) GOTO 290
C  Read reach link data
C  ********************
      READ(LUNRD,'()')               ! Comment line
	maxrno = 0
      DO 40 I=1,NRCH
	LINE = 3
c	READ(LUNRD,'(3I5)',ERR=280) (LINKR(J,I),J=1,3)
      READ(LUNRD,*,ERR=280) (LINKR(J,I),J=1,3)  ! DGT prefers free format
	do 500 j=1,3
  500 clinkr(j,i)=linkr(j,i)
! RPI 28/7/2003 reckons this is covered by MAXCHN in version 6 and later
	maxrno=max(maxrno,linkr(1,i))  !  DGT Keep track of largest reach number
	IF(LINKR(1,I).LE.0.OR.LINKR(2,I).LE.0) GOTO 280
   40 CONTINUE
c	write(6,5090)NGAUGE,NS,NRCH,NKA,ND,LL,NTR,NTS,LINKS,LINKR
CDGT  Sequence to read response and output reach numbers 5.26.98
      read(LUNRD,'()')               ! Comment line
	read(lunrd,*)neq,nout
	if(nout .gt. maxchn)
     1 write(21,*)'ERROR - cannot output more than',maxchn,
	2 ' channels'
c RPI 16/3/99 added the next 4 stmts
	if(max(neq,nout).gt.maxresponse) then
	write(21,*)'Error - neq or nout more than maxresponse',neq,nout,
     1	maxresponse
		go to 290
	endif
	if(neq .gt. iex) then  ! RPI 17/3/99
		write(21,*)'Error - neq more than dimension allows',neq ! RPI 17/3/99
		go to 290 ! RPI 17/3/99
	endif  ! RPI 17/3/99
c  the first neq of the following list will be used as response time series
c  Any remaining are output.
	read(lunrd,*)(kllout(i),i=1,max(neq,nout))
C If nudging a forecast the some of the rel(i)  will .ne. zero. Set
C relflag to one in this case, zero otherwise. Initial relflag here
C in case there is no rchareas.txt
	relflag=0
c
c RPI 18/9/01 - This is where we check the reach numbers against those
c in runoff.dat using rchareas.txt. Initially rchareas.txt contained
c reach no., area upstream of reach, and site no. These have been supplemented
c with easting and northing. This addition should not affect the Matlab
c Topplot procedure since it reads only the first 3 values.
      INQUIRE(FILE='rchareas.txt',EXIST=limits)
	rchareas=.false.
	IF(limits) THEN
		rchareas=.true.
		OPEN (UNIT=82,FILE='rchareas.txt',STATUS='OLD',IOSTAT=IERROR)
		IF(IERROR.NE.0) THEN
			WRITE(21,*) ' **** There is a problem the rchareas.txt file'
			STOP
		ENDIF
C	Read in rcharea.txt - may need to revise this if more or less than
C maxresponse values in rchareas.txt
		i=0
3201		i=i+1
		if(i.le.maxrchareas) then   ! RPI 22/5/2002 changed maxresponse to maxrchareas
			read(82,*,end=3200) rchno(i),dummy,qsite(i),qeast(i),
     +                    qnorth(i),REL(i),ishift0(i)
			go to 3201
		endif
3200		i=i-1
		CLOSE (82)
		nrchno=i
c RPI 21/6/2002 nshifted this code. As it was all entries in rchareas.txt were
c checked. Should only check those entries being used
c		do i=1,nrchno
c			if(rel(i).gt.0) then
c				relflag=1
c				go to 3202
c			endif
c		enddo
3202		continue
C Compare reaches in modelspc.dat, in kllout, with those just read in
C and select the subset of sites from rchareas.txt
c          krch=0
c		do i=1,neq
c			do j=1,nrchno
c				if( kllout(i).eq. rchno(j)) then
c				krch=krch+1
c				qsite1(krch)=qsite(j)
c				endif
c			enddo
c		enddo
	ELSE
		WRITE(21,*) ' ****Warning, there is no rchareas.txt file'
		WRITE(21,*)
     +' Data in streamflow_calibration.dat assumed in correct order'
	ENDIF
C get flow site numbers from runoff.dat and set up mapping to reaches RPI 18/9/01
	INQUIRE(FILE='streamflow_calibration.dat',EXIST=limits)
C Open runoff file to get header line with site numbers on
	if(.not.limits) then
		write(21,*) ' No streamflow_calibration.dat available in MDDATA'
		stop
	endif
      OPEN(UNIT=82,FILE='streamflow_calibration.dat',
     +STATUS='OLD',ERR=294)
	read(82,*)
	read(82,*)
	read(82,*)
	read(82,*,err=294,end=294) verno
	close (82)
      OPEN(UNIT=82,FILE='streamflow_calibration.dat',
     +STATUS='OLD',ERR=294)
C RPI 8/7/2002 expanded the commented out if stmt below for ver2 modelspc.dats
	if(verno.eq.'Ver1'.or.verno.eq.'ver1') then
           if(ver_msp.ne.'Ver1'.and.ver_msp.ne.'ver1'.and.
     +		ver_msp.ne.'Ver2'.and.ver_msp.ne.'ver2') then
			write(21,*)
     +' Incompatible MODELSPC.DAT and streamflow_calibration.dat'
			stop
	endif
      else
!           if(ver_msp.eq.'Ver1'.or.ver_msp.eq.'ver1'.or.
!     +		ver_msp.eq.'Ver2'.or.ver_msp.eq.'ver2') then
           if(ver_msp.eq.'Ver1'.or.ver_msp.eq.'ver1') then
	write(21,*) 'Warning - Incompatible MODELSPC.DAT and',
     +	' streamflow_calibration.dat'
	write(21,*) '    It is the users responsibility to get order of'
	write(21,*) '    sites and data columns in ',
     +'streamflow_calibration.dat in correct order'
cRPI 5/6/2003			stop
		 endif
	endif
c	if( ((verno.eq.'Ver1'.or.verno.eq.'ver1').and.
c     +      (ver_msp.ne.'Ver1'.and.ver_msp.ne.'ver1')) .or.
c     +	((verno.ne.'Ver1'.and.verno.ne.'ver1').and.
c     +      (ver_msp.eq.'Ver1'.or.ver_msp.eq.'ver1'))) then
c			write(21,*) ' Incompatible MODELSPC.DAT and RUNOFF.DAT'
c			stop
c	endif
C Initialise qmap so that OK if an old file
	do i=1,neq
		qmap(i)=i
	enddo

C RPI 8/7/2002 expanded the commented out if stmt below for ver2 modelspc.dats
C RPI 6/6/2003 changed the "if" statement to allow for old style "runoff.dats"
      l1:	if(verno.eq.'Ver1'.or.verno.eq.'ver1') then
      l2:  if(ver_msp.eq.'Ver1'.or.ver_msp.eq.'ver1'.or.
     +    ver_msp.eq.'Ver2'.or.ver_msp.eq.'ver2') then
c      if((verno.eq.'Ver1'.or.verno.eq.'ver1') .and.
c     +   (ver_msp.eq.'Ver1'.or.ver_msp.eq.'ver1'.or.
c     +    ver_msp.eq.'Ver2'.or.ver_msp.eq.'ver2')) then
C We have a new format Runoff.dat and a new format Modelspc.dat - we must also
C have a Rchareas.txt - if not stop
	l8:	if(.not.rchareas) then
			write(21,*) ' No RCHAREAS.TXT file - execution stops'
			stop
		endif l8
c	if((verno.eq.'Ver1'.or.verno.eq.'ver1') .and.
c     +   (ver_msp.eq.'Ver1'.or.ver_msp.eq.'ver1')) then
		read(82,*,err=294,end=294) verno,ntri,(qsite2(i),i=1,ntri)
C
C There must be neq columns of data in runoff.dat
C
	l3:	if(ntri.ne.neq) then
		write(21,*) ' ****ERROR The number of columns in',
     +' streamflow_calibration.dat'
		write(21,*) ' is different from the number,',neq,' of reaches'
		write(21,*) ' in modelspc.dat'
		stop
		endif l3
C Compare reaches in modelspc.dat, in kllout, with those just read in
C and select the subset of sites from rchareas.txt. Can't do this until
C we have qsite2 because some sites may fall in the same reach RPI 22/5/2002
          krch=0
C RPI 25/7/02 put in the neq=0 test etc.
	neq_temp=neq
c		if(neq.eq.0) neq_temp=ntri
		do i=1,neq_temp
			do j=1,nrchno
	l4:			if( kllout(i).eq. rchno(j)) then
C Make sure you have the correct site as well - RPI 22/5/2002
					do k=1,neq_temp
						if(qsite(j).eq.qsite2(k)) go to 3300 ! We have a match
					enddo
					go to 3302
3300					krch=krch+1
					qsite1(krch)=qsite(j)
c RPI 21/6/2002 - We need the rel(i) in the same order as the qsite1 otherwise
c in CALCTS the wrong rel values are applied to weighting modelled and corrected
c modelled flows in the "nudging" of forecasts.
					reltemp(krch)=rel(j)  ! j>=krch so this should not be a problem
					ishift0temp(krch)=ishift0(j)  ! j>=krch so this should not be a problem
					if(reltemp(krch).gt.0) relflag=1 ! RPI 21/6/2002 - see above
					go to 3301
				endif l4
3302				continue
			enddo
3301			continue
		enddo
C RPI 26/6/2002 - Now put reltemps into rel
		do k=1,krch
		rel(k)=reltemp(k)
		ishift0(k)=ishift0temp(k)
		enddo
C We now have the site numbers of the data in Runoff.dat (the qsite2s)
C and the mapping of these to reach numbers from rchareas.txt. Now we
C need to work out which column in runoff.dat corresponds to each reach
C number in modelspc.dat. Initially we will ignore the easting and northing
C data and focus on the site number to reach number relationship.
C
C We can't proceed any further here because the flow data have not yet
C been read in, and since we are going to
C take the cautious approach, i.e. rearrange the columns in
C array flow to match the order of the sites in kllout, this will have
C to be done in inputt after the flows have been read in by hydata.
C This will effectively
C set flow up the way it would have been read prior to this development.
C The alternative high risk approach is to leave the columns in flow the
C way they were read in, and alter every occurrence of the use of flow to
C reflect the new mapping.
C
C Set up the mapping as follows: use an array called qmap. Define
C qmap(i) to be the column in runoff.dat corresponding to i'th reach
C in the list read in from modelspc.dat, e.g. through the reach-site no.
C linkage data for the 2nd reach in the list might be in the 4th column,
C in which case qmap(2)=4
C read in from modelspc.dat.
C We can only do this if there is a rchareas.txt
	l5:	if(rchareas) then
			neq_chk=0
c RPI 16/5/2002 introduced the max and changed k to krch because there
c are legimate cases where more than one site number can be attached to
c the same reach and in this case qsite1 has more than neq entries in it.
			do i=1,max(neq,krch)
				do j=1,neq_temp
	l6:				if(qsite1(i).eq.qsite2(j)) then
c RPI 16/5/2002					qmap(i)=j
						neq_chk=neq_chk+1
						qmap(neq_chk)=j
					endif l6
				enddo
			enddo
	l7:		if(neq_chk.ne.neq_temp) then
		write(21,*) ' ****ERROR - Not all the reaches in MODELSPC.DAT'
				write(21,*) ' could be found in RCHAREAS.TXT - fix!'
				stop
			endif l7

c If both verno and ver_msp are .ne. Ver1 then assume old files
c and it is the users responsibility to get things right!!!
C RPI 6/6/2003 - next 2 lines are irrelevant here!
c		else
c			read(82,*) verno
		endif l5
	endif l2
C RPI 6/6/2003 added the "else" clause for backwards compatibility with old
C style format files. Come here if old style "modelspc.dat" file
	else
C Decide if there is a RCHAREAS.TXT. If there isn't then there can be no nudging
C of forecasts and we have to assume that the order of data in RUNOFF.DAT is the
C same as the order of the reaches in MODELSPC.DAT. If there is a RCHAREAS.TXT then
C we can do nudging but still have to assume the reaches in RUNOFF.DAT are the
C same order as in MODELSPC.DAT
	l9:	if(rchareas) then

		do i=1,neq
			do j=1,nrchno
	l10:			if( kllout(i).eq. rchno(j)) then
					qsite1(i)=qsite(j)
					reltemp(i)=rel(j)
					ishift0temp(i)=ishift0(j)
					if(rel(j).gt.0) relflag=1 ! RPI 21/6/2002 - see above
					go to 4301
					endif l10
			enddo
4301			continue
		enddo
C RPI 26/6/2002 - Now put reltemps into rel
		do k=1,neq
craw		rel(k)=reltemp(k)
craw		ishift0(k)=ishift0temp(k)
		enddo
	else
C No RCHAREAS.TXT so no nudging possible
		relflag=0
	endif l9
C RPI 6/6/2003 - end of additions
	endif l1
c	endif
	close (82)
C
C End of changes needed to match flows in runoff.dat to reaches in
C modelspc.dat using site numbers
C -------------------------------------------------------------------------
C
C At this point we have all the data needed to sort out the how to update
C the ZBAR0s for each measured sub-basin
C     call SET_COR_DATA(linkr,neq,nout,nrch,ns,kllout)
C                       ----- NOTE ------
C  From this point onwards until label 220 the reach link data will be
C  checked and processed , and only after that more data will be read
C  from the disk file.
C
C TOPMODEL: Create artificial gutters between subcatchments and reaches
C The first NRCHSAV lines are all reaches fed by subcatchments or
C reaches. Whenever we find a subcatchment, convert it to a gutter, and
C and add in a new line which says our new gutter is fed by that
C subcatchment. The values of N0RCH and LINKR(4,.) will be done later on
C
      NRCHSAV=NRCH
c  DGT - Changes below to generalize the numbering of gutters
c   in case there is a reach number bigger than 100.
c
! RPI 28/7/2003 reckons this is covered by MAXCHN in version 6 and later
c      ngbase = max(100, maxrno)
	if (nooksack.eq.1) then
		do i=1,NRCH
			ll(i)=i
		end do !not the usual routing
		N0RCH=0
	else
      ngbase = max(100, maxrno)
      DO 501 I = 1,NRCHSAV
	 DO 451 J = 1,2
	    II = LINKR(J+1,I)
	    IF ( II .LE. NS .AND. II .GT. 0 ) THEN
C Have found a subcatchment, so convert it to a gutter
c	       LINKR(J+1,I) = 100+II
		   LINKR(J+1,I) = ngbase+II   ! DGT
C Add a new line for the gutter
	       NRCH = NRCH+1
C The gutter (100+ii) is fed by subcatchment ii
c	       LINKR(1,NRCH) = 100+II
	       LINKR(1,NRCH) = ngbase+II  ! DGT
	       LINKR(2,NRCH) = II
	       LINKR(3,NRCH) = 0
	    ENDIF
  451    CONTINUE
  501 CONTINUE
C  Find 0th order reaches and fill in 4th column of LINKR
C  ******************************************************
      N0RCH = 0
      DO 60 I=1,NRCH
	LINKR(4,I) = 0
	if(linkr(2,i).gt.0 .and. linkr(3,i).eq.0) linkr(4,i)=1
	if(linkr(2,i).gt.0 .and. linkr(3,i).gt.0) linkr(4,i)=2
	if(linkr(4,i).eq.0) then
c       ELSE
c         IF(LINKR(3,I).EQ.0) LINKR(4,I) = 1
c         IF(LINKR(3,I).GT.NS) LINKR(4,I) = 2
c         IF(LINKR(3,I).LE.NS.AND.LINKR(3,I).NE.0) THEN
	    WRITE(21,55)LINKR(1,I)
   55       FORMAT(' Error in the reach link data , reach number ',I3)
	    GOTO 290
	  ENDIF
	IF(LINKR(2,I).LE.NS .and. linkr(3,i).le.ns) THEN
	  N0RCH = N0RCH + 1
	  LLN(N0RCH) = LINKR(1,I)
	  LL(N0RCH) = I
	ENDIF
   60 CONTINUE
C  Fill in 4th column of LINKR for 0th order reaches
C  *************************************************
c      DO 65 I=1,N0RCH
c       II = LL(I)
c       IF(LINKR(2,II).LE.0) THEN
c         WRITE(21,55)LINKR(1,II)
c         GOTO 290
c       ELSE
c         IF(LINKR(3,II).GT.0.AND.LINKR(3,II).LE.NS) LINKR(4,II) = 2
c         IF(LINKR(3,II).LE.0) LINKR(4,II) = 1
c         IF(LINKR(3,II).GT.NS) THEN
c         WRITE(21,55)LINKR(1,II)
c           GOTO 290
c         ENDIF
c       ENDIF
c  65 CONTINUE
C  Check to see if all the reaches are not zero order reaches.
C  Check also if N0RCH > MAXGUT or N0RCH = 0 , both fatal errors.
C  **************************************************************
      ALLZER = .FALSE.
c     IF(N0RCH.EQ.NRCH) THEN
c       WRITE(21,67)
c  67   FORMAT(' WARNING! All the reaches in the river network are ',
c    1         'zero order reaches')
c       ALLZER = .TRUE.
c       GOTO 180
c     ENDIF
C  Test each 'non-zero' order segment or link to see if it gets all its
C  inputs from segments above* it in the stream network. (* 'Above' is
C  only figurative and it really refers to those reaches which already
C  have their relative positions allocated in the array LL. This is a
C  recursive process and it carries on until all the segments have been
C  allocated a position in the array LL. )
C  ********************************************************************
      N1RCH = N0RCH
      ICHECK = N1RCH
   70 CONTINUE
      DO 110 I=1,NRCH
C  Goto the end of the loop if reach I has already been assigned a
C  position.
	DO 75 J=1,N1RCH
	  IF(LL(J).EQ.I) GOTO 100
   75   CONTINUE
	IF(LINKR(4,I).EQ.1) THEN
C  Enter here if only one upstream link .
	  DO 80 J=1,N1RCH
	    IF(LINKR(2,I).EQ.LLN(J)) THEN
	      N1RCH = N1RCH + 1
	      LLN(N1RCH) = LINKR(1,I)
	      LL(N1RCH) = I
	      GOTO 100
	    ENDIF
   80     CONTINUE
	ELSE IF(LINKR(4,I).EQ.2) THEN
C  Enter here if two upstream links.
	  US1 = .FALSE.
	  US2 = .FALSE.
	  DO 90 J=1,N1RCH
 	    IF(LINKR(2,I).EQ.LLN(J).OR.LINKR(2,I).LE.NS) US1 = .TRUE.
	    IF(LINKR(3,I).EQ.LLN(J).OR.LINKR(3,I).LE.NS) US2 = .TRUE.
	    IF(US1.AND.US2) THEN
	      N1RCH = N1RCH + 1
	      LLN(N1RCH) = LINKR(1,I)
	      LL(N1RCH) = I
	      GOTO 100
	    ENDIF
   90     CONTINUE
	ENDIF
C  Enter here to check if entire array LL has been filled out.
  100   IF(N1RCH.EQ.NRCH) GOTO 120
  110 CONTINUE
C  Enter here after looping and check if any new position has been
C  out in array LL . If not then the link data is corrupt.
      IF(N1RCH.EQ.ICHECK) THEN
	WRITE(21,112)LLN(N1RCH)
  112   FORMAT(' Link data corrupt after link ',I3)
	WRITE(21,114)
  114   FORMAT(' This is the order in which the river links will be ',
     1         'processed so far :')
	WRITE(21,116) (LLN(I),I=1,N1RCH)
  116   FORMAT(80I5)
	GOTO 290
      ENDIF
C  Start looping from beginning again.
      ICHECK = N1RCH
      GOTO 70
  120 CONTINUE
C  Fill in the vector NTR
C  **********************
      DO 130 I=1,NRCH
	NTR(I) = 0
  130 CONTINUE
      DO 150 I=N0RCH+1,NRCH
	II = LL(I)
	DO 140 J=1,NRCH
	  JJ = LINKR(1,J)
	  IF(LINKR(2,II).EQ.JJ) NTR(J) = NTR(J) + 1
	  IF(LINKR(3,II).EQ.JJ) NTR(J) = NTR(J) + 1
  140   CONTINUE
  150 CONTINUE
C  Check vector NTR i.e. find out if all 0th order reaches are used (as
C  inputs), or if a higher order reach is used more than once , or if
C  more than one higher order reach is not used as an input, or if there
C  is no outlet to the system i.e. NTR(I).NE.0 for all I.
C  ******************************************************
      DO 160 I=1,N0RCH
	II = LL(I)
	IF(NTR(II).EQ.0) WRITE(21,550) LINKR(1,II)
  550 FORMAT('2 WARNING ! Reach number',I3,' (a 0th order reach) is not
     1used as an input'/ ' in the river network.')
  160 CONTINUE
      HIGH = .FALSE.
      DO 170 I=N0RCH+1,NRCH
	II = LL(I)
	IF(NTR(II).EQ.0) THEN
c RAW 16/7/2002 wanted to simulate more than one catchment at once so removed
c the test on HIGH. We will only re-instate if we have problems - RPI
c	  IF(HIGH) THEN  ! RAW commented this out 16/7/2002
c	    WRITE(21,555) LINKR(1,II)   ! RAW commented this out 16/7/2002
c  555 FORMAT(' Reach link data corrupt! Reach no. ',I3,' is the second r
c     1each encountered'/' that does not have a downstream link.')
c	    GOTO 290  ! RAW commented this out 16/7/2002
c	  ENDIF   ! RAW commented this out 16/7/2002
	  HIGH = .TRUE.
	ENDIF
	IF(NTR(II).GT.1) WRITE(21,560) LINKR(1,II)
  560 FORMAT('2 WARNING ! Reach number',I3,' (a higher order reach) is u
     1sed more than'/' once as an input. This will cause errors in the w
     2ater balance calculations.')
  170 CONTINUE
      IF(.NOT.HIGH) THEN
	WRITE(21,565) LINKR(1,II)
  565 FORMAT(' Reach link data corrupt! There is no final outlet in the
     1river network.'/ ' The last reach processed was reach no. ',I3)
	GOTO 290
      ENDIF
  180 CONTINUE
c	write(6,5090)NGAUGE,NS,NRCH,NKA,ND,LL,NTR,NTS,LINKS,LINKR
C  Fill in the vector NTS by first filling in the matrix NTSG which
C  indicates how many times each slope is used for each gutter.
C  ***********************************************************
      DO 200 I=1,NS
	NTS(I) = 0
	DO 190 J=1,N0RCH
	  JJ = LL(J)
	  NTSG(I,J) = 0
	  IF(LINKR(2,JJ).EQ.I) NTSG(I,J) = NTSG(I,J) + 1
	  IF(LINKR(3,JJ).EQ.I) NTSG(I,J) = NTSG(I,J) + 1
  190   CONTINUE
  200 CONTINUE
      DO 220 I=1,NS
	DO 210 J=1,N0RCH
	  JJ = LL(J)
	  IF(ALLZER) NTR(JJ) = 1
	  NTS(I) = NTS(I) + NTSG(I,J)*NTR(JJ)
  210   CONTINUE
  220 CONTINUE
c  Sort out llout array so that it is an index to ll array
C Array LL defines the processing order of reaches. Array LLOUT contains
C the position within LL of reaches with measured values or for which
C output is required.
      do 33 i = 1,max(neq,nout)
	  ii=kllout(i)
	  llout(i) = 0
	  do 341 j = 1,nrch        ! As before ?
	    jj=ll(j)               ! RPI & RAW changed removed ngut 18/9/98
          if(linkr(1,jj) .eq. iabs(ii))then  ! RPI 23/8/00
	      if(llout(i) .eq. 0)then
	        llout(i) = isign(j,ii)  ! RPI 23/8/00
	      else
	        write(21,*)'Logic error or duplicate output reach'
	      endif
	    endif
 341     continue
 33   continue
	endif !nooksack.eq.0
CDGT  Sequence to read subbasin output numbers 6.1.98
      read(LUNRD,'()')               ! Comment line
	read(lunrd,*)nbout
	read(lunrd,*)(itbout(i),i=1,nbout)
	do i = 1,ns
	  ibout(i)=0
	enddo
	do i = 1,nbout
	  ibout(itbout(i))=1  !  Set flags for basins to output
	enddo
C
C  Resume reading main data disk file
C  Read basin properties
C  *********************
craw 14-feb-2005

	open(unit=88,file='basinpars.txt',status='old')
	read(88,*)
	read(88,*)((bp(j,i),j=1,num_basinpars),i=1,ns)
	close(88)
c
c  DGT 7/23/05  Insertion to get latitude and longitude for each basin
c
c   Read latlongfromxy.dat
c
	open(unit=88,file='latlongfromxy.txt',status='old')
	read(88,*)
	read(88,*)
	read(88,*)flat
	read(88,*)clat
	read(88,*)flong
	read(88,*)clong
	close(88)
c
c   calculate basin lat long.
c   Assumptions
c   1.  Linear tranformation between local coords and lat long is sufficient
c   2.  Outlet local coordinates is representative of subbasin
	do j=1,ns
		bxlat(j)=flat*bp(7,j)+clat
		bxlon(j)=flong*bp(6,j)+clong
	enddo
c
c   END DGT 7/23/05  Lat and long modifications
C
c   DGT modified below to streamline subbasin property input  3/97
C See if there is a constraints file

	INQUIRE(FILE='modelcon.dat',EXIST=limits)
c RPI 12/8/2002 introduced limitc here and rplaced limits in the argument list
c because limits is used when opening many files but we need to keep the value
c associated with modelcon for use elsewhere. It leaves here as limitc, undergoes
c a name change to limits in input, i.e. we change the name in the calls so as
c not to have to alter code elsewhere
	limitc=limits   ! limits is local, limitc is glpobal for constraints
C Open constraints file if it exists
	if(limits)
     +  OPEN(UNIT=LUNopt,FILE='modelcon.dat',STATUS='OLD',ERR=281)
      READ(LUNRD,'()')               ! Comment line
	READ(LUNRD,'()')               ! Comment line
      DO 240 I=1,NS
      READ(LUNRD,'()')               ! Comment line
	if(limits.and.i.eq.1) READ(LUNopt,'()') ! Comment line
       LINE = 4
	  do 241 j=1,isp
	   if(limits.and.i.eq.1) READ(LUNopt,*,ERR=281) minsp(j),maxsp(j)
         READ(LUNRD,*,ERR=280) sp(j,i)   !  One property per line
 241    continue
	do j=1,nsp !craw overwrite using basinpars
		sp(j,i)=bp(j+7,i) !bp(1:7,:) has other stuff we also need
	enddo

c   END DGT modifications
C
C READ A/TANB DISTBN
      READ(LUNRD,'()')               ! Comment line
       LINE = 5
	READ(LUNRD,*,ERR=280) NKA(I)
c	write(6,5090)NGAUGE,NS,NRCH,NKA,ND,LL,NTR,NTS,LINKS,LINKR
	IF ( NKA(I) .GT. MAXA ) THEN
		WRITE(21,35)MAXA
   35 FORMAT(' Too many points in ln(a/tanb) distbn, MAXA =',I3)
		GOTO 290
      ENDIF
CRAW improved all error messages where there was a GOTO 290
      READ(LUNRD,'()')               ! Comment line
      LINE = 6
      READ(LUNRD,*,ERR=280) (ATB(J,I),PKA(J,I),J=1,NKA(I))
	TEMP = 0D0
c	DO 4000 J=1,NKA(I)
	DO 4000 J=2,NKA(I)    ! DGT because first one is not used
c	IF ( ATB(J,I).LT.0D0.OR.PKA(J,I).LT.0D0.OR.PKA(J,I).GT.1D0 )
	IF ( PKA(J,I).LT.0D0.OR.PKA(J,I).GT.1D0 ) ! DGT ATB < 0 is a valid number
     *                                    THEN
           write(21,351)j,i
  351      format(' Error in line ',i2,' of LOG(A/TANB) for subcat ',i3,
     *            ' PKA(J,I)<0 OR PKA(J,I)>1')
           GOTO 290
        ENDIF
	IF(J.GT.1 .AND. ATB(J,I).Le.ATB(J-1,I) ) THEN
           write(21,352)j,i
  352      format(' Error in line ',i2,' of LOG(A/TANB) for subcat ',i3,
     *            ' : ATB(J,I)<=ATB(J-1,I)')
           GOTO 290
        ENDIF
	  if (nooksack.eq.1) pka(nka(i),i)=0 !RAW nooksack don't accept the flat areas
	  TEMP = TEMP+PKA(J,I)
 4000 CONTINUE
	IF ( nooksack.ne.1 .and. ABS(TEMP-1D0) .GT. .01 ) THEN
		WRITE(21,45)TEMP
   45       FORMAT(' Sum of area proportions for a/tanb <> 1: ',D16.8)
		GOTO 290
      ENDIF
cDGT
C  Here temp is the sum of proportions and is close to 1.
c  Normalize all proportions so that the proportions are numerically
c  equal to 1 to avoid mass balance errors.
      do 4001 j = 2,nka(i)
	  pka(j,i)=pka(j,i)/temp
 4001 continue
C AREAL INTEGRAL OF A/TANB (=LAMBDA)
C USE VALUES AT CENTRE OF CLASS RANGES TO CALCULATE MEAN
	TL(I) = ATB(1,I)*PKA(1,I)
	DO 50 J=2,NKA(I)
	  TL(I) = TL(I)+PKA(J,I)*(ATB(J,I)+ATB(J-1,I))/2D0
   50 CONTINUE
C
C READ OVERLAND FLOW ROUTING DISTBN
      READ(LUNRD,'()')               ! Comment line
        READ(LUNRD,*,ERR=280) ND(I)
c	WRITE(6,5090)NGAUGE,NS,NRCH,NKA,ND,LL,NTR,NTS,LINKS,LINKR
        IF ( ND(I) .GT. MAXC ) THEN
                WRITE(21,5500)MAXC
 5500           FORMAT(' Too many points in channel distbn, MAXC =',I3)
                GOTO 290
      ENDIF
      READ(LUNRD,'()')               ! Comment line
      LINE = 7
      READ(LUNRD,*,ERR=280) (CL2(J,I),PD2(J,I),J=1,ND(I))
        DO 6000 J=1,ND(I)
	rtemp1=cl2(j,i)
	ptemp1=pd2(j,i)
	if (j.gt.1) then
		rtemp2=cl2(j-1,i)
		ptemp2=pd2(j-1,i)
	endif
	if(rtemp1.lt.0.or.ptemp1.lt.0.or.ptemp1.gt.1) then
           write(21,551)j,i
 551      format(' Error in line ',i2,' of Flow Dist. for subcat ',i3,
     *            ' : cl(j,i).lt.0.or.pd(j,i).lt.0.or.pd(j,i).gt.1')
           GOTO 290
        endif
	if(j.gt.1) then
	   if(rtemp1.lt.rtemp2.or.ptemp1.lt.ptemp2) then
              write(21,502)j,i
  502      format(' Error in line ',i2,' of Flow Dist. for subcat ',i3,
     *            ' : cl(j,i).lt.cl(j-1,i).or.pd(j,i).lt.pd(j-1,i)')
              go to 290
           endif
	endif
 6000 CONTINUE
	rtemp1=cl2(1,i)
	ptemp1=pd2(1,i)
	ptemp2=pd2(ND(I),i)
	if(rtemp1.ne.0.or.ptemp1.ne.0.or.ptemp2.ne.1) then
           write(21,51)i
  51      format(' Error in Flow Dist. for subcat ',i3,
     *         ' : cl(1,i).ne.0.or.pd(1,i).ne.0.or.pd(last,i).ne.1')
           go to 290
        endif
C  Read basin initial properties
C  **********************************
c	write(6,7712) LUNRD
c 7712	format(' LUNRD',i6)
      READ(LUNRD,'()')               ! Comment line
      if(limits.and.i.eq.1) then
	 READ(LUNopt,'()') ! Comment line
	do 400 j=1,NSI
	 READ(LUNopt,*,ERR=281) minsi(j),maxsi(j)
  400 continue
      endif
      LINE = 8
c      DO 230 I=1,NS
         READ(LUNRD,*,ERR=280) (SI(J,I),J=1,NSI)   !  One property per line
c	write(6,5070) (si(j,i),j=1,nsi)
c 5070	format(' The SIs are: ',2f14.3)
c  230 CONTINUE
  240 CONTINUE
craw 14-feb-2004
	close(88)
c	write(6,5090)NGAUGE,NS,NRCH,NKA,ND,LL,NTR,NTS,LINKS,LINKR
c Read in the parameter map. The first number tells if the following data
c relates to a sub-basin (1), a reach (2), or an initial condition (3)
c The second value tells which hillslope, reach or initial condition is
c to be altered. The third indicates which sub-basin the parameter relates to.
c If the third parameter is .le. 0 then the parameter value is to be assigned
c to all sub-basins. The fourth value indicates where the parameter lies in the
c NLFIT parameter box list.
c
c   DGT Parameter map read was here - moved.
C
C CHECK THE USE OF NEGATIVE AREA VALUES TO INDICATE  BASIN COPIES
C
      DO 250 I=1,NS
          IF ( SP(1,I) .LE. 0D0 ) THEN
              II = NINT(SP(2,I))
              IF ( II .LT. 1 .OR. II.GT.NS ) THEN
                  WRITE(21,247)I,NS
  247 FORMAT(' Basin',I3,' area is <=0 : must have 1<=length<=',I3)
                  GOTO 290
              ENDIF
              IF ( SP(1,II) .LT. 0D0 ) THEN
                  WRITE(21,249)I
  249 FORMAT(
     1' Basin',I3,'area is <=0:length must point to real basin')
                  GOTO 290
              ENDIF
          ENDIF
  250 CONTINUE
C
C  Read reach properties
C  *********************
C Note that because we added artificial gutters, we don't expect to
C read any data for them, so replace NRCH by NRCH-N0RCH
      READ(LUNRD,'()')               ! Comment line
      DO 260 I=1,NRCH-N0RCH
	LINE = 6
	if(limits.and.i.eq.1) then
6665  FORMAT(1X,L1,4I10)
	  READ(LUNopt,'()')  ! Comment line
      do 401 j=1,NRP
        READ(LUNopt,*,ERR=281) minrp(j),maxrp(j)
  401 continue
      endif
      READ(LUNRD,*,ERR=280) (RP(J,I),J=1,NRP)   !  One property per line
  260 CONTINUE
C
C  Return to calling program
C  *************************
  270 CLOSE(LUNRD)
	close(73)     ! DGT 7/24/05 WRG1 Interpolation weights
      RETURN
C
  299 CONTINUE
      WRITE(21,298)
  298 FORMAT(' Error reading rain.dat in MDDATA')
      STOP
  294 CONTINUE
      WRITE(21,293)
  293 FORMAT(' Error reading streamflow_calibration.dat in MDDATA')
      STOP
  281 CONTINUE
      WRITE(21,286)
  286 FORMAT(' Error reading modelcon.dat')
      STOP
  280 CONTINUE
      WRITE(21,285) LINE,I
  285 FORMAT(' Error reading the model description file ',I4,I3)
  290 CONTINUE
      WRITE(21,295)
  295 FORMAT(' Error in data in the model description file ')
       STOP
  296 CONTINUE
      WRITE(21,297)
  297 FORMAT(' Error in the parameter map data in the model',
     *       ' description file ')
       STOP
      END
****************************************************************************
*         SUBROUTINE READ_LAKES
*****************************************************************************
	SUBROUTINE READ_LAKES(LAKE_REACH,LZERO,LAKE_AREAS,LAKE_BEACH_SLPS,
     *	LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,MAX_LAKES,MAX_LHEADS)

	IMPLICIT NONE  !  30/03/2004 ITB

      INCLUDE 'maxvariables.inc'
	INCLUDE  'Luns.inc'
      INCLUDE  'Tdims_v7.inc'
C Lake variables
	INTEGER NLAKES,LAKE_REACH(MAX_LAKES),LZERO(MAX_LAKES),
     *        LK_LINE(MAX_LAKES)
	INTEGER LAKE_BEACH_SLPS(MAX_LAKES),NUM_RAT_VALS(MAX_LAKES)
	INTEGER LHEADS(MAX_LAKES,MAX_LHEADS)
	integer LOFLOWS(MAX_LAKES,MAX_LHEADS)
	INTEGER*4 I,J,LAKE    ! 30/03/2004 ITB


      REAL*4 LAKE_AREAS(MAX_LAKES)  ! RPI 17/5/2002 added ini_levels
	REAL*8 TEMP   ! 30/03/2004  ITB

      COMMON /LAKES1/ NLAKES !,LAKE_REACH,LZERO,LAKE_AREAS,
C     *           LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,
C     *           INI_LEVELS ! RPI 17/5/2002 added ini_levels
      COMMON /LAKES2/ LAKE

      WRITE(21,*) ' Found LAKES.DAT'
	READ(LUNRD,*,end=5,err=2)
	READ(LUNRD,*,end=5,err=2) NLAKES
	READ(LUNRD,*,end=5,err=2)
	READ(LUNRD,*,end=5,err=2) (LAKE_REACH(I),I=1,NLAKES)
	READ(LUNRD,*,end=5,err=2)
	DO 1 I=1,NLAKES
	READ(LUNRD,*,end=5,err=2)
	READ(LUNRD,*,end=5,err=2)
     *              LZERO(I),LAKE_AREAS(I),LAKE_BEACH_SLPS(I),LK_LINE(I)
	READ(LUNRD,*,end=5,err=2)
	READ(LUNRD,*,end=5,err=2) NUM_RAT_VALS(I)
	READ(LUNRD,*,end=5,err=2)
	READ(LUNRD,*,end=5,err=2)
     *                    (LHEADS(I,J),LOFLOWS(I,J),J=1,NUM_RAT_VALS(I))
1	CONTINUE
	RETURN
5	WRITE(21,3)
3	FORMAT(' **** Error - End of LAKES.DAT reached prematurely')
	STOP
2	WRITE(21,4)
4	FORMAT(' **** There was an error reading LAKES.DAT')
	STOP
	END
*************************************************************************
	SUBROUTINE CHK_SITES(LRG,SITES,J,IMATCH)
! RPI 23/1/03 added IMATCH

      IMPLICIT NONE ! 30/03/2004 ITB


      INTEGER*4 SITES,ITOL,LPE,LPN,SPE,SPN,DE,DN
	INTEGER LRG,J,IMATCH
c itol=1 means we will tolerate an error of 1 part in 1000 in the easting or northing
c component of the site number for a mesoscale grid point
	ITOL=1
c we want an exact test for equality if we are using raingauges
	if (lrg .le. 9999999) itol=0
	LPE=LRG/10000
	LPN=LRG-LPE*10000
	SPE=SITES/10000
	SPN=SITES-SPE*10000
	DE=IABS(LPE-SPE)
	DN=IABS(LPN-SPN)
	IF(DE+DN.EQ.0) THEN
		LRG=J
		IMATCH=1
		RETURN
	ENDIF
	IF(DE.LE.ITOL.AND.DN.LE.ITOL) THEN
C RPI 3/3/03 commented out the write stmt.
C	WRITE(21,1) LRG,SITES
C    1 FORMAT('**** Warning - not an exact match for rainfall sites',2i9)
    		LRG=J
		IMATCH=1
		RETURN
	ENDIF
	RETURN
	END

******************************************************************************
	SUBROUTINE READ_LAKES_LEVELS(LAKE_REACH,LZERO,LAKE_AREAS,
     * LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,INI_LEVELS,
     * MAX_LAKES,MAX_LHEADS)

      IMPLICIT NONE ! 30/03/2004 ITB


	INCLUDE 'maxvariables.inc'
	INCLUDE  'Luns.inc'
      INCLUDE  'Tdims_v7.inc'
C Lake variables
	INTEGER NLAKES,LAKE_REACH(MAX_LAKES),LZERO(MAX_LAKES),
     *        LK_LINE(MAX_LAKES),LAKE_REACH_INI(MAX_LAKES),NLAKES_INI
	INTEGER LAKE_BEACH_SLPS(MAX_LAKES),NUM_RAT_VALS(MAX_LAKES)
	INTEGER LHEADS(MAX_LAKES,MAX_LHEADS)
	integer LOFLOWS(MAX_LAKES,MAX_LHEADS)
	INTEGER*4 I,J,ITEMP, LAKE    ! 30/03/2004 ITB

      REAL*4 LAKE_AREAS(MAX_LAKES),INI_LEVELS(MAX_LAKES)  ! RPI 17/5/2002 added ini_levels
	REAL*8 TEMP ! 30/03/2004 ITB

      COMMON /LAKES1/ NLAKES !,LAKE_REACH,LZERO,LAKE_AREAS,
C     *           LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,
C     *           INI_LEVELS ! RPI 17/5/2002 added ini_levels
      COMMON /LAKES2/ LAKE

      WRITE(21,*) ' Found LAKES_LEVELS.DAT'
	do i=1,nlakes
	INI_LEVELS(I)=-1
	enddo

      READ(LUNRD,*)
C RPI 23/7/02 added the next 2 lines + the sorting code after the read
      READ(LUNRD,*,end=1,err=2) NLAKES_INI
	READ(LUNRD,*,end=1,err=2) (LAKE_REACH_INI(I),I=1,NLAKES_INI)
! Negative values mean program is to calculate the initial level
	READ(LUNRD,*,end=1,err=2) (INI_LEVELS(I),I=1,NLAKES_INI)
C Sort the initial levels into the same order as the rest of the lake data

      do i=1,nlakes
		do j=1,nlakes_ini
		if(LAKE_REACH(i).eq.LAKE_REACH_INI(j)) then
			temp=INI_LEVELS(I)
			itemp=LAKE_REACH_INI(i)
			INI_LEVELS(I)=INI_LEVELS(j)
			LAKE_REACH_INI(i)=LAKE_REACH_INI(j)
			INI_LEVELS(j)=temp
			LAKE_REACH_INI(j)=itemp
		endif
		enddo
	enddo
	RETURN
1	WRITE(21,3) NLAKES
3	FORMAT
     +(' **** Error - not enough data in file, no. of lakes = ',I3)
	STOP
2	WRITE(21,4)
4	FORMAT(' **** There was an error reading LAKE_LEVELS.DAT')
	STOP
	END

