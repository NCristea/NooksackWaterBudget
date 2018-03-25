C***********************************************************************
C
C This version, V2, has enhanced checking for missing data in the RAMS input
C and rainfilling
C V3 also has checking/matching of the flow sites with reaches - RPI 17/9/01
C
	SUBROUTINE HYDATA(SDATE,SHOUR,interv,M,MI,MPS,MPE,ngauge,neq,! nout,  RPI added nout 16/3/99
     &         AREA,brain,FLOW,IRET,IGAPS,temper,dewp,trange,dtbar,
     &	     NS,WRG,LRG,MAXSLP,elevtg,btmax,btmin,btdew,bdtbar,sp,
     &			bxlat,bxlon,xlat,xlon,stdlon,
     &		 MAXGAUGE,MAXINT,MAXSITES,MAXRESPONSE,MAXTGAUGE,wind2m,
     &         wrg1,idebugoutput,idebugbasin,idebugcase)  ! DGT 7/24/05 WRG1 Interpolation weights
	! DGT 8/17/05 added idebugoutput and kcase.

      IMPLICIT NONE  ! 30/03/2004 ITB


	INCLUDE 'maxvariables.inc'
	INCLUDE 'Tdims_v7.inc'
	INCLUDE 'Luns.inc'

	INTEGER M, MI, IRET
	INTEGER IGAPS(3)
	INTEGER*4 SDATE, SHOUR  ! RPI 17/3/2004, INT
	INTEGER*4 DATE , HOUR
	LOGICAL OD
	INTEGER NGAUGE,NEQ  ! RPI added declarations 27/1/03
      INTEGER*4  NTRI, INT,JJ,IJ, MPE  ! 30/03/2004 ITB
      INTEGER*4  JS, NS, KG ! 30/03/2004 ITB


	INTEGER*8 interv
C RPI 2/9/2002 made these INTEGER*8 to go with the new Tideda td8_micsec
	INTEGER*8 ITEMP1 ,ITEMP2, ITEMP3 ! RPI 2/8/01 added ITEMP3
	REAL*8 brain(MAXSLP,MAXINT),wind2m(maxint)
	REAL*8 btmax(MAXSLP,MAXINT),btmin(MAXSLP,MAXINT)
	REAL*8 btdew(MAXSLP,MAXINT),elevtg(MAXTGAUGE),SP(NSP,MAXSLP)
	real*8 bxlat(MAXSLP),bxlon(MAXSLP)  ! ,bstdlon(MAXSLP)   ! 7/23/05  no longer used
	real*8 xlat(MAXTGAUGE),xlon(MAXTGAUGE),stdlon  ! DGT 5/14/12 to make compile stdlon(MAXTGAUGE)
	real*8 FLOW(maxresponse,MAXINT),bdtbar(12,MAXSLP)
	real*8 temper(MAXINT),dewp(MAXINT),trange(MAXINT)
	real*8 dtbar(12,MAXTGAUGE)
	REAL*8  AREA,TEMP
	REAL*8 WRG(MAXSLP,MAXGAUGE), WRG1(MAXSLP,MAXGAUGE)  ! DGT 7/24/05 WRG1 Interpolation weights
	INTEGER LRG(MAXSLP,MAXGAUGE), kk
	REAL*8 tempr(maxsites), tempr_last(maxsites) ! RPI 2/8/01 added tempr_last
	REAL*8 tempt(maxsites,3) !raw 9-dec-2004
	real*8, allocatable :: tempf(:), tempf_last(:) !raw 9-dec-2004
C
	INTEGER I, J, MPS, nwind, wsite(1)
C RPI 7/8/02 changed the dimension of dsite from maxrchareas
! RPI 16/3/04 removed dsite as it is checked by MDDATA where it is called qsite2
! and where it is compared with values from RCHAREAS.TXT
C	integer*4 dsite(maxsites) ! RPI 17/9/01 dsite holds the flow site no.s
C RPI 14/12/00 re-dimensioned fsite to be consistent with sub rainfill
	integer fsite(maxgauge,maxgauge),nfill(maxgauge)	!RAW rainfill 29/9/99
	integer*4 rsite(MAXGAUGE),fillflag		!RAW rainfill 29/9/99
	integer*4 tsite(MAXGAUGE)		!raw 9-dec-2004
	real*4 rcoeff(maxgauge,maxgauge)			!RAW rainfill 29/9/99
	character*128 rainfill_file,line					!RAW rainfill 13/10/99
	character*4 verno
	character str*200
	integer it_save_old,idebugoutput,idebugbasin, idebugcase
	logical i_reset_flag
	REAL*4 RAIN_FACTOR           ! RPI 8/7/2002

	COMMON /RAIN1/ RAIN_FACTOR   ! RPI 8/7/2002
c****************************************************************************
! RPI 16/3/04 changed neq to maxresponse - this test should not be needed in V7
c	if(maxsites.lt.max(maxresponse,ngauge,3)) then
c	write(*,*) ' **** Maxsites is less than either neq,ngauge or 3'
c	write(21,*)
c     &      ' **** Maxsites is less than either maxresponse,ngauge or 3'
c	stop
c	endif
C
	inquire(file='topinp.dat' ,opened=od)
	if (od .eqv. .false.) then
	open(20, file='topinp.dat',status='old',err=250)
	else
		rewind(20)
	endif
c	WRITE(*,*) 'Give starting date for simulation in form yymmdd: '
!	READ(20,500,end=250) SDATE
	READ(20,*,end=250) SDATE
  500   FORMAT(BN,I7)		!RAW 22/3/2000 changed from I6 to I7 to cope with year 2000
c	WRITE(*,*) 'Give starting time for simulation in form hhmmss: '
!	READ(20,500, end=250) SHOUR
	READ(20,*, end=250) SHOUR
c	WRITE(*,*) 'Give simulation time interval length in seconds: '
   90 READ(20,*, end=250) interv
c	IF ( INT .LT. 300 ) THEN
c		WRITE(*,*) 'The interval is less than 5 minutes',
c     1                  ' - please re-enter a larger value: '
c		GO TO 90
c	ENDIF
c	WRITE(*,*) 'Give number of required time steps in the run: '
!      READ(20,500, end=250) M
      READ(20,*, end=250) M
c	IF ( M .LE. 0 ) THEN
c		WRITE(*,*) 'The number of intervals is <=0 - ',
c     1                  ' - please re-enter a larger value: '
c		GO TO 91
c	ENDIF
	IF ( M .GT. MAXINT ) THEN
		WRITE(21,15)M,MAXINT
   15   FORMAT(' The simulation length of ',I5,' timesteps exceeds the'/
     1    ' program limit of ',I7,' The program limit will be used ')
	  m=maxint
	ENDIF
	IF ( M .le. 0 ) THEN   ! RPI 17/1/01 added next bit
		WRITE(21,16)
   16   FORMAT(' The simulation length < 1, see TOPINP.DAT')
	stop
	ENDIF
c	WRITE(*,*) 'Give time step at which detailed ouptut is to begin:'
!        READ(20,500, end=250) MPS
        READ(20,*, end=250) MPS
	IF ( MPS .LE. 0 ) MPS=1
	IF ( MPS .GT. M ) MPS=M
c	WRITE(*,*) 'Give time step at which detailed ouptut is to end:'
! RAW uses mpe=-1 for special output to dteail.txt - see sub topmod and calcts
!        READ(20,500, end=250) MPE ! RAW uses mpe=-1 for special output to dteail.txt
        READ(20,*, end=250) MPE ! RAW uses mpe=-1 for special output to dteail.txt
!
!  DGT 8/17/05  following 5 lines added to get variables needed to control debug IO
	read(20,'(A50)')line
      read(line,*)idebugoutput
	idebugbasin=0
	idebugcase=0
	if(idebugoutput .ne. 0)then
	   read(line,*)idebugoutput,idebugbasin,idebugcase
	endif

cRAW Landcare Ross removed next 3 statements to optionally suppress detail.txt output 17/7/2002
c	IF ( MPE .LE. 0 ) MPE=1
c	IF ( MPE .GT. M ) MPE=M
c	IF ( MPE .LT. MPS ) MPE=MPS
c	WRITE(LUNp,9595) SDATE,SHOUR,INT,M,MPS,MPE  ! DGT changed info output from lunmod to lunp
c 9595	FORMAT(' Simulation starts at ',2I8,' uses a time interval of',
c     *		I6, ' seconds'/' and contains',I5,' steps. Detailed'
c     *          ' printout begins at interval ',I5,' and ends at ',I5)

	CALL TD8MICSEC(SDATE,SHOUR,ITEMP2)
	MI=0
cmmm		DO 30 J = 1,M
cmmm			TIMES(J) = INT
cmmm 30              CONTINUE
cRAW 29/9/99 Read in data from rainfill.txt to fill missing rainfall
c need to read line 1 of rain.dat to look for site numbers
c if they're not there, we'll assume gauges are referred to as 1,2,3, ... ngauge
	do 2010 i=1,maxgauge  ! RPI 23/1/03 changed from ngauge
2010			 rsite(i)=i
	OPEN(LUNG,FILE='rain.dat',STATUS='OLD',ERR=200)
	read(lung,*)
	read(lung,*)
	READ(LUNG,*,ERR=2020) verno
	CLOSE(LUNG)
      OPEN(LUNG,FILE='rain.dat',STATUS='OLD',ERR=200)
	if(verno.eq.'Ver1'.or.verno.eq.'ver1'.or.
     +   verno.eq.'Ver2'.or.verno.eq.'ver2') then
c		READ(LUNG,*,ERR=2020,END=201) verno,ntri,(rsite(i),i=1,ngauge)
C RPI 7/8/02 replaced ngauge with ntri and put in the test
	read(lung,*)
	read(lung,*)
		READ(LUNG,*,ERR=2020,END=201) verno,ntri,(rsite(i),i=1,ntri)
! RPI 23/1/03 removed this test as now covered by chk_sites subroutine - see below
!		if(ntri.ne.ngauge) then
!			write(21,*) ' *****ERROR - Number of sites in rain.dat',
!    +                 ' does not match what modelspc.dat is expecting'
!			stop
!		endif
	ELSE
		READ(LUNG,*,ERR=2020,END=201)(rsite(i),i=1,ngauge)
		ntri=ngauge ! RPI 23/1/03 need to define ntri for changes below
	ENDIF
2020	close(lung)
      rainfill_file='rainfill.txt' 				!RAW rainfill 13/10/99

c Added by Prabu to intialize fsite after the dynamic allocation process
c on 08/07/2002  Prabu Chandra
	do i=1,maxgauge
		do j=1,maxgauge
			fsite(i,j) = 0
		end do
	nfill(i)=0
	end do
! RPI 23/1/03 changed ngauge to ntri

	call rainfill_read(rainfill_file,ntri,rsite,
     *	rcoeff,fsite,nfill,fillflag,MAXGAUGE) !If rainfill.txt is missing we continue on
	CLOSE(LUNG)
	DATE=0
	HOUR=0
	OPEN(LUNG,FILE='rain.dat',STATUS='OLD',ERR=200)
	read(lung,*)
	read(lung,*)
	READ(LUNG,*,ERR=200,END=201)
	I = 0
! RPI 23/1/03 changed ngauge to ntri to go with chk_sites subroutine changes
 203    READ(LUNG,*,ERR=200,END=202) (TEMPr(jj),jj=1,ntri),DATE,HOUR
! 203    READ(LUNG,*,ERR=200,END=202) (TEMPr(jj),jj=1,ngauge),DATE,HOUR
		CALL TD8MICSEC(DATE,HOUR,ITEMP1)
		IF( I .EQ. 0 ) THEN
c data starts after given start - flag as an error
		IF ( ITEMP2 .lt. ITEMP1-interv ) GO TO 230
c present data is before given start - go and read the next value
		IF ( ITEMP2 .gt. ITEMP1 ) GO TO 203   ! RPI 23/8/00 changed ge to gt
		END IF
		I = I + 1
C 2/8/01 RPI added some code here to allow for breaks in the RAIN.DAT file
		IF(I.EQ.1) THEN  ! First time through so need to set up values for ITEMP3 & TEMPR_LAST
			ITEMP3=ITEMP1-interv
		ENDIF
		IF(ITEMP1-ITEMP3.NE.interv) THEN
282			IF(ITEMP1-ITEMP3.LT.interv) THEN
				WRITE(21,*) ' Check data in RAIN.DAT near', DATE,HOUR
				STOP
			ENDIF
			WRITE(21,*) ' Data missing in RAIN.DAT near', DATE,HOUR
			WRITE(21,*) ' Missing data replaced with last given value'
			ITEMP3=ITEMP3+interv
			DO 281 JJ=1,ntri ! RPI 23/1/03 changed ngauge to ntri NGAUGE
c281			PREC(JJ,I)=TEMPR_LAST(JJ)/1000.
281			TEMPR(JJ)=TEMPR_LAST(JJ)
C Calculate basin rainfalls
c 			do js=1,ns			 ! for each subbasin
c				brain(js,i)=0.		 ! 'brain' has basin rain time series
c				do kg=1,maxgauge ! get brain by adding weighted 'rain'
c					if(lrg(js,kg) .gt. 0)
c    1       			brain(js,i)=brain(js,i)+
c   1				TEMPr(lrg(JS,kg))*wrg(JS,kg) ! /1000. no div for nooksack
c				enddo
c  			enddo
c			I=I+1
c			IF(ITEMP1-ITEMP3.NE.interv) GO TO 282
		ENDIF
C
C 2/8/01 RPI - New code ends here
C
C Convert rainfall from um/h to mm/h
CRAW was here to allow multiple raingauges
ccc RAW not for nooksack
c          do 297 jj=1,ntri ! RPI 23/1/03 changed ngauge to ntri ngauge
c 297		TEMPr(jj) = TEMPr(jj)/1000.
cRAW 29/9/99 Fill missing rainfall for this timestep: TEMPr overwritten
c don't try to fill missing data unless we found a rainfill.txt
C need to come here even if rainfall ends prematurely to ensure filling.
C******************** ignore filling gaps initially RPI 17/3/2004
  220		if (fillflag .gt. 0) then  ! RPI moved 220 from the close stmt below 25/9/01
! RPI 23/1/03 changed ngauge to ntri
! RPI 3/3/03 changed I to M in the argument list
			 call rainfill_doit(TEMPr,I,ntri,rcoeff,fsite,nfill,
     *			i_reset_flag,it_save_old,MAXGAUGE)
		endif
C Calculate basin rainfalls - code transferred from CALCTS
 	do js=1,ns			 ! for each subbasin
		brain(js,i)=0.		 ! 'brain' has basin rain time series
		do kg=1,maxgauge ! get brain by adding weighted 'rain'
			if(lrg(js,kg) .gt. 0) then
            	    brain(js,i)=brain(js,i)+
     1			TEMPr(lrg(JS,kg))*wrg(JS,kg) !RAW mm to um
			endif
  		enddo
  	enddo
		ITEMP3=ITEMP1 ! Finished filling, so update ITEMP3 AND TEMPR_LAST in case neede at next time step
		DO 283 JJ=1,ntri  ! RPI 23/1/03 changed ngauge to ntri NGAUGE
283		TEMPR_LAST(JJ)=TEMPR(JJ)
C RPI 3/3/03 - reset number of values if missing data at end of record
	if ( i_reset_flag ) then
          M = it_save_old
	    write(errlun,*) ' +++++++++++ WARNING WARNING ++++++++++++'
		write(errlun,800) it_save_old
800	format('***** Owing to missing rainfall at all statio
     1ns to the end of the'/
     +'      record it is not possible to reliably estimate flows beyond
     2'/    '      interval',i5,', therefore M reset accordingly')
	endif
C RPI 3/3/03 - end of additions here
	IF ( I .LT. M  ) GO TO 203
	CLOSE(LUNG)
C RPI 8/7/2002 put in the RAIN_FACTOR material. RAIN_FACTOR read in MDDATA
C from MODELSPC.DAT and the multiplication is done here to because we have
C to wait until Ross has done any filling required.
	do i=1,m
		do jj=1,NS ! RPI 23/1/03 changed ngauge to ntri ngauge
			brain(jj,i)=brain(jj,i)*RAIN_FACTOR
		enddo
	enddo
C RPI added down to next RPI comment on 23/2/96
c DGT does not need evap
c	DATE=0
c	HOUR=0
c	OPEN(LUNG,FILE='evap.dat',STATUS='OLD',ERR=300)
c	READ(LUNG,*,ERR=300,END=301)
c	I = 1
c 303    READ(LUNG,*,ERR=300,END=301) TEMP,DATE,HOUR
c		IF( I .EQ. 1 ) THEN
c		CALL MICSEC(DATE,HOUR,ITEMP1)
c data starts after given start - flag as an error
c		IF ( ITEMP2 .lt. ITEMP1-INT ) GO TO 300
c present data is before given start - go and read the next value
c		IF ( ITEMP2 .ge. ITEMP1 ) GO TO 303
c		END IF
c required starting date is straddled by ITEMP and ITEMP-INT
C Convert rainfall from um/h to mm/h
c		EVAP(I) = TEMP/1000.
c		I = I + 1
c		IF ( I .LE. M  ) GO TO 303
c	CLOSE(LUNG)
c     GO TO 304
c  300 WRITE(21,308) DATE,HOUR
c  308 FORMAT(' ***** Warning: EVAP.DAT does not exist or'/
c     1       '       contains an error at ',2I7/
c     2       '       If file missing all data set to zero')
c     3       '       Hit 1 to return otherwise 0' )
c      READ(*,*) itemp
c      IF (itemp .ne. 1) go to 304
c	IRET=4
c	RETURN
c  301   WRITE(21,*) ' ***** Evaporation file is empty, or ends ',
c     1'prematurely all values set to zero'
c       IRET=4
c       RETURN
c  304 CONTINUE
C RPI 23/2/96 - end of additions

	DATE=0
	HOUR=0
 	CLOSE (LUNG)
CRAW	OPEN(LUNG,FILE='temper.dat',STATUS='OLD',ERR=300)
	OPEN(LUNG,FILE='wind.dat',STATUS='OLD',ERR=300)
	I = 0 !RAW 28/10/99 code works as before, but error checking is easier for me
	read(lung,*)
	read(lung,*)
	READ(LUNG,*,ERR=2020,END=201) verno,nwind,(wsite(i),i=1,nwind)
	I = 0
 3103    READ(LUNG,*,ERR=3100,END=3101) TEMPr(1),DATE,HOUR
		CALL td8micsec(DATE,HOUR,ITEMP1)
		IF( I .EQ. 0 ) THEN  ! RPI 23/8/00 changed 1 to 0 RPI 26/05/2003 put this after call to td8_micsec
c data starts after given start - flag as an error
		IF ( ITEMP2 .lt. ITEMP1-interv ) GO TO 3100
c present data is before given start - go and read the next value
		IF ( ITEMP2 .gt. ITEMP1 ) GO TO 3103   ! RPI 23/8/00 changed ge to gt
		END IF
		I = I + 1
		wind2m(i)=tempr(1)
C 26/05/2003 RPI added some code here to allow for breaks in the TEMPER.DAT file
		IF(I.EQ.1) THEN  ! First time through so need to set up values for ITEMP3 & TEMPR_LAST
			ITEMP3=ITEMP1-interv
		ENDIF
		IF(ITEMP1-ITEMP3.NE.interv) THEN
1682			IF(ITEMP1-ITEMP3.LT.interv) THEN
				WRITE(21,*) ' Check data in WIND.DAT near', DATE,HOUR
				STOP
			ENDIF
			WRITE(21,*) ' Data missing in WIND.DAT near', DATE,HOUR
			WRITE(21,*) ' Missing data replaced with last given value'
			ITEMP3=ITEMP3+interv
			I=I+1
			IF(ITEMP1-ITEMP3.NE.interv) GO TO 1682
		ENDIF
			ITEMP3=ITEMP1 ! Finished filling, so update ITEMP3 AND TEMPR_LAST in case neede at next time step

		IF ( I .LT. M  ) GO TO 3103
	CLOSE(LUNG)
      GO TO 3104
 3100 WRITE(21,3108) DATE,HOUR
 3108 FORMAT(' ***** Warning: wind.DAT does not exist or'/
     1       '       contains an error at ',I9,I7,/
     1       ' Proceeding with constant temperature!')
 3101 if (m .ne. i) then
	   m=i
         WRITE(21,*) 'Warning: Temperature file Ends prematurely ',
     1               'number of time steps set to',m
	endif
	IF ( M .le.0 ) THEN   ! RPI added next few stmts 16/1/01
		WRITE(21,18)
	  stop
	ENDIF
c       IRET=4
c       RETURN
 3104 CONTINUE




c DGT 17/5/98 added block to read temperature data for evaporation
c RAW 28/10/99 put in default temperatures if there's no temperature data
 	do 219 i=1,m   ! RPI moved 220 from the close stmt below 16/1/01
	   temper(i)=10.
	   dewp(i)=7.
	   trange(i)=10.
 219	continue
	I=M
	DATE=0
	HOUR=0
 	CLOSE (LUNG)
CRAW	OPEN(LUNG,FILE='temper.dat',STATUS='OLD',ERR=300)
	OPEN(LUNG,FILE='tmaxtmintdew.dat',STATUS='OLD',ERR=300)
	I = 0 !RAW 28/10/99 code works as before, but error checking is easier for me
	read(lung,*)
	read(lung,*)
	READ(LUNG,*,ERR=2020,END=201) verno,ntri,(tsite(i),i=1,ntri)
	I = 0
!	READ(LUNG,*,ERR=300,END=301)
! 303    READ(LUNG,*,ERR=300,END=301) (TEMPr(jj),jj=1,3),DATE,HOUR
 303    READ(LUNG,*,ERR=300,END=301)((TEMPt(jj,js),js=1,3),jj=1,NTRI),
     &			DATE,HOUR
		CALL td8micsec(DATE,HOUR,ITEMP1)
		IF( I .EQ. 0 ) THEN  ! RPI 23/8/00 changed 1 to 0 RPI 26/05/2003 put this after call to td8_micsec
c data starts after given start - flag as an error
		IF ( ITEMP2 .lt. ITEMP1-interv ) GO TO 300
c present data is before given start - go and read the next value
		IF ( ITEMP2 .gt. ITEMP1 ) GO TO 303   ! RPI 23/8/00 changed ge to gt
		END IF
		I = I + 1
C 26/05/2003 RPI added some code here to allow for breaks in the TEMPER.DAT file
		IF(I.EQ.1) THEN  ! First time through so need to set up values for ITEMP3 & TEMPR_LAST
			ITEMP3=ITEMP1-interv
		ENDIF
		IF(ITEMP1-ITEMP3.NE.interv) THEN
682			IF(ITEMP1-ITEMP3.LT.interv) THEN
				WRITE(21,*) ' Check data in tmaxtmintdew.dat near', DATE,HOUR
				STOP
			ENDIF
			WRITE(21,*) ' Data missing in tmaxtmintdew.dat near', DATE,HOUR
			WRITE(21,*) ' Missing data replaced with last given value'
			ITEMP3=ITEMP3+interv
c  4/23/07  DGT added code below to implement missing within IF
 	do js=1,ns			 ! for each subbasin
		btmax(js,i)=0.		 ! 'btmax' has basin tmax time series
		btmin(js,i)=0.		 ! 'btmin' has basin tmax time series
		btdew(js,i)=0.		 ! 'btdew' has basin tmax time series
		do kg=1,maxtgauge ! get btmax by adding using lapse to get from gauge_elev to basin_elev, plus spatial weighting on tmax
			if(lrg(js,kg) .gt. 0) then  ! DGT 7/24/05 WRG1 Interpolation weights
            	    btmax(js,i)=btmax(js,i) + wrg1(JS,kg)*
     1	(TEMPt(lrg(JS,kg),1)+sp(13,js)*(elevtg(lrg(JS,kg))-sp(14,js)))
            	    btmin(js,i)=btmin(js,i) + wrg1(JS,kg)*
     1	(TEMPt(lrg(JS,kg),2)+sp(13,js)*(elevtg(lrg(JS,kg))-sp(14,js)))
            	    btdew(js,i)=btdew(js,i) + wrg1(JS,kg)*
     1	(TEMPt(lrg(JS,kg),3)+sp(13,js)*(elevtg(lrg(JS,kg))-sp(14,js)))
			endif
  		enddo
  	enddo
			I=I+1
			IF(ITEMP1-ITEMP3.NE.interv) GO TO 682
		ENDIF
C Calculate basin temps - code adapted from basin rain raw 9-dec-2004
 	do js=1,ns			 ! for each subbasin
		btmax(js,i)=0.		 ! 'btmax' has basin tmax time series
		btmin(js,i)=0.		 ! 'btmin' has basin tmax time series
		btdew(js,i)=0.		 ! 'btdew' has basin tmax time series
		do kg=1,maxtgauge ! get btmax by adding using lapse to get from gauge_elev to basin_elev, plus spatial weighting on tmax
			if(lrg(js,kg) .gt. 0) then  ! DGT 7/24/05 WRG1 Interpolation weights
            	    btmax(js,i)=btmax(js,i) + wrg1(JS,kg)*
     1	(TEMPt(lrg(JS,kg),1)+sp(13,js)*(elevtg(lrg(JS,kg))-sp(14,js)))
            	    btmin(js,i)=btmin(js,i) + wrg1(JS,kg)*
     1	(TEMPt(lrg(JS,kg),2)+sp(13,js)*(elevtg(lrg(JS,kg))-sp(14,js)))
            	    btdew(js,i)=btdew(js,i) + wrg1(JS,kg)*
     1	(TEMPt(lrg(JS,kg),3)+sp(13,js)*(elevtg(lrg(JS,kg))-sp(14,js)))
			endif
  		enddo
  	enddo
c  DGT 7/24/05 moved averaging that was here to outside the time step loop.  It only needs to be done once

			ITEMP3=ITEMP1 ! Finished filling, so update ITEMP3 AND TEMPR_LAST in case neede at next time step
!			DO 683 JJ=1,3
!683			TEMPR_LAST(JJ)=TEMPR(JJ)
C
C 26/05/2003 RPI - New code ends here
c required starting date is straddled by ITEMP and ITEMP-INT
!		temper(I) = tempr(1)/1000.
!	    dewp(i)=tempr(2)/1000.
!	    trange(i)=tempr(3)/1000.
		IF ( I .LT. M  ) GO TO 303

c  DGT 7/24/05 moved this averaging outside the time step loop.  It only needs to be done once
c	bxlat(:)=0;bxlon(:)=0;bstdlon(:)=0;   ! DGT 7/23/05  Incorrect to do this weighting for lat and long
	do js=1,ns
		bdtbar(1:12,js)=0
		do kg=1,maxtgauge ! get basin average dtbar by averaging dtbar values for gauges
			if(lrg(js,kg) .gt. 0) then
				do kk=1,12
					bdtbar(kk,js)=bdtbar(kk,js) + wrg1(JS,kg)*
     1					dtbar(kk,lrg(JS,kg))  ! DGT 7/24/05 WRG1 Interpolation weights
				end do
! DGT 7/23/05  Incorrect to do this weighting for lat and long
c				bxlat(js)=bxlat(js)+wrg(JS,kg)*xlat(lrg(js,kg))
c				bxlon(js)=bxlon(js)+wrg(JS,kg)*xlon(lrg(js,kg))
c				bstdlon(js)=bstdlon(js)+wrg(JS,kg)*stdlon(lrg(js,kg))
			endif
		enddo
	enddo


	CLOSE(LUNG)
      GO TO 304
  300 WRITE(21,308) DATE,HOUR
  308 FORMAT(' ***** Warning: temper.DAT does not exist or'/
     1       '       contains an error at ',I9,I7,/
     1       ' Proceeding with constant temperature!')
c     3       '       Hit 1 to return otherwise 0' )
c      READ(*,*) itemp
c      IF (itemp .ne. 1) go to 304
c	IRET=4
c	RETURN
  301 if (m .ne. i) then
	   m=i
         WRITE(21,*) 'Warning: Temperature file Ends prematurely ',
     1               'number of time steps set to',m
	endif
	IF ( M .le.0 ) THEN   ! RPI added next few stmts 16/1/01
		WRITE(21,18)
	  stop
	ENDIF
c       IRET=4
c       RETURN
  304 CONTINUE

c End of DGT temperature data additions
c RAW 28/10/99 put in zero runoff in case there's no runoff data (e.g. flood forecasting)
      do jj = 1,maxresponse   ! RPI 16/3/2004 changed this from neq
	   do i=1,m
	    	FLOW(jj,I)=0.
		enddo
	enddo
	I=M
	DATE=0
	HOUR=0
! RPI 16/3/04 removed reference to dsite - see dimension stmt for comment as to why
!	do i=1,neq ! RPI 17/9/01 there should be this no. of sites in modelspc.dat
!		dsite(i)=i
!	enddo
	CLOSE (LUNG)
	OPEN(LUNG,FILE='streamflow_calibration.dat',STATUS='OLD',ERR=2031)
	read(lung,*)
	read(lung,*)
	read(lung,*)
	READ(LUNG,*,ERR=2020) verno
	CLOSE(LUNG)
      OPEN(LUNG,FILE='streamflow_calibration.dat',STATUS='OLD',ERR=2031)
	if(verno.eq.'Ver1'.or.verno.eq.'ver1'.or.
     +   verno.eq.'Ver2'.or.verno.eq.'ver2') then
! RPI 16/3/04 removed reference to dsite - see above
	read(lung,*)
	read(lung,*)
	read(lung,*)
		READ(LUNG,*,ERR=2020,END=201) verno,ntri !(dsite(i),i=1,ntri)
		if(ntri.ne.neq) then
			write(21,*)
     +' *****ERROR - Number of sites in streamflow_calibration.dat',
     +                 ' does not match what modelspc.dat is expecting'
!		stop !raw 9-dec-2004
			neq=ntri !raw 9-dec-2004
		endif
		allocate (tempf(ntri),tempf_last(ntri)) !raw 9-dec-2004
	ELSE
		READ(LUNG,*,ERR=2020,END=201)
	ENDIF
C      OPEN(LUNG,FILE='runoff.dat',STATUS='OLD',ERR=2031)
C	READ(LUNG,*,ERR=204,END=205)
	I = 0 !RAW 28/10/99 code works as before, but error checking is easier for me
  207   READ(LUNG,*,ERR=204,END=206) (tempf(jj),jj=1,neq),
     1        DATE,HOUR  ! RPI 16/3/99 added max test on upper index max(neq,nout)
		CALL td8micsec(DATE,HOUR,ITEMP1)
		IF( I .EQ. 0 ) THEN  ! RPI 23/8/00 changed 1 to 0 RPI 26/05/2003 put this after call to td8_micsec
c data starts after given start - flag as an error
		IF ( ITEMP2 .lt. ITEMP1-interv ) GO TO 230
c present data is before given start - go and read the next value
		IF ( ITEMP2 .gt. ITEMP1 ) GO TO 207  ! RPI 23/8/00 changed ge to gt
		END IF
		I = I + 1
C 26/05/2003 RPI added some code here to allow for breaks in the RUNOFF.DAT file
		IF(I.EQ.1) THEN  ! First time through so need to set up values for ITEMP3 & tempf_LAST
			ITEMP3=ITEMP1-interv
		ENDIF
		IF(ITEMP1-ITEMP3.NE.interv) THEN
782			IF(ITEMP1-ITEMP3.LT.interv) THEN
				WRITE(21,*)
     +' Check data in streamflow_calibration.dat near', DATE,HOUR
				STOP
			ENDIF
			WRITE(21,*)
     +' Data missing in streamflow_calibration.dat near', DATE,HOUR
			WRITE(21,*) ' Missing data replaced with last given value'
			ITEMP3=ITEMP3+interv
			DO 781 JJ=1,neq ! RPI 23/1/03 changed ngauge to ntri NGAUGE
781			FLOW(JJ,I)=tempf_LAST(JJ)/1000.
			I=I+1
			IF(ITEMP1-ITEMP3.NE.interv) GO TO 782
		ENDIF
			ITEMP3=ITEMP1 ! Finished filling, so update ITEMP3 AND tempf_LAST in case neede at next time step
			DO 783 JJ=1,neq
783			tempf_LAST(JJ)=tempf(JJ)
C
C 26/05/2003 RPI - New code ends here
c required starting date is straddled by ITEMP and ITEMP-INT
C Convert runoff from um/ts to mm/ts
          do jj = 1,neq ! RPI 16/3/99 added max test on upper index max(neq,nout)
	    	FLOW(jj,I)=tempf(jj)/1000.
	    enddo
		IF ( I .LT. M  ) GO TO 207
	CLOSE(LUNG)
c	close(20)
cDGT not closed so that top1 can read from this
 2031 if (m .ne. i) then
			 WRITE(21,3031) DATE,HOUR
 3031 FORMAT(
     1' ***** Warning: streamflow_calibration.dat does not exist or'/
     1       '       contains an error at ',I9,I7,/
     1       ' Proceeding with measured runoff=0!')
	endif
	return
 3011 if (m .ne. i) then
	   m=i
         WRITE(21,*) 'Warning: Temperature file Ends prematurely ',
     1'number of time steps set to',m
	 endif
	IF ( M .le.0 ) THEN   ! RPI added next few stmts 16/1/01
	  WRITE(21,18)
   18   FORMAT(' **** Change to length of temperature data has caused '/
     1        ' a value of data length < 1 - check simulation dates in'/
     2         ' TOPINP.DAT and TEMPER.DAT')
	  stop
	ENDIF
	RETURN
 250  write(21,251)
 251	format('Error or End of file reading topinp.dat')
	iret=4
	return
  200   WRITE(21,208) DATE,HOUR
  208   FORMAT(
     1' ***** Error in RAIN.DAT or streamflow_calibration.dat at '
     1 ,I9,I7)
	IRET=4
	RETURN
  201   WRITE(21,*) ' ***** Rainfall file is empty'
	IRET=4
	RETURN
  202   M=I
	IF ( M .le.0 ) THEN   ! RPI added next few stmts 16/1/01
		WRITE(21,17)
   17   FORMAT(' **** Premature end to data has caused a value of data'/
     1' length < 1 - check simulation dates in TOPINP.DAT and/or'/
     2' the dates in RAIN.DAT and streamflow_calibration.dat')
	  stop
	ENDIF
	WRITE(21,209) M
  209   FORMAT(' ***** Rainfall ends prematurely - number of ',
     1          'values reduced to ',I6)
	GO TO 220
  204   WRITE(21,210) DATE,HOUR
  210   FORMAT(
     1' ***** Error in file streamflow_calibration.dat at ',I9,I7)
	IRET=4
	RETURN
  205   WRITE(21,*) ' ***** File streamflow_calibration.dat is empty'
	IRET=4
	RETURN
C RPI 22/7/02 replaced the code below with new code for forecasting
C The aim is to remove the need to keep making RUNOFF.DATs with -1 values
C in them
c  206   M=I
c	WRITE(21,211) M
c  211   FORMAT(' ***** RUNOFF DATA ENDS PREMATURELY - NUMBER OF ',
c     1          'VALUES REDUCED TO ',I7)
c	IF ( M .le.0 ) THEN   ! RPI added next few stmts 16/1/01
c		WRITE(21,17)
c	  stop
c	ENDIF
c	RETURN
  206 do ij=I+1,M
 		do jj=1,neq
 			FLOW(jj,ij)=-1.
 		enddo
 	enddo
 	WRITE(21,211)
  211 FORMAT(' ***** RUNOFF DATA ENDS PREMATURELY - Flows set to ',
     1          '-1 - Ignore this message if forecasting')
	IF ( M .le.0 ) THEN   ! RPI added next few stmts 16/1/01
		WRITE(21,17)
	  stop
	ENDIF
cRAW 4/6/03 add CLOSE statement: need to close this because we will be opening it again later
	CLOSE(LUNG)
	RETURN
  230   WRITE(21,232) DATE,HOUR,SDATE,SHOUR
  232   FORMAT(' Start time of the rainfall, or runoff data ',
     1        I9,I7,' before start of filed data at ',I9,I7)
	IRET=4
	RETURN
c  231   WRITE(*,233) DATE,HOUR,SDATE,SHOUR
c  233   FORMAT(' Start time of the runoff data ',
c     1        2I7,' differs from',2I7)
c        IRET=4
c        RETURN
	END
cRAW 29/9/99
*******************************************************************
*          SUBROUTINE  RAINFILL_DOIT
c Code to fill in missing rainfall data using regression on other sites
c This new section of code contains two subroutines and two functions
c Designed to be called after rain.dat has been read (e.g. from HYDATA),
c but before rain data is used.
c -1 in rain.dat is assumed to be the indicator for missing data
c Regression constants are stored in text file to be developed by model user
c File name is hard-coded 'rainfill.txt'.
c If the file is not present, the code doesn't do any filling

      	subroutine rainfill_doit(train,it,ngauge,rcoeff,fsite,nfill,
     *			i_reset_flag,it_save_old,
     *			MAXGAUGE)

      IMPLICIT NONE ! 30/03/2004 ITB

	INCLUDE 'maxvariables.inc'
	include 'Tdims_v7.inc'
	include 'Luns.inc'

      integer ngauge, i, itry, iok, ig, ig1, ifill, nt, it
	integer it_save,it_save_old        ! RPI 3/3/03 introduced these
	integer nfill(maxgauge), im, fsite(maxgauge,maxgauge)
	real*4 rcoeff(maxgauge,maxgauge)
	real*8 train(maxgauge),train_last(maxgauge)
	logical miss,canfill,i_reset_flag  ! RPI 3/3/03 introduced i_reset_flag

      if(it.eq.1) then
		it_save=0                      ! RPI 3/3/03
		i_reset_flag=.false.           ! RPI 3/3/03
	endif
c	do 100 it=1,nt !repeat separately for each timestep
	   !keep looping around the gauges until they're all filled, or we've tried too many times
								!write(6,*)'Timestep ',it
		itry=0 !just to get started
		iok=0
		do while (iok .lt. ngauge .and. itry .lt. ngauge)
								!write(6,*)'Try #',itry
			itry=itry+1
			iok=0
			do 50 ig=1,ngauge !check each gauge to see if it's missing
								!write(6,*)'Gauge ',ig
				if (miss(train(ig),nfill(ig))) then
					ifill=0 !if this gauge is missing try to fill it using available eqns
					do 30 im=1,iabs(nfill(ig))
						ig1=fsite(ig,im) !fill it if it's not already done
	               if (ig.ne.ig1) then !first decide whether we have some data to fill with
					      canfill=.NOT. miss(train(ig1),nfill(ig1))
	               else
	                  if (it.gt.1) then
	                     canfill=.NOT. miss(train_last(ig),nfill(ig))
	                  else
	                     canfill=.FALSE.
	                  endif
	               endif
						if ((ifill.eq.0) .and. canfill) then !if we haven't filled it yet and there's data available at the fill site then do it
							if (ig.ne.ig1) then
							    train(ig)=train(ig1)*rcoeff(ig,im)
							else
							    train(ig)=train_last(ig)*rcoeff(ig,im)
							endif
							ifill=1
		!write(errlun,*)'ig,im,ig1,train(ig1,it),rcoeff(ig,im),train(ig,it)'
		!write(errlun,*)ig,im,ig1,train(ig1,it),rcoeff(ig,im),train(ig,it)
						endif
 30					continue
					iok = iok+ifill
				else
					iok=iok+1
					train_last(ig)=train(ig)
				endif
 50			continue
		end do
		if (iok .eq. 0) then
			write(errlun,*)'Warning: all gauges missing data: step ',it
C RPI 3/3/03 introduced the following code to reduce the length of the forecast
C window when there is missing data at all gauges from time step IT_OLD_SAVE to
C the end of the rainfall data
			if ( it.eq.it_save+1 .and. i_reset_flag.eqv..false.) then
				it_save_old = it_save
				i_reset_flag = .true.
			endif
C RPI 3/3/03 - end of addtions here
			do 60 ig=1,ngauge
				if (it .gt. 1) then
					train(ig)=train_last(ig)
				else
					train(ig)=0
				endif
 60			continue
C RPI 3/3/03 inserted the next 2 lines
		else
			i_reset_flag = .false.
		endif
		it_save = it  ! RPI 3/3/03
c100	continue
	return
	end

	logical function miss(train,nfill)
	real*8 train
	integer nfill
	if (train .lt. 0 .or. (train.eq.0 .and. nfill.lt.0) ) then
		miss=.true.
	else
		miss=.false.
	endif
	return
	end
*****************************************************************
*     SUBROUTINE  RAINFILL_READ
******************************************************************

	subroutine rainfill_read(rainfill_file,ngauge,rsite,
     *			rcoeff,fsite,nfill,fillflag,MAXGAUGE)

      IMPLICIT NONE  ! 30/03/2004 ITB

	INCLUDE 'maxvariables.inc'
	include 'Tdims_v7.inc'
	include 'Luns.inc'

	integer lun, ig, ngauge, is(maxgauge), i, ierr, j
	integer nfill(maxgauge), nfill0, isrch
	integer fsite(maxgauge,maxgauge),fsite0(maxgauge)
	integer*4 msite, rsite(maxgauge), fillflag
	real*4 rcoeff(maxgauge,maxgauge),rcoeff0(maxgauge)
	character*128 filenm, rainfill_file

	lun=LUNG
	close(unit=lun)
	filenm=rainfill_file
	fillflag=-1
c	fsite(1,1)=-1
c	rcoeff(1,1)=-1.
c	nfill(1)=-9999
c	write(21,*)filenm
	open(unit=lun,file=filenm,err=100)
	fillflag=1
	do 40 j=1,ngauge
		ierr=0
		read(lun,*,end=100)msite,nfill0
 		read(lun,*,end=100)(rcoeff0(i),fsite0(i),i=1,iabs(nfill0))
		ig=isrch(msite,rsite,ngauge)
		if (ig .eq. 0) then
			write(errlun,*)'Rainfill: Site ',msite,' not in rain.dat'
			ierr=1
		endif
		do 20 i=1,iabs(nfill0)
			is(i)=isrch(fsite0(i),rsite,ngauge)
			if (is(i) .eq. 0) then
				write(errlun,*)'Rainfill: Site ',fsite0(i),' not in rain.dat'
				ierr=1
			endif
 20		continue

		if (ierr .eq. 0) then
			nfill(ig)=nfill0
			do 30 i=1,iabs(nfill0)
				rcoeff(ig,i)=rcoeff0(i)
C RPI 14/12/00 put in the next statements to help with debugging
c			if(is(i) .gt. 17) then
c	write(errlun,*) i,is(i),fsite0(i),nfill0
c	stop
c	endif
				fsite(ig,i)=is(i)
 30			continue
		endif
 40	continue
 100	close(unit=lun)
	return
	end

	integer function isrch(msite,rsite,ngauge)

      integer ngauge
	integer*4 msite, rsite(ngauge)
	integer*4 IG ! 30/03/2004 itb

      isrch=0
	do 400 ig=1,ngauge
		if (isrch .eq. 0 .and. msite .eq. rsite(ig)) then
			 isrch=ig
		endif
 400	continue
	return
	end


