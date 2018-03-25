C  This version, V4, automatically estimates zbar0 from initial flows
C
c
c Subroutines to link topmodel to NLFIT
c  The purpose of this routine is to read input and parameter data needed to
c  run TOPMODEL within nlfit, as well as outputs to be used in calibration.
c  The first 7 variables are required outputs returned to NLFIT for calibration.
c  nrx and iex are array dimensions. Common is used to pass input and parameter
c  data to the subroutine MODEL.
c
c  The main output is qact, a two dimensional array of data for use in calibration.
c  this has dimension (nrx,iex) with each column, j, representing a response vector
c  with values from init(j) to ined(j).  Times of rows in qact are given by actime.
c  npar gives the number  of model parameters that NLFIT may adjust in calibration
c  modelid gives a model name.
c
c RPI added the code related to ishift0 16/7/2002
c***************************************************************************
*         SUBROUTINE INPUTT
****************************************************************************

      SUBROUTINE INPUTT (initt, iend, neq, qact, actime, modelid,
     &                   npar, nrx, iex)
c	!MS$ATTRIBUTES DLLEXPORT :: inputt

	USE Data_Read

      IMPLICIT NONE ! 30/03/2004 ITB

      INCLUDE 'Tdims_v7.inc'
      INCLUDE 'Luns.inc'

      integer stat
      INTEGER*4  NGAUGE,NRCH,NS,IRET,pmap(4,dpm),nout,nrchsav,CHN,TOPINP
	INTEGER*4  MAXA2,MAXA1,MAXC2,MAXC1,neq0,nout0,linecount,ntri
	INTEGER*4  RPAIRS1,RPAIRS2

      CHARACTER(LEN=200) TEXT, str1, str2, str3, str4
	CHARACTER*4 verno

      LOGICAL OD,ext

      INTEGER, ALLOCATABLE :: LL(:),NTR(:),llout(:)
	INTEGER, ALLOCATABLE :: NTS(:)
	integer, ALLOCATABLE :: kllout(:)
      INTEGER, ALLOCATABLE :: LINKS(:,:)
	INTEGER, ALLOCATABLE :: LINKR(:,:)
	INTEGER, ALLOCATABLE :: LRG(:,:)
	integer, ALLOCATABLE :: clinkr(:,:)

      REAL*8, ALLOCATABLE :: SI(:,:),SP(:,:), bp(:,:)
	REAL*8, ALLOCATABLE :: RP(:,:)
	REAL*8 minsp(nsp),maxsp(nsp),minsi(nsi),maxsi(nsi)
	REAL*8 minrp(nrp),maxrp(nrp)
	REAL*8, ALLOCATABLE :: SId(:,:),SPd(:,:)
	REAL*8, ALLOCATABLE :: RPd(:,:)

	REAL*8 AREA
c   The arrays sid, spd and rpd record the original values  DGT
      INTEGER, ALLOCATABLE :: NKA(:),ND(:),ibout(:)
	INTEGER*4  nbout
      REAL*8, ALLOCATABLE :: ATB(:,:),PKA(:,:),TL(:)
      REAL*8, ALLOCATABLE :: PD(:,:),CL(:,:)
	REAL*8 UNITS
	REAL*8,ALLOCATABLE :: WRG(:,:)
	REAL*8,ALLOCATABLE :: WRG1(:,:)  ! DGT 7/24/05 WRG1 Interpolation weights
	INTEGER*4  IDOIT,maxfgauge
	integer*8 interv ! DGT 5/14/12 to get to compile


      COMMON /MODEL_MASTER/ IDOIT
      COMMON /MODEL1/ NGAUGE,NS,NRCH !,LL,NTR,NTS,LINKS,LINKR,
C     *               IRET,lrg,wrg
C      COMMON /MODEL3/ SI,SP,RP,ATB,PKA,TL,PD,CL,UNITS,sid,spd,rpd
      COMMON /MODEL4/ pmap,nout,nbout !,ibout,NKA,ND,llout

      INTEGER M, MI, MPS, MPE, NGUT, JG1, JG, IRCH, JR, JR1, IPOS, IPOSN
	INTEGER IGAPS(3), relflag, neq_temp
	INTEGER, ALLOCATABLE :: qmap(:)
C PHC 22/11/2002 rel changed to allocatable
	REAL*4, allocatable :: rel(:) ! RPI 19/9/01 for forecasting e
	INTEGER*4 SDATE, SHOUR, INT, STIM
C
cmmmm	INTEGER*4, ALLOCATABLE :: TIMES(:)
C RPI 2/9/2002 put in stim8 for Tideda v4 compatibility
	INTEGER*8 STIM8
C PHC 22/11/2002 ishift0 changed to allocatable
	integer, allocatable :: ishift0(:)
	REAL*8, ALLOCATABLE :: bRAIN(:,:)
	REAL*8, ALLOCATABLE :: btmax(:,:)
	REAL*8, ALLOCATABLE :: btmin(:,:)
	REAL*8, ALLOCATABLE :: wind2m(:)
	REAL*8, ALLOCATABLE :: btdew(:,:)
	REAL*8, ALLOCATABLE :: bxlat(:)
	REAL*8, ALLOCATABLE :: bxlon(:)
!	REAL*8, ALLOCATABLE :: bstdlon(:)   ! DGT 7/23/05  No longer use this
	real*8, ALLOCATABLE :: FLOW(:,:)
	REAL*8  dummyq
c   ET variables  DGT
	real*8, ALLOCATABLE :: xlat(:), xlong(:) ! , stdlon(:)   ! DGT 7/23/05  stdlon now a single variable
	real*8 stdlon   ! DGT 7/23/05  stdlon now a single variable
	real*8, ALLOCATABLE :: elevtg(:), dtbar(:,:),bdtbar(:,:)
	integer, ALLOCATABLE :: temper_id(:)
	real*8, ALLOCATABLE :: temper(:),dewp(:),trange(:)
	integer ns_temper,idebugoutput, idebugbasin, idebugcase

!      common /evap/xlat,xlong,stdlon,elevtg,dtbar !,temper,dewp,trange
	COMMON /MODEL2/ SDATE,SHOUR,INT,M,MI,MPS,MPE,
     *          STIM,AREA !TIMES,,PREC,EVAP,FLOW,IGAPS
	COMMON /CONSTR/ minsp,maxsp,minsi,maxsi,minrp,maxrp,limitc
! RPI 21/11/2002 removed the ishift0 from common
C PHC 22/11/2002 commented out common statement to avoid conflicts
C	COMMON /FORECAST/ relflag,rel
! RPI 21/11/2002 **********	COMMON /FORECAST/ relflag,rel,ishift0
c
      integer*4  nrx, iex
      integer*4 initt(iex), iend(iex), neq, i, j, npar, k
      REAL*8 qact(nrx,iex), actime(nrx)
      character*30 modelid
	LOGICAL*4 limitc
c-----
c Labelled common used to pass rainfall stored in vector p to
c subroutine model
c
CCC RAW 7/4/97 changed p(9000) to p(MAXINT)
c      double precision p(MAXINT)
c      common /abc_model/ p
c      save /abc_model/
c-----
C***********************************************************************
C Read values from data files
C RPI 15/7/2002 IDOIT tells model what operations it is to do only once
	IDOIT=0

c  open error output
	OPEN(21, file='toperror_v7.txt',status='unknown')

      INQUIRE(FILE='topinp.dat' ,OPENED=OD,EXIST=ext) ! RPI 15/8/2003 added EXIST
	IF(ext .eqv. .FALSE.) THEN
		WRITE(21,*) ' NO TOPINP.DAT - provide one!'
		STOP
	ENDIF
	IF (OD .eqv. .FALSE.) THEN
	OPEN(UNIT=20,FILE= 'topinp.dat',STATUS='OLD',access='sequential')
	ELSE
		REWIND(20)
	ENDIF
	DO j=1,3
		READ(20,*)
	END DO
	READ(20,*)TOPINP
	MAXINT=TOPINP+200
	REWIND(20)
	INQUIRE(FILE='modelspc.dat',OPENED=OD,EXIST=ext) ! RPI 15/8/2003 added EXIST
	IF(ext .eqv..FALSE.) THEN
		WRITE(21,*) ' NO MODELSPC.DAT - provide one!'
		STOP
	ENDIF
	IF (OD .eqv. .FALSE.) THEN
	OPEN(UNIT=2,FILE= 'modelspc.dat' ,
     *	STATUS='OLD')
	ELSE
		REWIND(2)
	ENDIF
	READ(2,*)
	READ(2,*)
	READ(2,*) MAXGAUGE,MAXSLP,CHN
	MAXCHN = MAXSLP + CHN
	REWIND (UNIT=2)

	! DGT 5/14/12  Initializing
	maxa2=0
	maxc2=0
	  ! prepare for long test strings
	  str1 = 'Following identifies the reaches to be used as r'
	  str2 = 'esponse time series and output time series'
      str3 = 'The number of points in the overland flow distan'
      str4 = 'ce distribution'
	stat = 0
	DO WHILE (stat .eq. 0)
		READ(2,'(a)', iostat=stat) TEXT
		IF (TEXT == 'Number of points in a/tan b distribution') THEN
			READ(2,*) MAXA1
			IF (MAXA2<MAXA1) THEN
				MAXA2=MAXA1
			ENDIF
!		ELSEIF (TEXT == 'Following identifies the reaches to be used as r
!     &esponse time series and output time series') THEN
        elseif (text == trim(str1) // trim(str2)) then
		READ(2,*) neq0,nout0
		ENDIF
!		IF (TEXT == 'The number of points in the overland flow distan
!     &ce distribution') THEN
        if (text == trim(str3) // trim(str4)) then
			READ(2,*) MAXC1
			IF (MAXC2<MAXC1) THEN
				MAXC2=MAXC1
			ENDIF
		ENDIF
	ENDDO
	MAXA=MAXA2
	MAXC=MAXC2
	maxresponse=max(neq0,nout0)
	CLOSE(2)

c raw 8-dec-2004 how many flow gauges?
	OPEN(UNIT=2,FILE='streamflow_calibration.dat')
	read(2,*)
	read(2,*)
	read(2,*)
	read(2,*)verno,MAXFGAUGE
	close(2)
	maxresponse=max(maxresponse,MAXFGAUGE)

c raw 8-dec-2004 how many temperature gauges?
	OPEN(UNIT=2,FILE='clipar.dat')
	read(2,*)
	read(2,*)
	read(2,*)MAXTGAUGE
	close(2)

C PHC 22/11/2002 Added to determine MAXRCHAREAS
C which is the no of lines in rchareas.txt
	linecount=0
	INQUIRE(FILE='rchareas.txt',OPENED=OD,EXIST=ext) ! RPI 15/8/2003 added EXIST
	IF(ext .eqv. .FALSE.) THEN
		WRITE(21,*) ' NO RCHAREAS.TXT - provide one!'
		STOP
	ENDIF
	IF (OD .eqv. .FALSE.) THEN
		OPEN(UNIT=9,FILE= 'rchareas.txt' ,STATUS='OLD')
	ELSE
		REWIND(9)
	ENDIF
	stat = 0
	DO WHILE (stat .eq. 0)
		READ(9,*, iostat=stat)
	linecount = linecount+1

	ENDDO
	MAXRCHAREAS = linecount
	close(9)

C RPI 21/8/2003 added the code relating to RAIN.DAT to cope with when
C there are more sites in RAIN.DAT than are required by the model.
	NTRI=0
	INQUIRE(FILE='rain.dat',OPENED=OD)
	IF (OD .eqv. .FALSE.) THEN
		OPEN(UNIT=9,FILE= 'rain.dat' ,STATUS='OLD',ERR=200)
		READ(9,*) verno
		CLOSE(9)
		OPEN(9,FILE='rain.dat',STATUS='OLD')
		if(verno.eq.'Ver1'.or.verno.eq.'ver1'.or.
     +			verno.eq.'Ver2'.or.verno.eq.'ver2') then
			READ(9,*) verno,ntri
		ENDIF
	CLOSE(9)
	ELSE
		REWIND(9)
	ENDIF
	MAXGAUGE=max(MAXGAUGE,ntri)
c  open error output  PRABU 13/08/02
	OPEN(21, file='toperror_v7.txt',status='unknown')

C Error check on iex and nrx  Prabu 13/08/02
	IF (nrx < maxint) THEN
		WRITE(21,*)'Error !!!'
		WRITE(21,*)'Number of observations in longest response record
     & has to be atleast',maxint
		STOP
	ELSE
		CONTINUE
      ENDIF

	IF (iex < maxresponse) THEN
		WRITE(21,*)'Error !!!'
		WRITE(21,*)'Maximum number of responses used in fitting of
     &model has to be atleast',maxresponse
		STOP
	ELSE
		CONTINUE
      ENDIF

C Read values from data files
c 21/01/2003 moved here from the model
C	CLOSE(5)  ! ITB 30/03/2004 PUT IT AS A COMMENT

	INQUIRE(FILE='lakes.dat',EXIST=OD)
	IF (OD .eqv. .TRUE.) THEN
		OPEN(UNIT=5,FILE= 'lakes.dat' ,STATUS='OLD',access='sequential')
	READ(5,*)
	READ(5,*) MAX_LAKES
	REWIND (5)
      stop !checkpoint
	RPAIRS1=0
	stat = 0
	DO WHILE (stat .eq. 0)
		READ(5,'(a)', iostat=stat) TEXT

		IF (TEXT == 'Give the number of rating "pairs"') THEN
			READ(5,*) RPAIRS1
			IF (RPAIRS2<RPAIRS1) THEN
				RPAIRS2=RPAIRS1
			ENDIF
		ENDIF
	ENDDO
	CLOSE(5)
	MAX_LHEADS=RPAIRS2
	ELSE
	MAX_LAKES=1
	MAX_LHEADS=1
	ENDIF

	MAXSITES = max(maxgauge,maxresponse,3)
C************************************************************************************************

C ALLOCATE DYNAMIC ARRAYS
	ALLOCATE (rel(MAXRCHAREAS),ishift0(maxrchareas))	!cmmmmTIMES(MAXINT),
	ALLOCATE (bRAIN(MAXSLP,MAXINT))
	ALLOCATE (btmax(MAXSLP,MAXINT))
	ALLOCATE (btmin(MAXSLP,MAXINT))
	ALLOCATE (wind2m(MAXINT))
	ALLOCATE (btdew(MAXSLP,MAXINT))
	ALLOCATE (bxlat(MAXSLP))
	ALLOCATE (bxlon(MAXSLP))
!	ALLOCATE (bstdlon(MAXSLP))   ! DGT 7/23/05  No longer use this
	ALLOCATE (FLOW(maxresponse,MAXINT))
	ALLOCATE (temper(MAXINT),dewp(MAXINT),trange(MAXINT))
	ALLOCATE (WRG(MAXSLP,MAXGAUGE),LRG(MAXSLP,MAXGAUGE))
	ALLOCATE (WRG1(MAXSLP,MAXGAUGE))  ! DGT 7/24/05 WRG1 Interpolation weights
	ALLOCATE (NTS(MAXSLP),LINKS(2,MAXSLP),SI(NSI,MAXSLP))
	Allocate (SP(NSP,MAXSLP),BP(num_basinpars,MAXSLP))
	ALLOCATE (SId(NSI,MAXSLP),SPd(NSP,MAXSLP))
	ALLOCATE (NKA(MAXSLP),ND(MAXSLP),ibout(maxslp))
	ALLOCATE (ATB(MAXA,MAXSLP),PKA(MAXA,MAXSLP),TL(MAXSLP))
	ALLOCATE (PD(MAXC,MAXSLP),CL(MAXC,MAXSLP))
	ALLOCATE (LL(MAXCHN),NTR(MAXCHN),llout(maxchn),LINKR(4,MAXCHN))
	ALLOCATE (clinkr(3,maxchn),RP(NRP,MAXCHN),RPd(NRP,MAXCHN))
	ALLOCATE (kllout(maxresponse),qmap(maxresponse))
	ALLOCATE (xlat(MAXTGAUGE),xlong(MAXTGAUGE)) !,stdlon(MAXTGAUGE))  ! DGT 7/23/05  No longer use this
	ALLOCATE (elevtg(MAXTGAUGE),dtbar(12,MAXTGAUGE),bdtbar(12,MAXSLP))
	ALLOCATE (temper_id(MAXTGAUGE))
C***********************************************************************

c
c Define model identification string and number of parameters
c
	modelid = 'TOP model'
	CALL CLIPARAM(xlat,xlong,stdlon,elevtg,dtbar,ns_temper,temper_id)
	IRET = 0

      CALL MDDATA(NGAUGE,NS,NRCH,NKA,TL,ATB,PKA,ND,CL,PD,UNITS,LL,NTR,
     *	 NTS,LINKS,LINKR,SId,SPd,RPd,IRET,pmap,npar,lrg,wrg,iex,llout,
     *	 neq,nout,nbout,ibout,nrchsav,qmap,rel,relflag,minsp,maxsp,
     *	 minsi,maxsi,minrp,maxrp,limitc,clinkr,kllout,ishift0,MAXINT,
     *     MAXGAUGE,MAXSLP,MAXRESPONSE,MAXA,MAXC,MAXCHN,MAXRCHAREAS,
     *     MAXSITES,bp,bxlat,bxlon,wrg1)  ! 7/23/05  DGT added bxlat, bxlon ! DGT 7/24/05 WRG1 Interpolation weights

C RPI 25/7/02 added the next bit for neq=0
	neq_temp=neq
c RPI 11/7/2003 neq=0 stuff obsolete in v7	if(neq.eq.0) neq_temp=nrchsav
cRAW moved MDDATA to before HYDATA, so we know how many raingauges to read
cRAW was here to allow multiple raingauges via ngauge
cDGT added capability for reading temperature, dewpoint etc for ET.
cDGT added neq to argument list for multiple response series
	CALL HYDATA(SDATE,SHOUR,interv,M,MI,MPS,MPE,ngauge,neq_temp,! nout,  RPI added nout 16/3/99
     &         AREA,bRAIN,FLOW,IRET,IGAPS,temper,dewp,trange,dtbar,
     &         NS,WRG,LRG,MAXSLP,elevtg,btmax,btmin,btdew,bdtbar,SPd,
     &			bxlat,bxlon,xlat,xlong,stdlon,
     &		 MAXGAUGE,MAXINT,MAXSITES,MAXRESPONSE,MAXTGAUGE,wind2m, !raw 9-dec-2004
     &         wrg1,idebugoutput,idebugbasin, idebugcase)  ! DGT 7/24/05 WRG1 Interpolation weights
	! DGT 8/17/05 added idebugoutput and kcase.

	IF ( IRET .NE. 0 ) THEN
	WRITE(21,*) ' **** An error has occurred on data input'
	STOP
	END IF
C Rearrange the FLOW array so that the "columns" match the reach order
C read in from MODELSPC.DAT using qmap.
C qmap(i) is the column in runoff.dat corresponding to reach j
	do i=1,neq_temp
		do j=1,neq_temp
		if(qmap(j).ne.j) then
			if(qmap(j).eq.i) then ! need to change order of ith and jth  columns
c Interchange columns i and qmap(i) and flag which columns are affected using imap
				do k=1,m
				dummyq=flow(i,k)
				flow(i,k)=flow(qmap(i),k)
				flow(qmap(i),k)=dummyq
				enddo
				qmap(j)=qmap(i)
				qmap(i)=i
			go to 150
			endif
		endif
		enddo
150	continue
	enddo

C At this point we have all the data needed to sort out the how to update
C the ZBAR0s for each measured sub-basin
	if(neq_temp.gt.maxresponse) then
		write(21,*) ' ****ERROR, maxresponse needs to be set to: ',
     +                                                         neq_temp
		stop
	endif
c      call SET_COR_DATA(clinkr,neq,nout,nrchsav,ns,kllout,flow,spd,
c     *  barea,ll,llout,MAXINT,MAXSLP,MAXCHN,MAXRESPONSE,DFLOW,SUMA)
c RPI 9/6/99 added the following line to define STIM

      CALL TD8MICSEC(SDATE,SHOUR,STIM8)	! RPI 9/2/2002 changed stim to stim8
	STIM=STIM8
c	neq = 1
c	initt(neq) = 1
c	iend(neq) = M
c
c Read flow data and precip data
c
	 do i = 1, max(neq,1)
	   initt(i)=1
	   iend(i)=m
		if ( iend(i) .le. nrx ) then
		  do j = initt(i),iend(i)
			qact(j,i) = FLOW(i,j)
c			p(j) = PREC(1,j)
		  end do
		else
		  write(21,*) ' **** You are trying to read too much data'
		  stop
		end if
	 end do
c
c Assign actual time to each observation
c
c        do i = initt(1), iend(1)
	 do i = 1,M
	    actime(i) = dble(i)
	 end do

c Prabu Chandra 30/01/2003
c All these variables have been deallocated b4 they enter MODEL
c Deallocate dynamic arrays
	DEALLOCATE (rel,ishift0,bRAIN,FLOW,kllout)  !cmmmmTIMES,
	DEALLOCATE (btmax,btmin,btdew,wind2m) !raw 9-dec-2004
	DEALLOCATE (temper,dewp,trange,WRG,wrg1,LRG,NTS,LINKS,SI,SP) ! DGT 7/24/05 WRG1 Interpolation weights
	DEALLOCATE (SId,SPd,NKA,ND,ibout,ATB,PKA,TL,PD,CL)
	DEALLOCATE (LL,NTR,llout,LINKR,clinkr,RP,RPd,qmap)

      return
 9090 write(21,*)
     1    ' **** Could not open toperror_v7.txt for writing'
      stop
  200   WRITE(21,208)
  208   FORMAT(' ***** Error in, or no RAIN.DAT')
	stop
      end
c
c************************************************************************72
c
c     Subroutine model designed to be called to compute response given a
c     time set and set of parameter values.  This is called once for each time step
c      Arguments
c      it - integer time step number
c      iflag - initialization flag set to 1 when model called first in an interation
c       0 otherwise.  This logic is circumvented here.  The model is run
c       only once each time step.  When iflag = 0 all this routine does is
c       copy computed results into the output array qfit
c      iopt - derivative flag set to 1 when nlfit requires response derivatives.
c       This is not possible here so an error is written
c      prt, a print indicatior 'y' or 'n' to control I/O
c      neq - number of response variables
c      npar - number of model parameters
c      npx - dimensioned size of number of parameters
c      iex - dimensioned size of number of response variables
c      nfor - fortran unit for I/O  - not used
c      qfit - Vector output flows - length iex, giving each response
c      dfit - array to return response derivatives - not used
c      par - vector of model parameters
c      mfit - flag specifying fit status indicating when a response
c        should be given - not used here - we always return flow
c        and never return derivative.
c      ifit - vector specifying fit status of each parameter.
c         Could apply parameter mapping only to these parameters - taking
c         others from modelspc.dat file
c      ibeale - flag intended to control parameter constraint checking
c         No param constraint checking yet implemented.
c**************************************************************************
      SUBROUTINE MODEL (it, iflag, iopt, prt, neq, npar, npx, iex,
     *                  nfor, qfit, dfit, par, mfit, ifit, ibeale)
c	!MS$ATTRIBUTES DLLEXPORT :: model
C
C         THE MODEL
      USE Data_Read
      IMPLICIT NONE ! 30/03/2004 ITB


      INCLUDE 'Tdims_v7.inc'
      INCLUDE 'Luns.inc'

C  Passed variables
C  ***********
	SAVE
      INTEGER NGAUGE,NS,NCHN,pmap(4,dpm),nout
	INTEGER NRCH,nrchsav
      INTEGER,ALLOCATABLE ::  LL(:),NTR(:),llout(:)
      INTEGER,ALLOCATABLE ::  LINKS(:,:),NTS(:)
	INTEGER,ALLOCATABLE ::  LINKR(:,:)
	INTEGER,ALLOCATABLE ::  LRG(:,:)
	INTEGER,ALLOCATABLE ::  qmap(:),clinkr(:,:)
      INTEGER IRET
      REAL*8,ALLOCATABLE ::  SI(:,:),SP(:,:),BP(:,:)
	REAL*8,ALLOCATABLE ::  RP(:,:)
	REAL*8 minsp(nsp),maxsp(nsp),minsi(nsi),maxsi(nsi)
	REAL*8 minrp(nrp),maxrp(nrp)
	REAL*8,ALLOCATABLE ::  SId(:,:),SPd(:,:)
	REAL*8,ALLOCATABLE ::  RPd(:,:)
!	REAL*8 BAREA(MAXCHN)  !  DGT 5/27/12  Seems to be a problem in that maxchn is dynamic and barea has not been passed in
      real*8,ALLOCATABLE:: barea(:)
c   The arrays sid, spd and rpd record the original values  DGT
	INTEGER nbout,relflag
	INTEGER NGUT, JG1, JG, IRCH, JR, JR1, IPOS, IPOSN
	REAL*4,ALLOCATABLE :: rel(:)
      INTEGER,ALLOCATABLE ::  NKA(:),ND(:),ibout(:)
	INTEGER,ALLOCATABLE ::  KLLOUT(:)
	INTEGER,ALLOCATABLE :: LOUT(:) ! RPI 4/10/00
      INTEGER,ALLOCATABLE :: ireach(:),KNT(:)
	INTEGER neq1   ! RPI 4/10/00
      INTEGER neq_temp ! 30/03/2004 itb


      REAL*8,ALLOCATABLE ::  ATB(:,:),PKA(:,:),TL(:)
      REAL*8,ALLOCATABLE ::  PD(:,:),CL(:,:)
	REAL*8  UNITS
	REAL*8,ALLOCATABLE ::  WRG(:,:)
	REAL*8,ALLOCATABLE ::  WRG1(:,:)  ! DGT 7/24/05 WRG1 Interpolation weights

	REAL*8 trangmaxgauge, dummyq ! 30/03/2004 tb

      INTEGER IDOIT
      integer*8 interv ! DGT 5/14/12

      COMMON /MODEL_MASTER/ IDOIT
	COMMON /MODEL1/ NGAUGE,NS,NCHN !,LL,NTR,NTS,LINKS,LINKR,
C    *               IRET,lrg,wrg
C     COMMON /MODEL3/ SI,SP,RP,ATB,PKA,TL,PD,CL,UNITS,sid,spd,rpd
      COMMON /MODEL4/ pmap,nout,nbout !,ibout,NKA,ND,llout
	COMMON /MODEL5/ NEQ1 !,LOUT,IREACH
	COMMON /CONSTR/ minsp,maxsp,minsi,maxsi,minrp,maxrp,limitc
	integer,ALLOCATABLE ::  ishift0(:)  ! RPI 21/11/2002  ***********
C PHC 22/11/2002 commented out common statement to avoid conflicts
c	COMMON /FORECAST/ relflag,rel
c   inputs
      INTEGER*4 ISITE
      INTEGER IPRCH,IPSUB,IPATB
C
C time series info
c      INTEGER M, MI
      LOGICAL*1 REINIT,MODWRT,SPEAK
	LOGICAL*4 limits,limitc ! limits is local, limitc global for constraints
c   outputs
      REAL*8 ,ALLOCATABLE ::Q4(:,:)  ! DGT 5/27/12 made allocatable as was not passed in
      real*8 qmod,mse,mae
C RPI 14/8/2003 remove Q5
c	REAL*8 Q5(MAXINT+1,MAXCHN)  ! RPI 31/7/2002 for neq=0 + MAXCHN
      LOGICAL*1 OK
	INTEGER M, MI, MPS, MPE
	INTEGER IGAPS(3),js ! , ntdh(MAXSLP),js   !RAW changed maxint to max slp! RPI 5/8/02 dimensioned ntdh
      integer, allocatable :: ntdh(:)  ! DGT 5/27/12 made allocatable
	INTEGER*4 SDATE, SHOUR, INT
C
	INTEGER*4 STIM
cmmmm	INTEGER*4,ALLOCATABLE :: times(:)
	INTEGER*4 CHKDATE,CHKHOUR,ndump ! RPI  25/07/02 added CHK variables
C RPI 2/9/2002 changed chktime from i4 to i8 for Tideda v4
	INTEGER*8 CHKTIME
	REAL*8,ALLOCATABLE ::  bRAIN(:,:)
	REAL*8, ALLOCATABLE :: btmax(:,:)
	REAL*8, ALLOCATABLE :: btmin(:,:)
	REAL*8, ALLOCATABLE :: wind2m(:)
	REAL*8, ALLOCATABLE :: btdew(:,:)
	REAL*8, ALLOCATABLE :: bxlat(:)
	REAL*8, ALLOCATABLE :: bxlon(:)
c	REAL*8, ALLOCATABLE :: bstdlon(:)   ! DGT 7/23/05  No longer use this
	real*8,ALLOCATABLE ::  FLOW(:,:)
	REAL*8 AREA
c   ET variables  DGT
	real*8, ALLOCATABLE :: xlat(:), xlong(:)  ! , stdlon(:)  ! DGT 7/23/05  stdlon single variable
	real*8 stdlon   ! DGT 7/23/05  stdlon single variable
	real*8, ALLOCATABLE :: elevtg(:), dtbar(:,:),bdtbar(:,:)
	integer, ALLOCATABLE :: temper_id(:)
	real*8,ALLOCATABLE ::  temper(:),dewp(:),trange(:)
	real*8,ALLOCATABLE :: DFLOW(:),SUMA(:)
!	common /evap/xlat,xlong,stdlon,elevtg,dtbar !,temper,dewp,trange
      real,allocatable :: ubi(:),wai(:),fraci(:) !raw 10-jan-2005

	COMMON /MODEL2/ SDATE,SHOUR,INT,M,MI,MPS,MPE,
     *          STIM,AREA !,TIMES,PREC,EVAP,FLOW,IGAPS
	CHARACTER*20 FNAME
C Declarations for lake varibales
	logical limits1 !RAW 12/8/02 to use instead of re-using limits
	INTEGER NLAKES,IERROR, ns_temper
C	INTEGER*4 SITES(MAXSITES) ! commented out 16/3/04 by RPI as no evidence of use
	INTEGER, ALLOCATABLE :: LAKE_REACH(:),LZERO(:),LK_LINE(:)
	INTEGER, ALLOCATABLE :: LAKE_BEACH_SLPS(:),NUM_RAT_VALS(:)
	INTEGER, ALLOCATABLE :: LHEADS(:,:), LOFLOWS(:,:)
	REAL*4, ALLOCATABLE :: LAKE_AREAS(:),INI_LEVELS(:)  ! RPI 17/5/2002 added ini_levels
	COMMON /LAKES1/ NLAKES !,LAKE_REACH,LZERO,LAKE_AREAS,
C     *           LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,
C     *           INI_LEVELS ! RPI 17/5/2002 added ini_levels
c      COMMON /LAKES2/ LAKE
      integer npx, iex
      integer mfit(iex), ifit(npx), it, iflag, iopt, neq, npar, nfor,
     *        ibeale, j ,i ,k,count
      double precision qfit(iex), dfit(iex,npx), par(npx),
     *       zero, one
      character*1 prt
      parameter (zero=0.0d0, one=1.0d0)
	integer idebugoutput, idebugbasin, idebugcase  ! DGT 8/17/05 idebugoutput

!  DGT 5/27/12  Allocate barea
!      write(6,*)maxchn,maxint,maxchn,maxslp
      allocate (barea(maxchn))
      allocate (Q4(MAXINT,MAXCHN))
      allocate (ntdh(MAXSLP))
C********************************************
c To solve the problem created by common block
C by Prabu Chandra on 09/07/2002 @ 5.16pm
	IRET = 0
C RPI 15/7/2002
	once: if (IDOIT.EQ.0) then
	IDOIT=1

c Prabu Chandra 30/01/2003
c All these variables have been dynamically allocated to solve the problem
C ALLOCATE DYNAMIC ARRAYS
	ALLOCATE (rel(MAXRCHAREAS),ishift0(maxrchareas))  !cmmmmTIMES(MAXINT),
	ALLOCATE (bRAIN(MAXSLP,MAXINT))
	ALLOCATE (btmax(MAXSLP,MAXINT))
	ALLOCATE (btmin(MAXSLP,MAXINT))
	ALLOCATE (wind2m(MAXINT))
	ALLOCATE (btdew(MAXSLP,MAXINT))
	ALLOCATE (bxlat(MAXSLP))
	ALLOCATE (bxlon(MAXSLP))
!	ALLOCATE (bstdlon(MAXSLP))   ! DGT 7/23/05  No longer use this
	ALLOCATE (bdtbar(12,MAXSLP))
	ALLOCATE (FLOW(maxresponse,MAXINT))
	ALLOCATE (temper(MAXINT),dewp(MAXINT),trange(MAXINT))
	ALLOCATE (WRG(MAXSLP,MAXGAUGE),LRG(MAXSLP,MAXGAUGE))
	ALLOCATE (WRG1(MAXSLP,MAXGAUGE))   ! DGT 7/24/05 WRG1 Interpolation weights
	ALLOCATE (NTS(MAXSLP),LINKS(2,MAXSLP),SI(NSI,MAXSLP))
	Allocate (SP(NSP,MAXSLP),BP(num_basinpars,MAXSLP))
	ALLOCATE (SId(NSI,MAXSLP),SPd(NSP,MAXSLP))
	ALLOCATE (NKA(MAXSLP),ND(MAXSLP),ibout(maxslp))
	ALLOCATE (ATB(MAXA,MAXSLP),PKA(MAXA,MAXSLP),TL(MAXSLP))
	ALLOCATE (PD(MAXC,MAXSLP),CL(MAXC,MAXSLP))
	ALLOCATE (LL(MAXCHN),NTR(MAXCHN),llout(maxchn),LINKR(4,MAXCHN))
	ALLOCATE (clinkr(3,maxchn),RP(NRP,MAXCHN),RPd(NRP,MAXCHN))
	ALLOCATE (kllout(maxresponse),qmap(maxresponse))
	ALLOCATE (LOUT(maxresponse),ireach(maxslp),KNT(MAXRESPONSE))
	ALLOCATE (DFLOW(maxresponse),SUMA(maxresponse))
	ALLOCATE (xlat(MAXTGAUGE),xlong(MAXTGAUGE)) ! ,stdlon(MAXTGAUGE))  ! DGT 7/23/05  stdlon now a single variable
	ALLOCATE (elevtg(MAXTGAUGE),dtbar(12,MAXTGAUGE))
	allocate (temper_id(MAXTGAUGE))

      allocate (ubi(MAXSLP),wai(MAXSLP),fraci(MAXSLP)) !raw 10-jan-2005


	CALL CLIPARAM(xlat,xlong,stdlon,elevtg,dtbar,ns_temper,temper_id)

      CALL MDDATA(NGAUGE,NS,NRCH,NKA,TL,ATB,PKA,ND,CL,PD,UNITS,LL,NTR,
     *	 NTS,LINKS,LINKR,SId,SPd,RPd,IRET,pmap,npar,lrg,wrg,iex,llout,
     *	 neq,nout,nbout,ibout,nrchsav,qmap,rel,relflag,minsp,maxsp,
     *	 minsi,maxsi,minrp,maxrp,limits,clinkr,kllout,ishift0,MAXINT,
     *     MAXGAUGE,MAXSLP,MAXRESPONSE,MAXA,MAXC,MAXCHN,MAXRCHAREAS,
     *     MAXSITES,bp,bxlat,bxlon,wrg1)  ! 7/23/05  DGT added bxlat, bxlon ! DGT 7/24/05 WRG1 Interpolation weights
	MAX_OF_SHIFTS=0
	DO I = 1,MAXRCHAREAS
	MAX_OF_SHIFTS=MAX(MAX_OF_SHIFTS,ISHIFT0(I))
	ENDDO
	neq_temp=neq

	CALL HYDATA(SDATE,SHOUR,interv,M,MI,MPS,MPE,ngauge,neq,! nout,  RPI added nout 16/3/99
     &         AREA,bRAIN,FLOW,IRET,IGAPS,temper,dewp,trange,dtbar,
     &         NS,WRG,LRG,MAXSLP,elevtg,btmax,btmin,btdew,bdtbar,SPd,
     &			bxlat,bxlon,xlat,xlong,stdlon,
     &		 MAXGAUGE,MAXINT,MAXSITES,MAXRESPONSE,MAXTGAUGE,wind2m, !raw 9-dec-2004
     &         wrg1,idebugoutput,idebugbasin, idebugcase)  ! DGT 7/24/05 WRG1 Interpolation weights
	! DGT 8/17/05 added idebugoutput and kcase.
	IF ( IRET .NE. 0 ) THEN
	WRITE(21,*) ' **** An error has occurred on data input'
	STOP
	END IF

C Rearrange the FLOW array so that the "columns" match the reach order
C read in from MODELSPC.DAT using qmap.
C qmap(i) is the column in runoff.dat corresponding to reach j
	do i=1,neq_temp
		do j=1,neq_temp
		if(qmap(j).ne.j) then
			if(qmap(j).eq.i) then ! need to change order of ith and jth  columns
c Interchange columns i and qmap(i) and flag which columns are affected using imap
				do k=1,m
				dummyq=flow(i,k)
				flow(i,k)=flow(qmap(i),k)
				flow(qmap(i),k)=dummyq
				enddo
				qmap(j)=qmap(i)
				qmap(i)=i
			go to 150
			endif
		endif
		enddo
150	continue
	enddo

	if (nooksack.eq.0) then
C  Calculate sum of catchment areas above each reach
      NGUT=NS
      DO 120 JG1 = 1,NGUT
	  JG = LL(JG1)
C RPI 8/2/94 inserted the following line to ensure that area(jg) defined
	  BAREA(JG) = 0.
	  DO 110 J=1,LINKR(4,JG)
		  BAREA(JG) = BAREA(JG) + DABS(SPd(1,LINKR(J+1,JG)))
  110     CONTINUE
  120 CONTINUE
      IRCH = NRCH - NGUT
      DO 140 JR1 = 1,IRCH
	  JR = LL(JR1+NGUT)
	  BAREA(JR) = 0.D0
	  DO 130 J=1,LINKR(4,JR)
		  IPOS = IPOSN(MAXCHN,LINKR,LINKR(J+1,JR))
		  BAREA(JR) = BAREA(JR) + BAREA(IPOS)
  130     CONTINUE
  140 CONTINUE
C At this point we have all the data needed to sort out the how to update
C the ZBAR0s for each measured sub-basin

      CALL SET_COR_DATA(clinkr,neq,nout,nrchsav,ns,kllout,flow,spd,
     *barea,ll,llout,DFLOW,SUMA,KNT,IREACH,LOUT,MAXINT,MAXSLP,MAXCHN,
     *MAXRESPONSE)
C  ************************************************
	end if

C ALLOCATE DYNAMIC ARRAYS
	ALLOCATE (LAKE_REACH(MAX_LAKES),LZERO(MAX_LAKES))
	allocate (LK_LINE(MAX_LAKES))
	ALLOCATE (LAKE_BEACH_SLPS(MAX_LAKES),NUM_RAT_VALS(MAX_LAKES))
	ALLOCATE (LHEADS(MAX_LAKES,MAX_LHEADS))
	allocate (LOFLOWS(MAX_LAKES,MAX_LHEADS))
	ALLOCATE (LAKE_AREAS(MAX_LAKES),INI_LEVELS(MAX_LAKES))
C***********************************************************************
C
c See if there is a Lakes.dat file to be processed
c RPI 29/8/01 removed to make UNIX version work
	NLAKES=0 ! Initialise in case there is no LAKES.DAT - RPI 17/5/2002
      FNAME=TRIM('LAKES.DAT')//CHAR(0)
      INQUIRE(FILE=FNAME,EXIST=limits1) !RAW 12/8/02 changed limits to limits1 to preserve limits for modelcon.dat
      IF(limits1) THEN !RAW 12/8/02 changed limits to limits1 to preserve limits for modelcon.dat
        OPEN (UNIT=LUNRD,FILE=FNAME,STATUS='OLD',IOSTAT=IERROR)
	  IF(IERROR.NE.0) THEN
	      WRITE(21,*) ' **** There is a problem with event file',
     +                     FNAME,' program stops'
            STOP
	  ENDIF
C	Read in lake data

	CALL READ_LAKES(LAKE_REACH,LZERO,LAKE_AREAS,LAKE_BEACH_SLPS,
     *	LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,MAX_LAKES,MAX_LHEADS)

	CLOSE (LUNRD)
      ENDIF
c RPI 17/05/2002 added in the initial lake levels stuff
c See if there is a initial lake levels file Lakes_levels.dat file to be processed
C Initialise initial lake levels to -1 in case none are read in
      IF(NLAKES.GT.0) THEN
		DO I=1,NLAKES
		INI_LEVELS(I)=-1.
		ENDDO
	ENDIF
      FNAME=TRIM('LAKE_LEVELS.DAT')//CHAR(0)
      INQUIRE(FILE=FNAME,EXIST=limits1) !RAW 12/8/02 changed limits to limits1 to preserve limits for modelcon.dat
      IF(limits1) THEN !RAW 12/8/02 changed limits to limits1 to preserve limits for modelcon.dat
        OPEN (UNIT=LUNRD,FILE=FNAME,STATUS='OLD',IOSTAT=IERROR)
	  IF(IERROR.NE.0) THEN
	      WRITE(21,*) ' **** There is a problem with event file',
     +                     FNAME,' program stops'
            STOP
	  ENDIF
C	Read in initial lake level data

	CALL READ_LAKES_LEVELS(LAKE_REACH,LZERO,LAKE_AREAS,

     *	LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,
     *	INI_LEVELS,MAX_LAKES,MAX_LHEADS)
	CLOSE (LUNRD)
      ENDIF
	CLOSE(UNIT=20)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	endif once

c      call SET_COR_DATA(clinkr,neq,nout,nrchsav,ns,kllout,flow,spd,
c     *  barea,ll,llout,MAXINT,MAXSLP,MAXCHN,MAXRESPONSE)
************************************************************************


c
c Labelled common passed from subroutine inputt
c
CCC RAW 7/4/97 changed p(9000) to p(MAXINT)
c      double precision p(MAXINT)
c      common /abc_model/ p
C      save
c-----
c p(it) is rainfall input for current time step it
c dq is a vector of runoff derivatives wrt a,b,c and so
c ds is a vector of storage derivatives wrt a,b,c and s0
c
c If this is the first call in an iteration initialize groundwater storage
c and its derivatives and give parameters meaningful names to make code
c more readable
c
c so is antecedent storage
c
c Note use of the save statement to ensure the assignments are not lost
c
c-----
c If Beale nonlinearity measure is being computed check for
c infeasible parameters
c
      flag: if(iflag .eq. 1)then    !  Only do work first time through
c   copy parameters from defaults  ! Inserted by DGT at to work with interpretation
c     of parameters varied in calibration as multiplication factors.
cDGT  Keep track of calling parameters in toperror.txt
c RPI 30/8/01 removed      write(21,*)neq,npar
c RPI 30/8/01 removed      write(21,*)(par(j),j=1,npar)

      do i = 1,ns
	  do j = 1, nsi
	    si(j,i)=sid(j,i)
	  enddo
	  do j = 1,nsp
	    sp(j,i)=spd(j,i)
	  enddo
	enddo
	do i = 1,nchn
	  do j = 1,nrp
	    rp(j,i)=rpd(j,i)
	  enddo
	enddo
c  Now treat calibration parameters as multiplication factors without worrying
c  about losing information
c Unscramble the parameter map
	do i=1,npar
	if(pmap(3,i).le.0) then
		k=ns
		if(pmap(1,i).eq.2) k=nchn
		do j=1,k
c dealing with a sub-basin's properties
		if(pmap(1,i).eq.1)
     *		sp(pmap(2,i),j)=par(pmap(4,i))*sp(pmap(2,i),j)
c dealing with a river reach
		if(pmap(1,i).eq.2)
     *		rp(pmap(2,i),j)=par(pmap(4,i))*rp(pmap(2,i),j)
c dealing with an initial condition
		if(pmap(1,i).eq.3)
     *		si(pmap(2,i),j)=par(pmap(4,i))*si(pmap(2,i),j)
		end do
	else
c dealing with a sub-basin's properties
	if(pmap(1,i).eq.1)
     *  sp(pmap(2,i),pmap(3,i))=par(pmap(4,i))*sp(pmap(2,i),pmap(3,i))
c dealing with a river reach
	if(pmap(1,i).eq.2)
     *  rp(pmap(2,i),pmap(3,i))=par(pmap(4,i))*rp(pmap(2,i),pmap(3,i))
c dealing with an initial condition
	if(pmap(1,i).eq.3)
     *  si(pmap(2,i),pmap(3,i))=par(pmap(4,i))*si(pmap(2,i),pmap(3,i))
	endif
	end do
c check if constraints apply and if so check them
	if(limitc) then
      do i = 1,ns
	  do j = 1, nsi
	    si(j,i)=max(minsi(j),min(si(j,i),maxsi(j)))
	  enddo
	  do j = 1,nsp
	    sp(j,i)=max(minsp(j),min(sp(j,i),maxsp(j)))
	  enddo
	enddo
	do i = 1,nchn
	  do j = 1,nrp
	    rp(j,i)=max(minrp(j),min(rp(j,i),maxrp(j)))
	  enddo
	enddo
	endif

C Calculate the initial value of ZBAR0 for each sub-basin. A uniform
C value is assigned to all sub-basins in each measured sub-basin

	ndump=0
	call Setup_zbar0s(ns,si,sp,tl,interv,LOUT,IREACH,DFLOW,SUMA,KNT,
     *MAXRESPONSE,MAXSLP)
c
c      if (ibeale.eq.1) then
c               write(*,*) ' **** Constraint checking still to be done'
c       do i = 1,11
c           if ( par(i) .le. 0.) then
c           ibeale = 2
c           return
c            end if
c       end do
c      end if
c
c-----
c Print option which is enabled in output mode in nlfit and in
c predict.  Note that this is optional

	l1: if (prt.ne.'Y') then
		REINIT = .TRUE.
		IPSUB = NS/2+1
		IPATB = NKA(NS)/2+1
		iprch=linkr(1,ll(nchn))
		MODWRT = .FALSE.

C RPI 14/8/2003 removed Q5 from the argument list
	 CALL CALCTS(SI,SP,RP,LINKS,LINKR,LL,NS,NCHN,lrg,wrg,NKA,TL,ATB,
     * PKA,ND,CL,PD,UNITS,IPSUB,IPATB,NTS,REINIT,FLOW,MODWRT,
     * IPRCH,STIM,ISITE,SPEAK,bRAIN,interv,M,MI,MPS,MPE,OK,q4,
     * xlat, xlong,stdlon, elevtg, bdtbar, sdate, shour,temper,dewp,
     * trange,llout,neq,nout,nbout,ibout,LAKE_REACH,LZERO,LAKE_AREAS,
     * LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,INI_LEVELS,
     * ubi,wai,fraci,           !snow initial condictions
     * wind2m, btmin, btmax, btdew, bp,bxlat,bxlon,
     * ntdh,ndump,rel,relflag,ishift0,MAXINT,MAXGAUGE,MAXSLP,
     * MAXRESPONSE,MAXA,MAXC,MAXCHN,MAXRCHAREAS,
     * MAX_LAKES,MAX_LHEADS,MAX_OF_SHIFTS,
     * idebugoutput,idebugbasin,idebugcase)
	! DGT 8/17/05 idebugoutput and kcase added above
	ELSE l1
	if(idebugoutput .ge. 1)then ! DGT 8/17/05 idebugoutput
		open(lunmod,file='topsbd_v8.txt',status='unknown',err=9090)
		open(luntop,file='topreachd_v7.txt',status='unknown',err=9090)
	OPEN(UNIT=LUNDAT,FILE='DETAIL_v7.TXT',STATUS='UNKNOWN',err=9090)
		open(lunp,file='topinfo_v7.txt',status='unknown',err=9090)
		write(lunp,3490)
 3490		format('TOPMODEL OUTPUT ')
		WRITE(LUNp,9595) SDATE,SHOUR,interv,M,MPS,MPE  ! DGT changed info output from lunmod to lunp
 9595	FORMAT(' Simulation starts at ',2I8,' uses a time interval of ',
     *		I6, ' seconds'/' and contains ',I8,' steps. Detailed'
     *          ' printout begins at interval ',I8,' and ends at ',I8)
	endif  ! DGT 8/17/05 idebugoutput
		if(limitc) then   ! RPI 30/8/01 tidying up
			write(21,*) ' Limits are being applied: Min Value Max'
			write(21,2000) (minsp(i),sp(i,1),maxsp(i),i=1,nsp)
			write(21,2000) (minsi(i),si(i,1),maxsi(i),i=1,nsi)
			write(21,2000) (minrp(i),rp(i,1),maxrp(i),i=1,nrp)
 2000			format(1x,3d12.4)
		endif
		REINIT = .TRUE.
		IPSUB = NS/2+1
		IPATB = NKA(NS)/2+1
		iprch=linkr(1,ll(nchn))
		MODWRT = .TRUE.

C RPI 14/8/2003 removed Q5 from the argument list
	CALL CALCTS(SI,SP,RP,LINKS,LINKR,LL,NS,NCHN,lrg,wrg,
     * NKA,TL,ATB,PKA,ND,CL,PD,UNITS,IPSUB,IPATB,NTS,REINIT,FLOW,MODWRT,
     * IPRCH,STIM,ISITE,SPEAK,bRAIN,interv,M,MI,MPS,MPE,OK,q4,
     * xlat, xlong,stdlon, elevtg, bdtbar, sdate, shour,temper,dewp,
     * trange,llout,neq,nout,nbout,ibout,LAKE_REACH,LZERO,LAKE_AREAS,
     * LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,INI_LEVELS,
     * ubi,wai,fraci,           !snow initial condictions
     * wind2m, btmin, btmax, btdew, bp,bxlat,bxlon,
     * ntdh,ndump,rel,relflag,ishift0,MAXINT,MAXGAUGE,MAXSLP,
     * MAXRESPONSE,MAXA,MAXC,MAXCHN,MAXRCHAREAS,
     * MAX_LAKES,MAX_LHEADS,MAX_OF_SHIFTS,
     * idebugoutput,idebugbasin,idebugcase)
	! DGT 8/17/05 idebugoutput and kcase added above
CCC RAW 5/3/97 added two close statements. Without them, files get very big
	if(idebugoutput .ge. 1)then  ! DGT 8/17/05 idebugoutput
		CLOSE(UNIT=LUNDAT)
		close(lunmod)
		close(luntop)
		close(lunp)
	endif  ! DGT 8/17/05 idebugoutput
		close(lundmp)
	endif l1

cDGT
c    Keep track of some stats in file toperror.txt
c
      do j = 1,max(1,neq)
	  mse=0.
	  mae=0.
	  do i = 1,m
          qmod=0 !raw 9-dec-2004 q4(i,ll(iabs(llout(j))))    ! RPI 23/8/00
	     mse=mse+(qmod-flow(j,i))**2
	    mae=mae+abs(qmod-flow(j,i))
	  enddo
	  mse=mse/real(m)
	  mae=mae/real(m)
	  write(21,*) ' Mean Sq. Error and Mean Abs. Error',mse,mae
	enddo
c
c
	endif flag
c-----
c If required compute model derivatives algebraically
c Algebraic derivatives are OPTIONAL; you can use NLFIT's finite
c difference option. The advantage is speed and better accuracy,
c but coding is very error prone.
c
      if (iopt.eq.1) write(21,99)

   99 format(' ***** Warning - analytical derivatives unavailable')

c

c Put observed responses into array qfit for return
c
      do j = 1, max(1,neq)
       qfit(j) = 0 !raw 9-dec-2004 q4(it,ll(iabs(llout(j))))   !RPI 23/8/00  DGT pass back variables to calibrate on
      end do

c
c On first call to model write heading for table
c
c       if (iflag.eq.1) write(nfor,1)
c
c Write rainfall with predicted and observed runoff and storage
c
c        if (mfit(1).eq.-1.or.mfit(2).eq.-1) then
c           write(nfor,2) it, p(it), (qfit(j),qa(j),j=1,2), 'Yes'
c        else
c           write(nfor,2) it, p(it), (qfit(j),qa(j),j=1,2), ' No'
c        end if
c      end if
c
c...
c1     format(' Time  Rainfall  Pred runoff  Obs runoff  Pred storage',
c     &       '  Obs storage Censored'/
c     &       ' -----------------------------------------------------',
c     &       '----------------------')
c2     format(' ',i4,f10.0,f13.0,f11.0,f14.0,f13.0,6x,a)
c-----
      return
 6767 write(21,*) ' **** Reached end-of-file on STATE_DUMP.TXT file'
	stop
 9090 write(21,*)
     1    ' **** Could not open an output file'
      stop
      end

************************************************************************

	SUBROUTINE SET_COR_DATA(linkr,neq,nout,nrch,ns,kllout,flow,spd,
     *barea,ll,llout,DFLOW,SUMA,KNT,IREACH,LOUT,MAXINT,MAXSLP,MAXCHN,
     *MAXRESPONSE)

      IMPLICIT NONE ! 30/03/2004 ITB

	INCLUDE  'Tdims_v7.inc'
	INCLUDE 'maxvariables.inc'

	REAL*8 DFLOW(maxresponse),SPd(NSP,MAXSLP)
	REAL*8	flow(maxresponse,MAXINT)
	REAL*8 SUMA(maxresponse),BAREA(MAXCHN),FAREA(maxresponse),
     +                          NFLOW_MEAN,  PAREA(maxresponse) ! RPI 3/7/2002
	INTEGER NEQ,NOUT,NEQ1,NRCH,NS,idflow(maxresponse)
	INTEGER KLLOUT(maxresponse),LOUT(maxresponse)  ! RPI 4/10/00
      INTEGER LINKR(3,MAXCHN),ireach(maxslp),knt(maxresponse)
      INTEGER LL(MAXCHN),llout(maxchn)

      REAL AREA_MAX, AREA_I ! 30/03/2004 ITB
      REAL DFLOW_MAX, DFLOW_MEAN ! 30/03/2004 ITB
      INTEGER NREACH, I ! 30/03/2004 ITB
      INTEGER I1,I2,I3,I6,I7,I_KEEP,IKEEP_I6,INB,INZ_ZBAR0  ! itb
      INTEGER*4 II,IB,I4,I5, NEQ2  ! 30/03/2004 ITB
      INTEGER I_AREA, IPREV,IX  ! 30/03/2004  ITB

      COMMON /MODEL5/ NEQ1 !,KNT !LOUT,IREACH
C	COMMON /MODEL6/ DFLOW !SUMA,

C Calculate the number of measured sub-basins removing possible duplicates
C caused by having both flows and levels - remember only have data for a
C maximum of neq basins

	neq1=0
	do i=1,neq
	if(kllout(i).gt.0) then
		neq1=neq1+1
C lout contains the locations in LL of the reaches that are measured
		lout(neq1)=kllout(i)
		farea(neq1)=barea(ll(llout(i)))
		parea(neq1)=farea(neq1)  ! RPI 3/7/2002 - make a copy for later use
	endif
	enddo
	do i=1,ns
		ireach(i)=0
	enddo

	do 2 i2=nrch,1,-1 ! Search backwards since most d/s reach is the first in
c in the list and this reach does not appear elsewhere in the list
		do 3 i3=2,3
		ii=i2
		if((linkr(i3,i2).gt.ns).or.(linkr(i3,i2).le.0)) go to 3 ! Not a sub-basin
		if(ireach(linkr(i3,i2)).gt.0) go to 3  ! This basin already assigned to a reach
C have a basin not yet assigned to a reach - ib is the next reach d/s
   10		ib=linkr(1,ii)
			do 4 i4=neq1,1,-1 ! Note reaches in modelspc.dat might need to have
c								u/s reaches to the right of d/s reaches
			if(ib.eq.lout(i4)) then
				ireach(linkr(i3,i2))=lout(i4)
				go to 3
			endif
    4			continue
c			else
C find next connecting reach d/s
				do 5 i5=nrch,1,-1 ! Search backwards since most d/s reach is the first in
c in the list and this reach does not appear elsewhere in the list
					do 6 i6=2,3
					if(linkr(i6,i5).eq.ib) then  ! found next d/s reach
						ii=i5
						go to 10
					endif
    6					continue
    5				continue
C If we get here we have a problem
			write(21,30) linkr(i3,i2)
   30		format(' ***** Could not associate a measured reach with',
     +                                            ' basin',i4)
			write(21,31)
   31		format(' Hint - this might occur if the most d/s model reach',
     +           ' is below the most d/s measured reach')
			ireach(linkr(i3,i2))=1
c			stop
c			endif
c    4			continue
    3		continue
    2 continue
c RPI 30/8/01	removed      write(21,32) (i1,ireach(i1),i1=1,ns)
c RPI 30/8/01 removed   32 format(' For testing only'/' Basin no.  Reach no.'/(1x,i9,i11))
C have found appropriate reach
C
C this code finds which measured basins are u/s of which so that
C we can find the appropriate flows to use in calculating ZBAR0s
C
c	do 20 i0=1,neq1
c20	lout(i0,2)=-1        ! initialise values of u/s reach
c	do 21 i1=nrch,1,-1
c	if((linkr(2,i1).le.ns).or.(linkr(3,i1).le.ns)) then ! have a basin
c		nrchprev=0
c		nreach=linkr(1,i1)  ! This is the reach the basin flows into
c		ix = i1-1
c100		if(ix.eq.0) go to 21  !  Am at d/s node, go to end of loop
c			do 22 i2 = ix,1,-1
c			if((linkr(2,i2).eq.nreach).or.(linkr(3,i2).eq.nreach))
c     +															 then
c found next d/s reach
c			ix=i2-1
c			nreach=linkr(1,i2)
c				do 23 i3=1,neq1
c				if(nreach.eq.lout(i3,1)) then
c					if(lout(i3,2).gt.0) then
c	dflow(i3)=dflow(i3)-dflow(llout(i3,2))
c					lout(i3,2)=nrchprev
c					nrchprev=nreach
c					go to 100
c					endif
c				endif
c23				continue
c			endif
c22			continue
c	endif
c21	continue
	do 41 i1=1,neq1
	suma(i1)=0
	knt(i1)=0
	do 44 i4=1,ns
	if(ireach(i4).eq.lout(i1)) then
		suma(i1)=suma(i1)+spd(1,i4) ! area in mm*2
		knt(i1)=knt(i1)+1
	endif
   44 continue
   41 continue
	neq2=0
C RAW 28/6/01 Take note of the flow from the biggest area,
C so we can use it for ungauged subcats
	area_max=0.
	area_i=0
	do i=1,neq
	if(kllout(i).gt.0) then
		neq2=neq2+1
C setup initial flows for the ZBAR0 calcs
		idflow(neq2)=1
		dflow(neq2)=flow(neq2,1)*farea(neq2)  ! flow in mm**3/int
	   if (farea(neq2).gt.area_max) then
			 area_max=farea(neq2)
			 i_area=neq2
		endif
	endif
	enddo
	dflow_max=flow(i_area,1)*farea(i_area)
C now subtract flows from u/s from d/s flows, but do it only once!
	do 51 i1=nrch,1,-1
	if((linkr(2,i1).le.ns).or.(linkr(3,i1).le.ns)) then ! have a basin
		iprev=0
		nreach=linkr(1,i1)  ! This is the reach the basin flows into
		ix = i1-1
500		if(ix.eq.0) go to 51  !  Am at d/s node, go to end of loop
			do 52 i2 = ix,1,-1
			if((linkr(2,i2).eq.nreach).or.(linkr(3,i2).eq.nreach))
     +															 then
C found next d/s reach
			ix=i2-1
			nreach=linkr(1,i2)
				do 53 i3=1,neq1
				if(nreach.eq.lout(i3)) then ! at a measured reach
C is there an u/s reach feeding into this one?
					if(iprev.gt.0) then
						if(idflow(iprev).gt.0) then
      						dflow(i3)=dflow(i3)-dflow(iprev)
C Parea is the partial area between 2 sites - farea = full area to a site
 							parea(i3)=parea(i3)-parea(iprev)  ! RPI 3/7/2002
							idflow(iprev)=0
						endif
c					lout(i3,2)=nrchprev
					endif
					iprev=i3
					go to 500
C calculate tributary inflow her
				endif
53				continue
			endif
52			continue
	endif
51	continue
 	dflow_mean=0.   ! RPI 1/7/2002
 	nflow_mean=0    ! RPI 1/7/2002
 	do i=1,neq1     ! RPI 1/7/2002
C RPI 3/7/2002 - weight flow with area contributing this flow
 		dflow_mean=dflow_mean+dflow(i)*parea(i)  ! RPI 3/7/2002
 		nflow_mean=nflow_mean+parea(i)           ! RPI 3/7/2002
 	enddo   ! RPI 1/7/2002
 	dflow_mean=dflow_mean/nflow_mean     ! RPI 1/7/2002
 	if(dflow_mean.le.0) dflow_mean=dflow_max   ! RPI 1/7/2002
 	dflow_max=dflow_mean   ! RPI 1/7/2002
	do i=1,neq1
C	if(dflow(i).le.0.) dflow(i)=1.d-4  ! RPI 12/4/01 to avoid -ve tributary flows
	if(dflow(i).le.0.) then
! C RAW 28/6/01 1e-4 gave awful results - RPI tried dflow_max and found that
! it was not much better and substituted the mean of the dflows instead on - 1/7/2002
 			dflow(i)=dflow_max*parea(i)/nflow_mean
 	write(21,5002) i,dflow_max
 5002  format(' Test - for response ',i3,' dflow set to ',d12.4)
	endif
c		dflow(i)=dabs(dflow(i))
	enddo
9	RETURN
	END
*****************************************************************************
*     SUBROUTINE SETUP_ZBAR0S
*****************************************************************************
	subroutine setup_zbar0s(ns,si,sp,tl,dt,LOUT,IREACH,DFLOW,SUMA,KNT,
     *MAXRESPONSE,MAXSLP)

      IMPLICIT NONE ! 30/03/2004 ITB

	INCLUDE 'maxvariables.inc'
      INCLUDE 'Tdims_v7.inc'

      REAL*8 TL(MAXSLP)
	real*8 DFLOW(maxresponse)
      REAL*8 SI(NSI,MAXSLP),SP(NSP,MAXSLP)
	REAL*8 SUMF(maxresponse),SUMC(maxresponse),SUMA(maxresponse)
	REAL*8 area(maxslp),k(maxslp),f(maxslp),lambda(maxslp)
	REAL*8, ALLOCATABLE :: SI_copy(:,:) ! RAW 29/10/2002
	REAL*8 SUM_ZBAR0, AV_ZBAR0 ! 30/03/2004 itb

      INTEGER*4 LOUT(maxresponse),temp_lout(maxresponse) ! RPI 4/10/00 & 1/11/2001
      INTEGER*4 ireach(maxslp),knt(maxresponse)
	INTEGER*8 dt  ! dgt 5/14/12
	INTEGER I1,I2,I3,I6,I7,I_KEEP,IKEEP_I6,INB,INZ_ZBAR0,NEQ1,NS !30/03/2004 ITB


      COMMON /MODEL5/ NEQ1 !,KNT LOUT,IREACH
c	COMMON /MODEL6/ DFLOW !SUMA,

	ALLOCATE (SI_copy(NSI,MAXSLP)) ! RAW 29/10/2002
	do 1 i1=1,neq1
	sumf(i1)=0
      sumc(i1)=0
    1 continue
	do 2 i2=1,ns
	si_copy(2,i2)=si(2,i2)
	si(2,i2)=-100000.
		do 3 i3=1,neq1
		if(ireach(i2).eq.lout(i3)) then
			sumf(i3)=sumf(i3)+sp(2,i2)
			sumc(i3)=sumc(i3)+
     +				 (sp(1,i2)/1.e6)*sp(3,i2)*exp(-tl(i2))/sp(2,i2)
			if(sumc(i3).le.0) then
	write(21,*) ' **** Error - SUMC(I) is negative - this probably',
     +				' means that f, or k needs to be constrained'
			stop
			endif
			go to 5
		endif
    3 continue
    5 continue
    2 continue
      inz_zbar0=0  ! RPI 12/4/01
	sum_zbar0=0.
C The code below solves the third eqn from the bottom of the ZBAR0.DOC report
C It does this for each of the neq1 basins for which there are measurements
C The DFLOW variables are the incremental flow between 2 sites in VOLUME/time
C inb is the number of sub-basins contributing flow to the i7'th site
	temp_lout=lout ! save the whole of lout for later reference RPI 1/11/2001
	do 7 i7=1,neq1
		inb=0
		if(dflow(i7).gt.0) then ! we can't cope with influent reaches
C Pick out which basins contribute to the outlet reach defined by lout(7)
		do 6 i6=1,ns
			if(ireach(i6).eq.lout(i7)) then
c the basin in ireach(i6) contributes directly to outlet reach lout(i7)
C WARNING the units of dflow need sorting out should be mm*3/dt
			    si(2,i6)=(-knt(i7)/sumf(i7))*log(dflow(i7)
     +                     /(((dt/3600.)*1.e+9)*sumc(i7)))
     				inb=inb+1
				area(inb)=sp(1,i6)
				f(inb)=sp(2,i6)
				k(inb)=sp(3,i6)
				lambda(inb)=tl(i6)
			endif
    6		continue
C At this point we have all the relevant data from all the upstream basins
C for solving for ZBAR0 for the reach defined by lout(i7)
C Since we assume the same ZBAR0 applies to all the upstream basins we
C only need to solve once for each set and then assign the solution to
C each basin - RPI 1/11/2001
	ikeep_i6=0
		do 16 i6=1,ns
			if(ireach(i6).eq.lout(i7)) then
				if(temp_lout(i7).gt.0) then		! RPI 1/11/2001
			call solve_for_zbar0(si(2,i6),area,f,k,lambda,dflow(i7),
     +                             dt,inb)
				temp_lout(i7)=-temp_lout(i7)	! RPI 1/11/2001
				ikeep_i6=i6						! RPI 1/11/2001
				else							! RPI 1/11/2001
				si(2,i6)=si(2,ikeep_i6)			! RPI 1/11/2001
				endif							! RPI 1/11/2001
				inz_zbar0=inz_zbar0+1
				sum_zbar0=sum_zbar0+si(2,i6)
			endif
   16     continue
		endif
    7 continue
c RAW 29/10/2002 guard against division by zero
		if (inz_zbar0 .gt. 0) then
			av_zbar0=sum_zbar0/inz_zbar0
		else
			av_zbar0=1.
		endif
		do 9 i6=1,ns
				if(si(2,i6).lt.-99999.) then
c RAW 29/10/2002 following line moved to inside new if statement below,
c to cope with case when no flow data is available and so
c ALL the zbar0 values would be undefined (so av_zbar0 undefined)
c					si(2,i6)=av_zbar0
					if (inz_zbar0 .gt. 0) then
						si(2,i6)=av_zbar0
						write(21,5000) i6,av_zbar0
5000	format(' Test - zbar0 for basin',i4,' set to average of',d12.4)
					else
						si(2,i6) = si_copy(2,i6)
					endif
                  endif
    9		continue
	return
	end
