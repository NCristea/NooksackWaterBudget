C This version,V3, has lakes and snow modelling added to it
C
*$debug

C RPI 14/08/2003 removed Q5 from argument list
******************************************************
*  SUBROUTINE  CALCTS
*******************************************************
      SUBROUTINE CALCTS(SI,SP,RP,LINKS,LINKR,LL,NSUB,NCHN,lrg,wrg,
     * NKA,TL,ATB,PKA,ND,CL,PD,UNITS,IPSUB,IPATB,NTS,REINIT,QM,MODWRT,
     * IPRCH,STIM,ISITE,SPEAK,bRAIN,interv,M,MI,MPS,MPE,OK,q4save, ! RPI 15/8/2003 q4->q4save
     * xlat, xlong,stdlon, elevtg, bdtbar, sdate, shour,temper,dewp,
     * trange,llout,neq,nout,nbout,ibout,LAKE_REACH,LZERO,LAKE_AREAS,
     * LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,INI_LEVELS,
     * ubi,wai,fraci,           !snow initial condictions
     * wind2m, btmin, btmax, btdew, bp,bxlat,bxlon,
     * ntdh,ndump,rel,relflag,ishift0,MAXINT,MAXGAUGE,MAXSLP,
     * MAXRESPONSE,MAXA,MAXC,MAXCHN,MAXRCHAREAS,
     * MAX_LAKES,MAX_LHEADS,MAX_OF_SHIFTS,
     * idebugoutput, idebugbasin, idebugcase)  ! DGT 8/17/05 idebugoutput and kcase
C
C         THE MODEL
C

      IMPLICIT NONE   ! ITB 30/03/2004

	INCLUDE 'maxvariables.inc'
      INCLUDE 'Tdims_v7.inc'
      INCLUDE 'Luns.inc'
	integer nsout, nsbout,ievap_method,n_irrig,n_drainage
	integer kk,i_irrig,imstep,i_drainage,kp
	real*8 wt0,wt1,wt2,wt12,depth_irrig_dem,rate_irrig,art_drainage
	real*8 dep,q5_0,quc_0,qinst_out_0,dr_out_d,DR_OUT_0
	real*8 zbm_d,ACSEM_d,S1_d,ZBAR_d,SR_d,CV_d,BAL_d,QINST_OUT_d
	real*8 SUMR_d,SUMQ_d,SUMAE_d,S0_d,SUMpE_d,DTH1
	real*8 art_drainage_out   ! DGT 6/28/05 artificial drainage
	integer icall ! DGT 10/21/12
	real*8 scalefactor

!	real*8 totalrunoff(MAXSLP),baseflow(MAXSLP)
!     real*8 ArtDrainage(MAXSLP)  ! DGT 6/28/05
!		real*8 volume_irrig_sup(MAXSLP),vol_irrig_demand(MAXSLP),quc(MAXINT)
! DGT 5/27/12 allocating arrays that seemed to be allocated dynamically before
      real*8, allocatable :: totalrunoff(:),baseflow(:),ArtDrainage(:)
      real*8, allocatable :: potentialevap(:) ! DGT 10/21/12 added for output
      real*8, allocatable :: tempave(:),canstore(:) ! DGT 10/21/12 added for output
      real*8, allocatable :: surfro(:),soilstore(:) ! DGT 10/21/12 added for output
      real*8, allocatable :: tiled(:),ditchd(:) ! DGT 10/21/12 added for output
      real*8, allocatable :: volume_irrig_sup(:),vol_irrig_demand(:)
      real*8, allocatable :: quc(:)
      real*8, allocatable :: groundwater_to_take(:) ! groundwater_to_take(MAXSLP)
	real*8 evap_mm,qlat_mm
!	real*8 evap_for_watermgmt(MAXSLP),precip_for_watermgmt(MAXSLP)
	real*8, allocatable::evap_for_watermgmt(:),precip_for_watermgmt(:)
	PARAMETER(nsout=35,nsbout=12) !yjs for snow.  s means snow
	integer nreg !raw 12-jan-2005 how many regions can we model within a sub-basin?
	parameter (nreg=6) !use these regions for irrigation and drainage
	integer idebugoutput,idebugbasin, idebugcase ! DGT 8/17/05 idebugoutput
c      INCLUDE 'COMMON.INC'
c   inputs
	SAVE

c water management

      REAL*8 bRAIN(MAXSLP,MAXINT),bp(num_basinpars,MAXSLP)
      REAL*8 QM(maxresponse,MAXINT)
!	REAL*8 Q_MEAS(END_STEPS_MAX+MAX_OF_SHIFTS),
!     &			Q_CORR(END_STEPS_MAX+MAX_OF_SHIFTS) ! RPI 19/2/2004 100 should be a parameter
!	REAL*8 QMODEL(END_STEPS_MAX,MAXRESPONSE)
	REAL*8 UNITS !,QINST_OUT(MAXSLP)
	real*8, allocatable :: Q_MEAS(:)
	real*8, allocatable :: Q_CORR(:)
	real*8, allocatable :: QMODEL(:,:)
	real*8, allocatable :: QINST_OUT(:) !,QINST_OUT(MAXSLP)
! Q4SIZE is the current size of expandable arrays Q4 and T4 RPI 18/3/2004
      INTEGER Q4SIZE,Q3SIZE,Q5SIZE,Q5RSIZE ! ,interv
      integer*8 interv  ! DGT 5/14/12 to avoid compiler warnings due to above
      INTEGER STIM,ISITE,sdate,shour
	INTEGER IPRCH,IPSUB,IPATB
C
C time series info
      INTEGER M, MI, MPS, MPE, NTDH(MAXSLP) !RAW changed maxint to max slp INT) ! RPI 5/8/02 dimensioned NTDH
      LOGICAL*1 REINIT,MODWRT,SPEAK
c   outputs
      LOGICAL*1 OK,FOUND_LAKE  ! RPI 16/2/2004
c   locals
!      REAL*8 AREA(MAXCHN)
!      real*8, allocatable :: AREA(:)  DGT 10/16/12.  Area not used
!	REAL*8 CA,  ! CA not used DGT 10/16/12
	real*8 SQCI
      REAL*8 SQC,T1,T2,TEMP,SCALE
C     REAL*8 TEMP1,QZERO
CFLOOD COMMENT OUT NEXT LINE
      INTEGER*4 IR(16),II !,IRR(NIP1,MAXSLP),II   !RPI 15/7/2002 MAXSLP is only for testing
      INTEGER*4, allocatable :: IRR(:,:)
	integer*4 IRR_L(NIP1)
c	real*8 xirr(nip1)
	real*8 rirr(NIP1)
      INTEGER*8 TIM ! RPI 2/9/2002 as part of Tideda v4 changes
      INTEGER ISTEP,J,K,IPOS,IPOSN,T_now,SHIFT,itime
c      INTEGER IRO1,IRO2,IRO3,IC1,IC2,IC3
CNE                                             /BYTE/CHARACTER/
c      CHARACTER STR(80),STR1(141)
c      CHARACTER EQSTR*80,EQSTR1*141
C
c      EQUIVALENCE(EQSTR,STR)
c      EQUIVALENCE(EQSTR1,STR1)
C
C         THE SUBCATCHMENT PROCESS
C
c   inputs
      INTEGER NSUB
	INTEGER LINKS(2,MAXSLP),NTS(MAXSLP)
	INTEGER LRG(MAXSLP,MAXGAUGE)
      REAL*8 SI(NSI,MAXSLP),SP(NSP,MAXSLP)
      INTEGER NKA(MAXSLP),ND(MAXSLP)
      REAL*8 CL(MAXC,MAXSLP),ATB(MAXA,MAXSLP),PKA(MAXA,MAXSLP)
      REAL*8 TL(MAXSLP),PD(MAXC,MAXSLP)
	REAL*8 WRG(MAXSLP,MAXGAUGE)
c   locals
      INTEGER JS
!	real*8 QTEMP1(maxresponse),QTEMP2(maxresponse)
	real*8, allocatable :: QTEMP1(:), QTEMP2(:)
	CHARACTER*4 verno
c   ET variables  DGT
	real*8 xlat(*), xlong(*), stdlon, elevtg(*), bdtbar(12,MAXSLP)
	real*8 albedo,elevsb,rlapse
	real, allocatable :: bdtbarr(:,:)
!	real bdtbarr(12,MAXSLP)
	real PETsngl,elev
	real*8 temper(MAXINT),dewp(MAXINT),trange(MAXINT)
	REAL TEMPER_TEMP,DEWP_TEMP,TRANGE_TEMP
c snow RAW
	real*8 ddf
C
cyjs  add a common variable block to pass the variables to snow ueb

      real ubi(MAXSLP),wai(MAXSLP),fraci(MAXSLP) !jinsheng had NSP instead of MAXSLP
!      real ub(MAXSLP),wa(MAXSLP),frac(MAXSLP) !jinsheng had NSP instead of MAXSLP
!	real*8 baoutt(5,MAXINT) !baoutt,outvv,baoutv,outt are used to communicate with UEB snow subroutine
!	real*8 outvv(13,maxint), baoutv(nsbout,maxint), outt(nsout,maxint)
	integer msnow, irad !msnow is a flag to tell us what sort of snow modelling we are doing

C         REACH ROUTING
C
c   inputs
      REAL*8 RP(NRP,MAXCHN)
      INTEGER NCHN
	INTEGER LINKR(4,MAXCHN),LL(MAXCHN),llout(maxchn)
	integer nbout
	integer ibout(maxslp),fdate,fhour
C   locals
      REAL*8 BIGN
!	REAL*8 QI(MAXSLP),HI(MAXCHN),SQI(MAX_LAKES),HQI(MAX_LAKES)
	real*8, allocatable :: QI(:), HI(:),SQI(:), HQI(:)
	REAL*8 HQI_KEEP  ! RPI 20/2/2004
C
C         REACH ROUTING ( ROUTE )
C
c   locals
      REAL*8 SRCHS,SRCHF
 !     REAL*8 ALPRCH(MAXCHN),SRCH1(MAXCHN)
      INTEGER JR,JR1,NRCH
!      INTEGER N5(MAXCHN),N7(MAXCHN),N5R(MAXCHN) ! N7 used in data compression RPI 14/08/2003
      integer, allocatable :: n5(:),n7(:),n5r(:)
C
C LOCALS:
	REAL*8  Q4SAVE(MAXINT,MAXCHN) ! RPI added Q4SAVE
	REAL*8 ,POINTER,DIMENSION (:,:) :: Q4,Q3,Q5,Q5R
!	REAL*8 Q7(100,MAXCHN),qtemp ! used in data compression RPI 14/08/2003
!     INTEGER*4 T6(MAXINT)
	INTEGER*4 ,POINTER,DIMENSION (:,:) :: T4,T3,T5,T5R
	INTEGER*4, ALLOCATABLE :: T4TEMP(:,:) ! T4(:,:),
	INTEGER*4 T7(100,MAXCHN),ttemp,ntemp ! used in data compression RPI 14/08/2003
C RPI 28/7/2003 inserted for version 7
!	REAL*8 Q5SAVE(2,MAXCHN)
	real*8, allocatable :: q5save(:,:)
!	INTEGER*4 T5SAVE(2,MAXCHN)
	integer*4, allocatable :: t5save(:,:)
!	INTEGER N5SAVE(MAXCHN)
	integer, allocatable :: n5save(:)
      INTEGER IR1,IR2
      INTEGER IERR,N6
!	INTEGER N3(MAXCHN),N4(MAXCHN)
      INTEGER NSTAR,NPT
      REAL*8 CTOL
	REAL*8 PREC,PET !,SNOWST(MAXSLP)
      REAL*8, allocatable :: SNOWST(:)

      INTEGER*4 NOUT,JSUB,MAXRNO,I,ITEMP,NDUMP,i_method  ! 30/03/2004 ITB
	INTEGER*4 JJ, JR2,ILAKE, JLAKE, ISTEP1,JSTEP, INDX ! 30/03/2004 ITB

	REAL*8 SMIN, MAX  ! 30/04/2004 ITB
	REAL*8 QTEMPRPI,QHOLD,QFIRST, QLAST,TFIRST,TLAST   ! 30/03/2004 ITB
      REAL*8 DQINI, DQEND,DDQ,SCORE  ! 30/03/2004 ITB

C
C THIS IS AWFUL BUT IT COULD BE WORSE
C
C      EQUIVALENCE (VOLSLP,T4),(SLPWID,HI)
C
C INTRODUCED FOR LAKE ROUTING BY RPI 16/8/00
C
C Lake variables

	INTEGER NLAKES
	INTEGER LAKE_REACH(MAX_LAKES),LZERO(MAX_LAKES),LK_LINE(MAX_LAKES)
	INTEGER LAKE_BEACH_SLPS(MAX_LAKES),NUM_RAT_VALS(MAX_LAKES)
	INTEGER LHEADS(MAX_LAKES,MAX_LHEADS),LOFLOWS(MAX_LAKES,MAX_LHEADS)

	REAL*4 LAKE_AREAS(MAX_LAKES),INI_LEVELS(MAX_LAKES) ! RPI 17/5/2002 added ini_levels
	COMMON /LAKES1/ NLAKES !,LAKE_REACH,LZERO,LAKE_AREAS,
C     *           LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,
C     *           INI_LEVELS ! RPI 17/5/2002 added ini_levels
C     COMMON /LAKES2/ LAKE
C these variables are for nudging flood forecasts
	INTEGER relflag,END_STEPS ! RPI 19/02/2004
	real*4 rel(MAXRCHAREAS)
	integer ishift0(maxrchareas)
C RPI 23/9/02 substituted NFORE for N4 in calls to CORRECT
C so that he could export the number of values actually forecast.
!	INTEGER NFORE(MAXCHN),NFOREMAX
C PHC 22/11/2002 commented out common statement to avoid conflicts
c	COMMON /FORECAST/ relflag ,rel,ishift0
C This is special for CALCTS
!	real*8 lake_lvls(max_lakes)
	INTEGER  iyear,month,iday,ihr,imm,isec, ihh,hour,iss
      REAL   hour1
!      REAL*8 DR(MAX_NTDH,nreg,MAXSLP),QINST(MAX_NTDH,nreg,MAXSLP) ! RPI 19/2/2004
	REAL*8, allocatable :: DR(:,:,:), QINST(:,:,:)
!	REAL*8 ZBAR(MAXSLP,nreg),SR(MAXSLP,nreg),CV(MAXSLP,nreg)
	real*8, allocatable :: ZBAR(:,:), SR(:,:), CV(:,:), zbar_in(:,:,:)
!	REAL*8 Q0(MAXSLP),S0(MAXSLP,nreg)
	real*8, allocatable :: Q0(:), S0(:,:)
!	REAL*8 TDH(MAX_NTDH,MAXSLP)
	real*8, allocatable :: TDH(:,:)
!	REAL*8 ACIEM(MAXSLP,nreg),ACSEM(MAXSLP,nreg),ZBM(MAXSLP,nreg)
!	real*8 SUMR(MAXSLP,nreg),SUMAE(MAXSLP,nreg),
!     +       SUMPE(MAXSLP,nreg),SUMQ(MAXSLP,nreg)
!	real*8 SUMIE(MAXSLP,nreg),SUMSE(MAXSLP,nreg),
!     +	   SUMQB(MAXSLP,nreg),SUMCE(MAXSLP,nreg)
      real*8, allocatable :: ACIEM(:,:)
      real*8, allocatable :: ACSEM(:,:)
      real*8, allocatable :: ZBM(:,:)
      real*8, allocatable :: SUMR(:,:)
      real*8, allocatable :: SUMAE(:,:)
      real*8, allocatable :: SUMPE(:,:)
      real*8, allocatable :: SUMQ(:,:)
      real*8, allocatable :: SUMIE(:,:)
      real*8, allocatable :: SUMSE(:,:)
      real*8, allocatable :: SUMQB(:,:)
      real*8, allocatable :: SUMCE(:,:)


	real*8 sumad   ! DGT 6/28/05
!      real*8 SUMSLE(MAXSLP,nreg),SUMR1(MAXSLP,nreg),SUMQV(MAXSLP,nreg)
      real*8, allocatable :: SUMSLE(:,:)
      real*8, allocatable :: SUMR1(:,:)
      real*8, allocatable::  SUMQV(:,:)

	real*8 SUMR_L,SUMQ_L,SUMAE_L,SUMPE_L,SUMIE_L,S0_L
	real*8 SUMQB_L,SUMCE_L,SUMSLE_L,SUMR1_L,SUMQV_L
	real*8 SUMSE_L,ACIEM_L,ACSEM_L

!	REAL*8 QB(MAXSLP)
!	REAL*8 ZR(MAXSLP),ak0fzrdt(MAXSLP),logoqm(MAXSLP),qvmin(MAXSLP),
!     &       DTH(MAXSLP)
      real*8, allocatable :: QB(:)
      real*8, allocatable :: ZR(:)
      real*8, allocatable :: ak0fzrdt(:)
      real*8, allocatable :: logoqm(:)
      real*8, allocatable :: qvmin(:)
      real*8, allocatable :: DTH(:)


C RPI added this because we now need the value of MAXRNO from MDDATA
	COMMON /MDDATA1/ MAXRNO

      integer ngut,jg1,natball,jg,it,kg       ! itb 30/03/2004
      integer neq  ! itb 30/03/2004
      INTEGER ISTEP2  ! 30/03/2004 ITB

	integer i_evapmethod
      REAL*8 btmin(MAXSLP,MAXINT),btmax(MAXSLP,MAXINT)
	real*8 btdew(MAXSLP,MAXINT), wind2m(MAXINT)
	real*8 bxlat(*),bxlon(*) !basin average lat, long

c SNOWUEB
	integer nsv,npar,nxv,niv,ndepletionpoints,ipflag,mQgOption
	integer nintstep,isurftmpoption,nstepday,isnow_method
	real bca,bcc,a,b,timestep !,W1,cump,cume,cummr,errmbal
	real surfacewaterinput, snowevaporation, areafractionsnow
	real TA, P, WS, RH, qsiobs,qnetob
      PARAMETER(nsv=9,npar=25,nxv=11,niv=7)
	CHARACTER*200 inFILE,outFILE,pfile,svfile,bcfile
	CHARACTER*200 afile,dfcfile,outfiledet
      INTEGER YEAR,DAY,iflag(5)  ! Pass a flag to control surface temperature modeling
      REAL IO,LAT
      real snowsitev(nsv)
 !     real snowstatev(nxv,MAXSLP)
      real, allocatable :: snowstatev(:,:)

	real snowforcing(niv)
      real snowparam(npar)
	integer snowcontrol(7)
!      real cump(MAXSLP),cume(MAXSLP),cummr(MAXSLP)
!	real errmbal(MAXSLP),w1(MAXSLP)
      real dtbar(12)
	real, allocatable :: cump(:)
	real, allocatable :: cume(:)
	real, allocatable :: cummr(:)
	real, allocatable :: errmbal(:)
	real, allocatable :: w1(:)

c arrays to keep records of surface temperature and snowpack average
c temperature. This is for the fourth model (Modified force restore approach)
	real, allocatable :: snowsurfacetemp(:,:)
	real, allocatable :: snowaveragetemp(:,:)
	real, allocatable :: dfc(:,:)

!  DGT 5/27/12 allocate and initialize all the now dynamic arrays
      allocate (totalrunoff(MAXSLP),baseflow(MAXSLP))
      totalrunoff=0; baseflow=0
      allocate (potentialevap(MAXSLP),tempave(MAXSLP))
      potentialevap=0.0  !  DGT 10/21/12
      tempave=0.0
      allocate (surfro(MAXSLP),canstore(MAXSLP),soilstore(MAXSLP))
      surfro=0.0  !  DGT 10/21/12
      canstore=0.0
      soilstore=0.0
      allocate (tiled(MAXSLP),ditchd(MAXSLP))
      tiled=0.0  !  DGT 10/21/12
      ditchd=0.0

      allocate (ArtDrainage(MAXSLP))
      ArtDrainage=0
      allocate (volume_irrig_sup(MAXSLP))
      volume_irrig_sup=0
      allocate (vol_irrig_demand(MAXSLP),quc(MAXINT))
      vol_irrig_demand=0; quc=0
      allocate (groundwater_to_take(MAXSLP))
      groundwater_to_take=0
      allocate (evap_for_watermgmt(MAXSLP))
      evap_for_watermgmt=0
      allocate (precip_for_watermgmt(MAXSLP))
      precip_for_watermgmt=0

      allocate (Q_MEAS(END_STEPS_MAX+MAX_OF_SHIFTS))
      allocate (Q_CORR(END_STEPS_MAX+MAX_OF_SHIFTS))
      allocate (QMODEL(END_STEPS_MAX,MAXRESPONSE))
      allocate (QINST_OUT(MAXSLP))

 !     allocate (AREA(MAXCHN))  ! DGT 10/16/12 area not used
      allocate (IRR(NIP1,MAXSLP))

      allocate (QTEMP1(maxresponse),QTEMP2(maxresponse))
      allocate (bdtbarr(12,MAXSLP))

      allocate (QI(MAXSLP),HI(MAXCHN),SQI(MAX_LAKES),HQI(MAX_LAKES))
      allocate (N5(MAXCHN),N7(MAXCHN),N5R(MAXCHN))

      allocate (Q5SAVE(2,MAXCHN))
      allocate (T5SAVE(2,MAXCHN))
      allocate (N5SAVE(MAXCHN))

      allocate (SNOWST(MAXSLP))

      allocate (DR(MAX_NTDH,nreg,MAXSLP),QINST(MAX_NTDH,nreg,MAXSLP))
      allocate (ZBAR(MAXSLP,nreg),SR(MAXSLP,nreg),CV(MAXSLP,nreg))
      allocate (zbar_in(m+1,MAXSLP,nreg))
      allocate (Q0(MAXSLP),S0(MAXSLP,nreg))
      allocate (TDH(MAX_NTDH,MAXSLP))
      allocate (ACIEM(MAXSLP,nreg),ACSEM(MAXSLP,nreg),ZBM(MAXSLP,nreg))
      allocate (SUMR(MAXSLP,nreg),SUMAE(MAXSLP,nreg))
      allocate (SUMPE(MAXSLP,nreg),SUMQ(MAXSLP,nreg))
      allocate (SUMIE(MAXSLP,nreg),SUMSE(MAXSLP,nreg))
      allocate (SUMQB(MAXSLP,nreg),SUMCE(MAXSLP,nreg))

      allocate (SUMSLE(MAXSLP,nreg))
      allocate (SUMR1(MAXSLP,nreg))
      allocate (SUMQV(MAXSLP,nreg))

      allocate (QB(MAXSLP))
      allocate (qvmin(MAXSLP))
      allocate (ZR(MAXSLP),ak0fzrdt(MAXSLP),logoqm(MAXSLP))
      allocate (DTH(MAXSLP))

      allocate (snowstatev(nxv,MAXSLP))

      allocate (cump(MAXSLP))
      allocate (cume(MAXSLP))
      allocate (cummr(MAXSLP))
      allocate (errmbal(MAXSLP))
      allocate (w1(MAXSLP))

*******************************************************************************
C THIS SECTION IS RELEVANT TO THE WHOLE MODEL
C
C ASSUME EVERYTHING WILL TURN OUT OK
      OK = .TRUE.
C SUM OF CALCULATED FLOWS IS INITIALISED TO ZERO
      SQC = 0D0
C
C  Calculate sum of catchment areas above each reach
	jg=0
	Q4SIZE=INI_ARRAY_SIZE  ! RPI 18/3/2004 initialise record of array size
	Q3SIZE=INI_ARRAY_SIZE  ! RPI 23/3/2004 initialise record of array size
	Q5SIZE=INI_ARRAY_SIZE+1  ! RPI 23/3/2004 initialise record of array size
	Q5RSIZE=INI_ARRAY_SIZE+1  ! RPI 23/3/2004 initialise record of array size
	ALLOCATE (Q4(1:Q4SIZE,MAXCHN),T4(1:Q4SIZE,MAXCHN))
	ALLOCATE (Q3(0:Q3SIZE,MAXCHN),T3(0:Q3SIZE,MAXCHN))
	ALLOCATE (Q5(0:Q5SIZE,MAXCHN),T5(0:Q5SIZE,MAXCHN))
	ALLOCATE (Q5R(0:Q5RSIZE,MAXCHN),T5R(0:Q5RSIZE,MAXCHN))

      NGUT=NSUB
! 5/27/12  DGT The logic below seems messed up.
!  In Mddata in Nooksack linkr(4,*) is not initialized so this logic fails
!      DO 120 JG1 = 1,NGUT
!	  JG = LL(JG1)
C         AREA(JG) = RP(3,JG) * RP(4,JG)
C RPI 8/2/94 inserted the following line to ensure that area(jg) defined
!	  AREA(JG) = 0.
!	  DO 110 J=1,LINKR(4,JG)
!		  AREA(JG) = AREA(JG) + DABS(SP(1,LINKR(J+1,JG)))
! 110     CONTINUE
! 120 CONTINUE
!     NRCH = NCHN - NGUT
!     DO 140 JR1 = 1,NRCH
!	  JR = LL(JR1+NGUT)
!	  AREA(JR) = 0.D0
!	  DO 130 J=1,LINKR(4,JR)
!		  IPOS = IPOSN(MAXCHN,LINKR,LINKR(J+1,JR))
!		  AREA(JR) = AREA(JR) + AREA(IPOS)
!  130     CONTINUE
!  140 CONTINUE
!      CA = AREA(LL(NCHN))
C
	ievap_method=2 !0=P-T, 1 and 2 are P-M
c initialise snowueb model
	afile='snow.in'
	open(1,FILE=afile,STATUS= 'OLD')
	read(1,*)inFILE,OutFILE,outfiledet,pfile,svfile,bcfile,dfcfile
	read(1,*)irad
c   Flag to control radiation
c	0 is no measurements - radiation estimated from diurnal temperature range
c    1 is incoming shortwave radiation read from file (measured), incoming longwave estimated
c    2 is incoming shortwave and longwave radiation read from file (measured)
c    3 is net radiation read from file (measured)
	read(1,*)ipflag  ! Flag to control printing (1=print)
	read(1,*)nintstep  ! Number of internal time steps to use
	read(1,*)isurftmpoption ! Surface temperature algorithm option
	read(1,*)mQgOption  !ground heat input
	close(1)

      OPEN(88,FILE=pfile,STATUS='OLD')
	do i=1,npar
        READ(88,*)snowparam(i)
	enddo
	close(88)
c
c   DGT 7/23/05. Site variables file not used
c      OPEN(88,FILE=svfile,STATUS='OLD')
c	do i=1,nsv
c        READ(88,*)snowsitev(i)
c	enddo
c	close(88)

      CALL bcparm(dtbar,bca,bcc,bcfile) !warning we wont use the dtbar values we read here but use this read to get bca and bcc
	! dtbar values used are read in hydata and averaged for nearby gages

      OPEN(88,File=dfcfile,STATUS='OLD')
	i=0
101   Read(88,*, end=100) a, b
	 i=i+1
	 goto 101
100   rewind(88)
      ndepletionpoints=i
	allocate(dfc(ndepletionpoints,2))
c      OPEN(21,File=dfcfile,STATUS='OLD')
	do i=1,ndepletionpoints
        Read(88,*) dfc(i,1), dfc(i,2)
	enddo
      Close(88)

	do i=1,nxv
		snowstatev(i,:)=0
	enddo
!!	month,day,year,hour,
	timestep=24

	snowcontrol(1)=irad
	snowcontrol(2)=ipflag
	snowcontrol(3)=10
	snowcontrol(4)=nintstep

	snowcontrol(7)=isurftmpoption

	nstepday=24/timestep*nintstep
	allocate(snowsurfacetemp(nstepday,MAXSLP))
	allocate(snowaveragetemp(nstepday,MAXSLP))
cyjs initialize tsbackup and TaveBackup
	do 3 i = 1,nstepday
	snowsurfacetemp(i,:)=-9999.
	snowaveragetemp(i,:)=-9999.0
 3	continue
	snowsurfacetemp(nstepday,:)=0
	snowaveragetemp(nstepday,:)=0

      if (snowcontrol(2).ge.1) then
		OPEN(snowcontrol(3),FILE=OUTFILE,STATUS='UNKNOWN')
	endif
C debug	OPEN(108,FILE=OUTFILEdet,STATUS='UNKNOWN')

c   initialize variables for mass balance
      W1(:)=snowstatev(2,:)
      cump(:)=0.
      cume(:)=0.
      cummr(:)=0.
	errmbal(:)=0.

C
C***********************************************************************
C
C SUBCATCHMENT SECTION
C
C
C   REINITIALISE THE SUBCATCHMENT IF NECESSARY
C RPI 19/7/2003 - it looks as though REINIT is redundant  ??????????????
C
      IF ( REINIT ) THEN
	 BIGN=5D0/3D0
      ENDIF
C
C   END OF REINITIALISING
CRAW Landcare
      natball=0
C RAW 4-May-98 implementing multiple raingauges for each sub-basin
C RAW (see also top.f, mddatav4.f)
	do 355 js=1,nsub			 ! for each subbasin
c		do 350 it=1,m			 ! for each timestep
c			brain(js,it)=0.		 ! 'brain' has basin rain time series
c			do 345 kg=1,maxgauge ! get brain by adding weighted 'rain'
c	           if(lrg(js,kg) .gt. 0)
c     1       	     brain(js,it)=brain(js,it)+
c     1				rain(lrg(JS,kg),it)*wrg(JS,kg)
c  345			continue
c  350		continue
CRAW Landcare
          natball=natball+nka(js)
  355	continue					 ! Now pass 'brain' instead of 'rain'
c								 ! and use row js of brain, rather
c								 ! than row links(2,js) of rain
c	do 3551 it=1,m
c 3551	write(21,*)(brain(js,it),js=1,nsub)
c
cDGT write subbasin output headers
	IF ( MODWRT )then
	 if(idebugoutput .ge. 1)then   ! DGT 8/17/05 debugout
	  write(lunmod,*)nbout,m
	  WRITE(LUNMOD,2500)
 2500   FORMAT(' Basin',
     2  ' TimeStep IrrDrainCat Afrac SWInput_mm Qlat_mm Qtot_mm Qb_mm',
     2  ' Recharge_mm SatEx_mm InfEx_mm SurfRo_mm SatAfrac InfAfrac',
     3  ' IntStore_mm WTDepth_mm SoilStore_mm Pet_mm Aet_mm',
	3  ' Irrig_mm GWTake_mm IrrDem_mm Prec_mm SWE_mm Sublim_mm',
     4  ' Tave_C Tdew_C Trange_C ErrClosure_mm')
CRAW Landcare
        if (mpe .eq. -1) then
          write(lundat,*)nsub
	  endif
     	  write(luntop,3505) neq+1,neq+2,neq+nout+1
 3505   format(   'Measured and modeled flows'/
     1  'Timestep (column 1), Measured reaches (columns 2 to',i3,
     2  ') Modeled reaches (columns',i4,' to',i4,')'/
	3  'UNITS ARE um/interval normalized by each basins own area'
     *  ' unless they have a -ve site No. in which case they are lake'
     *  ' levels in metres'/,
     3  'The next two rows give the number of flows to be used for fitt'
     3  'ing and printing, followed by the reach numbers to which they '
     3  'relate')
!	  write(luntop,3527) neq,
!     *  (linkr(1,ll(iabs(llout(jr1))))*isign(1,llout(jr1)),jr1=1,neq)
3527	FORMAT(12X,I6,11I11)
!	  write(luntop,3527) nout,
!     *  (linkr(1,ll(iabs(llout(jr1))))*isign(1,llout(jr1)),jr1=1,nout)
      endif
	endif
C get any snow input parameters
	ddf=-1.
	open(unit=81,file='snowinp.txt',status='old',err=4390)
	read(81,*,err=4390)ddf
	close(81)
	open(unit=78,file='snowout.txt')
4390	continue
C***********************************************************************
C
C INITIALISE FOR REACH ROUTING
C
cDGT The routing cannot handle 0 slopes so fix those.
c The relationship used is
c  S = C A^theta = .133351 (A[m^2])^(-.425) = 47.31513 (A[mm^2])^(-.425)
c  This was fit as a "lower bound" to the scatter in a slope vs area
c  plot for the Grey river, New Zealand.
	open(unit=85,file='testn_v7.ed',status='unknown')  ! for testing only
      DO JR1=1,NRCH
	 JR=LL(JR1+NGUT)
	 !smin=47.31513 * area(jr)**(-.425)  ! DGT 5/27/12   Area not valid due to reachlogic above inoperable
	 rp(1,jr)=max(smin,rp(1,jr))
      enddo
	SNOWST(:)=0.
	TIM = STIM
	T5SAVE(2,:)=STIM
	N5SAVE(:)=0
	  !open(unit=101,file='zbar_fortran_in.dat',status='old')
	  open(unit=101,file='results/zbar_in.dat',status='old')
	  do itime=0,m
        do j=1,nsub
	     read(101,fmt=*) istep,js,(zbar_in(itime+1,j,i),i=1,nreg)
	     !write(*,fmt=150) istep,js,(zbar_in(itime+1,j,i),i=1,nreg)
        end do
      end do
      close(100)
 150  format(i4,i3,6f10.6)
C start of time loop +++++++++++++++++++++++++++++++++++++++++
      write(6,*)'Starting time loop'
	time_loop: DO ISTEP=0,M
	!write(6,*)
	!write(6,*)'timestep',istep
	if(mod(istep,100) .eq. 0) write(6,*)'timestep',istep
	IF(ISTEP.GT.0) TIM = TIM + interv
c
c   LOOP OVER SUBBASINS
c
!raw	subbasin_loop: DO JSUB = 1,NRCH   ! NSUB
C RPI 20/7/2003 - work through basins in "stream order" order, i.e.,
C do basins that feed a first order channels before second order and so on.
C The order is already defined by array LL, and LINKR(2,:) gives the basin number.
C The first NGUT rows in LINKR refer to basins (basin # are transformed by
C having NGUT added to them). The next NRCH rows of LINKR tell which reach
C is fed by which basin and/or upstream reach. We want the basin numbers
C from column 2, rows NGUT+1 to NGUT+NRCH in LINKR and we want them in the order they
C should be processed in. The order is in LL(NGUT+1) to LL(NGUT+NRCH). DGT
C introduced an additive factor of MAXCHN to avoid confusion between basin
C and reach numbers and we have to remove it.
!raw	JS=LINKR(2,LL(JSUB+NGUT))-max(100,MAXRNO) ! 100=ngbase - see mddata

	subbasin_loop: DO JSUB = 1,NSUB
	js=jsub
	l2: IF(JS.GT.0) THEN
	PREC=bRAIN(JS,MAX(ISTEP,1))
	JR = LL(JS)
	T5(1,JR) = STIM
c
c snow RAW
	IF(ISTEP.GT.0) THEN
c
c        Subroutine to compute ET  DGT 17 May 1998
c parameters for ET - because at least albedo may be fitted, it is not possible
C to put this loop outside calcts - therefore leave as is for the time being RPI 18/3/2004
cdebugging.  To debug a particular watershed uncomment the lines below and enter its topnetID
c      if(jsub .eq. 87 .and. istep .eq. 245)then
c	  js=jsub

c	snowcontrol(2)=1
c	else
c	snowcontrol(2)=0
c	endif
	    albedo=sp(12,js)
	    rlapse=sp(13,js)
	    elevsb=sp(14,js)
		TEMPER_TEMP=TEMPER(ISTEP)
		DEWP_TEMP=DEWP(ISTEP)
		TRANGE_TEMP=TRANGE(ISTEP)
		if (ievap_method .ne. 0) DEWP_TEMP=btdew(js,istep) !RAW 1-jan-05 for use with penman-Monteith
		if (ievap_method .ne. 0)
     *		TRANGE_TEMP=min(30d0,max(0d0,
     *			btmax(js,istep)-btmin(js,istep))) !RAW 22-mar-2005 check for bad data
		if (ievap_method .ne. 0)
     *		TEMPER_TEMP=(btmax(js,istep)+btmin(js,istep))/2
		bdtbarr=bdtbar
		shour=0  ! 7/23/05 DGT changed to 0.
c  Warning here - this code would have to be changed for time steps other than daily
c  In general it would be better to inherit shour from the calling program but coming in at 240000
c  the integration of radiation across a day fails.  A more general solution to this issue would involve
c  reprogramming hyri to handle time steps that cross the day break.
c  Setting shour=0 also achieves compatibility with the convention that inputs are associated with
c  measurements recorded at the end of the time step (240000 for daily) but ET and snow computations
c  integrate from the start time over the time step.
c
		elev=elevsb !we are driving this using a basin average temperature, so it must be at basin average elev
          call etall(sngl(bxlat(js)),sngl(bxlon(js)),sngl(stdlon),
     *			sngl(elev), bdtbarr(1,js),PETsngl,
     *            TEMPER_TEMP,DEWP_TEMP,TRANGE_TEMP,sngl(elevsb),
     *			sngl(albedo),sngl(rlapse),sdate,
     +			shour,interv,m,ISTEP,iyear,month,
     +			iday,ihr,imm,isec,hour1,
     +			sngl(btmin(js,istep)), sngl(btmax(js,istep)),
     +			sngl(wind2m(istep)),ievap_method)
		PET=PETsngl
!	i_method=2
!          call etall(xlat, xlong, stdlon, elevtg, dtbar,PET,TEMPER_TEMP,
!     *			DEWP_TEMP,TRANGE_TEMP,elevsb,albedo,rlapse,sdate,
!     +			shour,interv,m,ISTEP,iyear(js),month(js),
!     +			iday(js),ihr(js),imm(js),isec(js),hour1(js),i_method)
		TA=(btmin(js,istep)+btmax(js,istep))/2
		P=brain(js,istep)*3600./real(interv)/1000. !mm/int *3600s/h / (s/int) / (1000mm/m) = m/hr for snowueb
		WS=wind2m(istep)
cdgt**		RH=1 !unknown for Nooksack, hope we don't need it!
		qsiobs=0 !unknown for Nooksack, hope we don't need it!
		qnetob=0 !unknown for Nooksack, hope we don't need it!
		isnow_method=2
		if (isnow_method.eq.2) then
!!			TA,P,WS,RH,trange,QSIOBS,QNETOB,qg
! 1			if(mQgOption .eq. 2) then !try if new ground heat input exit
!				read(88,*,end=99) TA,P,WS,RH,trange,QSIOBS,QNETOB,qg
!				snowparam(24)=qg
!			else
!				read(88,*,end=99) TA,P,WS,RH,trange,QSIOBS,QNETOB
!			endif
			snowcontrol(5)=iyear*10000+month*100+iday
			ihh=hour1
			imm=(hour1-ihh)*60
			iss=hour1*3600-ihh*3600-imm*60
			snowcontrol(6)=ihh*10000+imm*100*iss
			snowforcing(1)=TA
			snowforcing(2)=P
			snowforcing(3)=WS
			snowforcing(4)=DEWP_TEMP
cdgt 4/1/05			snowforcing(4)=237.3/(1/(log(RH)/17.27+Ta/(Ta+237.3))-1)  ! from http://williams.best.vwh.net/ftp/avsig/avform.txt
			snowforcing(5)=btmax(js,istep)-btmin(js,istep)
			snowforcing(6)=qsiobs
			snowforcing(7)=qnetob
			snowsitev(1)=0   !  sp(39,js) !0. !forest cover   DGT 4/1/05
			snowsitev(2)=sp(14,js) !elevsb
			snowsitev(3)=bxlat(js) !lat
			snowsitev(4)=bxlon(js) !lon
			snowsitev(5)=stdlon  !stdlong
			snowsitev(6)=sp(14,js) !elevtg is assumed = elevsb
			snowsitev(7)=sp(13,js) !rlapse
			snowsitev(8)=0. !slope
			snowsitev(9)=0. !azimuth
			call snowueb(snowsitev,snowstatev(1,js),snowparam,
     +			ndepletionpoints,dfc, snowcontrol,bdtbarr(1,js),
     +			snowforcing,
     +			snowsurfacetemp(1,JS),snowaveragetemp(1,JS),
     +			timestep,nstepday,
     +			surfacewaterinput, snowevaporation,  !outputs (both in m/h)
     +            areafractionsnow,js)   ! 4/2/05  DGT added js so that within snow one knows which element one is in for debugging
			cump(js)=cump(js)+p*timestep
			cume(js)=cume(js)+snowevaporation*timestep
			cummr(js)=cummr(js)+surfacewaterinput*timestep
			errmbal(js)=
     +			w1(js)+cump(js)-cummr(js)-cume(js)-snowstatev(2,js)
			if(snowcontrol(2) .ge. 2)then
c				WRITE(snowcontrol(3),*) ! js,istep,
c     +			iyear,month,iday,hour1,
c     +			surfacewaterinput,snowevaporation,areafractionsnow,
c     +			cump(js),cume(js),cummr(js),ERRMBAL(js),
c     +			(snowstatev(i,js),i=1,11),
c     +			(snowforcing(i),i=1,7)
				WRITE(snowcontrol(3),*)js,istep,iyear,month,iday,hour1,
     +			surfacewaterinput,snowevaporation,areafractionsnow,
     +			snowstatev(2,js),cump(js),cume(js),cummr(js) !swe
			endif
		else
			IF(DDF.GE.0) THEN
C    				call snow(temper,elevtg,elevsb,rlapse,PREC,ddf,SNOWST(JS),  ! DGT 5/14/12  passed only first grid value to get to compile
c     +			          interv,m,nsub,js,ISTEP,MAXSLP,MAXINT)
     				call snow(temper,elevtg(1),elevsb,rlapse,PREC,ddf,SNOWST(JS),
     +			          interv,m,nsub,js,ISTEP,MAXSLP,MAXINT)
			ENDIF
		endif
		IF ( MODWRT .AND. ISTEP.EQ.M ) THEN
	     if(idebugoutput .ge. 1)then  ! DGT 8/17/05 debugout
			WRITE(LUNp,5500) JS        ! dgt changed info output from lunmod to lunp
 5500			FORMAT(//' Output for sub-catchment ',I3/
     1		       ' ---------------------------')
c			WRITE(LUNp,4400) (SP(I,JS),I=1,NSP)     ! DGT changed info output from lunmod to lunp
c 4400			FORMAT(' The parameters are'/(1x,e15.6))
c			write(lunp,4401)(si(i,js),i=1,nsi)
c 4401			format(' Initial conditions'/(1x,e15.6))
C RPI removed 25/3/2004 			IF(ibout(js).eq.1) write(lunmod,'(f24.0,i8)')sp(1,js),js
            endif
		ENDIF
	ELSE
          N5(:)=1  ! RPI 25/7/2003 17/03/2004 added the IF part
	ENDIF
C RPI added the next statements 18/3/2004 to ensure values carried forward
	q5save(1,jr)=q5save(2,jr)
	t5save(1,jr)=t5save(2,jr)

	PREC=surfacewaterinput*1000.*interv/3600.   !mm/timestep=m/h*1000mm/m*h/timestep
	if (nooksack .eq. 1) then
		n_irrig=1   ! DGT warning.  If this is changed the logic associated with
	!                 averaging depth_irrig_dem will need to be adjusted
		n_drainage=2
	else
		n_irrig=0
		n_drainage=0
	endif

	wt0=1
	kk=0
	depth_irrig_dem=0.
c	xIRR(:)=0
	baseflow(JS)=0.
	totalrunoff(JS)=0.
	evap_for_watermgmt(js)=0.
	potentialevap(js)=0.   ! DGT 10/21/12 initialize for averaging over categories
	surfro(js)=0.
	canstore(js)=0.
	soilstore(js)=0.
	tiled(js)=0.
	ditchd(js)=0.

	zbm_d=0;ACSEM_d=0;S1_d=0;ZBAR_d=0;SR_d=0;CV_d=0;BAL_d=0
	SUMR_d=0;SUMQ_d=0;SUMAE_d=0;S0_d=0;SUMpE_d=0;QINST_OUT_d=0
	dr_out_d=0   !  DGT 6/10/05  Initializing
	art_drainage_out=0.   ! DGT 6/28/05   Artificial drainage

	!the previous timestep's call to watermgmt calculated groundwater_to_take
	if (istep.gt.1) then !can't do any pumping on first timestep because we haven't called watermgmt yet: assume zero pumping when istep=1
		DTH1=SP(4,js)  ! DGT 11/4/07 added this line to get DTH1 from corresponding basin parameter
		ZBAR(JS,:)=ZBAR(JS,:) +
	!  m/timestep = m3/timestep     /      (m^2)     / porosity
     +		groundwater_to_take(JS) / (SP(1,JS)/1e6) / DTH1     !RAW 18-Jul-2005 bug fix: SP(1,JS)/1e6 was SP(1,JS)*1e6!!
C   DGT 11/4/07 changed the above from DTH to DTH1 because topmodel saturated zone works with that.
	end if
	write(TRACE,'(i4,i3)', advance='no') istep,js
c  Debugging code to stop here js=94
c	if(js .eq. 94)then
c	   js=js
c	endif
	do i_irrig=1,(n_irrig+1)
		if (i_irrig.eq.1) then
			wt1=wt0*(1-SP(20,JS)) !unirrigated fraction
			rate_irrig = 0
		else
			wt1=wt0*SP(20,JS) !irrigated fraction
			if (istep.eq.0 .or. wt1 .le. 0.) then
				rate_irrig=0
			else
				!use volume_irrig_sup(js) calculated in previous timestep
				!                       mm^3        /      mm^2      / (sec) = mm/s
!   DGT 8/18/05.  Commented out the /float(interv).  The units going to topmod need to be mm/ts, the same as precipitation
!   DGT 8/18/05.                 mm^3                 /      mm^2 = mm/ts
				rate_irrig=volume_irrig_sup(js)*1e9 / (wt1*SP(1,JS))
!     *							/(float(interv))
			endif
		end if
		do i_drainage=1,(n_drainage+1)
	        kp=(i_irrig-1)*3+i_drainage   ! DGT 11/3/07
	! kp is a variable to index the combinations of irrigation and drainage to compare with kcase and control
	! detail output.  There are 3 drainage cases.  The first the associated with i_irrig will be numbered 1,2,3
	! The next three will be numbered 4, 5, 6 and so on.
			if (i_drainage.eq.1) then !Naturally drained
				wt2=1-SP(16,JS)-SP(17,JS) !16=tilefraction, 17=ditchfraction
				art_drainage=0
			elseif (i_drainage.eq.2) then !Tile drained
				wt2=SP(16,JS) !16=tilefraction
				art_drainage=SP(18,JS) !watch for UNITS!!!
			elseif (i_drainage.eq.3) then !Ditch drained
				wt2=SP(17,JS) !17=ditchfraction
				art_drainage=SP(19,JS)
			end if
			wt12=wt1*wt2 !wt12 is fraction of basin covered by this drainage-irrigation combo
			kk=kk+1

			if (wt12.gt.0) then
			CALL TOPMOD(SI,SP,JS,NKA,TL(JS),ATB,PKA,ND,CL,PD,UNITS,
     *		IRR_L,MODWRT,IPSUB,IPATB,STIM,SPEAK,PREC,PET,interv,
     *		art_drainage,rate_irrig,month,
     *		M,MPS,MPE,QINST_OUT_0, DR_OUT_0,ndump,ntdh,ISTEP,
     *      MAXC,ZBM(JS,kk),
     3		MAXA,MAXSLP,MAXINT,SUMR(JS,kk),SUMQ(JS,kk),
     *		SUMAE(JS,kk),S0(JS,kk),Q0(JS),SR(JS,kk),CV(JS,kk),
     4	    ACIEM(JS,kk),ACSEM(JS,kk),SUMPE(JS,kk),SUMIE(JS,kk),
     *		SUMQB(JS,kk),SUMCE(JS,kk),SUMSLE(JS,kk),SUMR1(JS,kk),
     5	    QB(JS),QINST(1,kk,JS),DR(1,kk,JS),SUMQV(JS,kk),
     6	    SUMSE(JS,kk),ZBAR(JS,kk),zbar_in(istep+1,js,kk),TDH,
     *      ZR(JS),ak0fzrdt(JS),
     7		logoqm(JS),qvmin(JS),dth(JS),sumad,evap_mm,
     8       qlat_mm,ipflag,rirr) ! RPI and RAW
!  DGT 11/2/07 added qlat and ipflag.  ipflag for debugging.  qlat to return lateral outflows without
!     overland flow delays to facilitate mass balance checks
!  DGT 6/10/05.  Added subscript (JS) to Q0 in the calling statement above so that Q0 differences
!  between subbasins are preserved
c  DGT 6/25/05.  Added sumad to retrieve artificial drainage calcs
!  DGT 8/17/05 added evap_mm to the arguments above
c
c  code below may be redundant
c 				xIRR(1)=xIRR(1)+wt12*IRR_L(1) ! This variable is time so never selective with this
c				do ii=2,NIP1
c	              if(kcase .eq. 0)then  ! use weighting for output variables
c					xIRR(ii)=xIRR(ii)+wt12*IRR_L(ii)
c				  elseif(kp .eq. kcase)then
c					xIRR(ii)=IRR_L(ii)  ! output just the case of interest
c				  endif
c				end do

				IF (i_irrig.gt.1) then
					call irrigation(SR(JS,kk),SP,JS,INTERV,MAXSLP,dep) !watch for UNITS!!!
					depth_irrig_dem=depth_irrig_dem+wt2*dep !dep is in m/timestep
	! DGT 8/20/05 changed the weight in the above  from wt12 to wt2 because depth only should be weighted
	! among the different drainage classes within irrigated area
				else
					dep=0.
				end if
				zbm_d=zbm_d+wt12*zbm(js,kk)
				ACSEM_d=ACSEM_d+wt12*ACSEM(js,kk)
				ZBAR_d=ZBAR_d+wt12*ZBAR(js,kk)
				SR_d=SR_d+wt12*SR(js,kk)
				CV_d=CV_d+wt12*CV(js,kk)
				SUMR_d=SUMR_d+wt12*SUMR(js,kk)
				SUMQ_d=SUMQ_d+wt12*SUMQ(js,kk)
				SUMAE_d=SUMAE_d+wt12*SUMAE(js,kk)
				SUMpE_d=SUMpE_d+wt12*SUMpE(js,kk)
				S0_d=S0_d+wt12*S0(js,kk)
				QINST_OUT_d=QINST_OUT_d+wt12*QINST_OUT_0
				dr_out_d=dr_out_d+wt12*DR_OUT_0    ! 6/10/05  DGT keeping track of total runoff
				art_drainage_out=art_drainage_out+wt12*sumad  ! 6/28/05   DGT Artificial drainage
				evap_for_watermgmt(js)=evap_for_watermgmt(js)
     +                +evap_mm*wt12   ! DGT 8/17/05 keeping track of evap
                potentialevap(js)=potentialevap(js)+wt12*rirr(13)  ! DGT 10/21/12  keeping track of pet
                surfro(js)=surfro(js)
     +           +wt12*rirr(7)/1000.*(SP(1,JS)/1e6)/interv
                canstore(js)=canstore(js)+wt12*rirr(10)
                soilstore(js)=soilstore(js)+wt12*rirr(12)
                if(i_drainage.eq.2) then
                  tiled(js)=tiled(js)+wt12*sumad*1d-9 ! sumad is mm^3/s - now in m^3/s
                endif
                if(i_drainage.eq.3) then
                  ditchd(js)=ditchd(js)+wt12*sumad*1d-9
                endif

c  Detail topsbd writes
      if(idebugoutput .ge. 1)then
	if(modwrt .and. ibout(js) .eq. 1)then
	if(istep .ge. mi. and. istep .gt. 0)then
	if(js .eq. idebugbasin .or. idebugbasin .eq. 0)then
	if(kp .eq. idebugcase .or. idebugcase .eq. 0)then
	write(lunmod,937)js,istep,kp,wt12,prec,qlat_mm,(rirr(jj),jj=2,14),
     +rate_irrig,groundwater_to_take(JS)/(SP(1,JS)/1e6)*1000.,
     +dep*1000.,brain(js,istep),snowstatev(2,js)*1000.,
     +snowevaporation*timestep*1000.,
     +			(btmax(js,istep)+btmin(js,istep))/2,
     +				btdew(js,istep),btmax(js,istep)-btmin(js,istep),
	5								rirr(15)
 937  FORMAT(i5,I8,1X,I3,f11.7,25(1x,f11.5),1x,f14.8)
	endif
	endif
	endif
	endif
	endif
			end if
		end do !i_drainage
	end do !i_irrig
      write(TRACE,'(6f10.6)') zbar(js,:)
!  The one line below is a substantive change DGT 10/13/12.  It amounts to an assumption of
!   lumped depth to water table, rather than separate depth to water table for each drainage and
!   irrigation component.  It was made to fix the issue of groundwater levels declining indefinitely
!   due to there being no recharge from artificially drained areas
      zbar(js,:)=zbar_d
      if(istep .gt. 0)tempave(js)=(btmax(js,istep)+btmin(js,istep))/2 ! DGT 10/21/12 record ave temperature for output

!accumulate the fluxes from each part of the sub-basin as m3/timestep
	baseflow(JS) = QINST_OUT_d*interv*1d-9 !m^3/ts = mm^3/s *s/ts *m3/mm3
c	totalrunoff(JS) = (xIRR(2)/1000.)*SP(1,JS)/1d9 !(mm/timestep)*mm^2 / (1d9mm^3/m^3) = m^3/timestep
c   DGT 6/10/05  Use Total Runoff from the floating point DR variable rather than the integer IRR topmodel output
c    to avoid some divide by zero's later on in water management
	totalrunoff(JS) = dr_out_d*interv*1d-9 !m^3/ts = mm^3/s *s/ts *m3/mm3
	ArtDrainage(js)=art_drainage_out*interv*1d-9   ! DGT 6/28/05  Keep in same units as totalrunoff

	finish: IF(modwrt .and. ISTEP.EQ.M) THEN
	  if (idebugoutput .ge. 1)then  ! DGT 8/17/05 debugout
		IF(MODWRT) WRITE(LUNp,*)'MIN ZBAR=',ZBM_d,
     +                               ' MAX CONT AREA=',ACSEM_d
		IF(MODWRT) WRITE(LUNp,*)'LAMBDA=',TL(JS)
	  endif
		!  Storage at the end
		DTH1=SP(4,js)
		S1_d = - ZBAR_d*DTH1 + SR_d + CV_d
		BAL_d = SUMR_d - SUMQ_d - SUMAE_d + ( S0_d - S1_d )
	  if (idebugoutput .ge. 1)then  ! DGT 8/17/05 debugout
		WRITE(LUNp,*)'WATER BALANCE: BAL=',BAL_d,' SUMR=',SUMR_d
	  endif
		IF ( DABS(BAL_d) .GT. (SUMR_d+SUMQ_d)*1D-3 ) THEN
			! can't write to lunco1, so put it in toperror.txt
c			WRITE(21,*)'WATER BALANCE: BAL=',BAL_d,' SUMR=',SUMR_d,
c    +					' SUMQ=',SUMQ_d,' IT=',ISTEP
		ENDIF
	  if (idebugoutput .ge. 1)then  ! DGT 8/17/05 debugout
		WRITE(LUNp,'()')
		WRITE(LUNp,9110)SUMR_d/UNITS,SUMAE_d/UNITS
9110		FORMAT(' Rainfall    =',G12.5,' Total Evap  =',G12.5/)
		WRITE(LUNp,9115)SUMQ_d/UNITS,sumpe_d/units
9115		FORMAT(' Model Runoff=',G12.5,' Total PET =',g12.5/)
		WRITE(LUNp,9120)(S1_d-S0_d)/UNITS
9120		FORMAT(' Incr.Storage=',G12.5/)
       endif
	ENDIF finish

c	IRR(:,js)=xIRR(:)+.5         ! DGT 8/20/05 Changed the meaning of vol_irrig_demand to depth
	! to accommodate multiple users within a drainage that may have different water rights
	vol_irrig_demand(js) = depth_irrig_dem  ! *(SP(1,JS)/1e6) !m3/timestep = m/timestep * m^2
!	Q5SAVE(2,JR) = MAX(QINST_OUT,0.000001)
!	N5SAVE(JR)=N5SAVE(JR)+1

!     IF(ISTEP.EQ.0) THEN
!		T5SAVE(1,JR) = T5SAVE(2,JR)
!		Q5SAVE(1,JR) = Q5SAVE(2,JR)
!	ENDIF
	IF ( MODWRT .AND. ISTEP.EQ.M ) THEN
	if(idebugoutput .ge. 1)then  ! DGT 8/17/05 debugout
c			WRITE(LUNp,5500) JS        ! dgt changed info output from lunmod to lunp
c 5500			FORMAT(//' Output for sub-catchment ',I3/
c     1		       ' ---------------------------')
		WRITE(LUNp,4400) (SP(I,JS),I=1,NSP)     ! DGT changed info output from lunmod to lunp
 4400		FORMAT(' The parameters are'/(1x,e15.6))
		write(lunp,4401)(si(i,js),i=1,nsi)
 4401		format(' Initial conditions'/(1x,e15.6))
C RPI removed 25/3/2004 			IF(ibout(js).eq.1) write(lunmod,'(f24.0,i8)')sp(1,js),js
      endif
	ENDIF
C
C     SET UP THE START TIME OF THE OUTPUT TIME SERIES OF EACH GUTTER
C
	IF(ISTEP.GT.0) THEN
		QI(JS)=Q5SAVE(1,JR)   ! UNITS ARE mm**3/sec
		T5SAVE(2,JR)=TIM  !CHANGE THE TIME INTERVALS TO ABSOLUTE TIME
c
c     Debug write output
c
c		if(idebugoutput .ge. 1)then  ! DGT 8/17/05 debugout
c		IF ( MODWRT .and. ibout(js) .eq. 1) THEN
c			IF ( ISTEP .GE. MI ) THEN
c				ITEMP=PREC*1000+.5 !RAW bRAIN(js,ISTEP)*1000+.5
c				WRITE(LUNMOD,99)	js,IRR(1,js),ITEMP,
c    1								(IRR(JJ,js),JJ=2,10),
c     2								IRR(11,js)/1000,
c     3								(IRR(jj,js),jj=12,14),
c	4				(btmax(js,istep)+btmin(js,istep))/2,
c     +				btdew(js,istep),btmax(js,istep)-btmin(js,istep),
c	5								irr(15,js)
c   99 FORMAT(i5,I8,1X,I8,1X,I8,1X,I8,1X,I8,1X,I8,1X,I8,1X,I8,
c     1  4X,I3,5X,I3,1X,I7,1X,I8,1X,I7,1x,i7,1x,i7,3(1x,f6.2),1x,i12)
c			ENDIF
c		ENDIF
c	endif
	ENDIF
	ENDIF l2
cDGT end of subbasin write block
	ENDDO subbasin_loop
C
C THAT IS THE END OF THE SUBCATCHMENTS

!now we know runoff and soil moisture, so we can do a water managment step
!
	if (istep.gt.0) then
!		evap_for_watermgmt= irr(14,1:NSUB)/1000. !mm/timestep= um/timestep /1000
! DGT 8/17/05 - evap now handled above and not through irr
		precip_for_watermgmt=bRAIN(1:NSUB,ISTEP) !mm/timestep
	end if
! DGT 10/21/12  Additional writes for Christina
      icall=5  ! 0 to 4 were used in water management
	scalefactor=1.
      call Write_OutputLine('Potential_evapotranspiration_mm.txt',istep,
     & potentialevap,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
	scalefactor=1.
      call Write_OutputLine('TemperatureAve_C.txt',istep,
     & tempave,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
	scalefactor=1.
      call Write_OutputLine('Surface_runoff_cms.txt',istep,
     & surfro,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
	scalefactor=1.
      call Write_OutputLine('Canopy_storage_mm.txt',istep,
     & canstore,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
	scalefactor=1.
      call Write_OutputLine('Soil_storage_mm.txt',istep,
     & soilstore,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
	scalefactor=1000.
      call Write_OutputLine('Depth_to_Water_mm.txt',istep,
     & zbar,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
	scalefactor=1.
      call Write_OutputLine('Tile_drainage_cms.txt',istep,
     & tiled,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
	scalefactor=1.
      call Write_OutputLine('Ditch_drainage_cms.txt',istep,
     & ditchd,nsub,icall,scalefactor)

	! DGT 6/28/05   Added ArtDrainage to arguments for output
	call watermgmt(SDATE,SHOUR,ISTEP,M,
     +	totalrunoff,baseflow,ArtDrainage,vol_irrig_demand,MAXSLP, !inputs
     +	evap_for_watermgmt,precip_for_watermgmt, !inputs
     +	volume_irrig_sup,groundwater_to_take)	!outputs

	if (istep.gt.0) CALL UPDATETIME(iYEAR,MONTH,iDAY,HOUR1,timestep)
	ENDDO time_loop   ! RPI 17/3/2004           2 !RAWv7
C End of time loop  for reaches ***************************************************
! DGT 6/28/05   Added ArtDrainage to arguments for output
	call watermgmt(SDATE,SHOUR,-1,M, !final call to write/close output files
     +	totalrunoff,baseflow,ArtDrainage,vol_irrig_demand,MAXSLP, !inputs, ignored this time
     +	evap_for_watermgmt,precip_for_watermgmt,	 !inputs
     +	volume_irrig_sup,groundwater_to_take)	!outputs, ignored this time
		if(idebugoutput .ge. 1)then  ! DGT 8/17/05 debugout
      WRITE(lunp,*)'TOTAL SNOW MASS BALANCE ERROR =',ERRMBAL
	    endif
       close(98)
c debug	 close(108)

! DGT 10/21/12  Close files on additional writes for Christina
      icall=5  ! 0 to 4 were used in water management
      istep=-1
      call Write_OutputLine('Potential_evapotranspiration_mm.txt',istep,
     & potentialevap,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
      call Write_OutputLine('TemperatureAve_C.txt',istep,
     & tempave,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
	scalefactor=1.
      call Write_OutputLine('Surface_runoff_cms.txt',istep,
     & surfro,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
	scalefactor=1.0
      call Write_OutputLine('Canopy_storage_mm.txt',istep,
     & canstore,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
	scalefactor=1.
      call Write_OutputLine('Soil_storage_mm.txt',istep,
     & soilstore,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
	scalefactor=1000.0
      call Write_OutputLine('Depth_to_Water_mm.txt',istep,
     & zbar,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
	scalefactor=1.
      call Write_OutputLine('Tile_drainage_cms.txt',istep,
     & tiled,nsub,icall,scalefactor)

      icall=icall+1  ! 0 to 4 were used in water management
	scalefactor=1.
      call Write_OutputLine('Ditch_drainage_cms.txt',istep,
     & ditchd,nsub,icall,scalefactor)


      SRCHF = 0D0
 5000 CONTINUE
      SQCI = 0D0
		if(idebugoutput .ge. 1)close(lunmod)  ! DGT 8/17/05 debugout   ! DGT close model writes in case routing fails
      RETURN
C ***************************************************************************
C This routine must follow a "contain" stmt and be before the end of CALCTS
	CONTAINS
	SUBROUTINE EXPAND (Q,T,QSIZE,INI_DIM)
! Arrays Q and T should have the same shape, but different types
	INTEGER,POINTER,DIMENSION (:,:) :: T
	REAL*8 ,POINTER,DIMENSION (:,:) :: Q
	INTEGER ,DIMENSION(2) :: TB,xb
	INTEGER ,POINTER,DIMENSION (:,:) :: TTEMP_COPY
	REAL*8 ,POINTER,DIMENSION (:,:) :: QTEMP_COPY
	INTEGER QSIZE,INI_DIM
! Get the bounds of T and allocate a new, larger
! array
	TB = SHAPE(T)
	ALLOCATE (TTEMP_COPY(INI_DIM:TB(1)+100,TB(2)))
	ALLOCATE (QTEMP_COPY(INI_DIM:TB(1)+100,TB(2)))
	QSIZE=TB(1)+1
! Copy T into TEMP_COPY & make general for any INI_DIM
	TB(1)=TB(1)-(1-INI_DIM)
	TTEMP_COPY(INI_DIM:TB(1),1:TB(2)) = T(INI_DIM:TB(1),1:TB(2))
	QTEMP_COPY(INI_DIM:TB(1),1:TB(2)) = Q(INI_DIM:TB(1),1:TB(2))
! Deallocate old data
	DEALLOCATE (T)
	DEALLOCATE (Q)
! Point T at the new data
	T => TTEMP_COPY
	Q => QTEMP_COPY
	RETURN
	END SUBROUTINE EXPAND
      END

************************************************************************
*     SUBROUTINE  LAKE_ROUTE
************************************************************************


      SUBROUTINE LAKE_ROUTE(ILAKE,HQI,Q4,N6,Q5,DEL_T,LAKE_REACH,
     *  LZERO,LAKE_AREAS,LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,
     *  LOFLOWS,INI_LEVELS,MAX_LAKES,MAX_LHEADS)
C Q4 are the inflows, Q5 are the outflows, N6 are the number of intervals
C Lake variables
      IMPLICIT NONE  ! 30/03/2004 ITB


      INCLUDE 'maxvariables.inc'
      INCLUDE 'Tdims_v7.inc'

	INTEGER NLAKES,LAKE_REACH(MAX_LAKES),LZERO(MAX_LAKES),
     *        LK_LINE(MAX_LAKES)
	INTEGER N6
      INTEGER J,ILAKE,I,LAKE  ! 30/03/2004 ITB


      INTEGER LAKE_BEACH_SLPS(MAX_LAKES),NUM_RAT_VALS(MAX_LAKES)
	  INTEGER LHEADS(MAX_LAKES,MAX_LHEADS)
	  integer LOFLOWS(MAX_LAKES,MAX_LHEADS)

      REAL*4 LAKE_AREAS(MAX_LAKES),INI_LEVELS(MAX_LAKES)  ! RPI 17/5/2002 added ini_levels

      COMMON /LAKES1/ NLAKES !,LAKE_REACH,LZERO,LAKE_AREAS,
C     *           LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,
C     *           INI_LEVELS ! RPI 17/5/2002 added ini_levels
      COMMON /LAKES2/ LAKE

      REAL*8 Q4(N6),Q5(N6+1),HQI(MAX_LAKES),H0,DELH,AREA0

	INTEGER*4 RATNG(MAX_LHEADS*2),PTRATE,IH0,DEL_T,R2(MAX_LHEADS*2)  !RAW test
	INTEGER*4 IQ5  !RAW test
C Load the rating for lake ILAKE



      DO 2 J =1,NUM_RAT_VALS(ILAKE)
	RATNG(J)=LHEADS(ILAKE,J)
	RATNG(J+NUM_RAT_VALS(ILAKE))=LOFLOWS(ILAKE,J)
c	R2(J)=LOFLOWS(ILAKE,J)  !RAW test
c	R2(J+NUM_RAT_VALS(ILAKE))=LHEADS(ILAKE,J)  !RAW test
2	CONTINUE
	H0=HQI(ILAKE)
	DO 1 I=1,N6
	AREA0=LAKE_AREAS(ILAKE)+LAKE_BEACH_SLPS(ILAKE)*LK_LINE(ILAKE)*
     *     (H0-LZERO(ILAKE))*1000000.  ! Units = mm*2
	IH0=H0+0.5
	Q5(I)=PTRATE(IH0,RATNG,NUM_RAT_VALS(ILAKE))*1000000. !Q5 is mm**3/s
	q5(i)=max(q5(i),0.0) ! RPI 31/5/2002
	DELH=(Q4(I)-Q5(I))*DEL_T/AREA0
	H0=H0+DELH
1	CONTINUE
	AREA0=LAKE_AREAS(ILAKE)+LAKE_BEACH_SLPS(ILAKE)*LK_LINE(ILAKE)*
     *     (H0-LZERO(ILAKE))*1000000.  ! Units = mm*2
	IH0=H0+0.5
c	Q5(N6+1)=2*Q4(1)-Q5(1) !RAW test
c	IQ5=Q5(N6+1)/1000000+0.5  !RAW test
c	H0=PTRATE(IQ5,R2,NUM_RAT_VALS(ILAKE))  !RAW test
	HQI(ILAKE)=H0  ! RPI v7 - need to update HQI for next call
	Q5(N6+1)=PTRATE(IH0,RATNG,NUM_RAT_VALS(ILAKE))*1000000. !Q5 is mm**3/s  !RAW test
	q5(N6+1)=max(q5(N6+1),0.0) ! RPI 31/5/2002

	RETURN
	END


******************************************************************
*        SUBROUTINE  INLAKE_LVL
******************************************************************

      SUBROUTINE INLAKE_LVL(ilake,SQI,HQI,LAKE_REACH,LZERO,LAKE_AREAS,
     *  LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,INI_LEVELS,
     *  MAX_LAKES,MAX_LHEADS)

C Lake variables

      IMPLICIT NONE  ! 30/03/2004 ITB

	INCLUDE 'maxvariables.inc'
      INCLUDE 'Tdims_v7.inc'

	REAL*8 SQI,HQI
	INTEGER NLAKES,LAKE_REACH(MAX_LAKES),LZERO(MAX_LAKES),
     *        LK_LINE(MAX_LAKES)
	INTEGER LAKE_BEACH_SLPS(MAX_LAKES),NUM_RAT_VALS(MAX_LAKES)
	INTEGER LHEADS(MAX_LAKES,MAX_LHEADS)
	integer LOFLOWS(MAX_LAKES,MAX_LHEADS)
	INTEGER IH,IQ

      INTEGER*4 LAKE, ILAKE, J, IMIN, NFORE ! 30/03/2004 TB
      REAL*4 LAKE_AREAS(MAX_LAKES),INI_LEVELS(MAX_LAKES)  ! RPI 17/5/2002 added ini_levels
	REAL*8 SCORE   ! 30/03/2004 ITB

      COMMON /LAKES1/ NLAKES !,LAKE_REACH,LZERO,LAKE_AREAS,
C     *           LAKE_BEACH_SLPS,LK_LINE,NUM_RAT_VALS,LHEADS,LOFLOWS,
C     *           INI_LEVELS ! RPI 17/5/2002 added ini_levels
      COMMON /LAKES2/ LAKE
C Local variables
	INTEGER*4 RATNG(MAX_LHEADS*2),PTRATE
C	DO 1 I=1,NLAKES
C load LHEADS(I,1 to NUM_RAT_VALS(I)) AND LOFLOWS(I,1 to NUM_RAT_VALS(I))
C in approriate order (flows then heads) and pass the values of SQI(I) to
C the routine that interpolates to get the corresponding value of HQI(I)
C Load the rating for lake I
C RPI 17/5/2002 added the coded associated with INI_LEVELS
	IF(INI_LEVELS(ILAKE).LT.0.) THEN
		DO 2 J =1,NUM_RAT_VALS(ILAKE)
		RATNG(J)=LOFLOWS(ILAKE,J)
		RATNG(J+NUM_RAT_VALS(ILAKE))=LHEADS(ILAKE,J)
2		CONTINUE
C Get the level corresponding to the initial flow for lake I
		IQ=SQI/1.E+6+0.5 ! Rating l/s - SQI in mm**3/s
		IH=PTRATE(IQ,RATNG,NUM_RAT_VALS(ILAKE))
		HQI=IH
	ELSE
		HQI=INI_LEVELS(ILAKE)
	ENDIF
C1	CONTINUE
	RETURN
	END
*****************************************************************
*     SUBROUTINE SNOW
*****************************************************************
	subroutine snow(temper,elevtg,elevsb,rlapse,brain,ddf,SNOWST,
     *				dt,m,nsub,js,IT,MAXSLP,MAXINT)

	implicit none

	INCLUDE 'maxvariables.inc'
	SAVE

      integer m,nsub,js,it
      integer*8 dt  ! DGT 5/14/12
	real*8 elevtg, elevsb,rlapse,ddf,snowst,newsnow,t,melt
	real*8 lastsnowst
	real*8 temper(maxint), bRAIN, temp, snowbal
	IF(IT.EQ.1) then
C	if (js.eq.1) open(unit=78,file='snowout.txt')
		write(78,*)js,nsub,m
		write(78,'(A132)')'it, precip(mm), snow(mm), rain(mm),
     * temper(deg), snowstore(mm), melt(mm), snowbal(mm)'
	endif
C	do it=1,m
		lastsnowst=snowst
		t=temper(it)-(elevsb-elevtg)*rlapse
		temp=brain
		if (t<0) then
			newsnow=bRAIN !bRAIN in mm/interval, newsnow in mm
			snowst=snowst+newsnow !snowst and newsnow both in mm
			bRAIN=0.
			melt=0.
		else
			newsnow=0.
			melt=min(snowst,ddf*t*dt/86400.) !ddf in mm/deg/day, t in deg, dt in seconds, melt in mm
			snowst=snowst-melt	!melt in mm, snowst in mm
			brain=brain+melt !melt in mm, brain in mm/interval
		endif
		snowbal=snowst-lastsnowst+melt-newsnow
		write(78,5000)
     *it, temp, newsnow, brain, t, snowst, melt, snowbal
5000  format(i8,8(f12.2))
C	enddo
	return
	end
C RPI 23/9/02 substituted NFORE for N4 which is not used in CORRECT,
C so that he could export the number of values actually forecast.
**********************************************************************
*         SUBROUTINE CORRECT
**********************************************************************
	SUBROUTINE CORRECT(Qmodel,N,Qmeas,M,T_NOW,Qcorr,NFORE,BESTSHIFT,
     +                   BESTSCALE,MAX_SHIFT,MAXDIM)

	IMPLICIT NONE  ! 30/03/2004 ITB


      INCLUDE 'maxvariables.inc'

	INTEGER N, M, T_NOW, BESTSHIFT, SHIFT, MAX_SHIFT, T_START, I
	INTEGER *4 IMIN,NFORE  ! 30/03/2004  ITB
      INTEGER*4 MAXDIM ! 30/03/2004 ITB

      REAL*8 Qmodel(N), Qmeas(MAXDIM), Qcorr(MAXDIM), BESTSCALE ! RPI 26/9/02 changed M,N to MAXINT
	REAL*8 SCALE, BESTSCORE, SUM1, SUM2, SUM_WGT, WGT ! RPI 24/7/02 introduced WGT
	REAL*8 SWGT ! RPI introduced SWGT 3/4/03
      REAL*8  SCORE ! 30/03/2004 ITB

c RPI 16/7/2002 allowed MAX_SHIFT to be read in from rchareas.txt
c	MAX_SHIFT=2 !26/6/02 Ross changed 12 to 2 because 12 hours is too radical. Needs to be adjustable parameter 12
	T_START=1+MAX_SHIFT
	BESTSCALE=1
	BESTSHIFT=0
	BESTSCORE=1e30
	DO SHIFT=-MAX_SHIFT,MAX_SHIFT
	   Sum1=0.
	   Sum2=0.
C Qmeas is defined from 1 to T_NOW.
C Qmodel is defined from 1 to M > T_NOW. Setting T_START=MAX_SHIFT+1
C guarantees the lower limit is always met.
c	   DO I=T_START+min(0, SHIFT),T_NOW+max(0, SHIFT)
c         DO I=T_START,T_NOW
c		  IF(SHIFT.LT.I) THEN
c		  IMIN=min(N,I-SHIFT)
c		  Sum1=sum1+ Qmeas(I)*Qmodel(IMIN)/(1+T_NOW-I)
c		  Sum2=sum2+ Qmodel(IMIN)*Qmodel(IMIN)/(1+T_NOW-I)
c		  wt=0.1**(T_NOW-I)
c		  Sum1=sum1+ Qmeas(I)*Qmodel(IMIN)*wt
c		  Sum2=sum2+ Qmodel(IMIN)*Qmodel(IMIN)*wt
c		  ENDIF
c	   ENDDO
c	   SCALE=1.0
c	   IF (SUM2 .NE. 0) SCALE=SUM1/SUM2
C Scale on the last value only !RAW 24/6/02 add max test so we don't access qmodel(0)
		scale=qmeas(T_now)/qmodel(max(1,min(n,T_now-shift)))
	   SCORE=0
C Qmeas is defined from 1 to T_NOW.
C Qmodel is defined from 1 to M > T_NOW. Setting the lower limit to T_START+1
C guarantees the lower limit is always met.
c	   NV=0
		SUM_WGT=0.
C By using T_START+1 we guarantee that IMIN-1>=1. There could be a problem
C if T_NOW is small in which case reset lower limit on this DO loop. We will
C then need an extra check on IMIN - RPI 22/7/2002
         DO I=min(T_START+1,T_NOW),T_NOW
			IMIN=I-SHIFT  ! This is the index for the shifted model flows
C This check is needed to avoid over-running the values defined in Qmodel
C  - RPI 22/7/2002
			IF(IMIN.LE.N) THEN
C RPI 24/7/2002 introduced the exponential weighting scheme
				WGT=exp(-(float(T_NOW)-float(I))/float(I))
C RPI 3/4/03 found that shifting could cause a large scaling factor and
C that this could distort the nudging. RPI therefore introduced weighted
C the weight to bias it in favour of SCALE values near 1.0. Note that although
C SCALE is always +ve (or it should be) it is asymetrically distributed,
C e.g. a 0.5 value should have the same effect as 2.0. However, the resultant
C weight should not be used for normalising as it is a scaling weight.
				SWGT=ABS(SCALE)+1.E-12
				IF(SWGT.LT.1.) SWGT=1./SWGT
				SCORE=SCORE+WGT*SWGT*
     +((Qmeas(I)-Qmeas(I-1))-SCALE*(Qmodel(IMIN)-Qmodel(IMIN-1)))**2
c				SCORE=SCORE+((Qmeas(I)-Qmeas(I-1))-
c     +                    SCALE*(Qmodel(IMIN)-Qmodel(IMIN-1)))**2
c				NV=NV+1
				SUM_WGT=SUM_WGT+WGT
c	 if(i.gt.29)
c     *	write(85,777) I,SHIFT,Qmeas(I)-Qmeas(I-1),
c     * SCALE*(Qmodel(IMIN)-Qmodel(IMIN-1)),
c     * (Qmeas(I)-Qmeas(I-1))-SCALE*(Qmodel(IMIN)-Qmodel(IMIN-1)),
c     * SCORE,SUM_WGT,SCORE/SUM_WGT,
c     * SCALE,WGT,BESTSCORE
c777	FORMAT(2i3,9g12.4)
		  ENDIF
	   ENDDO
C Normalise scores for valid comparison since the way IMIN is specified
C may result in there being different numbers of values contributing to SCORE
			IF(SUM_WGT.NE.0) then
			SCORE=SCORE/SUM_WGT
c		IF(NV.NE.0) then
c		SCORE=SCORE/NV
		else
		SCORE=1.e+30
		endif
	   IF (SCORE .LT. BESTSCORE) THEN
	      BESTSCORE=SCORE
	      BESTSHIFT=SHIFT
	      BESTSCALE=SCALE
	   ENDIF
	ENDDO
C Calculate the number of corrected flows before returning
	nfore=max(N+bestshift,1)  ! RPI 23/9/02
	do i=1,max(N,nfore) ! RPI 26/9/02 replaced what was just N
C		IF(BESTSHIFT.LT.I) THEN  ! RPI 22/7/02 removed this
c			qcorr(i)=bestscale*qmodel(min(N,max(i-bestshift,T_START))) ! RPI 22/7/02 removed this
			qcorr(i)=bestscale*qmodel(min(N,max(i-bestshift,1)))
				if(i.le.m) then
					write(85,200) i,Qmeas(i),qmodel(i),qcorr(i),
     +							bestscale,bestshift
200					format(i6,3f10.0,f10.2,i5)
				elseif (i.le.n) then  ! RPI 26/9/02 changed from else
					write(85,200) i,0.0,qmodel(i),qcorr(i),bestscale,
     +							bestshift
C RPI 26/9/02 added the next bit to complete the if statement
				else
					write(85,200) i,0.0,-1.,qcorr(i),bestscale,
     +							bestshift
			endif
C		endif ! RPI 22/7/02 removed this
	enddo
	RETURN
	END
