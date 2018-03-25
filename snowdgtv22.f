C  File  snowdgtv22.f
C  Utah Energy Balance Snow Accumulation and Melt Model.
C
C  Authors
C  David G. Tarboton (dtarb@cc.usu.edu), Utah Water Research Laboratory, Utah State University
C  Charlie Luce cluce (cluce@fs.fed.us), USDA Forest Service, Rocky Mountain Research Station, Boise, ID
C  Graduate Students who have worked on this code
c    Jinsheng You, PhD, 2004 (youjins@hotmail.com)
C    Tanveer Chowdhury, MS, 1993
C    Tom Jackson, PhD, 1994
c
C  Significant changes
C  5/4/04 Accommodate glacier surface runoff (version 2.1)
c  7/26/01 Jinsheng You dissertation work (version 2.0)
c    - New surface temperature parameterizations comprising
C       1) the Simple Gradient, almost the same as Original UEB,
C       2) Simple Gradient approach with shallow snow correction.
C       3) The classical force-restore approach.
C       4) Modified force-restore approach.
c    - New refreezing scheme
c
C
C  This program was written and prepared under agreement with and funding
C  by the U.S. Government , and therefore is in the public domain and not
C  subject to copyright.
C
      SUBROUTINE SNOWUEB2(dt,nt,input,sitev,statev,
     &   tsprevday, taveprevday, nstepday, param,iflag,
     &   cump,cume,cummr,outv,mtime)

c      parameter(nsv=5,nxv=5,niv=7,nov=14)
      parameter(niv=7)

      REAL MR,k,lc,ks
	real lans, lang
      integer pflag,ounit,iflag(*)
c	data pi/3.141592654/ !define PI forangle conversion

c Definitions
c  dt  Time step in hours
c  nt number of time steps
c input  -- input forcing
c	  input(1,*) air temperature (C)
c	  input(2,*) precipitation (m/hr)
c	  input(3,*) wind speed  (m/s)
c	  input(4,*) relative humidity (on a 0-1 scale)
c	  input(5,*) incoming short wave  (kJ/m^2/h)
c	  input(6,*) net radiation  (kJ/m^2/h)
c	  input(7,*) Cosine of Zenith angle
c SITEV -- site variables
c        site variables (1-5)
c        sitev(1)  forest cover fraction
c        sitev(2)  drift factor  (No detailed information give 1)
c        sitev(3)  air pressure (Pa)
c        sitev(4) ground heat flux  Kj/m^2/hr (3.6 = 1 W/m^2)
c        sitev(5) albedo extinction parameter (m)
c STATEV
c        statev(1)  Snow Energy Content  (KJ/m^2)
c        statev(2)  Snow Water Equivalent (m) relative to T = 0 C solid phase
c        statev(3)  Dimensionless age of snow surface (or albedo - depending on flag 4)
c        statev(4)  Refreezing depth (m) used as refdepth
c        statev(5)  Refreezing depth (m) used as totalrefdepth
c  totalrefdepth is a misnomer.  These are the same quantity - the refreezing depth.  They are repeated because
c  when refdepth exceeds the diurnal penetration depth it is set to 0 to tell the code to use the regular
c  surface temperature functions while totalrefdepth is only reset to 0 when there is actual melt generated
c  or energy content becomes negative indicating freezing of all liquid phase.  This ensures that the regular
c  surface temperature functions persist when there is liquid water present but the refreezing front has penetrated
c  to depth greater than diurnal temperature fluctuations.
c        TsPrevday(1:nstepday)   Surface temperature over the last 24 hours
c 	   TavePrevday(1:nstepday)   Depth average temperature over the last 24 hours
c
c PARAM  --  snowmelt model parameters (see below)
c iflag  -- flags
c	   iflag(1) 0=radiation is shortwave in col 5 and longwave in col 6, else = net radiation in column 7
c        iflag(2)        no 0 (/yes 1) printing
c        iflag(3)  unit to which to print
c        iflag(4)  how albedo calculations are done (a value 1 means albedo is calculated, otherwise statev(3) is albedo
c 	   iflag(5)  model option for surface temperature approximation
C              1) the Simple Gradient, almost the same as Original UEB,
C              2) Simple Gradient approach with shallow snow correction.
C              3) The classical force-restore approach.
C              4) Modified force-restore approach.
c cump,cume,cummr  -- cumulative precipitation (with df), cumulative evaporation, cumulative melt over time step in m
c outv  -- output variables
c       outv(1)=prain   rain  m/hr
c       outv(2)=ps     snow  m/hr
c       outv(3)=a     albedo
c       outv(4)=qh    sensible heat (kJ/m2/hr)
c       outv(5)=qe    latent heat (kJ/m2/hr)
c       outv(6)=e     sublimation m/hr
c       outv(7)=mr    melt outflow m/hr
c       outv(8)=qm    heat advected with melt
c       outv(9)=q     Energy rate of change (kJ/m2/hr)
c       outv(10)=fm   Mass rate of change (m/hr)
c       outv(11)=tave  Average temperature (C)
c       outv(12)=tsurf  Surface temperature C
c       outv(13)=qnet  Net Radiation (kJ/m2/hr)
c	 outv(14)=smelt   Surface melt  m/hr
c
c mtime   4 variable array with time information
c    mtime(1)  year
c    mtime(2)  month
c    mtime(3)  day
c    mtime(4)  hour (0-24 as a real number)
c    mtime(5)  model element number
c
Cyjs Note: in this subroutine, the outv is an array which passes value to this subroutine and back to the snow
cyjs drive program. The Outv(9) and outv(10) pass the daily average snowpack temperature and daily
cyjs snow surface temperature to this subroutine but pass the Qin total and combined mass fluxes
cyjs back.


      real input(niv,nt)
      real sitev(*)
      real outv(*)
      real statev(*)
      real param(*)   ! inherit dimension from calling program
	real mtime(*)
	real tsprevday(*)
	real taveprevday(*)
	integer iTsMethod       !yjs Add model time initialization 09/19/2000

	common /ts_save/ ts_old, tave_old, Ts_Ave, Tave_ave

cyjs  Constant data set
      data to /0.0/        !  Temperature of freezing (0 C)
      data tk /273.15/     !  Temperature to convert C to K (273.15)
      data sbc /2.0747e-7/ !  Stefan boltzman constant (2.0747e-7 KJ/m^2/hr/K)
      data hf /333.5/      !  Heat of fusion (333.5 KJ/kg)
      data hneu /2834.0/   !  Heat of Vaporization (Ice to Vapor, 2834 KJ/kg)
      data cw /4.18/       !  Water Heat Capacity (4.18 KJ/kg/C)
      data cs /2.09/       !  Ice heat capacity (2.09 KJ/kg/C)
      data cp /1.005/      !  Air Heat Capacity (1.005 KJ/kg/K)
      data ra /287.0/      !  Ideal Gas constant for dry air (287 J/kg/K)
      data k  /0.4/        !  Von Karmans constant (0.4)
      data hff /3600.0/    !  Factor to convert /s into /hr (3600)
      data rhoi /917.0/    !  Density of Ice (917 kg/m^3)
      data rhow /1000.0/   !  Density of Water (1000 kg/m^3)
      data g    /9.81/     !  Gravitational acceleration (9.81 m/s^2)
	data w1day /0.261799/!  Daily frequency (2pi/24 hr 0.261799 radians/hr)
	data pi /3.141592654/!  Pi

cyjs  End of constant declaration

c  Parameters
      tr=param(1)     !  Temperature above which all is rain (3 C)
      ts=param(2)     !  Temperature below which all is snow (-1 C)
      es=param(3)     !  emmissivity of snow (nominally 0.99)
      cg =param(4)    !  Ground heat capacity (nominally 2.09 KJ/kg/C)
      z=param(5)      !  Nominal meas. height for air temp. and humidity (2m)
      zo=param(6)     !  Surface aerodynamic roughness (m)
      rho=param(7)    !  Snow Density (Nominally 450 kg/m^3)
      rhog=param(8)   !  Soil Density (nominally 1700 kg/m^3)
      lc=param(9)     !  Liquid holding capacity of snow (0.05)
      ks=param(10)    !  Snow Saturated hydraulic conductivity (20 !160 m/hr)
      de=param(11)    !  Thermally active depth of soil (0.1 m)
      abg=param(12)   !  Bare ground albedo  (0.25)
      avo=param(13)   !  Visual new snow albedo (0.95)
      anir0=param(14) !  NIR new snow albedo (0.65)
	lans= param(15) !  the thermal conductivity of fresh (dry) snow (0.0576 kJ/m/k/hr)
	lang= param(16) !  the thermal conductivity of soil (9.68 kJ/m/k/hr)
	wlf= param(17)  !  Low frequency fluctuation in deep snow/soil layer (1/4 w1 = 0.0654 radian/hr)
	rd1= param(18)  !  Amplitude correction coefficient of heat conduction (1)
      fstab=param(19) !  Stability correction control parameter 0 = no corrections, 1 = full corrections
	Tref=param(20)  !  Reference temperature of soil layer in ground heat calculation input
	dNewS=param(21) !  The threshold depth of for new snow (0.001 m)
	gsurf=param(22) !  The fraction of surface melt that runs off (e.g. from a glacier)
c
c   debugging
c      if(mtime(1) .eq. 1948 .and. mtime(2) .eq. 11 .and.
c     + mtime(3) .eq. 8 .and. mtime(4) .gt. 12 .and.
c     + mtime(5) .eq. 169)then
c	   mtime(1)=mtime(1)
c	endif
C      write(6,*)(input(j,1),j=1,7)
C   State variables - These serve as initial conditions
      ub=statev(1)    ! Snow Energy Content  (KJ/m^2)
      w=statev(2)     ! Snow Water Equivalent (m) relative to T = 0 C solid phase
	if(ub.le.0.0) then
	  refDepth = 0.0
	  totalRefDepth = 0.0
	else
	  refDepth=statev(4)
	  totalRefDepth =statev(5)
      endif

Cyjs	Save old Value 07/23/01
	ub_old = ub
	refDepth_old=refDepth


c   Site variables
      fc=sitev(1)     !  Forest cover fraction (0-1)
      df=sitev(2)     !  Drift factor
      pr=sitev(3)     !  Atmospheric Pressure (Pa)
      qg=sitev(4)     !  Ground heat flux (KJ/m^2/hr)  This is more logically an
C               input variable,but is put here because it is never known at each
c                 time step. Usually it will be assigned a value 0.
      aep=sitev(5)   !  Albedo extinction parameter to smooth
C      transition of albedo when snow is shallow. Depends on Veg. height (m)


C   control flags
      iradfl=iflag(1)
      pflag=iflag(2)
      ounit=iflag(3)
	!iflag(4) albedo caculation
	iTsMethod = iflag(5) ! the method to approximate the surface temperature
						! 1 normal old snow melt model
						! 2 revised direct gradient method (New method for ke) and qe
						! 3 force restore approach
						! 4 modified force restore approach

cyjs model time step information
	yy=mtime(1)
	mm=mtime(2)
	dd=mtime(3)
	hr=mtime(4)

C   Calculate constants that need only be calculated once.
       cd=k*k*hff/(log(z/zo)**2)*(1-0.8*FC)   ! factor in turbulent fluxes
C    The FC correction is applied here to hit sensible and latent heat fluxes
C    and evaporation rate in one place and maintain consistency in the surface
C     temperature and stability calculations. It is consistent with wind speed
C     reduction by a factor of 1-0.8 FC which is the most reasonable physical
C     justification for this approach.
C     I recognize that this is not a very good way to parameterize vegetation.
C     This will be an area of future improvements (I hope).
c    FC is also used below to adjust radiation inputs.
      RRHOI=RHOI/RHOW
      RRHO=RHO/RHOW
      RID=1.0/RRHO-1.0/RRHOI
	rhom=lc*rho
c
c   Loop for each time step
c
      do 2 i = 1,nt   ! DGT Time looping disabled as temperature averaging not handled here - must be handled outside
C   Input variables
        ta=input(1,i)    ! Air temperature input (Degrees C)
        p=input(2,i)     ! Precipitation rate input (m/hr)
        ws=input(3,i)    ! Wind Speed (m/s)
        rh=input(4,i)    ! Relative humidity (fraction 0-1)
        ! DGT 5/27/12 initialize variables to avoid run time check problems
        qsi=0.
        qli=0.
        qnetob=0.
        if(iradfl.eq.0)then  ! input is incoming short and longwave
          qsi=input(5,i)   ! Incoming shortwave radiation (KJ/m^2/hr)
          qli=input(6,i)   ! Incoming longwave radiation (KJ/m^2/hr)
        else
          qnetob=input(5,i) ! Net allwave radiation (KJ/m^2/hr)
        endif
        coszen=input(7,i)   ! Cos(angle between direct sunlight and surface
c                             normal).
c         Representative value over time step used in albedo calculation.
c         We neglect the difference between direct and diffuse albedo.
cDGT Daily average temperatures handled internally so that multiple time steps will work
	  Ts_ave = daily_ave(tsprevday, nstepday, -100.)+tk ! (C)  Surface temperature average over last 24 hours
	  Tave_ave = daily_ave(taveprevday, nstepday, -100.)+tk ! (C)  Depth averaged temperature average over last 24 hours
        Ts_old = tsprevday(nstepday)+tk ! (C) Surface temperature from previous time step
        Tave_old = taveprevday(nstepday)+tk ! (C) Average temperature from previous time step
c   If any of these variables are out of range due to any problem set them back to freezing
	  if(ts_old .lt. 0.)then
	    write(6,*)"Invalid previous time step surface temperature ",
     +	ts_old," set to 273 K"
	    ts_old=tk
	  endif
	  if(tave_old .lt. 0.)then
	    write(6,*)"Invalid previous time step average temperature ",
     +    tave_old," set to 273 K"
	    tave_old=tk
	  endif
	  if(Ts_ave .lt. 0.)then
	    write(6,*)"Invalid last 24 hr average surface temperature ",
     +	Ts_ave," set to 273 K"
	    Ts_ave=tk
	  endif
	  if(Tave_ave .lt. 0.)then
	    write(6,*)"Invalid last 24 hr average temperature ",
     +	Tave_ave," set to 273 K"
	    Tave_ave=tk
	  endif

C  Separate rain and snow
        PS=PARTSNOW(P,TA,TR,TS)
        PRAIN = P - PS
C  Increase precipitation as snow by drift multiplication factor
        PS = PS*dF

c        if(iflag(4).eq.1)then
C  Calculate albedo
c          a=albedo(statev(3),coszen,w/rrho,aep,abg,avo,anir0)
c Use of this albedo throughout time step neglects the
c  changes due to new snow within a time step.
c        else
c          a=statev(3)
c        endif

ccyjs  if there is any new snow without rain the snow age is change to 0.

c       if(ps .gt. 0.0 .and. prain .le. 0.0) then
c	       statev(3) =0.0
c	  endif


        if(iflag(4).eq.1)then
C  Calculate albedo
          a=albedo(statev(3),coszen,w/rrho,aep,abg,avo,anir0)
c Use of this albedo throughout time step neglects the
c  changes due to new snow within a time step.o
        else
          a=statev(3)
        endif


C   Calculate neutral mass transfer coefficient
       rkn=cd*ws
C   Adjust radiation inputs for the effect of forest canopy.
      qsi=qsi*(1.-fc)
      qli=qli*(1.-fc)
      qnetob=qnetob*(1.-fc)
C     FC corrections are also in the following subroutines.
C      QFM where outgoing longwave radiation is calculated.
C      SNOTMP where outgoing longwave radiation is used in the surface
C      temperature equilibrium approx.
c  Debugging
c      if(mtime(1)== 1950 .and. mtime(2)==4 .and.
c     + mtime(3)==1 .and. mtime(4) >12 .and. mtime(5)==169) then
c	   mtime(1)=mtime(1)
c	endif

C   Call predictor corrector subroutine to do all the work
        CALL PREDICORR(DT,UB,W,a,ta,prain,ps,ws,rh,qsi,  !10
     *               qli,iradfl,rkn,   !13
     *               qnetob,cdh,cde,rid,param,sitev,iTsMethod,mtime, !21
c     Following variables are output
     *               QH,QE,E,
     *               MR,QM,Q,FM,TSURF,tave,qnet,refDepth, totalRefDepth,
     *               smelt,gsurf)
c
c  DGT 4/2/05   Despite all checks in predicor It can (and does) occur that
c   we still have ub so large that it results in tave greater than 0, which implies that all the
c   snow is liquid.  In these cases - just force the snow to disappear and add the energy involved to Qm.
       tave=tavg(ub,w,rhow,cs,to,rhog,de,cg,hf)
c	if(tave < -1000)then
c	   tave=tave
c	endif
	 if(tave .gt. 0.)then   !  all is liquid so snow must disappear
		mr=mr+w/dt
		qm=qm+w/dt*rhow*hf
		q=q-w/dt*rhow*hf
		ub=ub-w/dt*rhow*hf
		w=0.
       endif
cDGT 7/25/05   To guard against unreasonable UB when there is no snow do not allow bulk temperature to go above 10 C
       if(tave .gt. 10.)then
         ub=rhog*de*cg*10.
	 endif
cdgt 5/4/04 surface melt change
cyjsc   Update snow surface age based on snowfall in time step
cyjs       if(iflag(4).eq.1) call agesn(statev(3),dt,ps,tsurf,tk)
c   Update snow surface age based on snowfall in time step
       if(iflag(4).eq.1) call agesn(statev(3),dt,ps,tsurf,tk,dNewS)
c    accumulate for mass balance
       cump=cump+(ps+prain)*dt
       cume=cume+e*dt
       cummr=cummr+mr*dt
       tave=tavg(ub,w,rhow,cs,to,rhog,de,cg,hf)   !  this call
c   necessary to keep track of average internal temperature used in some surface energy algorithms.
c	if(tave < -1000)then
c	   tave=tave
c	endif

cyjs update the total depth of the refreezing depth in the snow pack according the
cyjs the refreezing depth at time step and the positive energy input. 07/22/01
c  DGT's revised logic  1/13/05
      if(lc.gt.0.0) then
       if(refDepth.gt. 0.0) then
	    totalRefDepth=refDepth  ! if refreezing depth increases totalrefdepth keeps track
	 else  ! here refdepth has gone to 0 somehow
	  if(mr.gt.0.0 .or. (ub.gt.ub_old .and. ub .gt.0.0))
c   If there is melt or an increase in energy refdepth is reset
     +	    totalRefDepth = 0.0
	 endif
	elseif(mr.gt.0.0 .or. (ub.gt.ub_old .and. ub .gt.0.0)) then
c   Here lc=0.  If there is melt or an increase in energy refdepth is reset
c   This is likely redundant because if lc=0 then there is no meltwater to refreeze
	 totalRefDepth =0.0
	endif
c  Jinsheng's original logic
c      if(lc.gt.0.0) then
c       if((refDepth-refDepth_old) .gt. 0.0) then
c	    totalRefDepth=totalRefDepth +refDepth-refDepth_old
c	 else
c	  if(mr.gt.0.0)  totalRefDepth = 0.0 !totalRefDepth-mr*rhow/rhom
c	  if((ub-ub_old) .gt. 0.0 .and. ub. gt.0.0) then
c	    TotalRefDepth = totalRefDepth-(ub-ub_old)/(rhom*hf)
c	  endif
c	 endif
c	elseif(mr.gt.0.0 .or. (ub.gt.ub_old .and. ub .gt.0.0)) then
c	 totalRefDepth =0.0
c	endif
	if(totalRefDepth .lt. 0.0)  totalRefDepth=0.0
c
cyjs update tsbackup and tavebackup
	 do 50 ii = 1 , nstepday-1
		tsprevday(ii)= tsprevday(ii+1)
	    taveprevday(ii)= taveprevday(ii+1)
 50	 continue
	   tsprevday(nstepday)= tsurf
	   taveprevday(nstepday)= tave


       IF(PFLAG.eq.1) THEN
            write(ounit,*) UB,W,statev(3),
     *                 prain,ps,a,qh,qe,e,mr,qm,q,fm,tave,tsurf
     *                ,cump,cume,cummr,qnet,smelt,refdepth,totalrefdepth
cdgt 5/4/04 surface melt smelt
       ENDIF
  2    continue

       statev(1)=ub
       statev(2)=w
	 statev(4)=refDepth
	 statev(5)=totalRefDepth
       outv(1)=prain
       outv(2)=ps
       outv(3)=a
       outv(4)=qh
       outv(5)=qe
       outv(6)=e
       outv(7)=mr
       outv(8)=qm
       outv(9)=q
       outv(10)=fm
       outv(11)=tave
       outv(12)=tsurf
       outv(13)=qnet
	 outv(14)=smelt   ! dgt 5/4/04
       RETURN
       END

*********************** PREDICORR () **************************
C     Predictor-corrector scheme to update the state variables,
C     U and W for each time step

Cyjs       SUBROUTINE PREDICORR(DT,UB,W,a,ta,prain,ps,ws,rh,qsi,
Cyjs     *                 qli,iradfl,rkn,
cyjs     *                 qnetob,cdh,cde,rid,param,sitev
c     Following variables are output
Cyjs     *                 QH,QE,E,
Cyjs     *                 MR,QM,Q,FM,TSURF,qnet)

       SUBROUTINE PREDICORR(DT,UB,W,a,ta,prain,ps,ws,rh,qsi,
     *               qli,iradfl,rkn,
     *               qnetob,cdh,cde,rid,param,sitev,iTsMethod,mtime,  !yjs Pass a modeling time
c     Following variables are output
     *               QH,QE,E,
     *               MR,QM,Q,FM,TSURF,tave,qnet,refDepth, totalRefDepth
     *,smelt,gsurf)
cdgt 5/4/04 surface melt smelt
cyjs  Constant data set
      data to /0.0/        !  Temperature of freezing (0 C)
      data tk /273.15/     !  Temperature to convert C to K (273.15)
      data sbc /2.0747e-7/ !  Stefan boltzman constant (2.0747e-7 KJ/m^2/hr/K)
      data hf /333.5/      !  Heat of fusion (333.5 KJ/kg)
      data hneu /2834.0/   !  Heat of Vaporization (Ice to Vapor, 2834 KJ/kg)
      data cw /4.18/       !  Water Heat Capacity (4.18 KJ/kg/C)
      data cs /2.09/       !  Ice heat capacity (2.09 KJ/kg/C)
      data cp /1.005/      !  Air Heat Capacity (1.005 KJ/kg/K)
      data ra /287.0/      !  Ideal Gas constant for dry air (287 J/kg/K)
      data k  /0.4/        !  Von Karmans constant (0.4)
      data hff /3600.0/    !  Factor to convert /s into /hr (3600)
      data rhoi /917.0/    !  Density of Ice (917 kg/m^3)
      data rhow /1000.0/   !  Density of Water (1000 kg/m^3)
      data g    /9.81/     !  Gravitational acceleration (9.81 m/s^2)
	data w1day /0.261799/!  Daily frequency (2pi/24 hr 0.261799 radians/hr)
	data pi /3.141592654/!  Pi

cyjs  End of constant declaration


      REAL MR,mr1,Lans,LanG
      real sitev(*)
      real param(*)
	real mtime(*)  !yjs add model time

      data wtol,utol / 0.025,2000./
      data ncall / 0/
      ncall=ncall+1

      CALL   QFM(ub,w,a,ta,prain,ps,ws,rh,qsi,qli,rkn,iradfl,qnetob,
     *           cdh,cde,rid,param,sitev,iTsMethod,mtime,  !yjs Pass a modeling time
     *           fm,q,qm,mr,qe,e,tsurf,tave,qh,qnet,dt,refDepth,
     *           totalRefDepth,smelt)     !yjs add average temperature
cdgt 5/4/04 surface melt smelt
C      PREDICTOR
       W1 = W + DT*FM
       IF(W1.LT.0.0) THEN
         w1=0.0
         CALL PREHELP(W1,W,DT,FM,0.,1.,PS,PRAIN,E,RHOW,HF,Q,QM,MR,qe,
     * hneu)
       ENDIF
       UB1 = UB + DT*Q

       Q1 = Q
       FM1 = FM
c   save values so that they can be averaged for output
       qh1=qh
       qe1=qe
       e1=e
       mr1=mr
	 smelt1=smelt  !cdgt 5/4/04 surface melt smelt

       qm1=qm
       tsurf1=tsurf
       qnet1=qnet
       CALL QFM(ub1,w1,a,ta,prain,ps,ws,rh,qsi,qli,rkn,iradfl,qnetob,
     *             cdh,cde,rid,param,sitev,iTsMethod,mtime, !yjs Pass a modeling time
     *             fm,q,qm,mr,qe,e,tsurf,tave, qh,qnet,dt,refDepth,
     *             totalRefDepth,smelt)  !yjs !yjs add average temperature
cdgt 5/4/04 surface melt smelt

C      CORRECTOR
       W2 = W + DT/2.0*(FM1 + FM)
       IF(W2.LT.0.0) THEN
         w2=0.0
         CALL PREHELP(W2,W,DT,FM,FM1,2.,PS,PRAIN,E,RHOW,HF,Q,QM,MR,qe,
     *   hneu)
       ENDIF
       UB2 = UB + DT/2.0*(Q1 + Q)
C   iterate to convergence to enhance stability
       niter=-3
       imax=1
 1     if((abs(w2-w1).gt.wtol .or. abs(ub2-ub1).gt.utol) .and.
     *  niter .lt. imax)then
C      write(6,10)ncall,"_ncall"
C 10   format(1x,i5,a6)
          w1=w2
          ub1=ub2
          CALL QFM(ub1,w1,a,ta,prain,ps,ws,rh,qsi,qli,rkn,iradfl,
     *           qnetob,cdh,cde,rid,param,sitev,iTsMethod,mtime,  !yjs Pass a modeling time
     *           fm,q,qm,mr,qe,e,tsurf,tave,qh,qnet,dt,refDepth,
     *           totalRefDepth,smelt)     !yjs add average temperature
cdgt 5/4/04 surface melt smelt
C    corrector again
         W2 = W + DT/2.0*(FM1 + FM)
         IF(W2.LT.0.0) THEN
           w2=0.0
           CALL PREHELP(W2,W,DT,FM,FM1,2.,PS,PRAIN,E,RHOW,HF,Q,QM,MR,
     *    qe,hneu)
         ENDIF
         UB2 = UB + DT/2.0*(Q1 + Q)
         niter=niter+1
         if(niter .ge. 1)then  ! had * steps to converge now hit it.
C  What follows is a fix to numerical instability that results from
c  nonlinearity when the snowpack is shallow and melting rapidly.  If
c  convergence does not occur when the snowpack is not melting (a very
c  rare thing) I just accept the predictor corrector solution.
c
c  Modified by DGT 7/27/05 to add complete meltout condition
c  The quantities that this changes are w2, ub2, mr and qm
c
c  The fix first checks whether the added energy is sufficient to melt the snow
c  completely.  If this is the case then the snow disappears.
c  In other cases we assume that the liquid fraction of the snow remains constant.
c  This to some extent bypasses the melt outflow estimates.
C  ae is added energy during the time step.
          ae=(q1+q+qm1+qm)*.5*dt
C   This fix is only physically sensible under melting conditions
c   and when ae is positive and there is snow
          if(ub .gt. 0. .and. ae .gt. 0. .and. w .gt. 0.)then
	        e2=(e+e1)*.5   !  This is the average sublimation
c   Check liquid fraction with added energy.  If this is greater than 1 then all snow melts
c   Otherwise implement a solution assuming that the liquid fraction remains constant
	        rlf=(ub+ae)/(rhow*w*hf)
			if(rlf .ge. 1.)then
				mr=w/dt+(ps+prain-e2)   ! Here snow disappears
				if(mr .lt. 0.)then
					mr=0.   !  Force this to not go negative
c      This can only occur if e2 is large compared to other terms.  Setting w2=0 implicitly reduces e2.
c      There is a resulting energy discrepancy due to a limitation on sublimation and latent heat flux
c      This is ignored because once the snow is gone energy computations are not pertinent.
                  endif
				qm=mr*rhow*hf
				w2=0.
				ub2=ub+ae-qm*dt
			else
c   Determine the w/ub ratio at the beginning of the time step.
c   Physically liquid fraction = ub/(rhow*w*hf) and since rhow and hf are constants
c   keeping r=w/ub constant is equivalent to keeping liquid fraction constant.
c   If liquid fraction is constant this is constant.
				r=w/ub
c   Solve for ub2 and w2 that satisfy the three equations
c            r=w2/ub2
c            ub2=ub+ae-rhow*hf*mr*dt     Energy balance the last term being the energy advected by melt
c            w2=w+(ps+prain-e2-mr)*dt    Mass balance
c   The unknowns in the above are ub2, w2 and m and the equations are linear
c   once the first eqn is multiplied by ub2
c   The solution is
				ub2=(rhow*hf*(w+(ps+prain-e2)*dt)-ae-ub)/(rhow*hf*r-1)
				w2=r*ub2
				if(w2 .lt. 0.)then  ! Avoid negative W
					w2=0.
				endif
				mr=(w-w2)/dt-e2+ps+prain
				if(mr .lt. 0.)then   ! Avoid negative mr
					mr=0.
					w2=w+(ps+prain-e2)/dt
					if(w2 .lt. 0)then
						w2=0.
c      This can only occur if e2 is large compared to other terms.  Setting w2=0 implicitly reduces e2.
c      There is a resulting energy discrepancy due to a limitation on sublimation and latent heat flux
c      This is ignored because once the snow is gone energy computations are not pertinent.
					endif
				endif
				qm=mr*rhow*hf
				ub2=ub+ae-qm*dt   ! redundant most of the time but recalc due to exceptions
			endif
c    Check that nothing went wrong
              if(mr .lt. 0.)then
	            write(6,*)'Error - negative melt rate in snow'
			endif
	        if(w2 .lt. 0.)then
	            write(6,*)'Error - negative w2 in snow'
			endif
			q=ae/dt-qm
c   Now set first pass values equal to final values to fake the averages below
			qm1=qm
			mr1=mr
			q1=q
          endif
         endif
         go to 1
       endif
       W = W2
       ub=ub2
C  average values from two time steps for output.  This is done for mr
C  and e to ensure mass balance and the others for better physical
C  comparisons
       qh=(qh+qh1)*.5
       qe=(qe+qe1)*.5
       e=(e+e1)*.5
       mr=(mr+mr1)*.5
       qm=(qm+qm1)*.5
       tsurf=(tsurf+tsurf1)*.5
       qnet=(qnet+qnet1)*.5
       q=(q+q1)*.5
	 smelt=(smelt+smelt1)*.5/(hf*rhow)  !cdgt 5/4/04 surface melt smelt
c  convert from energy KJ/m^2/hr to water depth m/hr of melt.
       RETURN
       END
************************ QFM () ********************************
C     Calculates Energy and Mass Flux at any instant

Cyjs       SUBROUTINE QFM(ub,w,a,ta,prain,ps,ws,rh,qsi,qli,rkn,iradfl,
Cyjs     *                qnetob,cdh,cde,rid,param,sitev,
Cyjs     *                fm,q,qm,mr,qe,e,tsurf,qh,qnet)
       SUBROUTINE QFM(ub,w,a,ta,prain,ps,ws,rh,qsi,qli,rkn,iradfl,
     *           qnetob,cdh,cde,rid,param,sitev,iTsMethod,mtime,  !yjs Pass a modeling time
     *           fm,q,qm,mr,qe,e,tsurf,tave,qh,qnet,dt,refDepth,
     *           totalRefDepth,smelt)     !yjs add average temperature
cdgt 5/4/04 surface melt smelt
      real sitev(*)
      real param(*)
	real mtime(*)   !yjs 09/19/2000 add the model time
      real mr,lc,ks,k,Lans,LanG

cyjs  Constant data set
      data to /0.0/        !  Temperature of freezing (0 C)
      data tk /273.15/     !  Temperature to convert C to K (273.15)
      data sbc /2.0747e-7/ !  Stefan boltzman constant (2.0747e-7 KJ/m^2/hr/K)
      data hf /333.5/      !  Heat of fusion (333.5 KJ/kg)
      data hneu /2834.0/   !  Heat of Vaporization (Ice to Vapor, 2834 KJ/kg)
      data cw /4.18/       !  Water Heat Capacity (4.18 KJ/kg/C)
      data cs /2.09/       !  Ice heat capacity (2.09 KJ/kg/C)
      data cp /1.005/      !  Air Heat Capacity (1.005 KJ/kg/K)
      data ra /287.0/      !  Ideal Gas constant for dry air (287 J/kg/K)
      data k  /0.4/        !  Von Karmans constant (0.4)
      data hff /3600.0/    !  Factor to convert /s into /hr (3600)
      data rhoi /917.0/    !  Density of Ice (917 kg/m^3)
      data rhow /1000.0/   !  Density of Water (1000 kg/m^3)
      data g    /9.81/     !  Gravitational acceleration (9.81 m/s^2)
	data w1day /0.261799/!  Daily frequency (2pi/24 hr 0.261799 radians/hr)
	data pi /3.141592654/!  Pi

	common /ts_save/ ts_old, tave_old, Ts_ave, Tave_ave

cyjs  End of constant declaration


c  Parameters
c      tr=param(1)     !  Temperature above which all is rain (3 C)
c      ts=param(2)     !  Temperature below which all is snow (-1 C)
      es=param(3)     !  emmissivity of snow (nominally 0.99)
      cg =param(4)    !  Ground heat capacity (nominally 2.09 KJ/kg/C)
      z=param(5)      !  Nominal meas. height for air temp. and humidity (2m)
      zo=param(6)     !  Surface aerodynamic roughness (m)
      rho=param(7)    !  Snow Density (Nominally 450 kg/m^3)
      rhog=param(8)   !  Soil Density (nominally 1700 kg/m^3)
      lc=param(9)     !  Liquid holding capacity of snow (0.05)
      ks=param(10)    !  Snow Saturated hydraulic conductivity (160 m/hr)
      de=param(11)    !  Thermally active depth of soil (0.4 m)
      abg=param(12)   !  Bare ground albedo  (0.25)
      avo=param(13)   !  Visual new snow albedo (0.95)
      anir0=param(14) ! NIR new snow albedo (0.65)
	lans= param(15) ! the thermal conductivity of fresh (dry) snow (0.0576 kJ/m/k/hr)
	lang= param(16) ! the thermal conductivity of soil (9.68 kJ/m/k/hr)
	wlf= param(17)  ! Low frequency fluctuation in deep snow/soil layer (1/4 w1 = 0.0654 radian/hr)
	rd1= param(18)  ! Amplitude correction coefficient of heat conduction (1)
      fstab=param(19) ! Stability correction control parameter
c                         0 = no corrections, 1 = full corrections
	Tref=param(20)  !  Reference temperature of soil layer in ground heat calculation input
	dNewS=param(21) !  The threshold depth of for new snow (0.001 m)
	gsurf=param(22) !  The fraction of surface melt that runs off (e.g. from a glacier)
cdgt gsurf added for modeling surface runoff from a glacier

c   Site variables
      fc=sitev(1)     !  Forest cover fraction (0-1)
	if (fc > 0)then
	   fc=fc
	endif
c      df=sitev(2)    !  Drift factor
      pr=sitev(3)     !  Atmospheric Pressure (Pa)
      qg=sitev(4)     !  Ground heat flux (KJ/m^2/hr)  This is more logically an
c      input variable, but is put here because it is never known at each
c      time step.  Usually it will be assigned a value 0.

      QSN = QSI*(1.0-A)
C     To ensure all temperatures in kelvin
      TAK = TA+TK
      TAVEK  = TAVE+TK
      DENSA  = PR/(RA*TAK)     ! Density of Air in kg/m3 if PR in Pa
      DENS = RHO
	rhom=lc*rho

Cysj calculate the ks=k/ze
	fKappaS = lans/(rho*cs)
	ds=sqrt(2*fKappaS/w1day)
	rs=Lans/(ds*rd1*rho*cs)


cyjs save the old values
	tave_old=tave

       EA=SVPW(TA)*RH   ! The saturation vapour pressure over water is used
C       because most instruments report relative humidity relative to water.
       QP=QPF(PRAIN,TA,TO,PS,RHOW,HF,CW,CS)
       tave=tavg(ub,w,rhow,cs,to,rhog,de,cg,hf)

Cyjs as described as below, the original UEB does not consider the refreezing. in this
c change, the model will consider the refreezing effect starting from the top to the bottom.
c Here is the predictor.

Cyjs the method is: refreezing can be modeled if the ub is greater than 0.0 without any precipiation,
c meanwhile the total refreezing depth in the snowpack is less than the Refreezing depth times the daily damping of wet snow.

	if(Ub .gt.0 .and. ps.le.0.0 .and. prain.le.0.0 .and.
     1  totalRefDepth .le. rd1*ds .and. w.gt.0.0) then
	  qc1=QcEst(TK,RKN,WS,TAK,Z,G,fstab,QP,DENSA,CP,HNEU,
     *      PR,EA,TK,DENS,CS,RS,TAVEK,qsn,qli,fc,es,sbc,qnetob,iradfl,
     *      mtime,param,iTsMethod,w,dt)
	  qc2=QcEst(TK-0.01,RKN,WS,TAK,Z,G,fstab,QP,DENSA,CP,HNEU,
     *      PR,EA,TK,DENS,CS,RS,TAVEK,qsn,qli,fc,es,sbc,qnetob,iradfl,
     *      mtime,param,iTsMethod,w,dt)


	  call Grad(qc1,qc2,0.0,-0.01, var_a, var_b)
          x1=refDep(lans,var_a, var_b, hf, rhom, dt, refDepth) !refreezing depth
	  refDepth=x1


	else
        refDepth=0.0
      endif


C       write(6,*)"Tave in qfm",tave
       TSURF = SRFTMP(QSI,A,QLI,QP,EA,TA,TAVE,TK,PR,RA,CP,RHO,
     *            RKN,HNEU,ES,SBC,CS,RS,W,QNETOB,IRADFL,WS,Z,G,FC,
     *            fstab,mtime,param,iTsMethod,dt,ub,refDepth,
     *            smelt)   !yjs Change to add model control
cdgt 5/4/04 surface melt smelt
C       write(6,*)tsurf
       qle=(1-FC)*es*sbc*(tsurf+tk)**4
       qlnet=qli-qle

       CALL TURBFLUX(PR,RA,TA,TK,tsurf,z,g,cp,rkn,WS,EA,RHOW,
     *                 HNEU,QH,QE,E,fstab) !yjs Add a fraction to reduce the evaporation after snow gone
       MR=FMELT(UB,RHOW,W,HF,LC,RID,KS,PRAIN)
C        MR in m/hr
       QM=MR*RHOW*(HF+(tave-to)*cw)        !  Include advection of
C         meltwater/rain that is at tave so that the model does not
C         crash when there is no snow and it rains.
C        QM in kj/m2/hr
c  dgt 5/4/04  Add surface melt that runs off to melt runoff and melt energy so that energy and mass balances
c   are still correct
       mr=mr+smelt/(hf*rhow)*gsurf
	 qm=qm+smelt*gsurf
       IF(IRADFL.EQ.0) THEN
          QNET = QSI*(1.0-A)+QLNET
       ELSE
          QNET = QNETOB
       ENDIF
       Q = QNET+QP+QG+QH+QE-QM
       FM=PRAIN+PS-MR-E
       RETURN
       END

**************************** TAVG () ***************************
C     Calculates the average temperature of snow and interacting
C     soil layer

      FUNCTION  TAVG(UB,W,RHOW,CS,TO,RHOG,DE,CG,HF)

      SNHC = RHOW*W*CS
      SHC  = RHOG*DE*CG
      CHC  = SNHC+SHC

C      SNHC = Snow heat capacity
C      SHC  = Soil heat capacity
C      CHC  = Combined heat capacity

      IF(UB.LE.0.) THEN
         TS=UB/CHC
      ELSE
         AL=UB/(RHOW*HF)
         IF(W.GT.AL) THEN
            TS=TO
         ELSE
            TS=(UB-W*RHOW*HF)/CHC
         ENDIF
      ENDIF
      TAVG=TS

      RETURN
      END



**************************** SRFTMP () ***************************
C     Computes the surface temperature of snow
      FUNCTION SRFTMP(QSI,A,QLI,QPIN,EA,TA,TAVE,TK,PR,RA,CP,RHO,RKN,
     *   HNEU,ES,SBC,CS,RS,W,qnetob,IRADFL,WS,Z,G,FC,fstab,mtime,param,
     *   iTsMethod,dt,ub,refDepth,smelt)	!yjs add three value to reflect model control changes
cdgt 5/4/04 surface melt smelt
c
	real param(*)
	real mtime(*)
	integer iTsMethod
c   This version written on 4/23/97 by Charlie Luce solves directly the
c   energy balance equation using Newtons method - avoiding the linearizations
c   used previously.  The derivative is evaluated numerically over the range
c   ts to fff*ts  where fff = 0.999
      data tol,nitermax/0.05,10/
	fff=(273-tol)/273.  ! Scale the difference used for derivatives by the tolerance
      QSN = QSI*(1.0-A)
C     To ensure all temperatures in kelvin
      TAK = TA+TK
      TAVEK  = TAVE+TK
      DENSA  = PR/(RA*TAK)     ! Density of Air in kg/m3 if PR in Pa
      DENS = RHO
c
      qp=qpin            ! store input variable locally without changing global value
c    debugging
c      if(mtime(1) .eq. 1948 .and. mtime(2) .eq. 11
c     + .and. mtime(3) .eq. 18 .and. mtime(4) .gt. 12 .and.
c     + mtime(4) .lt. 13 .and. mtime(5) .eq. 39)then
c		mtime(1)=mtime(1)
c	endif


      if(w.le.0. and. qp.gt.0.)qp=0.
c
c      ignore the effect of precip advected
C      energy on the calculation of surface temperature when there is no snow.
C      Without this ridiculously high temperatures can result as the model
C      tries to balance outgoing radiation with precip advected energy.
c
       Ts = TaK                             ! first approximation
       ER=tol*2    ! so that it does not end on first loop
       niter = 0
 1     if(ER.gt.tol.and.niter.lt. nitermax)then
           Tslast = Ts
           F1 = surfeb(ts,RKN,WS,TAK,Z,G,fstab,QP,DENSA,CP,HNEU,
     *      PR,EA,TK,DENS,CS,RS,TAVEK,qsn,qli,fc,es,sbc,qnetob,iradfl,  !25
     *	  mtime,param,iTsMethod,w,dt,ub,refDepth)	!yjs add three value to reflect model control changes))
           f2 = surfeb(fff*ts,RKN,WS,TAK,Z,G,fstab,QP,DENSA,CP,HNEU,
     *      PR,EA,TK,DENS,CS,RS,TAVEK,qsn,qli,fc,es,sbc,qnetob,iradfl,
     *	  mtime,param,iTsMethod,w,dt,ub,refDepth)	!yjs add three value to reflect model control changes))
           Ts = Ts - ((1.-fff) * Ts * F1) / (F1 - F2)
	     if(ts .lt. tak - 50)go to 11  !  If it looks like it is getting unstable go straight to bisection
	     ER = abs(Ts - Tslast)
           niter=niter+1
	     go to 1   ! loop back and iterate
	 endif

	if(er.le.tol) goto 10  ! The solution has converged

c   If still not converged use bisection method
 11    Tlb = TaK - 20.        ! First guess at a reasonable range
	 Tub = Tak + 10.
	 Flb = surfeb(tlb,RKN,WS,TAK,Z,G,fstab,QP,DENSA,CP,HNEU,
     *      PR,EA,TK,DENS,CS,RS,TAVEK,qsn,qli,fc,es,sbc,qnetob,iradfl,
     *	  mtime,param,iTsMethod,w,dt,ub,refDepth)
	 Fub = surfeb(tub,RKN,WS,TAK,Z,G,fstab,QP,DENSA,CP,HNEU,
     *      PR,EA,TK,DENS,CS,RS,TAVEK,qsn,qli,fc,es,sbc,qnetob,iradfl,
     *	  mtime,param,iTsMethod,w,dt,ub,refDepth)
	 ibtowrite=0
       if(Flb*fub .gt. 0.)then   ! these are of the same sign so the range needs to be enlarged
		Tlb= TaK - 150.    ! an almost ridiculously large range - solution should be in this if it exists
		Tub= Tak + 100.
		Flb = surfeb(tlb,RKN,WS,TAK,Z,G,fstab,QP,DENSA,CP,HNEU,
     *      PR,EA,TK,DENS,CS,RS,TAVEK,qsn,qli,fc,es,sbc,qnetob,iradfl,
     *	  mtime,param,iTsMethod,w,dt,ub,refDepth)
		Fub = surfeb(tub,RKN,WS,TAK,Z,G,fstab,QP,DENSA,CP,HNEU,
     *      PR,EA,TK,DENS,CS,RS,TAVEK,qsn,qli,fc,es,sbc,qnetob,iradfl,
     *	  mtime,param,iTsMethod,w,dt,ub,refDepth)
	    ibtowrite=1
          if(Flb*fub .gt. 0.)then   ! these are of the same sign so no bisection solution
			write(6,*)
     + 'Bisection surface temperature solution failed with large range'
			write(6,*)'Date: ',mtime(1),mtime(2),mtime(3)
			write(6,*)'Time: ',mtime(4)
			write(6,*)'Model element: ',mtime(5)
			write(6,*)
     + 'A surface temperature of 273 K assumed'
	        ts=tk
			go to 10
	    else
			write(6,*)
     +    'Bisection surface temperature solution with large range'
			write(6,*)'Date: ',mtime(1),mtime(2),mtime(3)
			write(6,*)'Time: ',mtime(4)
			write(6,*)'Model element: ',mtime(5)
			write(6,*)
     +	'This is not a critical problem unless it happens frequently'
	         write(6,*)
     +     'and solution below appears incorrect'
	     endif
        else
c		write(6,*)
c     +    'Bisection surface temperature solution'
c		write(6,*)'Date: ',mtime(1),mtime(2),mtime(3)
c		write(6,*)'Time: ',mtime(4)
c		write(6,*)'Model element: ',mtime(5)
c	    write(6,*)'This is not a critical problem'
        endif
c     Here do the bisection
       niter = log((Tub-Tlb)/tol)/log(2.)   ! Number of iterations needed for temperature tolerance
	 do iter=1,niter
           ts = 0.5*(tub+tlb)
           F1 = surfeb(ts,RKN,WS,TAK,Z,G,fstab,QP,DENSA,CP,HNEU,
     *      PR,EA,TK,DENS,CS,RS,TAVEK,qsn,qli,fc,es,sbc,qnetob,iradfl,  !25
     *	  mtime,param,iTsMethod,w,dt,ub,refDepth)	!yjs add three value to reflect model control changes))
		 if(f1.gt.0.0) then  ! This relies on the assumption (fact) that this is a monotonically decreasing function
			tlb=ts
	     else
			tub=ts
	     endif
       enddo
	if(ibtowrite .eq. 1)then
		write(6,*)'Surface temperature: ',ts,' K'
		write(6,*)'Energy closure: ',f1
		write(6,*)'Iterations: ',niter
	endif

 10    Ts = Ts - Tk
       IF(W.GT.0..AND.TS.GT.0.) THEN
cdgt 5/4/04 surface melt smelt
          SRFTMP = 0.0
	    smelt=surfeb(srftmp+Tk,RKN,WS,TAK,Z,G,fstab,QP,DENSA,CP,HNEU,
     *      PR,EA,TK,DENS,CS,RS,TAVEK,qsn,qli,fc,es,sbc,qnetob,iradfl,  !25
     *	  mtime,param,iTsMethod,w,dt,ub,refDepth)
cdgt 5/4/04 surface melt smelt is the energy not accommodated by conduction into the snow
c  so it results in surface melt which then infiltrates and adds energy to the snowpack
c  through refreezing
c  This change added to capture this quantity when the model is used for a glacier where
c  surface melt may run off rather than infiltrate.
c  For modeling of glaciers in Antarctica by Mike Gooseff
       ELSE
          SRFTMP = TS
	    smelt=0.0
c  No surface melt this case
       ENDIF
       RETURN
       END
c*********************************************************************
       function surfeb(ts,RKN,WS,TAK,Z,G,fstab,QP,DENSA,CP,HNEU,
     *      PR,EA,TK,DENS,CS,RS,TAVEK,qsn,qli,fc,es,sbc,qnetob,iradfl,
     *      mtime,param,iTsMethod,w,dt,ub,refDepth)	!yjs add three value to reflect model control changes)
c      function to evaluate the surface energy balance for use in solving for
c      surface temperature
c      DGT and C Luce 4/23/97

	real param(*)
	real mtime(*)
	integer iTsMethod
	real LanS, LanG, LanE,LanE_Ze, LanE_de, LanE_ze2,LanE_de2

	common /ts_save/ ts_old, tave_old, Ts_ave, Tave_ave

cyjs  Constant data set
      data to /0.0/        !  Temperature of freezing (0 C)
c      data tk /273.15/     !  Temperature to convert C to K (273.15)
c      data sbc /2.0747e-7/ !  Stefan boltzman constant (2.0747e-7 KJ/m^2/hr/K)
      data hf /333.5/      !  Heat of fusion (333.5 KJ/kg)
c      data hneu /2834.0/   !  Heat of Vaporization (Ice to Vapor, 2834 KJ/kg)
      data cw /4.18/       !  Water Heat Capacity (4.18 KJ/kg/C)
c      data cs /2.09/       !  Ice heat capacity (2.09 KJ/kg/C)
c      data cp /1.005/      !  Air Heat Capacity (1.005 KJ/kg/K)
      data ra /287.0/      !  Ideal Gas constant for dry air (287 J/kg/K)
      data k  /0.4/        !  Von Karmans constant (0.4)
      data hff /3600.0/    !  Factor to convert /s into /hr (3600)
      data rhoi /917.0/    !  Density of Ice (917 kg/m^3)
      data rhow /1000.0/   !  Density of Water (1000 kg/m^3)
c      data g    /9.81/     !  Gravitational acceleration (9.81 m/s^2)
	data w1day /0.261799/!  Daily frequency (2pi/24 hr 0.261799 radians/hr)
	data pi /3.141592654/!  Pi

cyjs  End of constant declaration

c  Parameters
c      tr=param(1)     !  Temperature above which all is rain (3 C)
c      ts=param(2)     !  Temperature below which all is snow (-1 C)
      es=param(3)     !  emmissivity of snow (nominally 0.99)
      cg =param(4)    !  Ground heat capacity (nominally 2.09 KJ/kg/C)
      z=param(5)      !  Nominal meas. height for air temp. and humidity (2m)
      zo=param(6)     !  Surface aerodynamic roughness (m)
      rho=param(7)    !  Snow Density (Nominally 450 kg/m^3)
      rhog=param(8)   !  Soil Density (nominally 1700 kg/m^3)
      lc=param(9)     !  Liquid holding capacity of snow (0.05)
      ks=param(10)    !  Snow Saturated hydraulic conductivity (160 m/hr)
      de=param(11)    !  Thermally active depth of soil (0.4 m)
      abg=param(12)   !  Bare ground albedo  (0.25)
      avo=param(13)   !  Visual new snow albedo (0.95)
      anir0=param(14) ! NIR new snow albedo (0.65)
	lans= param(15) ! the thermal conductivity of fresh (dry) snow (0.0576 kJ/m/k/hr)
	lang= param(16) ! the thermal conductivity of soil (9.68 kJ/m/k/hr)
	wlf= param(17)  ! Low frequency fluctuation in deep snow/soil layer (1/4 w1 = 0.0654 radian/hr)
	rd1= param(18)  ! Apmlitude correction coefficient of heat conduction (1)
      fstab=param(19) ! Stability correction control parameter
c                         0 = no corrections, 1 = full corrections
	Tref=param(20)  !  Reference temperature of soil layer in ground heat calculation input


	zs=w*rhow/rho

        RKIN=RKINST(RKN,WS,TAK,TS,Z,G,fstab)

cyjs  07/25/02   at Boise.  To make the UEB2 work under Unix/linux system the fancy stuff like "Select case" shall not be used
	    !select case(iTsMethod)			!Four choice of the surface temperature modeling
	    !case (1)						!1. Old snow, use calibrated snow surface heat conductance
          if(iTsMethod .eq. 1) then
			qcs = DENS*CS*RS*(Ts-TAVEK) !2. Revised scheme LanE/Ze of the snow surface
										!3. Force restore approach
		!case (2)						!4. Modified force restore approach.
          elseif (iTsMethod .eq. 2) then
			fKappaS = LanS/(rho*cs)

			fKappaG = LanG/(rhog*cg)

			d1 = sqrt(2*fKappaS/w1day)
			if(zs .ge. rd1*d1) then

				LanE_Ze=LanS/(rd1*d1)
	        else

				LanE_Ze=LanE(LanS, LanG, Zs, rho, rhog, cs, cg, rd1,
     *				ze, w1day) !Cyjs   call the subroutine to update the heat conductivity. LanE()
				LanE_Ze=LanE_Ze/ze
			end if

			qcs= LanE_Ze*(Ts-TAVEK)
		elseif (iTsMethod .eq. 3) then !case (3)
			fKappaS = LanS/(rho*cs)
			fKappaG = LanG/(rhog*cg)

			d1 = sqrt(2*fKappaS/w1day)

			if(zs .ge. rd1*d1) then
				LanE_Ze=LanS/(rd1*d1)
				Ze=rd1*d1
	        else

				LanE_Ze=LanE(LanS, LanG, Zs, rho, rhog, cs, cg, rd1,
     *				ze, w1day) !Cyjs   call the subroutine to update the heat conductivity. LanE()
				LanE_Ze=LanE_Ze/ze
			end if

			de=ze/rd1
			LanE_de=LanE_ze/de*ze
			qcs= LanE_de*(Ts-Ts_old)/(w1day*dt)+LanE_Ze*(Ts-TAVEK)


		else !case (4)   !change to all default cases. If not for model comparison
			fKappaS = LanS/(rho*cs)
			fKappaG = LanG/(rhog*cg)

			d1 = sqrt(2*fKappaS/w1day)
			dlf = sqrt(2*fKappaG/wlf)

			if(zs .ge. rd1*d1) then
				LanE_Ze=LanS/(rd1*d1)
				Ze=rd1*d1
			else
				LanE_Ze=LanE(LanS, LanG, Zs, rho, rhog, cs, cg, rd1,
     *				ze, w1day) !Cyjs   call the subroutine to update the heat conductivity. LanE()
				LanE_Ze=LanE_Ze/ze
			end if

			if(zs .ge. rd1*dlf) then
				LanE_Ze2=LanS/(rd1*dlf)
				Ze2=rd1*dlf
			else
				LanE_Ze2=LanE(LanS, LanG, Zs, rho, rhog, cs, cg, rd1,
     *				ze2, wlf) !Cyjs   call the subroutine to update the heat conductivity. LanE()
				LanE_Ze2=LanE_Ze2/ze2
			end if

			de=ze/rd1
			LanE_de=LanE_ze/de*ze
			de2=ze2/rd1
			LanE_de2=LanE_ze2/de2*ze2

			if(ub.le.0.0 .or. refDepth.le.0.0)then
			  qcs= LanE_de*(Ts-Ts_old)/(w1day*dt)+LanE_Ze*(Ts-Ts_Ave)+
     *				LanE_de2*(Ts_ave-Tave_Ave)
	        elseif(refDepth .gt. rd1*d1) then
			  qcs= LanE_de*(Ts-Ts_old)/(w1day*dt)+LanE_Ze*(Ts-Ts_Ave)+
     *				LanE_de2*(Ts_ave-Tave_Ave)
              else
                 qcs=lanE_ze*ze*(ts-tk)/refDepth
	        endif


		!End select
            endif


       	surfeb = QP + RKIN*DENSA*CP*(TAK - Ts)
     *          +(HNEU*DENSA*0.622*RKIN)/PR*(EA-SVP(Ts-Tk))
     *          -qcs                !yjs This is a change. The original has only one selection of case 1.
	    If(Iradfl.eq.0)then
			surfeb = surfeb + QSN+QLI-(1.-FC)*ES*SBC*Ts**4
      	ELSE

     			surfeb = surfeb + qnetob
      	ENDIF
        return
	  end
c*********************************************************************
       function QcEst(ts,RKN,WS,TAK,Z,G,fstab,QP,DENSA,CP,HNEU,
     *      PR,EA,TK,DENS,CS,RS,TAVEK,qsn,qli,fc,es,sbc,qnetob,iradfl,
     *      mtime,param,iTsMethod,w,dt)	!yjs add three value to reflect model control changes)
c      function to estimate surface heat conduction for use in solving for
c      surface temperature
c      DGT and C Luce 4/23/97

	real param(*)
	real mtime(*)
	integer iTsMethod
	real LanS, LanG, LanE,LanE_Ze, LanE_de, LanE_ze2,LanE_de2

cyjs  Constant data set
      data to /0.0/        !  Temperature of freezing (0 C)
c      data tk /273.15/     !  Temperature to convert C to K (273.15)
c      data sbc /2.0747e-7/ !  Stefan boltzman constant (2.0747e-7 KJ/m^2/hr/K)
      data hf /333.5/      !  Heat of fusion (333.5 KJ/kg)
c      data hneu /2834.0/   !  Heat of Vaporization (Ice to Vapor, 2834 KJ/kg)
      data cw /4.18/       !  Water Heat Capacity (4.18 KJ/kg/C)
c      data cs /2.09/       !  Ice heat capacity (2.09 KJ/kg/C)
c      data cp /1.005/      !  Air Heat Capacity (1.005 KJ/kg/K)
      data ra /287.0/      !  Ideal Gas constant for dry air (287 J/kg/K)
      data k  /0.4/        !  Von Karmans constant (0.4)
      data hff /3600.0/    !  Factor to convert /s into /hr (3600)
      data rhoi /917.0/    !  Density of Ice (917 kg/m^3)
      data rhow /1000.0/   !  Density of Water (1000 kg/m^3)
c      data g    /9.81/     !  Gravitational acceleration (9.81 m/s^2)
	data w1day /0.261799/!  Daily frequency (2pi/24 hr 0.261799 radians/hr)
	data pi /3.141592654/!  Pi

cyjs  End of constant declaration

c  Parameters
c      tr=param(1)     !  Temperature above which all is rain (3 C)
c      ts=param(2)     !  Temperature below which all is snow (-1 C)
      es=param(3)     !  emmissivity of snow (nominally 0.99)
      cg =param(4)    !  Ground heat capacity (nominally 2.09 KJ/kg/C)
      z=param(5)      !  Nominal meas. height for air temp. and humidity (2m)
      zo=param(6)     !  Surface aerodynamic roughness (m)
      rho=param(7)    !  Snow Density (Nominally 450 kg/m^3)
      rhog=param(8)   !  Soil Density (nominally 1700 kg/m^3)
      lc=param(9)     !  Liquid holding capacity of snow (0.05)
      ks=param(10)    !  Snow Saturated hydraulic conductivity (160 m/hr)
      de=param(11)    !  Thermally active depth of soil (0.4 m)
      abg=param(12)   !  Bare ground albedo  (0.25)
      avo=param(13)   !  Visual new snow albedo (0.95)
      anir0=param(14) ! NIR new snow albedo (0.65)
	lans= param(15) ! the thermal conductivity of fresh (dry) snow (0.0576 kJ/m/k/hr)
	lang= param(16) ! the thermal conductivity of soil (9.68 kJ/m/k/hr)
	wlf= param(17)  ! Low frequency fluctuation in deep snow/soil layer (1/4 w1 = 0.0654 radian/hr)
	rd1= param(18)  ! Apmlitude correction coefficient of heat conduction (1)
      fstab=param(19) ! Stability correction control parameter
c                         0 = no corrections, 1 = full corrections
	Tref=param(20)  !  Reference temperature of soil layer in ground heat calculation input

	zs=w*rhow/rho

        RKIN=RKINST(RKN,WS,TAK,TS,Z,G,fstab)

       	qcEst = QP + RKIN*DENSA*CP*(TAK - Ts)
     *          +(HNEU*DENSA*0.622*RKIN)/PR*(EA-SVP(Ts-Tk))

	    If(Iradfl.eq.0)then
			qcEst = qcEst + QSN+QLI-(1.-FC)*ES*SBC*Ts**4
      	ELSE

     			qcEst = qcEst + qnetob
      	ENDIF
        return
	  end
**************************** SRFTMPO () ***************************
C     Computes the surface temperature of snow
      FUNCTION SRFTMPO(QSI,A,QLI,QPIN,EA,TA,TAVE,TK,PR,RA,CP,RHO,RKN,
     *   HNEU,ES,SBC,CS,RS,W,qnetob,IRADFL,WS,Z,G,FC,fstab)
      dimension tint(0:10,2)
      DATA ncall,tol/0,0.05/
      ncall = ncall+1
      NITER = 10
      TSTAR = TA
      qp=qpin    ! store input variable locally without changing global value
      if(w.le.0. and. qp.gt.0.)qp=0.   ! ignore the effect of precip advected
C      energy on the calculation of surface temperature when there is no snow.
C      Without this ridiculously high temperatures can result as the model
C      tries to balance outgoing radiation with precip advected energy.
      TSURF = SNOTMP(TSTAR,QSI,A,QLI,QP,EA,TA,TAVE,TK,PR,RA,CP,RHO,
     *        RKN,HNEU,ES,SBC,CS,RS,qnetob,IRADFL,WS,Z,G,FC,fstab,mtime) !YJS pass modeling time
C      The first calculation gets the solution on the right side of ta
C      since it uses neutral stability and the linearization (equivalent
C      to Newton-Rhapson) will move in the direction of a solution in the case
C      of a well behaved function.  See Notebook 8/3/94 for further elaboration.
C      Use this to place bounds on surface temperature for iteration.
      if(tsurf.gt.ta)then
        tlb=ta
        tub=ta+30.   !  Max upper bound 30 C warmer than surface
        if(tsurf.gt.tub)tsurf=(tlb+tub)*.5  ! sometimes tsurf is outside these bounds
      else
        tlb=ta-30.
        tub=ta
        if(tsurf.lt.tlb)tsurf=(tlb+tub)*.5
      endif
      tint(0,1)=tstar
      tint(0,2)=tsurf
C   Now iterate
      tstar=tsurf
      DO 10 I=1,NITER
      TSURF = SNOTMP(TSTAR,QSI,A,QLI,QP,EA,TA,TAVE,TK,PR,RA,CP,RHO,
     *        RKN,HNEU,ES,SBC,CS,RS,qnetob,IRADFL,WS,Z,G,FC,fstab,mtime) !YJS pass modeling time

C      write(6,*)ta,tstar,tsurf
      tint(i,1)=tstar
      tint(i,2)=tsurf
      if(tlb.le.tsurf .and. tsurf .le. tub)then
         if(tsurf.gt.tstar)then
           tlb=tstar   ! increasing so can increase lower bound.
         else
           tub=tstar
         endif
       else if(tsurf .gt. tub)then   ! upper bound overshot
         tlb=tstar                   ! increase lower bound and
         tsurf=(tstar+tub)*.5        ! guess at solution halfway
       else    ! tsurf .lt. tlb  here, i.e. lower bound overshot
         tub=tstar
         tsurf=(tstar+tlb)*.5
       endif
C    Check for convergence
       IF(ABS(TSURF-TSTAR).LT. tol) THEN
          GO TO 20
       ELSE
          TSTAR = TSURF
       ENDIF
 10    CONTINUE
C       write(6,*) 'slipped through the loop in SRFTMP()',ncall
C       do 15 i = 0,niter
C 15      write(6,*)tint(i,1),tint(i,2)
C       write(6,*)"tsurf",tsurf
C    Newton rhapson not converging so use bisection
       f1=sntmpb(tlb,QSI,A,QLI,QP,EA,TA,TAVE,TK,PR,RA,CP,RHO,
     *         RKN,HNEU,ES,SBC,CS,RS,qnetob,IRADFL,WS,Z,G,FC,fstab)
       f2=sntmpb(tub,QSI,A,QLI,QP,EA,TA,TAVE,TK,PR,RA,CP,RHO,
     *         RKN,HNEU,ES,SBC,CS,RS,qnetob,IRADFL,WS,Z,G,FC,fstab)
       tsurf=(tlb+tub)*.5
       if(f1*f2 .gt.0.)then
         write(6,*)'SRFTMP has failed to find a solution',ncall
     *     ,tlb,tub,tsurf
       else
         nib=(alog(tub-tlb)-alog(tol))/alog(2.)
         do 16 i=1,nib
C         write(6,*)nib,tlb,tub,tsurf
         f=sntmpb(tsurf,QSI,A,QLI,QP,EA,TA,TAVE,TK,PR,RA,CP,RHO,
     *         RKN,HNEU,ES,SBC,CS,RS,qnetob,IRADFL,WS,Z,G,FC,fstab)
         if(f*f1 .gt.0.)then
           tlb=tsurf
           f1=f
         else
           tub=tsurf
C          f2=f
         endif
         tsurf=(tlb+tub)*.5
 16      continue
C         write(6,*)"Bisection",nib,tsurf
       endif
 20     IF(W.GT.0..AND.TSURF.GT.0.) THEN
          SRFTMPO = 0.0
       ELSE
          SRFTMPO = TSURF
       ENDIF
C       write(6,*)"Final",srftmp
       RETURN
       END

**************************** SNOTMP () ******************************
C     Function to compute surface temperature using Penman/surface
C     resistance analogy and equilibrium approach

Cyjs      FUNCTION SNOTMP(TSTAR,QSI,A,QLI,QP,EA,TA,TAVE,TK,PR,RA,CP,RHO,
Cyjs     *        RKN,HNEU,ES,SBC,CS,RS,qnetob,IRADFL,WS,Z,G,FC,fstab)
      FUNCTION SNOTMP(TSTAR,QSI,A,QLI,QP,EA,TA,TAVE,TK,PR,RA,CP,RHO,
     *        RKN,HNEU,ES,SBC,CS,RS,qnetob,IRADFL,WS,Z,G,FC,fstab,mtime) !add a modeltime

	real LanmdaE, LanS, LanG, LanE
      QSN = QSI*(1.0-A)
C     To ensure all temperatures in kelvin
      TAK = TA+TK
      TSTARK = TSTAR+TK
      TAVEK  = TAVE+TK
      DENSA  = PR/(RA*TAK)
C     DENSA in kg/m3 if PR in Pa
C      CP1 = CP/1000.0
C     CP1 in kj/kg/oK


      DENS = RHO
      RKIN=RKINST(RKN,WS,TAK,TSTARK,Z,G,fstab)

	LanmdaE=LanE(LanS, LanG, Zs, DENS, rhog, cs, cg, r, ze, w1day)

Cyjs      UPPER = QP+(DENSA*CP*TAK)*RKIN
Cyjs     *        -(HNEU*DENSA*0.622*RKIN)/PR*(SVPI(TSTAR)
Cyjs     *        -EA-DELTA(TSTAR)*TSTARK)
Cyjs     *        +DENS*CS*RS*TAVEK
Cyjs      DEN = DENS*CS*RS+(DENSA*CP)*RKIN+(DELTA(TSTAR)*
Cyjs     *       HNEU*DENSA*.622*RKIN)/PR

      UPPER = QP+(DENSA*CP*TAK)*RKIN
     *        -(HNEU*DENSA*0.622*RKIN)/PR*(SVPI(TSTAR)
     *        -EA-DELTA(TSTAR)*TSTARK)
     *        +LanmdaE*TAVEK                     !change to combined layer (snow and soil)
      DEN = LanmdaE+(DENSA*CP)*RKIN+(DELTA(TSTAR)*
     *       HNEU*DENSA*.622*RKIN)/PR
      IF(IRADFL.EQ.0) THEN
         UPPER = UPPER + QSN+QLI+3.0*(1.-FC)*ES*SBC*TSTARK**4
         DEN = DEN+4.0*(1.-FC)*ES*SBC*TSTARK**3
      ELSE
         UPPER = UPPER + qnetob
      ENDIF

      SNOTMP = UPPER/DEN-TK

      RETURN
      END
C*************************** sntmpb () ******************************
C     Function to compute surface temperature using bisection

      FUNCTION sntmpb(TSTAR,QSI,A,QLI,QP,EA,TA,TAVE,TK,PR,RA,CP,RHO,
     *         RKN,HNEU,ES,SBC,CS,RS,qnetob,IRADFL,WS,Z,G,FC,fstab)

      QSN = QSI*(1.0-A)
C     To ensure all temperatures in kelvin
      TAK = TA+TK
      TSTARK = TSTAR+TK
      TAVEK  = TAVE+TK
      DENSA  = PR/(RA*TAK)
C     DENSA in kg/m3 if PR in Pa
C      CP1 = CP/1000.0
C     CP1 in kj/kg/oK
      DENS = RHO
      RKIN=RKINST(RKN,WS,TAK,TSTARK,Z,G,fstab)

      UPPER = QP+(DENSA*CP*TAK)*RKIN
     *        -(HNEU*DENSA*0.622*RKIN)/PR*(SVPI(TSTAR)
     *        -EA)
     *        +DENS*CS*RS*TAVEK
      DEN = DENS*CS*RS+(DENSA*CP)*RKIN
      IF(IRADFL.EQ.0) THEN
         UPPER = UPPER + QSN+QLI-(1.-FC)*ES*SBC*TSTARK**4
      ELSE
         UPPER = UPPER + qnetob
      ENDIF
      sntmpb=upper-den*tstark
      RETURN
      END

C******************************* RKINST() **********************
      function rkinst(rkn,ws,ta,ts,z,g,fstab)
c     function to calculate no neutral turbulent transfer coefficient using the
c     richardson number correction.
      if(ws.le.0.)then
        rkinst=0.    !  No wind so no sensible or latent heat fluxes.
      else
        rich=g*(ta-ts)*z/(ws*ws*ta)    ! ta must be in K
        if(rich.ge.0.)then
        rkinst=rkn/(1.+10.*rich)
		! Cyjs PhiMH=1+10*rich    Change back
        else
cyjs          rkinst=rkn*(1-10.*rich)
		!PhiMH=(1-16*rich)**(-.75)
		rkinst=rkn*amin1(3.,(1-16*rich)**(.75))
        endif
      endif
C  Linear damping of stability correction through parameter fstab

      rkinst=rkn+fstab*(rkinst-rkn)
		!if(PhiMH .ne. 0.0) rkinst=rkn/PhiMH !!!!Change back to the original design purpose
      return
      end


******************************* DELTA () ****************************
C     Function to compute gradient of saturated vapour pressure,
C     temperature function over ice
C     Uses Lowe (1977) polynomial
      FUNCTION DELTA(T)
      dimension a(7)
      data a/0.5030305237,0.0377325502,0.001267995369,
     &   2.477563108e-5,3.005693132e-7,2.158542548e-9,
     &   7.131097725e-12/
      delta=a(1)+t*(a(2)+t*(a(3)+t*(a(4)+t*(a(5)+t*(a(6)+t*a(7))))))
      delta=delta*100.    ! convert from mb to Pa
      RETURN
      END

************************** PARTSNOW () ************************
C     Partitioning of precipitation into rain and snow

      FUNCTION PARTSNOW(P,TA,TR,TS)

      IF(TA.LT.TS) THEN
        PARTSNOW=P
      ELSEIF(TA.GT.TR) THEN
        PARTSNOW=0.0
      ELSE
        PARTSNOW=P*(TR-TA)/(TR-TS)
      ENDIF

      RETURN
      END

*************************** QPF () ***************************
C     Calculates the heat advected to the snowpack due to rain

      FUNCTION QPF(PR,TA,TO,PS,RHOW,HF,CW,CS)
      IF(TA.GT.TO) THEN
         TRAIN=TA
         TSNOW=TO
      ELSE
         TRAIN=TO
         TSNOW=TA
      endif
      QPF=PR*RHOW*(HF+CW*(TRAIN-TO))+PS*RHOW*CS*(TSNOW-TO)
      RETURN
      END

************************ TURBFLUX () ***************************
C     Calculates the turbulent heat fluxes (sensible and latent
C     heat fluxes) and condensation/sublimation.

      SUBROUTINE TURBFLUX(PR,RA,TA,TK,TS,z,g,cp,rkn,WS,EA,
     *                    RHOW,HNEU,QH,QE,E,fstab)


       REAL RHOA,QH,ES,EA,QE,E

      tak=ta+tk
      tsk=ts+tk
      rkin=rkinst(rkn,ws,tak,tsk,z,g,fstab)
      RHOA=PR/(RA*(TAK))
C     RHOA in kg/m3
      QH=RHOA*(TA-TS)*cp*rkin
      ES=SVPI(TS)
	QE=0.622*hneu/(RA*(TAK))*rkin*(EA-ES)
      E=-QE/(RHOW*HNEU)
C     E in  m/hr

      RETURN
      END

*********************** FMELT () *************************
C     Calculates the melt rate and melt outflow

      FUNCTION FMELT(UB,RHOW,W,HF,LC,RID,KS,PRAIN)
      REAL LC,KS

c       write(*,*) 'I am in FMELT!!!!'


      UU=0.0
      IF(UB.LT.0.) THEN
         FMELT=0.0
      ELSEIF(W.LE.0.0) THEN
         FMELT=PRAIN
         IF(PRAIN.LE.0.) THEN
            FMELT=0.0
         ENDIF
      ELSE
         UU=UB/(RHOW*W*HF)
c                              liquid fraction
         IF(UU.GT.0.99) THEN
            UU=0.99
C                              TO ENSURE HIGH MELT RATE WHEN ALL LIQUID
      ENDIF
      IF((UU/(1-UU)).LE.LC) THEN
         SS=0.0
      ELSE
         SS=(UU/((1- UU)*RID)-LC/RID)/(1-LC/RID)
      ENDIF
      FMELT=KS*SS**3
      ENDIF
      IF(FMELT.LT.0.0) THEN
          WRITE(*,*)'FMELT is NEGATIVE!'
          STOP
       ENDIF

c       write(*,*) ' I am leaving FMELT!!!'

       RETURN
       END

*************************** SVP () *****************************
C     Calculates the vapour pressure at a specified temperature over water or ice
c     depending upon temperature.  Temperature is celsius here.
      FUNCTION SVP(T)
      if(t .ge. 0.)then
        svp=svpw(t)
      else
        svp=svpi(t)
      endif
      return
      end
*************************** SVPI () *****************************
C     Calculates the vapour pressure at a specified temperature over ice.
C     using polynomial from Lowe (1977).
      FUNCTION SVPI(T)
      SVPI=6.109177956 + t * (0.503469897 + t * (0.01886013408 +
     & t * (0.0004176223716 + t * (5.82472028e-06 + t *
     &   (4.838803174e-08 + t * 1.838826904e-10)))))
      SVPI=SVPI*100   ! convert from mb to Pa
      RETURN
      END

*************************** SVPW () *****************************
C     Calculates the vapour pressure at a specified temperature over water
C     using polynomial from Lowe (1977).
      FUNCTION SVPW(T)
      SVPW=6.107799961 + t * (0.4436518521 + t * (0.01428945805 +
     & t * (0.0002650648471 + t * (3.031240936e-06 + t *
     & (2.034080948e-08 + t * 6.136820929e-11)))))
      SVPW=SVPW*100   ! convert from mb to Pa
      RETURN
      END

C**************************** PREHELP () ***************************
C      Routine to correct energy and mass fluxes when
c      numerical overshoots dictate that W was changed in
c      the calling routine - either because W went negative
c      or due to the liquid fraction being held constant.

       SUBROUTINE PREHELP(W1,W,DT,FM,FM1,fac,PS,PRAIN,E,RHOW,HF,Q,QM,MR,
     * qe,hneu)
       REAL MR,DT

C   The next statement calculates the value of FM given
C    the W and w1 values
       FM = (w1-W)/DT*fac-FM1
C   The next statement calculates the changed MR and E due to the new FM.
C    FM was = PRAIN+PS-MR-E
C    Here the difference is absorbed in MR first and then E if mr < 0.
c
       MR = MAX( 0.0 , (PS + PRAIN - FM - E))
       E = Ps + Prain - FM - MR
C    Because melt rate changes the advected energy also changes.  Here
C     advected and melt energy are separated,
       QOTHER = Q + QM - QE
C     then the part due to melt recalculated
       QM = MR*RHOW*HF
C     then the part due to evaporation recalculated
       QE = -E*RHOW*HNEU
C     Energy recombined
       Q = QOTHER - QM + QE
       RETURN
       END

C******************************** ALBEDO () *****************************
C     Function to calculate Albedo
C     BATS Albedo Model (Dickinson et. al P.21)
      FUNCTION ALBEDO(tausn,coszen,d,aep,abg,avo,airo)

      B = 2.0
      CS = 0.2
C      AVO = 0.95
      CN = 0.5
C      AIRO = 0.65

      FAGE = tausn/(1.0+tausn)

      IF(coszen.LT.0.5) THEN
         FZEN = 1.0/B*((B+1.0)/(1.0+2.0*B*coszen)-1.0)
      ELSE
         FZEN = 0.0
      ENDIF
      AVD = (1.0-CS*FAGE)*AVO
      AVIS = AVD+0.4*FZEN*(1.0-AVD)
      AIRD = (1.0-CN*FAGE)*AIRO
      ANIR = AIRD+0.4*FZEN*(1.0-AIRD)
      ALBEDO = (AVIS+ANIR)/2.0
      if(d.lt.aep)then   ! need to transition albedo to a bare ground value
        rr=(1.-d/aep)*exp(-d*.5/aep)
        albedo=rr*abg+(1.-rr)*albedo
      endif
      RETURN
      END

******************************** AGESN () *****************************
C     Function to calculate Dimensionless age of snow for use in
C     BATS Albedo Model (Dickinson et. al P.21)

      Subroutine agesn(tausn,dt,ps,tsurf,tk,dNewS)
cyjs      Subroutine agesn(tausn,dt,ps,tsurf,tk)
      tsk=tsurf+tk  ! Express surface temperature in kelvin
      R1 = EXP(5000.0*(1.0/TK - 1.0/TSK))
      R2 = R1**10
      IF(R2.GT.1.0) THEN
         R2 = 1.0
      ENDIF
      R3 = 0.3
c   Dickinson p 23 gives decay as DT*(R1+R2+R3)/tau0  with
c    tau0 = 10**6 sec.  Here 0.0036 = 3600 s/hr * 10**-6 s**-1
c    since dt is in hours.
cyjs      tausn = max((tausn+0.0036*(R1+R2+R3)*DT)*
cyjs     &            (1.0 - 100.0*PS*DT),0.)
      tausn = tausn+0.0036*(R1+R2+R3)*DT
	if(ps.gt.0.0) then
	  if(dNewS .gt. 0.0) then
	    tausn = max(tausn*(1-ps*dt/dNewS),0.)
	  else
	    tausn = 0.0
	  endif
	endif

      RETURN
      END


***************************** LanE () *************************************
C	Function to get the LanE which is the thermal conductivity by ze

	real function LanE(LanS,LanG,Zs,rho,rhog,cs,cg,r,ze,w1day)

	real LanS, LanG, Zs, Dlf


	 fKappaS = LanS/(rho*cs)
	 fKappaG = LanG/(rhog*cg)

	 d1 = sqrt(2*fKappaS/w1day)
	 d2 = sqrt(2*fKappaG/w1day)

	 LanE=amin1(r,zs/d1)*d1/LanS + amax1(0.,(r-zs/d1))*d2/LanG

	 ze=amin1(r,zs/d1)*d1+amax1(0.,(r-zs/d1))*d2

	 if (LanE .ne. 0.) LanE = 1/LanE*ze

	 return

	end
*************************** refDep() **************************************
c     function to calculate the refreezing depth
	real function refDep(flans, a, b, hf, rhom, dt, x1 )

	temp=flans*flans+2*b*(-a*flans*dt/(hf*rhom)+flans*x1+0.5*b*x1*x1)

	if(temp .lt. 0.0 .or. a.gt.0.0 .or. b.eq.0.0) then
	  refDep=0
	else
	  refDep=(-flans+sqrt(temp))/b

	endif

	return
	end

************************** Grad () ********************************
C Linear simple function to calculate the gradient of Q and T

      subroutine Grad(qc1,qc2,t1,t2, a, b)
	if((t2-t1) .ne. 0.0) then
	  b=(qc2-qc1)/(t1-t2)
	  a=qc1+b*t1
	endif

	return
	end

C********************* Caculate the daily average  *********************
cyjs  Calculate the daily average value
cyjs  n number of records, a minimum value -100 or some
	real function daily_ave(backup, n, a)
	real backup(*)
	sum = 0.
	count=0.
	do 1 i = 1, n
	if(backup(i).gt.a) then
		sum=sum+backup(i)
		count=count+1.
	end if
 1	continue
	if(count.ne.0) then
		daily_ave=sum/count
	else
		daily_ave=0.
	end if
	return
	end
