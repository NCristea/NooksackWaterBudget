c
c   Wrapper subroutine for calling by TOPNET
c   David Tarboton  1/10/05
c   This subroutine performs the following functions
c   -  subdivision of the external timestep into a number of internal time steps to represent diurnal forcing
c   -  Bypasses calling of snow subroutine when there is no snow on the ground or no snow falling
c   
      subroutine snowueb(snowsitev,snowstatev,snowparam,
     +				ndepletionpoints,dfc, snowcontrol, dtbar,
     +				snowforcing,snowsurfacetemp,snowaveragetemp,
     +				timestep,nstepday,
     +				surfacewaterinput, snowevaporation,  !outputs (both in m/h)
     +                areafractionsnow,modelelement)  ! 4/2/05  DGT added model element for debugging
	real snowsitev(1)  ! sitev 1:9  forestcoverfrac, elevsb (m), xlat, xlon, stdlon, 
c      elevtg (m), rlapse (C/m), slope (deg), azimuth (deg)
	real snowstatev(1)  ! 1:5 point state variables (U, W, dimensionless age, refdepth, totalrefdepth)
c        plus five depletion curve lumped parameters 6:11 (Wmax, areafrac, meltflag,Wtgt,Wtgtmax,Aftgt)
	real snowparam(1)  !  25 snow parameters - definitions and default values as follows
c (1) tr	  Temperature above which all is rain (3 C)
c (2) ts	  Temperature below which all is snow (-1 C)
c (3) es	  emmissivity of snow (nominally 0.99)
c (4) cg 	  Ground heat capacity (nominally 2.09 KJ/kg/C)
c (5) z	  Nominal meas. height for air temp. and humidity (2m)
c (6) zo	  Surface aerodynamic roughness (0.01 m) 
c (7) rho	  Snow Density (Nominally200 kg/m^3)
c (8) rhog	  Soil Density (nominally 1700 kg/m^3)
c (9) lc	  Liquid holding capacity of snow (0.05)
c (10) ks	  Snow Saturated hydraulic conductivity (200 m/hr) 
c (11) de	  Thermally active depth of soil (0.1 m)
c (12)  abg	  Bare ground albedo  (0.25)
c (13)  avo	  Visual new snow albedo (0.85)
c (14)  anir0	  NIR new snow albedo (0.65)
c (15) lans	  the thermal conductivity of fresh (dry) snow (0.33 kJ/m/k/hr)
c (16) lang	  the thermal conductivity of soil (6.5 kJ/m/k/hr)
c (17) wlf	  Low frequency fluctuation in deep snow/soil layer (1/4 w1) 
c (18) rd1	  Amplitude correction coefficient of heat conduction (1)
c (19) fstab	  Stability correction control parameter 1 - means stability corrections fully used
c (20) Tref	  Reference temperature of soil layer in ground heat calculation input
c (21) dNewS	  The threshold depth of for new snow (0.002 m)
c (22) gsurf	  The fraction of surface melt that runs off (e.g. from a glacier). 
c (23) df       Drift factor
c (24) G        Ground heat flux (kJ/m2/h)
c (25) AEF      Albedo extinction parameter    
 
 	real dfc(ndepletionpoints,2)  ! Depletion curve  wa/wamax, afrac
      integer snowcontrol(1)  ! 1:6 Control variables and flags
c   1 irad (0=estimated, 1=meas inc solar, 2=meas inc solar and longwave, 3=measured net radiation)
c   2 snowprintflag  0 do not print, 1 print
c   3 snow print unit
c   4 number of internal time steps in 1 calling time step
c   5 start date yyyymmdd
c   6 start time hhmmss
c
      real dtbar(12)  ! monthly mean diurnal temperature ranges (C)
	real snowforcing(1)  !1:7  Forcing variables
c	1 airtemp (deg C)
c	2 precip (m/h)
c	3 windspeed (m/s)
c	4 dewpt (deg C)
c	5 diurnal trange (deg C)
c	6 incoming solar (kJ/m2/h)
c	7 net radn (kJ/m2/h)

      real snowsurfacetemp(*)   ! array of model snow surface temperature extending back 1 day for surface temperature average
	real snowaveragetemp(*)    ! array of model snow average temperature extending back 1 day for average
	real timestep   !  Number of hours that model should advance
c   nstepday - the number of time steps in a day.   The dimension of daily arrays has to be at least this so that 
c    daily averages can be tracked
	real surfacewaterinput
	real snowevaporation
c  Variables and arrays to pass to snowlsub
	parameter (niv=7)
	real inpt(niv,1)
	real outv(23),sitev(8)
	integer iflag(5)
c     naming inputs that are used
      elevsb=snowsitev(2)
	xlat=snowsitev(3)
	xlon=snowsitev(4)
	stdlon=snowsitev(5)
	elevtg=snowsitev(6)
	rlapse=snowsitev(7)
	trange=snowforcing(5)   ! diurnal temperature range
	dewptg=snowforcing(4)  ! dew point temperature at temperature gage
c   Atmospheric pressure
	pa=101.3*((293. -0.0065*elevsb)/(293.))**5.256   ! Equation 4.4.12 - Handbook of Hydrology
      pa = pa * 1000.  !  convert to pascal
c	pa = 1012.4 - 11.34*(elevsb/100) + 0.00745*(elevsb/100)**2.4
c     pa = pa * 100.  !  convert to pascal
	pg=101.3*((293. -0.0065*elevtg)/(293.))**5.256   ! Equation 4.4.12 - Handbook of Hydrology
      pg = pg * 1000.  !  convert to pascal

c   Assume constant mixing ratio for adjustment of dewpoint by elevation
      edg = svp(dewptg)  ! Vapour pressure at gage (kPa)
	eda=edg *pa/pg  !  Vapor pressure adjusted to mean elevation of subbasin (kPa)   
      iyear = snowcontrol(5)/10000
	month= snowcontrol(5)/100 - iyear * 100
	iday = snowcontrol(5) - iyear * 10000 - month * 100
	ihr = snowcontrol(6)/10000
	imm = snowcontrol(6)/100 - ihr * 100
	isec = snowcontrol(6) - ihr * 10000 - imm * 100
	hour1 = real(ihr) + real(imm)/60.+real(isec)/3600.

c    Site variables
      sitev(1)=snowsitev(1)  ! Forest cover fraction
	sitev(2)=snowparam(23) ! Drift factor
	sitev(3)=pa               ! Atmospheric pressure
	sitev(4)=snowparam(24)    ! Ground heat flux 
	sitev(5)=snowparam(25)    ! Albedo Extinction parameter
	sitev(6)=snowsitev(8)     ! slope
	sitev(7)=snowsitev(9)     ! azimuth
	sitev(8)=xlat   ! latitude

C    set control flags 
	  iflag(1) = snowcontrol(1)   ! radiation is shortwave in (5) and longwave in (6)
	  iflag(2)= snowcontrol(2)
	  iflag(3)=snowcontrol(3)
        iflag(4) = 1      ! how albedo calculations are done - 1 means albedo is calculated
	  iflag(5) = snowcontrol(7)      !model option for surface temperature approximation
	nstepsint=snowcontrol(4)   !  Internal time step
      dt=timestep/nstepsint 
	templocal=snowforcing(1) - (elevsb - elevtg)*rlapse       !temperature adjustment by lapse rate

c      initialize water fluxes
	surfacewaterinput=0
	snowevaporation=0
	areafractionsnow=0
c  Time loop interpolating external forcing to internal time steps assuming diurnal cycle with tmax at 1500 (3pm)
c  and tmin at 0300 (3 am)
c
c    shift time for std longitude versus basin longitude

      shift = (xlon - stdlon)/15.
	hour1=hour1+shift
      do 2 jj = 1,nstepsint
	  if(nstepsint .gt. 1)then  ! interpolate
	    tmid=hour1+dt*0.5  ! time at mid point of interval used for diurnal cycle adjustment
 		t = templocal+sin((tmid-9.)/24.*6.283185)*trange*.5   ! diurnal cycle adjusted temperature  
	  else
	    t=templocal
	  endif
		es =svp(t) 
c    Mapping inputs onto input array
 		inpt(1,1)=t          ! temperature to subwatershed
	    inpt(2,1)=snowforcing(2)  ! precipitation
		inpt(3,1)=snowforcing(3)  ! wind speed
		if(inpt(3,1) .lt. 0.0) then
			inpt(3,1) = 2.0     !FAO Standard for no wind speed measurement
		endif
		inpt(4,1)=min(eda/es,1.)     !convert to the relative humidity
		inpt(5,1)=snowforcing(6)  ! short wave radiation
		inpt(6,1)=snowforcing(7)  ! net radiation
		inpt(7,1)=trange
! only call snow routine when there is snow - either on ground or falling.
	  if(snowstatev(2) .gt.0. .or. 
     +   (t .le. snowparam(1) .and. inpt(2,1) .gt. 0.))  then  
	  call snowLSub(iyear,month,iday,hour1,dt,1,INPT,SITEV,snowstatev,
     &  snowparam,iflag,dtbar,nstepday, 
     &  cump,cume,cummr,outv,snowsurfacetemp,snowaveragetemp,
     &  ndepletionpoints,dfc, modelelement)  ! 4/2/05  DGT added model element
	     surfacewaterinput=surfacewaterinput+outv(22)/timestep  ! to get answers in m/hr
	     snowevaporation=snowevaporation+outv(23)/timestep  
	     areafractionsnow=areafractionsnow+outv(16)/nstepsint  ! averaging the snow covered area over the interval
	  else
	     surfacewaterinput=surfacewaterinput+inpt(2,1)*dt/timestep
	     CALL UPDATETIME(iyear,month,iday,hour1,dt)
	  endif
 2    continue
      return
	end
