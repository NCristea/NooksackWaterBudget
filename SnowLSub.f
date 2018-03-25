cyjs  Lumped snowmelt module can be called outside  (With depletion curve)
cyjs  input of snow module include 
c year,month,day,hour,DT,nStep -- year, month, day, hour,  time step (hour), number of time step
c INPT  -- input forcing
c	  inpt(1,1)=t          ! temperature to subwatershed (C)
c	  inpt(2,1)=rain(j)    !precipitation 
c	  inpt(3,1)=ws(j)      !wind speed  (m/s)
c	  inpt(4,1)=rh         !relative humidity 
c	  inpt(5,1)=qsi(j)     !short wave  (kJ/m^2/h)
c	  inpt(6,1)=qnet(j)    !net radiation  (kJ/m^2/h)
c	  inpt(7,1)=trange(j)  !temperature range  (C, it will be updated to zeinth 
c           angle while calling the UEB point snowmelt module) 
c SITEV -- site variables
c        site variables (1-5)
c        sitev(1)=0                     !forest cover fraction
c        sitev(2)=1.0                   !drift factor  (No detailed information give 1)
c        sitev(3)=pa                    !air pressure
c        sitev(4)=3.6                   !ground heat flux  Assign a constant 1 W/m^2
c        sitev(5)=0.1                   !albedo extinction parameter (veghght)
c
c STATEV	1:5 point state variables (U, W, dimensionless age, refdepth, totalrefdepth)
c        plus six depletion curve lumped parameters 6:11 (Wmax, areafrac, meltflag,Wtgt,Wtgtmax,Aftgt)
c
c PARAM  --  snowmelt module parameters (as above)
c iflag  -- flags to call point snowmelt module 
c	   iflag(1) = irad   ! on input this has value
c         	0 is no measurements - radiation estimated from diurnal temperature range
c			1 is incoming shortwave radiation read from file (measured), incoming longwave estimated
c			2 is incoming shortwave and longwave radiation read from file (measured)
c			3 is net radiation read from file (measured)
c          internally iflag(1) is passed to snowueb with the meaning
c          0=radiation is shortwave in col 5 and longwave in col 6, else = net radiation in column 7
c        iflag(2) = 0      ! no(/yes 1) printing
c        iflag(3) = 9      ! unit to which to print
c        iflag(4) = 1      ! how albedo calculations are done
c 	   iflag(5) = 4      ! model option for surface temperature approximation (modified 
c            force-restorce approach (Luce, 2000; Luce and Tarboton, 2001; You et al.  ;
c            You, 2002))
c dtbar  -- monthly mean daily temperature range
c cump,cume,cummr  -- cumulative precipitation (with df), cumulative evaporation, cumulative melt rate
c outv  -- output variables (from point snowmelt modeling (UEB) 13 variables, see snow menu (Tarboton, 1995, 
c      Tarboton and Luce (1996))
c tsbackup,tavebackup  --  temperature storage of modeled snow surface temperature, snow average
c      temperature in last 24 hours (48 means 0.5 hour time step is possible)( for modified 
c      force restore approach
c dfc  -- depletion curve (wa/wmax, afraction) 
c baout,out  -- (basin output and point snowmelt output)

	subroutine SnowLSub(year,month,day,hour,DT,nStep,INPT,SITEV,STATEV
     &  ,PARAM,iflag,dtbar,nstepday, 
     &  cump,cume,cummr,outv,tsbackup,tavebackup,
     &  ndepletionpoints,dfc,modelelement)    ! 4/2/05  DGT added modelelement for debugging
      PARAMETER(niv=7)
      INTEGER YEAR,DAY,iflag(*),stepInDay  ! Pass a flag to control surface temperature modeling 
      REAL IO,LAT
      real INPT(niv,1)
      real sitev(*)
      real outv(*)
      real statev(*)
      real param(*)
      real dtbar(12)
	real mtime(5)      ! YJS pass to reflect the change of Snow
	real dfc(ndepletionpoints,2)
	real hour
	real tsbackup(*)
	real tavebackup(*)

cyjs  Constant data set 
      data io/4914.0/    !  solar constant  kJ/m^2/hr
      data to /0.0/        !  Temperature of freezing (0 C)
      data tk /273.15/     !  Temperature to convert C to K (273.15)
      data sbc /2.0747e-7/ !  Stefan boltzman constant (2.0747e-7 KJ/m^2/hr/K)
      data hf /333.5/      !  Heat of fusion (333.5 KJ/kg)
      data hneu /2834.0/   !  Heat of Vaporization (Ice to Vapor, 2834 KJ/kg)f
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
      data bca /0.8/ !A in Bristow Campbell formula for atmospheric transmittance
      data bcc /2.4/ !C in Bristow Campbell formula for atmospheric transmittance
cyjs  End of constant declaration

c     Unravel state variables
      afrac=statev(7)
	baw=statev(2)
      Wmax = statev(6)
	meltflag=statev(8)
	Wtgt = statev(9)     !target snow water equivalent for snowfall hysteresis loops during melt
	Wtgtmax=statev(10)
	Aftgt=statev(11)
	irad=iflag(1)
	slope=sitev(6)
	azi=sitev(7)
	lat=sitev(8)
c  debugging code to stop at a specific date
c      if((year .eq. 1960) .and. (month .eq. 10) .and.
c     + (day .eq. 27) .and. ! (hour .gt. 8) .and. 
c    + (modelelement .eq. 110))then
c	   year=year
c	endif

      do 2 iit=1,nStep    !  time loop
	IF(AFRAC.gt.0 .and. BAW .gt. 0) then
C  See discussion in "Depletion curve logic.doc"  ! DGT 7/25/05.  Revised to avoid spikes this was causing
C    The idea here is to use a weighted average of the snow water equivalent depths over the old and 
C    new snow covered area as the representative snow water equivalent to give the point model while 
C    on an excursion. 
		If(meltflag .eq.0 .and. Aftgt > 0 
     +	.and. Afrac > Aftgt .and. BAW > Wtgt)then  !  Here we are on an excursion
c			SCAW=Wtgt/Aftgt+BAW-Wtgt   ! DGT 7/25/05.  Initial approach superceded by the following
			AfNew=Afrac-Aftgt
			WoldSCA=Wtgt/Aftgt
			Wnew=BAW-Wtgt
			WnewSCA=Wnew/AfNew
			SCAW=(WnewSCA*Wnew+WoldSCA*Wtgt)/BAW
		else
		    SCAW = BAW/Afrac
		endif
      else  ! initialize for no snow
        SCAW = 0
	  Afrac=0
	  BAW=0
	  Wmax=0.000001   !  DGT 7/25/05 to avoid NaN when first used
	  Wtgtmax=0
	  meltflag=1
      endif
      statev(2)=SCAW
c   DGT 7/26/05.  Statev(1) in the lumped model stores the average temperature except in the case 
c   when liquid water is present in which case it stores the liquid fraction.
c   The assumption is that these quantities remain constant during depletion curve adjustments to W
c   and are used to reinstate a physically realistic U.  
      cg =param(4)    !  Ground heat capacity (nominally 2.09 KJ/kg/C)
      rhog=param(8)   !  Soil Density (nominally 1700 kg/m^3)
      de=param(11)    !  Thermally active depth of soil (0.1 m)
	if(statev(1)< 0.)then   
	    !  Here energy content state variable was temperature and needs to be converted to energy
		statev(1)=statev(1)*(rhog*de*cg+rhow*SCAW*cs)
	else
		if(SCAW > 0.)then
	       !  Here energy content state variable was liquid fraction
			statev(1)=statev(1)*rhow*SCAW*hf
		else
			!  Here energy content variable was temperature.  SCAW is 0 so energy is only of soil
			statev(1)=statev(1)*rhog*de*cg
		endif
	endif


c   *****************This is start of point model 
c
c   From the  input list 
      RH=inpt(4,1)
      QSIOBS=inpt(5,1)
	QNETOB=inpt(6,1)
	trange=inpt(7,1)  
	TA=inpt(1,1)   

      CALL hyri(YEAR,MONTH,DAY,HOUR,DT,SLOPE,AZI,LAT,HRI,COSZEN)
       INPT(7,1)=COSZEN
       IF(IRAD.LE.1)THEN
         call atf(atff,trange,month,dtbar,bca,bcc)
         IF(IRAD.EQ.0) THEN  ! NEED TO ESTIMATE RADIATION BASED ON AIR TEMP.
           INPT(5,1)=atff*IO*HRI
         ELSE 
c      Need to call HYRI for horizontal surface to perform horizontal
C      measurement adjustment
           call hyri(YEAR,MONTH,DAY,HOUR,DT,0.0,AZI,LAT,HRI0,COSZEN)
c      If HRI0 is 0 the sun should have set so QSIOBS should be 0.  If it is
C      not it indicates a potential measurement problem. i.e. moonshine
           if(HRI0 .GT. 0.0) then
	       atfimplied=min(qsiobs/(HRI0*IO),0.9) ! To avoid unreasonably large radiation when HRI0 is small
             INPT(5,1)=atfimplied * HRI * IO
           else
             INPT(5,1)=QSIOBS
             if(qsiobs .ne. 0.)then
               write(6,*)"Warning ! you have nonzero nightime"
               write(6,*)"incident radiation of",qsiobs
               write(6,*)"at date",year,month,day,hour
             endif
           endif
         ENDIF
C        cloud cover fraction dgt 10/13/94
         cf = 1.-atff/bca
         call qlif(INPT(6,1),TA,RH,tk,sbc,cf)
         IRADFL=0
       ELSE
         IRADFL=1
         INPT(5,1)=QNETOB
       ENDIF
	 iflag(1)=IRADFL
       CALL UPDATETIME(YEAR,MONTH,DAY,HOUR,DT)

	if(iflag(2) .eq. 1) then
       WRITE(iflag(3),*)YEAR,MONTH,DAY,HOUR,atff,HRI,(INPT(i,1),i=1,7)
	endif

c    set modeling time
	 mtime(1) = Year
	 mtime(2) = month
	 mtime(3) = day
	 mtime(4) = hour
	 mtime(5)=modelelement   ! This used to know where we are for debugging

c   cumulative variables used for single time step over snow covered area
	cump=0
	cume=0
	cummr=0

       CALL SNOWUEB2(DT,1,INPT,SITEV,STATEV,
     &  tsbackup,tavebackup,nstepday,PARAM,iflag,
     &  cump,cume,cummr,outv,mtime)   !Add a pass varible to snow mtime

c ************************ End of point model

c     DeltaW = Change in water equivalent
      DeltaW=statev(2)-SCAW
      if(outv(2) .gt. 0. .and. DeltaW .gt. 0.)then   ! Here an increase in snow water equivalent due to snowfall
		If(meltflag .eq. 1)then
		  Meltflag=0   !  Start new excursion
		  Wtgt=BAW
		  Aftgt=Afrac
		  Wtgtmax=Wtgt  ! Establish new upper limit of new excursion
		endif
		BAW=BAW+DeltaW
		Wtgtmax = max(BAW,Wtgtmax)
		If(BAW .gt. Wmax)then
		 Wmax=BAW
		 Meltflag=1   ! end up on main curve if Wmax increases
		elseif(BAW > 3.* Wtgt)then   !  DGT 7/25/05   If new snow dominates old snow we
		!  reset the process to be on the depletion curve.  
		!  Here domination is arbitrarily defined as greater than 3 times.
		    Meltflag=1 
			Wmax=BAW
		endif   
		Afracnew=1
		BaSwin=cummr    ! basin average surface water input
		Basub=cume   ! basin average sublimation
	Else if( DeltaW .gt. 0) then  ! here a rare case when W increases but there is no snowfall.  
	!   This may be due to rainfall absorbing and freezing into the snow or condensation.  
	!    This is assumed to only be effective over snow covered area fraction
		BAW=BAW+DeltaW*Afrac
		Wtgtmax = max(BAW,Wtgtmax)
		If(BAW .gt. Wmax)then
		 Wmax=BAW
		 Meltflag=1   ! end up on main curve if Wmax increases
		Endif
c   DGT 7/25/05  Logically the dominates check above should be done here too, but it is not 
c   because if Wmax is reset Afrac should go to 1 to put the process at the top of a depletion curve
c   and we are not increasing Afrac because there is no snowfall.  The implication is that BAW may
c   go greater than 3 * Wtgt and the depletion curve only get reset next time snow falls.  
c   The 3 is arbitrary so this does not seem like a serious shortcoming.
		Afracnew=Afrac   ! (emphasize does not change)
c         One could increase Wmax here to keep on main depletion curve.  However I decided to handle this issue below with the Min statements so that Af never increases when there is melt.  Excursions below the depletion curve will therefore return back to the same Wa before further reductions in Af occur.
          BaSwin=cummr*afrac+cump*(1-afrac)  !  All precipitation is surface water input over snow free area
		Basub=cume*Afrac  ! Sublimation is only counted over snow covered area.  Calling model has to handle ET from soil
	Else   ! Here (DeltaW < 0) so there is melt
	    If(meltflag .eq. 0 .and. (BAW-DeltaW*Afrac) < Wtgt)then  
c   Here reduction passes target so do in two steps to avoid overshooting and large steps towards end of season
			DW1=-(BAW-Wtgt)/Afrac
			DW2=-(DeltaW-DW1)
c             now logically in two steps
c                BAW=BAW+DW1*Afrac+Aftgt*DW2  
c             which simplifies to
			BAW=Wtgt+Aftgt*DW2
		else
	        BAW=BAW+DeltaW*Afrac 
	    endif 
		If(BAW .lt. 0)then
c      There is a mass balance issue here because BAW is insufficient to supply M and E that  
c      result in the reduction DeltaW. 
             r=1-(BAW/Afrac*DeltaW)   ! This assumes the shortfall will be evenly spread between M and E
		   BAW=0.
			cummr=cummr*r
			cume=cume*r
c       DGT 7/26/05  Since snow is disappearing here we should make sure energy behaves properly
c       Set energy content to 0.  There was snow so any energy was in liquid fraction not temperature greater
c       than 0 and that is now gone.  
             if(statev(1) >= 0.)then
	           statev(1)=0.
c			else
c				statev(1)=statev(1)    ! redundant but debugging to see if this ever occurs
			endif     
	    endif
		BaSwin=cummr*Afrac+cump*(1-Afrac)
		BaSub=cume*Afrac
		If(BAW .lt. Wtgt)meltflag=1  ! Back on main melt curve
		If(meltflag .eq. 1)then
			Afracnew=Min(Ablat(BAW/Wmax,ndepletionpoints,dfc),Afrac)   ! Regular depletion curve.  Make sure that Af does not increase in this step for special case where W may have increased to be below depletion curve by absorbing of precip or snow.  Assume Af remains the same until W returns to original Af
		Else
			Wref= Wmax-(Wtgtmax-BAW)/(Wtgtmax-Wtgt)*(Wmax-Wtgt)  ! See depletion curve logic.doc for derivation and picture
			Afracnew=Min(Ablat(Wref/Wmax,ndepletionpoints,dfc),Afrac)
		Endif
	endif

c  DGT 7/25/05.  Save in statev(1) the energy state variable that persists through depletion curve 
c   BAW and SCAW adjustments
c   
	if(statev(1) .le. 0.)then
	!  Here convert energy to temperature - using statev(2) output from the point model prior to
	!  depletion curve adjustments
	   statev(1)=statev(1)/(rhog*de*cg+rhow*statev(2)*cs)
	else
		if(BAW > 0.)then  
	!  Here snow water equivalent is positive and energy content positive so 
	!  energy content variable is the liquid fraction
	! Note - this is a strictly greater than.  A tolerance could be considered
	! Note also that the test is on BAW because depletion curve adjustments may result in this
	! being 0 when statev(2) is not so then the temperature inter
			if(statev(2) <= 0.)then
				if(BAW > 1e-6)then
	!  Due to arithmetic precision in the division and multiplication by afrac
	!   BAW > 0 can occur when statev(2) = 0.  Only consider this an error if > 1e-6
					write(6,*)"Error, BAW > 0, statev(2) <= 0."
					BAW=0.
					statev(1)=0.
				else
					BAW=0.
	!  Here energy content is positive and snow water equivalent is 0 so 
	!  energy content state variable is the average soil temperature
					statev(1)=statev(1)/(rhog*de*cg)
				endif
			else
				statev(1)=statev(1)/(rhow*statev(2)*hf)
			endif
		else
			if(statev(2) > 0.)then
	!  Here BAW was 0 but statev(2) is positive.  This can occur due to depletion curve adjustments
	!  Treat this as a case where snow water equivalent is 0 but disregard the previous energy content
	!  because it is greater than 0 and reflects liquid water fraction which has been removed. 
				statev(1)=0.
				write(6,*)'Liquid fraction energy content set to 0'
				write(6,*)'because depletion curve set W to 0'
			else
	!  Here energy content is positive and snow water equivalent is 0 so 
	!  energy content state variable is the average soil temperature
				statev(1)=statev(1)/(rhog*de*cg)
			endif
		endif
	endif

c   Output variables
	outv(15)=BAW
	outv(16)=Afrac
	if(Afracnew .eq. 1)outv(16)=Afracnew  ! output old Afrac unless in increased because old Afrac was used in mass balances
	Afrac=Afracnew
	outv(17)=meltflag
	outv(18)=Wtgt
	outv(19)=Wtgtmax
	outv(20)=Wmax
	outv(21)=Aftgt
	outv(22)=BaSwin
	outv(23)=BaSub 
	if(iflag(2) .eq. 1)WRITE(iflag(3),*)(outv(i),i=15,23)
 2	continue   ! end of time step loop

      iflag(1)=irad  ! reinstate iflag(1) for next call
      statev(2) = BAW
	statev(6)=Wmax
	statev(7)=Afrac
	statev(8)=meltflag
	statev(9)=Wtgt
	statev(10)=Wtgtmax
	statev(11)=Aftgt
	return
      END

c    Charlie's logic that I ditched - Sorry Charlie
c
c       Statev(2) is the snow covered area averaged W
c
c        If (Statev(2).gt.0) then
c	   If(Statev(2).gt.SCAW)then
c	      If (meltflag.eq.1) then
c	         BAWtgt = BAW
c	      endif
c	      
c	      If(outv(2).gt.0)then
c	         BAW = SCAW * Afrac
c	         Afrac = Afrac + (Statev(2)-SCAW)/(scaw*.1+.01)
c	         If(Afrac.gt.1.) Afrac = 1      
c	         If (Afrac.gt.Afracmx) Afracmx = Afrac	      
c	         BAW = BAW + (Statev(2)-SCAW) * Afrac + (1-Afrac)*outv(2)
c	         SCAW = BAW/Afrac  	
c	      else
c	         BAW = Scaw * Afrac
c	         Afrac = Afrac + (Statev(2)-SCAW)/0.1
c	         If(Afrac.gt.1.) Afrac = 1      
c	         If (Afrac.gt.Afracmx) Afracmx = Afrac	      
c	         BAW = BAW + (Statev(2)-SCAW) * Afrac + (1-Afrac)*outv(2)
c	         SCAW = BAW/Afrac  		         
c	      endif
c	      if(BAW.gt.Wmax) Wmax = BAW
c	      BAWmi = BAW
c	      AFRACmi = AFRAC
c	      Meltflag = 0
c	   
c	   else
c	      BAW = Statev(2)*Afrac
c	      If (BAW.lt.BAWtgt) then  !cyjs for  bawtgt=0 <baw<scaw 
c	          BAWtgt = 0
c	          BAWmi = Wmax
c	          AFRACmi = AFRACmx
c	      Endif
c
c             Calculating Area
c
c	      Wref = Wmax -(BAWmi-BAW)*(Wmax-BAWtgt)/(BAWmi-BAWtgt)
c	      Aref = Ablat(Wref,Wmax,ACExp,AFRACmx,dfc)
c	      Atgt = Ablat(BAWtgt,Wmax,ACExp,AFRACmx,dfc)
c	      Afrac=Afracmi-(Afracmx-Aref)*(Afracmi-Atgt)/(Afracmx-Atgt)
! make sure Afrac less than or equal to 1.0
c           if(Afrac .ge.1.0) Afrac =1.0
c
c	      If(Afrac.gt.0.0) then
c	         SCAW = BAW/Afrac
c	      else
c	         SCAW=0
c	      endif
c	      Meltflag = 1
c	      
c	   endif
c	else
c	   BAW=0
c	   SCAW=0
c	   Afrac=0
c	endif
c
c
c*******************************************************************       
       
       function Ablat(DimBaw, ndfc, dfc)

       REAL Ablat, DimBaw
	 real dfc(ndfc,2)
c
c      Function to interpolate dimensionless depletion curve
c
c    DimBaw is dimensionless Basin Average Water Equivalent corresponding to column (1) of dfc
c    ndfc is number of points rows in dfc
c    dfc is dimensionless (0-1) basin average snow water equivalent (col 1) and snow covered area fraction (0-1) col 2
c
       if(DimBaw .ge. 1.0) then
	   Afrac =1
	 else
	   if(DimBaw .le. 0.0) then
	     Afrac=0.0
	   else 
           do 500 i=1 , ndfc
	       if(i .eq.1) then 
		     Afrac=dfc(1,2)   ! DGT 7/25/05 to avoid Afrac ever being interpolated as 0 and avoid divide by 0 errors
	       else
               if(dfc(i,1).ge.DimBaw) then
	           Afrac = dfc(i-1,2)+(dfc(i,2)-dfc(i-1,2))*(DimBaw-
     1		   dfc(i-1,1))/(dfc(i,1)-dfc(i-1,1))   ! assumes points not repeated in the first column of dfc	        
	           goto 1000
	         endif
	       endif
500        continue
         endif
	 endif

1000   if(Afrac .ge. 1.0) Afrac = 1.0

       Ablat = Afrac
       return
       end
