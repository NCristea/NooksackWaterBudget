c   subroutines to compute Evapotranspiration following methods in 
c   Handbook of Hydrology, 1993, Maidment editor, Chapter 4 by 
c   Shuttleworth on ET.
c
C**************************** etall() ****************************
      subroutine etall(xlat, xlong, stdlon, elevtg, dtbar,EVAP,temp,
     * dewp,trange,elevsb,albedo,rlapse,sdate,shour,dtsec,m,ISTEP,
     + iyear,month,iday,ihr,imm,isec,hour1,
     + tmin, tmax,wind2m, method)  ! DGT Dec 10 2004.  Added the last 4 variables
      
      INTEGER IYEAR,MONTH,IDAY,IHR,IMM,ISTEP,ISEC ! 30/03/2004 ITB 
      INTEGER M ! RAW fix 30/03/2004 ITB 
	real  hour1  ! DGT Dec 11, 2004 fixing ITB error
      integer sdate,shour
      integer*8 dtsec ! DGT 5/14/12

      real  dtbar(12)
	real  evap,temp,dewp,trange
      REAL  DT, ALBEDO,ELEVSB, ELEVTG, STDLON ! 30/03/2004 ITB
	REAL   RLAPSE,XLAT,XLONG ! 30/03/2004 ITB 

	real  tmin, tmax ! Daily minimum temperature and max temperature in deg C
	real  wind2m  ! Wind speed at 2 m
	integer method ! A flag to indicate the reference ET method
c     method=0 refers to the Priestly Taylor Method
C     method=1 refers to Penman Monteith ASCE Standard Reference for short grass h=0.12 m known as ETo
C     method=2 refers to Penman Monteith ASCE Standard Reference for tall alfalfa h=0.5 m known as ETr
     
      IF(ISTEP.EQ.1) THEN
		iyear = sdate/10000
		month= sdate/100 - iyear * 100
		iday = sdate - iyear * 10000 - month * 100
		ihr = (shour)/10000
		imm = shour/100 - ihr * 100
		isec = shour - ihr * 10000 - imm * 100
		hour1 = dble(ihr) + dble(imm)/60.+dble(isec)/3600. 
	ENDIF
	  dt = dble(dtsec)/3600.   ! Time step is in hours
	  call et(iyear,month,iday,hour1,dt,albedo,elevsb,elevtg,rlapse,
     *  temp,dewp,trange,xlat,xlong,stdlon,dtbar,evap,
     +  tmin, tmax,wind2m, method)  ! DGT Dec 10 2004.  Added the last 4 variables
cRAW 18-Mar-2005 want to update time outside here, after the snow routine has run
cRAW	 call updatetime(iyear,month,iday,hour1,dt)
      return
	end
c
C**************************** et() ****************************
c    This subroutine does one time step
c
	subroutine et(iyear,month,iday,hour1,dt,albedo,elevsb,elevtg,
     * rlapse,temp,dewp,trange,xlat,xlong,stdlon,dtbar,ret,
     + tmin, tmax,wind2m, method)  ! DGT Dec 10 2004.  Added the last 4 variables

	INTEGER*4 IYEAR, MONTH, IDAY ! , HOUR1  ! 30/03/2004 ITB  
	real  hour1  ! DGT Dec 11, 2004 Fixing ITB error.
      real  dtbar(12)
	REAL  SHIFT,XLONG,STDLONG,SN,T,DEWPT      ! 30/03/2004 ITB 
      REAL  SOLCON,AE,BE,AC,BC,SIGMA,ALPHA,BCA,BCC ! 30/03/2004 ITB 
	REAL  STDLON,DT,XLAT,HRI,COSZEN,TF ! 30/03/2004 ITB 
      REAL  TRANGE,ALBEDO,TEMP, ELEVSB,ELEVTG,RLAPSE,DEWP ! 30/03/2004 ITB 
      REAL  ED,SVP,EMNET,SQRT,CF,XLN,ES,DEL,P  ! 30/03/2004 ITB  
      REAL  XLV,GAMMA,RET, num1, num2, den
	real  cn1, cn2, cd1, cd2, tmaxl, tminl, vpd

	real  tmin, tmax ! Daily minimum temperature and max temperature in deg C
	real  wind2m  ! Wind speed at 2 m
	integer*4 method ! A flag to indicate the reference ET method
c     method=0 refers to the Priestly Taylor Method
C     method=1 refers to Penman Monteith ASCE Standard Reference for short grass h=0.12 m known as ETo
C     method=2 refers to Penman Monteith ASCE Standard Reference for tall alfalfa h=0.5 m known as ETr
           
      data solcon /4914.0/  ! solar constant (4914 kJ/m^2/hr)
	data ae, be /0.34, -0.14/  ! Parameters in Brunt like net emissivity function
c        See equation 4.2.8 in Handbook of hydrology
	data ac, bc /1.0, 0.0/  ! ! Parameters in cloudiness factor equation
c        for Humid areas.  See equation 4.2.10 in Handbook of hydrology
      data sigma/2.0747e-7/ ! Stefan Boltzman constant  kJ/m^2/hr/K
	data alpha/1.26/  ! Priestly - Taylor coefficient for humid climates
	data bca,bcc/0.8, 2.4/  ! Bristow Campbell Parameters  
	data cn1, cd1/37, 0.34/  ! ASCE ETo constants
	data cn2, cd2/66, 0.38/  ! ASCE ETr constants

c   hour1 = start hour of time step
c   dt = time step in hours
c   albedo = surface reflectivity coefficient
c   elevsb = elevation of the basin (m)
c   elevtg = elevation of temperature guage (m)
c   rlapse = lapse rate in degrees C per meter (positive for decreases with height)
c   xlat = basin lattitude in degrees, negative for southern hemisphere
c   xlong = basin longitude in degrees, negative for western hemisphere
c   stdlon = reference time longitude, negative for western hemisphere
c   dtbar = 12 dimensional array of mean monthly temperature ranges
c   bca = A parameter in Bristow Campbell
c   bcc = C parameter in Bristow Campbell
c
c    shift time for std longitude versus basin longitude

      shift = (xlong - stdlon)/15.
	call hyri(iyear,month,iday,hour1+shift,dt,0.0,0.0,xlat,hri,
     + coszen)
	call atf(tf,trange,month,dtbar,bca,bcc)
	sn = (1. - albedo)*tf*hri*solcon
	t = temp - (elevsb - elevtg)*rlapse
	tmaxl=tmax-(elevsb - elevtg)*rlapse
	tminl=tmin-(elevsb - elevtg)*rlapse
	dewpt = min(dewp,t) ! Dewpoint to use is the minimum of lapsed temperature
c   and recorded dewpoint.  This assumes dewpoint (and therefore vapour pressure)
c   is roughly spatially constant, and therefore relative humidity increases as 
c   temperature drops
      ed = svp(dewpt)/1000.  ! Saturation vapour pressure at dew point  (kPa)
	emnet = ae + be *sqrt(ed)  ! Net emmissivity for longwave radiation
	cf = ac * (tf/bca) + bc  ! Cloudiness factor based on equation 4.2.10
      es = svp(t)/1000.   !  Saturation vapour pressure at actual temperature (kPa)
	del=4096. * es/(237.3 + t)**2   ! Equation 4.2.3
	p=101.3*((293. -0.0065*elevsb)/(293.))**5.256   ! Equation 4.4.12 
C           Atmospheric pressure kPa
      xlv = 2501. - 2.361 * t   ! Eqn 4.2.1 for latent heat of vaporization kJ/kg
	gamma=1.6286*p/xlv    !  Psychometric constant eqn 4.2.28 kPa/C
	if(method .eq. 1 .or. method .eq. 2)then  ! DGT Dec 10, 2004
C        Penman Monteith ASCE Standard Reference 
	    xln = -cf * emnet * sigma * ((tmaxl+273.15)**4 +
     +    (tminl+273.15)**4)*0.5 ! REF-ET Equation 26
          es=(svp(tmin)/1000.+svp(tmax)/1000.)*0.5 !convert Pa to kPa
		vpd=max(es-ed,0.0) !kPa
		num1=del * (sn + xln)/xlv
		if(method .eq. 1)then ! short grass ETo 
			num2=cn1*gamma*wind2m*vpd/(273.15+t)
			den=del+gamma*(1+cd1*wind2m)
		else   ! taller alfalfa ETr 
			num2=cn2*gamma*wind2m*vpd/(273.15+t)
			den=del+gamma*(1+cd2*wind2m)
		endif
		ret=(num1+num2)/den
		ret = max(dt * ret ,0.0)
c	  The units are formally kg/m^2 but this is the same as mm
	else ! Priestly Taylor is the default if method is anything else
		xln = -cf * emnet * sigma * (t+273.15)**4  ! Net Longwave (eqn 4.2.7)
		ret = alpha * del/(del+gamma) * (sn + xln)  ! Radiation based reference ET 
c       Above is energy units  kJ/m^2/hr	
		ret = max(dt * ret /(xlv),0.0)  !  Priestly Taylor
c	  The units are now formally kg/m^2 but this is the same as mm
	endif
c      Set ET = 0 when radiation is negative 
	return
	end

C**************************** cliparam () ****************************
C     To read in the parameters needed for ET calculations
C     
C     To read in the parameters needed for ET calculations
C     
      subroutine cliparam(xlat,xlong,stdlon,elevtg,dtbar,ns_temper,
     &					temper_id)
c raw 8-dec-2004 special nooksack version
C     implicit real*8 (a-h,o-z)
	
      IMPLICIT NONE  ! 30/03/2004 ITB 
      INTEGER*4 IUNIT,J,IERROR  ! 30/03/2004 ITB 
	integer ns_temper,i
      
	real*8 xlat(*), xlong(*), stdlon
	real*8 elevtg(*), dtbar(12,*)
      integer temper_id(*)
      LOGICAL*4 limits


	iunit=88
C RPI 25/6/2002 tidied up the input here
	INQUIRE(FILE='clipar.dat',EXIST=limits)
      IF(limits) THEN
		OPEN (UNIT=iunit,FILE='clipar.dat',STATUS='OLD',IOSTAT=IERROR)
	    IF(IERROR.NE.0) THEN
	      WRITE(21,*) ' **** There is a problem with CLIPAR.DAT',
     +                     ' program stops'
            STOP
	    ENDIF	
	ELSE
		WRITE(21,*) ' **** There is no CLIPAR.DAT - please provide one'
	    STOP
      ENDIF 
      
c      open(unit=iunit,file='clipar.dat',status='old')
	read(iunit,*)
	read(iunit,*)
	read(iunit,*)ns_temper
	read(iunit,*)stdlon   ! DGT 7/23/05  One std longitude for all locations
	read(iunit,*)
	do i=1,ns_temper
		dtbar(1:12,i) = 10   ! default diurnal temperature range
 		read(iunit,*)temper_id(i),xlat(i),xlong(i),    ! DGT 7/23/05  removed stdlon(i) array from here
     &					elevtg(i),(dtbar(j,i),j=1,12)
	end do

 99   close(iunit)
	return
      end
