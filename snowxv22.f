C  File  snowx.f  
C  Subroutines and function subprograms for the Utah Energy Balance
C  Snow Accumulation and Melt Model.
C  David G. Tarboton, Utah Water Research Laboratory, Utah State University
C  Version 2.1  
C  Last Change 5/4/04 to accommodate glacier surface runoff.
C
C  This program was written and prepared under agreement with and funding 
C  by the U.S. Government , and therefore is in the public domain and not
C  subject to copyright.

C**************READ Bristow Campbell Parameter values **************

      SUBROUTINE bcparm(dtbar,bca,bcc,bcfile)
      CHARACTER*200 bcfile
      REAL dtbar(12)
      OPEN(8,FILE=bcfile,STATUS='OLD')
      READ(8,*)bca,bcc
 1    read(8,*,end=20)month,val
      dtbar(month)=val
      go to 1
 20   CLOSE(8)
      RETURN
      END

C**************READ PARAMETER or state variable VALUES**************

      SUBROUTINE readvals(vals,vfile,n)
      CHARACTER*200 vfile
      REAL vals(1)
      OPEN(8,FILE=vfile,STATUS='OLD')
      READ(8,*)(vals(i),i=1,n)
      CLOSE(8)
      RETURN
      END

C********************* READ THE INITIAL CONDITIONS *********************

C      SUBROUTINE FINITIAL(statev,icfile,nxv,YEAR,MONTH,DAY,HOUR,DT)
C      CHARACTER*20 icfile
C      INTEGER YEAR,DAY
C      REAL statev(1)
C      OPEN(8,FILE=icfile,STATUS='OLD')
C      READ(8,*)(statev(i),i=1,nxv)
C      READ(8,*)YEAR,MONTH,DAY,HOUR,DT
C      CLOSE(8)
C      RETURN
C      END

C********************* READ THE site variables  *********************

      SUBROUTINE readsv(sitev,svfile,nsv,slope,azi,lat)
      CHARACTER*200 svfile
      REAL sitev(1),lat
      OPEN(8,FILE=svfile,STATUS='OLD')
      READ(8,*)(sitev(i),i=1,nsv)
      READ(8,*)slope,azi,lat
      CLOSE(8)
      RETURN
      END

C************************** UPDATETIME () ***************************
C                 Update Time for each time step

      SUBROUTINE UPDATETIME(YEAR,MONTH,DAY,HOUR,DT)
 
      INTEGER*4 YEAR,DAY,DMON(12),DM, MONTH,I       ! 30/03/2004 ITB 
      INTEGER*4 LYEAR  ! 30/03/2004 ITB  
	real  hour, dt  ! DGT Dec 10, 2004.  Fixing ITB errors 
   
      DATA (DMON(I),I=1,12)/31,28,31,30,31,30,31,31,30,31,30,31/
      HOUR=HOUR+DT
	DM=DMON(MONTH)
c   check for leap years 
      if(month .eq. 2)dm=lyear(year)
 10   continue
      IF(HOUR.GE.24.0) THEN
        HOUR=HOUR-24.0
        DAY=DAY+1
	go to 10
      ENDIF
 20   continue
      IF(DAY.GT.DM) THEN
        DAY=day - dm
        MONTH=MONTH+1
        IF(MONTH.GT.12) THEN
          MONTH=1
          YEAR=YEAR+1
	    DM=DMON(MONTH)
          if(month .eq. 2)dm=lyear(year)
	  endif
	  go to 20
      ENDIF
      RETURN
      END

C************************** lyear () ***************************
C     function to return number of days in February checking for
c     leap years
      function lyear(year)   
      integer year,lyear
      IF(MOD(YEAR,4).GT.0 .or. 
     1   (mod(year,100) .eq.0 .and. mod(year,400) .ne. 0)) THEN
C  Leap years are every 4 years 
c  - except for years that are multiples of centuries (e.g. 1800, 1900)
c  - except again that when the century is divisible by 4 (e.g. 1600, 2000)
c    then it is a leap year 
         lyear=28
      ELSE
         lyear=29
      ENDIF
	return
	end

C**************************** atf () ****************************
C     to get the atmospheric transmissivity using the Bristow and Campbell
C     (1984) approach

      Subroutine atf(atff,trange,month,dtbar,a,c)
      DIMENSION dtbar(12)
      b=0.036*exp(-0.154*dtbar(month))
      atff=a*(1-exp(-b*trange**c))
C      write(6,*)trange,month,a,c,dtbar(month),atf
      RETURN
      END


C************************** hourlyRI () **********************
C                To get hourly radiation index

      SUBROUTINE hyri(YEAR,MONTH,DAY,HOUR,DT,SLOPE,AZI,LAT,
     *                    HRI,COSZEN)
      INTEGER YEAR,DAY
      REAL LP,LAT1,LAT   
c  lp= latitude of equivalent plane in radians
c  lat1 = latitude in radians
c  lat = latitude in degrees
c  a number that speaks for itself - every kissable digit
      PI=3.141592653589793238462643383279502884197169399375105820974944592308
      CRAD=PI/180.0
c   crad = degree to radian conversion factor
C     CONVERT TIMES TO RADIANS FROM NOON
      T=(HOUR-12.0)*PI/12.0
      DELT1=DT*PI/12.0
C     CONVERT angles TO RADIANS
      SLOPE1=SLOPE*CRAD
      AZI1=AZI*CRAD
      LAT1=LAT*CRAD
	FJULIAN=FLOAT(JULIAN(year,MONTH,DAY))
      D=CRAD*23.5*SIN((FJULIAN-82.0)*0.017214206321)  
c  0.017214206321 is 2 pi / 365  
c  D is solar declination
      LP=ASIN(SIN(SLOPE1)*COS(AZI1)*COS(LAT1)
     *   +COS(SLOPE1)*SIN(LAT1))
c  LP is latitude of equivalent plane
c      TD=ACOS(-TAN(LAT1)*TAN(D))  This formula abandoned 1/8/04 
c      to make the code work for polar conditions
c  TD is half day length, i.e. the time from noon to sunset.  Sunrise is at -TD
      tanprod=TAN(LAT1)*TAN(D)
	if(tanprod .gt. 1.)then
	  td=pi  ! This is the condition for perpetual light
	else if(tanprod .lt. -1.)then
	  td=0   ! The condition for perpetual night
	else
	  td=acos(-tanprod)  ! The condition where there is a sunrise and set
	endif
c   Equivalent longitude offset.  Modified on 1/8/04
c   so that it correctly accounts for shift in longitude if equivalent 
c   plane slope goes over a pole.  Achieved using atan2.
c      DDT=ATAN(SIN(AZI1)*SIN(SLOPE1)/(COS(SLOPE1)*COS(LAT1)
c     *    -COS(AZI1)*SIN(SLOPE1)*SIN(LAT1)))
      ddt=atan2(SIN(AZI1)*SIN(SLOPE1),
     *(COS(SLOPE1)*COS(LAT1)-COS(AZI1)*SIN(SLOPE1)*SIN(LAT1)))  

c   Now similar logic as before needs to be repeated for equivalent plane
c   but with times reflecting
      TPeqp=TAN(LP)*TAN(D)
C  Keep track of beginning and end of exposure of equiv plane to sunlight
      IF(tpeqp .gt. 1.0) THEN
          TPbeg=-pi   ! perpetual light
	    tpend=pi
      ELSEif(tpeqp .lt. -1.)then
          TPbeg=0.0  ! perpetual dark
          tpend=0.0 
	else
	    tpbeg = -acos(-tpeqp)-ddt
	    tpend = acos(-tpeqp)-ddt
      ENDIF 

c   Start and end times for integration of radiation exposure
c   need to account for both horizon, slope and time step
      T1=AMAX1(T,tpbeg,-TD)
      T2=AMIN1(T+DELT1,TD,tpend)
C      write(6,*)t1,t2
      IF(T2.LE.T1) THEN
        HRI=0.0
      ELSE
        HRI=(SIN(D)*SIN(LP)*(T2-T1)+COS(D)*COS(LP)*(SIN(T2+DDT)
     *       -SIN(T1+DDT)))/(COS(SLOPE1)*DELT1)
c   In the above the divide by cos slope normalizes illumination to per unit horizontal area
      ENDIF
c   There is a special case if tpbeg is less than -pi that occurs in polar regions
c   where a poleward facing slope may be illuminated at night more than the day.
C   Add this in
      if(tpbeg .lt. -pi)then
	  T1=AMAX1(T,-tpbeg+2*pi,-TD)
	  T2=AMIN1(T+DELT1,TD)
	  if(t2 .gt. t1)then
	    hri=hri+(SIN(D)*SIN(LP)*(T2-T1)+COS(D)*COS(LP)*(SIN(T2+DDT)
     *       -SIN(T1+DDT)))/(COS(SLOPE1)*DELT1)
	  endif
	endif
c  for the purposes of calculating albedo we need a cosine of the illumination angle.  This
c  does not have slope correction so back this out again.  This is an average over the
c  time step
      COSZEN = HRI*COS(SLOPE1)
C      write(6,*)hri,coszen

      RETURN
      END


C***************************** JULIAN () ****************************
C              To convert the real date to julian date
cYJS  The Julian are change to a new version to take the Lean Yean into consideration
c     in the old version, there are 365 days each year.
c      FUNCTION JULIAN(MONTH,DAY)
      function julian(yy,mm,dd)
      integer yy,dd
      dimension mmstrt(1:12)
      data (mmstrt(i),i=1,12)/0,31,59,90,120,151,181,212,243,273,
     *                        304,334/
      jday = mmstrt(mm) + dd
      ileap = yy - int(yy/4)*4 
      if(ileap.eq.0.and.mm.ge.3) jday = jday + 1
      julian = jday
      return 
      end

C ****************************** QLIF () *******************************
C     Computes the incoming longwave radiation using satterlund Formula
C      Modified 10/13/94 to account for cloudiness.
C      Emissivity of cloud cover fraction is assumed to be 1.
C
      subroutine qlif(qliff,TA,RH,TK,SBC,cf)
      EA = SVPW(TA)*RH
      TAK = TA + TK
      EA1 = 1.08*(1.0-EXP(-(EA/100.0)**((TAK)/2016.0)))
      QLIFf =(cf+(1.-cf)*EA1)*SBC*TAK**4
      RETURN
      END

