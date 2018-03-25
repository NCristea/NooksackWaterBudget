C----------------------------------------------------------------- MIC.FOR
C
C DATE & TIME ROUTINES FOR TIDEDA
C
C Modified for integer*8 times (Apr-2001)
C Now official Tideda date format is yyyymmdd,
C But it still supports old Tideda yyymmdd format

C-------------------------------------------------------------------
C First the new "integer*8" versions
C-------------------------------------------------------------------

c---------------------------------------------------------- td8_micdh
	subroutine td81micdh(idate,ihour,jsec)
!MS$ATTRIBUTES DLLEXPORT :: TD8MICDH
c
c convert seconds to date & time (midnight is returned as 240000)
c 
      IMPLICIT NONE ! 30/03/2004 ITB 

	integer*4 ihour,idate,y,m,d,ihr,imn,jday
	integer*8 jsec,isc,iday

	iday=jsec/86400	! days since 1-jan-1940
	if (jsec.gt.86400*iday) iday=iday+1
	jday=14609+iday	! days since 1-jan-1900
	call td8_mowymd(y,m,d,jday)
	    idate=y*10000+m*100+d
	    isc=jsec-86400*(int8(jday)-14610)
	    ihr=isc/3600
	    isc=isc-ihr*3600
	    imn=isc/60
	    isc=isc-imn*60
	    ihour=ihr*10000+imn*100+isc
	end

c------------------------------------------------------------------- td8_micsec
	subroutine td8micsec(jdatem,jhour,isec)
!MS$ATTRIBUTES DLLEXPORT :: TD8_MICSEC
c
c convert date & time to seconds
c
c time format is "hhmmss" 
c supported date formats are "yyyymmdd","yyymmdd","yyyy","yyy"
c                        e.g. 20010101   1010101	 2001   101
c (all 1-jan-2001)
c
      implicit none  ! 30/03/2004 ITB 

	integer*4 idatem,jdatem,jhour,ihour,ihr,imn
	integer*4 td8_mowday,d,y,m,jday
	integer*8 isec,isc
      integer*4 is ! 30/03/2004 ITB


	idatem=jdatem
	if (iabs(idatem).lt.10000) go to 10	! short form (year only -> 1 jan)
    4	continue
	if (idatem.lt.1080101) then	! it is an old style "yyymmdd" date
	   if (idatem.lt.0) then	! between 1872 and 1900
	      idatem=(-1*idatem) + 18000000
	   else				! between 1900 and 2008
	      idatem=idatem+19000000
	   endif
	endif
	y=idatem/10000
	d=idatem-y*10000
	m=d/100
	d=d-m*100
	jday=td8_mowday(y,m,d)-14610
	is=0
	if(jhour)5,9,7
    5	is=-1
	ihour=-jhour
	goto 8
    7	is=1
	ihour=jhour
    8	ihr=ihour/10000
	isc=ihour-ihr*10000
	imn=isc/100
	isc=isc-imn*100
	isec=ihr*3600 + imn*60+isc
    9	isec=int8(jday)*86400 + is*isec
	return
C
C handle short form of date (year only)
   10	if (idatem.lt.109) then	! old form (year since 1900, e.g. 95 or 101)
	   if(idatem.lt.0)go to 12
	   idatem=idatem*10000+19000101
	   go to 4
   12	   idatem=-idatem
	   if(idatem.ge.101.and.idatem.le.108)then	! first 8 days of 1900
	      idatem=idatem+19000000
	      go to 4
	   endif
	   idatem=idatem+18000000
	   if(idatem.ge.18711213.and.idatem.le.20080101)go to 4
	   if (jdatem.lt.-711213) goto 14
	   idatem=-jdatem*10000+18000101
	   if(idatem.ge.18711213.and.idatem.le.20080101)go to 4
   14	   continue
	   idatem=18720101
	   jhour=0
	   go to 4
	else		! new form, 4 digit year (e.g. 1995 or 2001)
	   idatem=idatem*10000 + 101
	   goto 4
	endif
	end
C-----------------------------------------------------------------
C The guts of the Tideda time algorithm:
C----------------------------------------------------------------- td8_MOWDAY
	INTEGER*4 FUNCTION TD8_MOWDAY(KY,KM,KD)
!MS$ATTRIBUTES DLLEXPORT :: TD8_MOWDAY
C
C	TO CALCULATE THE NUMBER OF DAYS SINCE 1ST JAN 1900
C	FROM THE YEAR MONTH AND DAY
C
	IMPLICIT NONE  !  30/03/2004 ITB 
      
      INTEGER*4 KY,KM,KD
	INTEGER*4 IYA,IC
	INTEGER Y
	INTEGER*4 M ! 30/03/2004 ITB 
      
      Y=KY
	M=KM
C
C ADJUST MONTHS & YEARS TO GIVE MONTHS IN THE RANGE 1 TO 12
 200	IF (M.GT.12) THEN
	   M=M-12
	   Y=Y+1
	   GOTO 200
	ENDIF
C
	M=M-3
	IF (M.LT.0) THEN
	   Y=Y-1
	   M=M+12
	ENDIF
	IC=Y/100
	IYA=Y-100*IC
	TD8_MOWDAY=(146097*IC)/4 + (1461*IYA)/4 + (153*M+2)/5 +KD-693901
	END
C----------------------------------------------------------------- td8_MOWYMD
	SUBROUTINE TD8_MOWYMD(KY,KM,KD,DAYS)
!MS$ATTRIBUTES DLLEXPORT :: TD8_MOWYMD
C
C	RETURNS THE YEAR,MONTH,DAY GIVEN THE NUMBER OF DAYS SINCE 1900
C
	IMPLICIT NONE  ! 30/03/2004 ITB 
      
      
      INTEGER*4 KY,KM,KD,DAYS
	INTEGER*4 J,Y,M,D
	J=4*(693901+DAYS)-1
	Y=J/146097
	J=J-146097*Y
	D=J/4
	M=4*D+3
	J=M/1461
	D=M-1461*J
	D=(D+4)/4
	Y=100*Y+J
	J=5*D-3
	M=J/153
	D=J-153*M
	D=(D+5)/5
	M=M+3
	IF(M.LE.12)GO TO 1002
	M=M-12
	Y=Y+1
C         CALC ALGORITHM STOPS
1002	KY=Y
	KM=M
	KD=D
	END
