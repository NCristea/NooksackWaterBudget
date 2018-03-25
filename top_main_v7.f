c   Main program to drive topmodel in standalone mode
      program top1

      IMPLICIT NONE  ! ITB 2004/03/24

      INCLUDE 'Tdims_v7.inc'
c	include 'Q:\hydro\topnet\ver1\Topmodel\Nlfitcodes\nlfit.inc' !RPI added full directory path 23/12/98
      INCLUDE 'Nlfit.inc'  ! itb  2004/03/24

      INTEGER*4 initt(iex), iend(iex), neq, i, j, npar
      INTEGER*4 mfit(iex)
      INTEGER*4 it, iflag,iopt,npx,nfor  ! ITB 2004/03/24
      INTEGER*4 ibeale, ifit ! ITB 2004/03/24

      REAL*8 qact(nrx,iex), actime(nrx)
      REAL*8 qfit(iex), dfit(iex,dpm), par(dpm)

      CHARACTER*1 prt
	  CHARACTER*30 modelid
      open(unit=TRACE,file='zbar_fortran.dat',status='unknown')
      CALL INPUTT(initt, iend, neq, qact, actime, modelid,
     &                   npar, nrx, iex)
	  IF(neq .gt. iex)
     &  WRITE(6,*)'Too many responses - increase iex dimension'
	  IF(npar .gt. dpm)
     &  WRITE(6,*)'Too many parameters - increase dpm dimension'

      it=1
	  iflag=1
	  iopt=0
	  prt='Y'
	  npx=dpm
	  nfor=6

      DO i = 1,npar
	    READ(20,*)par(i)    ! Read parameter multipliers from topinp.dat
	  ENDDO

      DO i = 1,max(1,neq)  ! RPI 24/7/02 introduced the max for when NEQ=0
	    mfit(i) = 1
	  ENDDO

      ibeale=0
      write(6,*)'Starting model'  ! For an unkonwn reason the release version crashes without this line
      CALL MODEL(it, iflag, iopt, prt, neq, npar, npx, iex,
     &                  nfor, qfit, dfit, par, mfit, ifit, ibeale)

      END
