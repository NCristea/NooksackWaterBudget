*
* NLFIT V1.20g include file
*
* Author: George Kuczera,
* Department of Civil Engineering and Surveying,
* University of Newcastle, NSW, 2308, Australia
*
* Copyright, George Kuczera, 1994-97. All rights reserved.
*------
* Dimension parameters used by all NLFIT programs:
*        nmx   = Maximum number of model parameters
*        nrx   = Number of observations in longest response record
*        iex   = Maximum number of responses used in fitting of model
*        marma = Max number of AR or MA parameters
*
* These parameters set the dimensions for all arrays used in programs
*    NLFIT, RESPONS, PREDICT, COMPAT and EDPMF.
*
      integer nmx, nrx, iex, marma
c      parameter (nmx=9, nrx=10*365, iex=1, marma=1)        ! cc model
c      parameter (nmx=8, nrx=240, iex=2, marma=1)        ! abc model
c      parameter (nmx=30, nrx=250, iex=4, marma=1)      !catpro model
c      parameter (nmx=15, nrx=180, iex=6, marma=2)      !kinWave model
c      parameter (nmx=15, nrx=2500, iex=6, marma=2)      !RPI Whatawhata/Pukemanga
c	  parameter (nmx=15, nrx=1440, iex=8, marma=2)      !RAW flood forecasting
c	  parameter (nmx=15, nrx=9000, iex=8, marma=2)		! RPI Mahurangi NLFIT
c	  parameter (nmx=15, nrx=9000, iex=28, marma=2)		! RPI Mahurangi TOPNET
c	  parameter (nmx=15, nrx=500, iex=28, marma=2)		! Feddo Mahurangi TOPNET
c	  parameter (nmx=15, nrx=13000, iex=9, marma=2)      !RAW Motueka/Rangitata/Tukituki
c	  parameter (nmx=15, nrx=3000, iex=4, marma=2)      !RPI Kakahu
c	  parameter (nmx=15, nrx=4000, iex=4, marma=2)      !RPI Malaysian Weng basin
c	  parameter (nmx=15, nrx=720, iex=300, marma=2)      !RAW Waimak
c	  parameter (nmx=15, nrx=5000, iex=9, marma=2)      !RAW shag
c	  parameter (nmx=15, nrx=9000, iex=8, marma=2)      !RPI Opihi 2000
c	  parameter (nmx=15, nrx=15000, iex=2, marma=2)      !RPI Stung Sen
c	  parameter (nmx=8, nrx=3000, iex=2, marma=2)      !RPI snow modelling
c      parameter (nmx=26, nrx=900 , iex=9, marma=2)      ! RPI - WRC GW model
c	  parameter (nmx=15, nrx=367, iex=12, marma=2)      !RPI Waitaki500
c	  parameter (nmx=15, nrx=2000, iex=500, marma=2)      !RAW New Zealand
c      parameter (nmx=17, nrx = 12000, iex=500, marma=2)  
      parameter (nmx=17, nrx = 22000, iex=500, marma=2) ! itb setup for nooksack   
*      Probailistic search parameters used by NLFIT only:
*        nPopX    = Maximum number of populations in GA or random starts
*                 = Maximum number of SCE-UA complexes
*        maxBitX  = Maximum number of bits in GA string
*        maxBestX = Maximum number of good solutions to be saved
*
      integer nPopX, maxBitX, maxBestX
      parameter (nPopX=600, maxBitX=16, maxBestX=150)

