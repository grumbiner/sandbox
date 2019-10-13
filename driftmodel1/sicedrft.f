      PROGRAM SICEDRFT
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: SICEDRFT       SIMPLE ICE DRIFT GUIDANCE MODEL
C   PRGMMR: GRUMBINE         ORG: NP21        DATE: 1998-06-22
C
C ABSTRACT: PROVIDE SEA ICE DRIFT GUIDANCE USING GEOSTROPHIC
C   WIND RELATIONSHIPS AS DERIVED BY THORNDIKE AND COLONY, 1982
C   FOR THE ARCTIC, AND MARTINSON AND WAMSER, 1990 FOR THE 
C   ANTARCTIC.
C
C PROGRAM HISTORY LOG:
C   97-06-24 ROBERT GRUMBINE 
c   97-10-10 Lawrence Burroughs - added code for ice drift bulletins
c   98-05-04 Lawrence Burroughs - converted for Y2K and f90 conversions
C
C USAGE:
C   INPUT FILES:
C     FTNF47 - FORECAST.POINTS - SET OF POINTS TO MAKE FORECASTS FOR 
C            - ALWAYS.
C     FTNF48 - NICELINE - NORTHERN HEMISPHERE ICE EDGE FILE FROM THE
C            - NATIONAL ICE CENTER.  NOT REQUIRED.
C     FTNF49 - SICELINE - SOUTHERN HEMISPHERE ICE EDGE FILE FROM THE
C            - NATIONAL ICE CENTER.  NOT REQUIRED.
C     FTNF11 - MRF 00 HR FORECAST
C     FTNF12 - MRF 12 HR FORECAST
C     FTNF13 - MRF 24 HR FORECAST
C     FTNF14 - MRF 36 HR FORECAST
C     FTNF15 - MRF 48 HR FORECAST
C     FTNF16 - MRF 60 HR FORECAST
C     FTNF17 - MRF 72 HR FORECAST
C     FTNF18 - MRF 84 HR FORECAST
C     FTNF19 - MRF 96 HR FORECAST
C     FTNF20 - MRF 108 HR FORECAST
C     FTNF21 - MRF 120 HR FORECAST
C     FTNF22 - MRF 132 HR FORECAST
C     FTNF23 - MRF 144 HR FORECAST
C            -  THE FOREGOING PRESUMES THAT MRF OUTPUT IS AVAILABLE
C            -  ONLY EVERY 12 HOURS.  
C
C   OUTPUT FILES:
C     FTNF60 - FL.OUT - TEMPORARY FILE
C     FTNF61 - OPS.OUT - GLOBAL 
C     FNTF62 - AK.OUT  - ALASKA REGION 
C     FNTF63 - GLOBAL.TRAN - GLOBAL BULLETIN FILE
C     FNTF64 - ALASKA.TRAN - ALASKA BULLETIN FILE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - SK2SETUP, getsig, atmos, uice, movice, SK2OUT,
C                  geowin, fndflo, flovel wdir, arcdis, spherd, sphert94a
C     W3LIB:
C        W3TAGB, W3TAGE
C     COMMON:
C        DATE
C
C   EXIT STATES:
C     COND = 0 - SUCCESSFUL RUN
C
C   REMARKS:
C
C   ATTRIBUTES:
C     LANGUAGE: ANSI STANDARD FORTRAN 90 fixed.
C     MACHINE: ANY.  CURRENTLY CRAY3.
C
C$$$

C     Program to replace the Skiles model of 1965, documented
C       in 1968, with a non-interacting ice floe model.
C     Thorndike and Colony, 1982 for Arctic.
C     Martinson and Wamser, 1990 for Antarctic.
C     Read in mrf forecasts out to 6 days to drive the
C       ice forecast.
C     Robert Grumbine 2 June 1994.

      IMPLICIT none

      INCLUDE "sicedrft.inc"

      INTEGER nlat, nlong, tlat, tlong, npts
      PARAMETER (nlat  = (lat2-lat1)/dlat + 1)
      PARAMETER (nlong = (long2-long1)/dlon  )
      PARAMETER (tlat  = 180./dlat + 1)
      PARAMETER (tlong = 360./dlon    )
      PARAMETER (npts  = nlat*nlong)

      REAL x(npts), y(npts), x0(npts), y0(npts), dx(npts), dy(npts)
      REAL slp(mwave), temp(mwave), ztopo(mwave)
      REAL ua(nlong, nlat), va(nlong, nlat)

      INTEGER nnpts, skpt(npts)
      REAL minsep
      PARAMETER (minsep = 60.) !in units of arcdis -> km

      INTEGER i
      INTEGER time, twave, nfor

      CALL W3TAGB('SICEDRFT',1998,0173,0050,'NP21   ')

      open(63,recl=1280,access='sequential')
      open(64,recl=1280,access='sequential')

      CALL SK2SETUP(x, y, x0, y0, dx, dy, skpt, npts, nnpts, minsep)

      CALL getsig(slp, temp, ztopo, 11, twave)

      CALL atmos (slp, temp, ztopo, nlat, nlong, ua, va, mwave, twave)

      CALL movice(ua, va, x0, y0, x, y, dx, dy, nnpts)
      time = INT(dt/3600.+.5)

      CALL SK2OUT(x0, y0, dx, dy, skpt, nnpts, time)

      READ (*,*) nfor
      PRINT *,'nfor = ',nfor
      DO 1000 i = 2, nfor

        CALL getsig(slp, temp, ztopo, 10+i, twave)

        CALL atmos (slp, temp, ztopo, nlat, nlong, ua, va, mwave, twave)

        CALL movice(ua, va, x0, y0, x, y, dx, dy, nnpts)
        time = INT(i*dt/3600. +0.5)
        CALL SK2OUT(x0, y0, dx, dy, skpt, nnpts, time)

 1000 CONTINUE

      close(63)
      close(64)

      CALL W3TAGE('SICEDRFT')
      STOP
      END
