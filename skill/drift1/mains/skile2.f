      PROGRAM skile2
C$$$ MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: SKILE2  SIMPLE ICE DRIFT GUIDANCE MODEL
C    PRGMMR: ROBERT GRUMBINE  ORG: W/NP21 DATE: 97-06-24
C
C ABSTRACT: PROVIDE SEA ICE DRIFT GUIDANCE USING GEOSTROPHIC
C    WIND RELATIONSHIPS AS DERIVED BY THORNDIKE AND COLONY, 1982
C    FOR THE ARCTIC, AND MARTINSON AND WAMSER, 1990 FOR THE 
C    ANTARCTIC.
C
C PROGRAM HISTORY LOG:
C    97-06-24 ROBERT GRUMBINE 
C
C USAGE:
C   INPUT FILES:
C      FTNF40 - FORECAST.POINTS - SET OF POINTS TO MAKE FORECASTS FOR 
C             - ALWAYS.
C      FTNF41 - NICELINE - NORTHERN HEMISPHERE ICE EDGE FILE FROM THE
C             - NATIONAL ICE CENTER.  NOT REQUIRED.
C      FTNF42 - SICELINE - SOUTHERN HEMISPHERE ICE EDGE FILE FROM THE
C             - NATIONAL ICE CENTER.  NOT REQUIRED.
C      FTNF20 - MRF 00 HR FORECAST
C      FTNF21 - MRF 12 HR FORECAST
C      FTNF22 - MRF 24 HR FORECAST
C      FTNF23 - MRF 36 HR FORECAST
C      FTNF24 - MRF 48 HR FORECAST
C      FTNF25 - MRF 60 HR FORECAST
C      FTNF26 - MRF 72 HR FORECAST
C      FTNF27 - MRF 84 HR FORECAST
C      FTNF28 - MRF 96 HR FORECAST
C      FTNF29 - MRF 108 HR FORECAST
C      FTNF30 - MRF 120 HR FORECAST
C      FTNF31 - MRF 132 HR FORECAST
C      FTNF32 - MRF 144 HR FORECAST
C             -  THE FOREGOING PRESUMES THAT MRF OUTPUT IS AVAILABLE
C             -  ONLY EVERY 12 HOURS.  
C   OUTPUT FILES:
C      FTNF60 - FL.OUT - TEMPORARY FILE
C      FTNF61 - OPS.OUT - GLOBAL 
C      FNTF62 - AK.OUT  - ALASKA REGION 
C   SUBPROGRAMS CALLED:
C      UNIQUE:
C        sk2setup, getsig, atmos, uice, movice, sk2out, geowin, 
C        fndflo, flovel
C      POLAR LIBRARY:
C        wdir, arcdis, spherd, sphert94a
C      W3LIB:
C        W3LOG
C      COMMON:
C        DATE
C
C   EXIT STATES:
C      COND = 0 - SUCCESSFUL RUN
C
C  REMARKS:
C
C  ATTRIBUTES:
C    LANGUAGE: ANSI STANDARD FORTRAN 77.
C    MACHINE: ANY.  CURRENTLY CRAY3.
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

      INCLUDE "skile.inc"

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
      INTEGER time

CD      CALL W3LOG('$S', '$M')
      CALL sk2setup(x, y, x0, y0, dx, dy, skpt, npts, nnpts, minsep)

      CALL getsig(slp, temp, ztopo, 20)

      CALL atmos (slp, temp, ztopo, nlat, nlong, ua, va)

      CALL movice(ua, va, x0, y0, x, y, dx, dy, nnpts)
      time = INT(dt/3600.+.5)

      CALL sk2out(x0, y0, dx, dy, skpt, nnpts, time)

      DO 1000 i = 2, nfor

        CALL getsig(slp, temp, ztopo, 19+i)

        CALL atmos (slp, temp, ztopo, nlat, nlong, ua, va)

        CALL movice(ua, va, x0, y0, x, y, dx, dy, nnpts)
        time = INT(i*dt/3600. +0.5)
        CALL sk2out(x0, y0, dx, dy, skpt, nnpts, time)

 1000 CONTINUE

CD      CALL W3LOG('$E')
      STOP
      END
