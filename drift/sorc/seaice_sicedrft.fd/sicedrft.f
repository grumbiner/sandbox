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
C   97-10-10 Lawrence Burroughs - added code for ice drift bulletins
C   98-05-04 Lawrence Burroughs - converted for Y2K and f90 conversions
C 1999-09-02 Robert Grumbine - converted for IBM SP, removed legacy code
C   01-05-04 Lawrence Burroughs - converted bulletin software to go to 16 days
C 2012-04-20 Robert Grumbine - read in new ice edge, product kml output
C
C USAGE:
C   INPUT FILES:
C     FTNF47 - FORECAST.POINTS - SET OF POINTS TO MAKE FORECASTS FOR 
C            - ALWAYS.
C     FTNF48 - NICELINE - NORTHERN HEMISPHERE ICE EDGE FILE FROM THE
C            - NATIONAL ICE CENTER.  NOT REQUIRED.
C     FTNF49 - SICELINE - SOUTHERN HEMISPHERE ICE EDGE FILE FROM THE
C            - NATIONAL ICE CENTER.  NOT REQUIRED.
C     Changed to single files of 10 m winds from 12 hourly files of 
C        geostrophic wind info 14 March 2007
C     FTNF11 - 10 Meter winds -- U component
C     FTNF12 - 10 Meter winds -- V component
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
C     UNIQUE:    - SK2SETUP, getwin, uice, movice, SK2OUT, coldp, cnewp,
C                  fndflo, flovel wdir, arcdis
C atmos, getsig, geowin removed 6 March 2007
C getwin added 14 March 2007
C     W3LIB:
C        W3TAGB, W3TAGE
C     SPLIB:
C        SPTEZ, SPTEZD
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

!     Program to replace the Skiles model of 1965, documented
!       in 1968, with a non-interacting ice floe model.
!     Thorndike and Colony, 1982 for Arctic.
!     Martinson and Wamser, 1990 for Antarctic.
!     Read in mrf forecasts out to 6 days to drive the
!       ice forecast.
!     Robert Grumbine 2 June 1994.
!     Extended to 16 days in ~2001
!     Transition to 10 m winds 14 March 2007

      IMPLICIT none

      INCLUDE "sicedrft.inc"

      INTEGER nlat, nlong, atm_npts
      PARAMETER (nlat  = (lat2-lat1)/dlat + 1)
      PARAMETER (nlong = (long2-long1)/dlon  )
      PARAMETER (atm_npts  = nlat*nlong)

      REAL x(atm_npts), y(atm_npts), x0(atm_npts), y0(atm_npts)
      REAL dx(atm_npts), dy(atm_npts)

      REAL grid_lat(nx*ny), grid_lon(nx*ny)
      REAL grid_lat0(nx*ny), grid_lon0(nx*ny)
      REAL grid_dx(nx*ny), grid_dy(nx*ny)
      REAL dir(nx*ny), dist(nx*ny)

      REAL ua(nlong, nlat), va(nlong, nlat)

      INTEGER ice_edge_pts, skpt(atm_npts)
!Limit the frequency of ice edge points to being 'minsep' apart from
!  each other
      REAL minsep
      PARAMETER (minsep = 111.1*dlat) !in units of arcdis -> km

      INTEGER i
      INTEGER time, nfor
      INTEGER uunit, vunit

      CALL W3TAGB('SICEDRFT',1998,0173,0050,'NP21   ')

      open(63,recl=1280,access='sequential')
      open(64,recl=1280,access='sequential')
      OPEN (92, FILE="grid_ds", FORM="UNFORMATTED", STATUS="NEW")

      CALL SK2SETUP(x, y, x0, y0, dx, dy, skpt, 
     1              atm_npts, ice_edge_pts, minsep)

      CALL gridset(grid_lat, grid_lon, grid_lat0, grid_lon0, 
     1             grid_dx, grid_dy)


C Assumed attached externally:
      uunit = 11
      OPEN (UNIT=uunit, FORM="UNFORMATTED", STATUS="OLD")
      vunit = 12
      OPEN (UNIT=vunit, FORM="UNFORMATTED", STATUS="OLD")

      CALL getwin(ua, va, uunit, vunit, nlat, nlong)

      CALL movice(ua, va, x0, y0, x, y, dx, dy, ice_edge_pts)
      CALL movice(ua, va, grid_lon0, grid_lat0, grid_lon, grid_lat, 
     1               grid_dx, grid_dy, nx*ny)

      time = INT(dt/3600.+.5)
      PRINT *,'dt time = ',dt, time

      CALL SK2OUT(x0, y0, dx, dy, skpt, ice_edge_pts, time)
      CALL convert(grid_lon0, grid_lat0, grid_dx, grid_dy, 
     1              grid_lon, grid_lat, nx*ny, dir, dist) 
      WRITE (92) dir
      WRITE (92) dist

      READ (*,*) nfor
      DO 1000 i = 2, nfor
        CALL getwin(ua, va, uunit, vunit, nlat, nlong)

        CALL movice(ua, va, x0, y0, x, y, dx, dy, ice_edge_pts)
        CALL movice(ua, va, grid_lon0, grid_lat0, grid_lon, grid_lat, 
     1               grid_dx, grid_dy, nx*ny)

        time = INT(i*dt/3600. +0.5)

        CALL SK2OUT(x0, y0, dx, dy, skpt, ice_edge_pts, time)
        CALL convert(grid_lon0, grid_lat0, grid_dx, grid_dy, 
     1                grid_lon, grid_lat, nx*ny, dir, dist) 
        WRITE (92) dir
        WRITE (92) dist
        PRINT *,'max dir dist ',maxval(dir), maxval(dist)
        PRINT *,'min dir dist ',minval(dir), minval(dist)

 1000 CONTINUE

      CLOSE(63)
      CLOSE(64)

      CALL W3TAGE('SICEDRFT')
      STOP
      END
