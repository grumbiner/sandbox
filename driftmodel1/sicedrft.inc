! Robert Grumbine
! Last modification ages ago (~2007)
C     Drift model variables
      REAL long1, long2, lat1, lat2, rlat
      PARAMETER (long1  =   0.  )
      PARAMETER (long2  = 360.0 )
      PARAMETER (lat1   = -90.0 )
      PARAMETER (lat2   =  90.0 )
      PARAMETER (rlat   = lat1  )
      INTEGER maxpts
      PARAMETER (maxpts = 360*12*4)

      REAL dlat, dlon, dt
      PARAMETER (dlat = 1./4.)
      PARAMETER (dlon = 1./4.)
      PARAMETER (dt     = 12.*3600.)

C     Meteorological model-dependent
      INTEGER nwave, mwave
      PARAMETER (nwave  = 3000      ) !dimension greater than MRF T-NNN
      PARAMETER (mwave  = (nwave+1)*(nwave+2))

C Defining a polar stereographic grid for northern hemisphere:
      INTEGER divisor
      PARAMETER (divisor = 1 )
      INTEGER nx, ny
      PARAMETER (nx = 385/divisor - 1 )
      PARAMETER (ny = 465/divisor - 1 )

      REAL latmin, lonmin, deltax, deltay
      REAL polei, polej, bedient

      PARAMETER (latmin = +81.25)
      PARAMETER (lonmin =  -90.0)
      PARAMETER (polei  = 190./divisor )
      PARAMETER (polej  = 230./divisor )
      PARAMETER (bedient = 15.0)
      PARAMETER (deltax     = (381.E3/bedient)*divisor)
      PARAMETER (deltay     = (381.E3/bedient)*divisor)

      REAL sgn, rearth, eccen2, slat, slon, xorig, yorig
      PARAMETER (sgn    =    1.0)
      PARAMETER (rearth = 6378.160E3)
      PARAMETER (eccen2 =    0.006694604)
      PARAMETER (slat   =   60.0)  ! slat must be absolute value!!
      PARAMETER (slon   =  -10.0)
      PARAMETER (xorig  = -(polei)*deltax)
      PARAMETER (yorig  = -(polej)*deltay)
