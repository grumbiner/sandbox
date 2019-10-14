C     Drift model variables
      REAL long1, long2, lat1, lat2, rlat
      INTEGER nfor
      PARAMETER (long1  =   0.  )
      PARAMETER (long2  = 360.0 )
      PARAMETER (lat1   = -90.0 )
      PARAMETER (lat2   =  90.0 )
      PARAMETER (rlat   = lat1  )
      PARAMETER (nfor   =   12  )
      INTEGER maxpts
      PARAMETER (maxpts = 32 * 207)

      REAL dlat, dlon, dt
      PARAMETER (dlat = 1.       )
      PARAMETER (dlon = 1.       )
      PARAMETER (dt     = 12.*3600.)

C     Meteorological model-dependent
      INTEGER nwave, mwave, kdim
      PARAMETER (nwave  = 126      )
      PARAMETER (mwave  = (nwave+1)*(nwave+2))
      PARAMETER (kdim   = 28       )
      INTEGER idim, jdim, ijdim
      PARAMETER (idim   = 3*nwave+6)
      PARAMETER (jdim   = (3*nwave+2)/2 )
      PARAMETER (ijdim  = idim*jdim )
