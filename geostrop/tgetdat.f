      SUBROUTINE tgetdat(a, slp, temp, q, uten, vten, ztopo,
     1    tlat, tlong, nlat, nlong,
     2    lat1, long1, lat2, long2, dlat, dlong, iunit)

      IMPLICIT none

      INTEGER tlat, tlong, nlat, nlong
      REAL lat1, long1, lat2, long2, dlat, dlong
      INTEGER iunit
      REAL a(tlong, tlat)
      REAL slp(nlong, nlat), temp(nlong, nlat), q(nlong, nlat)
      REAL uten(nlong, nlat), vten(nlong, nlat), ztopo(nlong, nlat)

C     Local variables. 
      INTEGER i, j, k 

C     Parameters for setting up test fields
      REAL pref, plow, siglat, tref, qref
      REAL pi, zref, uref, vref
      PARAMETER (pref   = 1.01325E5)
      PARAMETER (plow   = -1.8E3   )
      PARAMETER (siglat = 3.0      )
      PARAMETER (tref   = 250.     )
      PARAMETER (qref   =   0.1E-3 )
      PARAMETER (pi     = 3.141592654)
      PARAMETER (zref   = 100.0    )
      PARAMETER (uref   =  30.     )
      PARAMETER (vref   = -30.     )
      REAL x0, y0

      x0 = (long1+long2)/2.
      y0 = (lat1 + lat2)/2.
      DO 1000 j = 1, nlat
        DO 1010 i = 1, nlong
          slp(i,j) = pref
     1  + plow * EXP(
     2    -( ( (long1+FLOAT(i-1)*dlong) -x0 )**2  + 
     3       ( (lat1 +FLOAT(j-1)*dlat ) -y0 )**2    )
     4       / 2. / siglat/siglat )
C         Now round to millibar resolution for comparisons with
C           the effects this could have on the MRFZ.
CD          slp(i,j) = 400.*FLOAT(INT(slp(i,j)/400.+0.5))
 1010   CONTINUE
 1000 CONTINUE

      DO 2000 j = 1, nlat
        DO 2010 i = 1, nlong
          temp(i,j) = tref
          q(i,j)    = qref
          ztopo(i,j) = zref*SIN(2.*pi*(j-1)*dlat/(lat2-lat1))
     1                     *SIN(2.*pi*(i-1)*dlong/(long2-long1))
          uten(i,j) = uref*ztopo(i,j)/zref
          vten(i,j) = vref*ztopo(i,j)/zref
CD          PRINT *,'ztopo ',i,j,ztopo(i,j)
 2010   CONTINUE
 2000 CONTINUE

      RETURN
      END
