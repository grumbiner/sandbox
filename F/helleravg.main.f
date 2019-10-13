      PROGRAM windst
C     Compute the annual average wind stress in the 
C       Hellerman and Rosenstein data set.
C     Robert Grumbine 27 September 1994

      IMPLICIT none

      INTEGER maxlat, maxlon
      PARAMETER (maxlat =  90)
      PARAMETER (maxlon =  180)
      REAL taux(maxlon, maxlat), tauy(maxlon, maxlat)
      REAL sumx(maxlon, maxlat), sumy(maxlon, maxlat)
      
      REAL radius, pi, degprd
      PARAMETER (radius = 6.371E6)
      PARAMETER (pi = 3.141592654)
      PARAMETER (degprd = 180./pi)

      REAL dlat, dlong, drlat, drlong
      INTEGER nlat, nlong
      CHARACTER*60 fname
      INTEGER i, j, k
      REAL maxmag

      dlat   = 2.0
      dlong  = 2.0
      drlat  = dlat/degprd
      drlong = dlong/degprd
      nlat   = INT(180./dlat)
      nlong  = INT(360./dlong)
      fname = "strxavg"
      OPEN (20, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      fname = "stryavg"
      OPEN (21, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      fname = "stress.two"
      OPEN (10, FILE=fname, FORM='FORMATTED', STATUS='OLD')

      DO 100 j = 1, nlat
        DO 200 i = 1, nlong
          sumx(i, j) = 0.0
          sumy(i, j) = 0.0
 200    CONTINUE
 100  CONTINUE

      DO 1000 k = 1, 12
        CALL sread(taux, tauy, nlong, nlat)
        DO 3200 j = 1, nlat
          DO 3210 i = 1, nlong
            sumx(i, j) = sumx(i,j)+taux(i,j)
            sumy(i, j) = sumy(i,j)+tauy(i,j)
 3210     CONTINUE
 3200   CONTINUE
 1000 CONTINUE
       DO 4000 j = 1, nlat
        DO 4100 i = 1, nlong
          sumx(i, j) = sumx(i,j)/12.
          sumy(i, j) = sumy(i,j)/12.
 4100   CONTINUE
 4000 CONTINUE

      WRITE (20) sumx
      WRITE (21) sumy

      CLOSE(20, STATUS='KEEP')
      CLOSE(21, STATUS='KEEP')

 9002 FORMAT (15F8.4)

      END
