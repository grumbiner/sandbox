      PROGRAM windst
C     Compute wind stress curl and divergence
C     Robert Grumbine 27 Sep 1995

      INTEGER maxlat, maxlon
      PARAMETER (maxlat =  90)
      PARAMETER (maxlon =  180)
      REAL taux(maxlon, maxlat), tauy(maxlon, maxlat)
      REAL curl(maxlon, maxlat), div(maxlon, maxlat)
      
      REAL radius, pi, degprd
      PARAMETER (radius = 6.371E6)
      PARAMETER (pi = 3.141592654)
      PARAMETER (degprd = 180./pi)

      REAL dlat, dlong, drlat, drlong
      INTEGER nlat, nlong
      CHARACTER*60 fname
      INTEGER i, j
      REAL maxmag

      PRINT *,'What is the latitude grid spacing in degrees?'
      READ (*,9001) dlat
      PRINT *,'What is the longitude grid spacing in degrees?'
      READ (*,9001) dlong
 9001 FORMAT (E13.6)
      drlat  = dlat/degprd
      drlong = dlong/degprd
      nlat   = INT(180./dlat)
      nlong  = INT(360./dlong)

      CALL sread(taux, tauy, nlong, nlat)
      PRINT *,'nlat, nlong, dlat, dlong',nlat, nlong, drlat, drlong
      PRINT *,'radius, taux(15,15), tauy(15,15), curl(15,15)',
     1  radius, taux(15,15), tauy(15,15), curl(15,15)
CD      PRINT *,'Time before call: ',LONG(362)
      CALL cs243(taux, tauy, radius, drlong, drlat, nlong, nlat, curl)
CD      PRINT *,'Time before call: ',LONG(362)
C     CALL ds241(taux, tauy, radius, drlong, drlat, nlong, nlat, div)
      
      PRINT *,'What do you want to call the curl file?'
      READ (*,9002) fname
      OPEN (20, FILE=fname, FORM='FORMATTED', STATUS='NEW')
C      PRINT *,'What do you want to call the divergence file?'
C      READ (*,9002) fname
C     OPEN (21, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      PRINT *,'nlat, nlong, dlat, dlong',nlat, nlong, drlat, drlong
      PRINT *,'radius, taux(15,15), tauy(15,15), curl(15,15)',
     1  radius, taux(15,15), tauy(15,15), curl(15,15)
      maxmag = 0.0
      DO 3000 j = 1, nlat
        DO 3010 i = 1, nlong
          maxmag = MAX(maxmag,ABS(curl(i,j)))
 3010   CONTINUE
 3000 CONTINUE
      PRINT *,'The curl magnitude extremum is',maxmag
      maxmag = 0.0
      DO 3100 j = 1, nlat
        DO 3110 i = 1, nlong
          maxmag = MAX(maxmag,ABS(taux(i,j)))
 3110   CONTINUE
 3100 CONTINUE
      PRINT *,'The taux magnitude extremum is',maxmag
      maxmag = 0.0
      DO 3200 j = 1, nlat
        DO 3210 i = 1, nlong
          maxmag = MAX(maxmag,ABS(tauy(i,j)))
 3210   CONTINUE
 3200 CONTINUE
      PRINT *,'The tauy magnitude extremum is',maxmag
      WRITE (20, 9003) ((curl(i,j)*1.E7,i=1, nlong),j=1, nlat)
C     WRITE (21, 9003) ((div (i,j)*1.E7,i=1, nlong),j=1, nlat)
 9002 FORMAT (A60)
 9003 FORMAT (15F8.4)
      CLOSE(20, STATUS='KEEP')
C     CLOSE(21, STATUS='KEEP')
      PAUSE
      END
