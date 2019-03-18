      SUBROUTINE mapsub(inter, lat1, long1, lat2, long2)
C     Subroutine to read in data, and plot points.

      REAL lat, long
      INTEGER subset, cmdid
      LOGICAL inter
      REAL lat1, long1, lat2, long2
      REAL latmax, latmin, lonmax, lonmin

      REAL arcdis, dist, mdist, ollat, ollong
      REAL latm(40000), longm(40000)
      CHARACTER*60 fname
      INTEGER*2 color 
      INTEGER i, n
CHP$EMA latm, longm

      IF (inter)
     1  PRINT *,'What is the name of the data file?'
      READ (*,9001) fname

      OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')

      IF (inter)
     1PRINT *,'What is the minimum spacing you want between points?'
      READ (*,9002) mdist

      n = 0
      ollat  = 0.0
      ollong = 0.0
C     latmin = AMIN1(lat1, lat2)
C     latmax = AMAX1(lat1, lat2)
C     lonmin = AMIN1(long1, long2)
C     lonmax = AMAX1(long1, long2)

      PRINT *,'Reading data'
 2000 CONTINUE
      READ (10, END=1000) lat, long, subset, cmdid
C     PRINT *,lat, long, subset, cmdid
C       First check to see that the point is inside the window for plotting.
C       The method below does not work.
C       IF (lat .GT. latmax) THEN
C         lat = latmax
C        ELSE IF (lat .LT. latmin) THEN
C         lat = latmin
C       ENDIF
C       IF (long .GT. lonmax) THEN
C         long = lonmax
C        ELSE IF (long .LT. lonmin) THEN
C         long = lonmin
C       ENDIF
         
C       Now check to see if the point is at least the minimum distance away. 
        dist = arcdis(lat, long, ollat, ollong)
        IF (dist .GE. mdist .OR. cmdid .EQ. 3) THEN
          n = n + 1
          latm(n)  = lat
          longm(n) = long
          ollat    = lat
          ollong   = long
        ENDIF
      GO TO 2000

 1000 CONTINUE

      IF (inter)
     1PRINT *,'What color do you want this part of the map in?'
      READ (*,9003) color
      CALL jcolr(color)

      PRINT *,'Calling supfst'
      CALL supfst( latm(1), longm(1) )

      PRINT *,'Loop for supvec'
      DO 3000 i = 2, n
        CALL supvec( latm(i), longm(i) )
 3000 CONTINUE

      CLOSE (10, STATUS='KEEP')

      CALL JMCUR

 9001 FORMAT (A60)

 9002 FORMAT (E13.6)

 9003 FORMAT (I1)

      RETURN 
      END
