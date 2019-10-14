      PROGRAM trim 
C     Given a whole bunch of buoy reports, trim down the file so that
C       no two buoys are within toler km of each other.
C     Useful for reducing noise in plotting.
C     Bob Grumbine 21 April 1994.

      IMPLICIT none

      REAL lat(50000), lon(50000)
      REAL x(50000), y(50000)
      INTEGER k, nbuoy
      CHARACTER*60 fname
      REAL r, toler, arcdis
      PARAMETER (toler = 60.0)
      LOGICAL near
      INTEGER i, j
 
      READ (*,9001) fname
      OPEN (12, FILE=fname, FORM='FORMATTED', STATUS='OLD')
     
      i = 0
C     First pass, trim out reports which are near the previous report
 1000 CONTINUE
        READ (12, 9002, END=1100) lat(i+1), lon(i+1)
        lon(i+1) = 360. - lon(i+1)
C       Convert from degrees west of buoys to degrees east of
C       rest of world
        IF (i+1 .GE. 2) THEN
          IF (arcdis(lon(i+1), lat(i+1), lon(i), lat(i)) .GT. toler) 
     1     THEN
            i = i + 1
            GO TO 1000
           ELSE
            GO TO 1000
          ENDIF
         ELSE
          i = i + 1
          GO TO 1000
        ENDIF
 1100 CONTINUE
      nbuoy = i
      PRINT *,'nbuoy = ', nbuoy

C     Now do the ugly N**2 thing, and trim out reports which are
C      within toler of _any_ other report.
      i = 1
      x(1) = lon(1)
      y(1) = lat(1)
      DO 2000 j = 2, nbuoy
        near = .FALSE.
        DO 2100 k = 1, i
          IF (arcdis(x(k), y(k), lon(j), lat(j)) .LT. toler) THEN
            near = .TRUE.
          ENDIF
 2100   CONTINUE
        IF (.NOT. near) THEN
          i = i + 1
          x(i) = lon(j)
          y(i) = lat(j)
        ENDIF
 2000 CONTINUE
      PRINT *,'Buoys after second trim ', i 

      DO 3000 j = 1, i
        WRITE (*,9002) y(j), 360. - x(j)
 3000 CONTINUE
 
 9001 FORMAT (A60)
 9002 FORMAT (7x, 9x, F5.1, F6.1)

      STOP
      END
