      PROGRAM sk1verf
C     Verify the drift forecast model(s).
C     Input is: 1) The location of the skiles points (for both 
C                  skile1 and skile2 programs)
C               2) The skile(1/2) forecast file(s)
C               3) The drifting buoy location file
C     Internal data are:
C               1) Names of the months
C               2) Number of days per month
C               3) 
      IMPLICIT none

      INTEGER nskile
      PARAMETER (nskile = 207)
      REAL lat(3*nskile), long(3*nskile)
      CHARACTER*3 mon(12)
      INTEGER days(12)
      DATA mon /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1          'SEP','OCT','NOV','DEC'/
      DATA days /31,28,31,30,31,30,31,31,30,31,30,31/
      
      CHARACTER*60 fname, dummy
      INTEGER mm, dd, yy
      INTEGER ntot, pmatch(6*nskile), nmatch(6*nskile), date(6*nskile)
      REAL mlat(6*nskile), mlong(6*nskile)
      REAL toler
      PARAMETER (toler = 100.0)
      REAL dir(nskile+1), dist(nskile+1), flat, flong
      INTEGER i, j, npt, nday, ptr
      CHARACTER*7 id(6*nskile)
      INTEGER totmatch

C====================================================================
C     Open the skile2 file and extract the verification date info.      
      PRINT *,'What is the name of the skile1 file?'
      READ (*,9001) fname
      OPEN(10, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      READ (*,9002) yy, mm, dd
      WRITE (*,9002) yy, mm, dd
      REWIND (10)
 9001 FORMAT (A60)
 9002 FORMAT (I2,I2,I2)
      
C     Open the buoy file
      PRINT *,'What is the name of the buoy file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='OLD')
      
C     Get the skiles points information.
      CALL getsk(lat, long, nskile, 12)

C     Get the locations of the N. Hem ice edge points
C       -- unneeded for skile1 forecasts
      ntot = nskile
 
C     Match up the locations of the points versus the buoys
      CALL matchpt(lat, long, ntot, mm, dd, yy, pmatch, nmatch, 
     1      id, date, mlat, mlong, toler, totmatch)
      PRINT *,'Totmatch = ',totmatch
      IF (totmatch .EQ. 0) THEN
        PRINT *,'Failed to find any matches'
        STOP
      ENDIF

      DO 1000 i = 1, ntot
        IF (nmatch(i) .NE. 0) THEN
          DO 1100 j = 1, nmatch(i)
            npt = pmatch(i) + j -1
            WRITE (*,9003) i, lat(i), long(i), 
     1                      id(npt), date(npt), mlat(npt), mlong(npt)
 1100     CONTINUE
        ENDIF
 1000 CONTINUE 
    
 9003 FORMAT (I3, 2F9.3, A7, I10, 2F9.3)
      PRINT *,'Date now = ',mm,dd,yy
C================================================================
C     Now begins the real match up and output:
C     Loop over days:
      DO 2000 nday = 1, 6
        PRINT *,'Working on forecast day ',nday
C       Read forcast information
        CALL getfor(dir, dist, nskile, ntot, 10)
CD        PRINT *,'Returned from getfor, day ',nday
C       Loop over points:
        DO 2100 npt = 1, ntot
          IF (nmatch(npt) .NE. 0) THEN
            DO 2200 j = 1, nmatch(npt)
              ptr = pmatch(npt)+j-1
              CALL find(mm, dd, yy, nday, id(ptr), flat, flong)
              WRITE (*,9005) npt, lat(npt), long(npt), 
     1           dir(npt), dist(npt), 
     2           id(ptr), flat, flong
 2200       CONTINUE
          ENDIF
 2100   CONTINUE
 2000 CONTINUE

 9005 FORMAT (I3, 2F9.3, 2F9.3, A7, 2F9.3)

      STOP
      END 
