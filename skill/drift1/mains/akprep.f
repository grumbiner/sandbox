      PROGRAM akprep
C     Prepare the forecast output for Alaska WSFO from the skiles1,2
C       forecast programs.
C     Bob Grumbine 11 April 1994.
C     Maximum number of points increased to 32*nskiles from 4*nskiles
C       (6624 from 832)  Bob Grumbine 22 March 1995.

      IMPLICIT none

      INTEGER nskile, ndays, npts, mult
      PARAMETER (nskile=207)
      PARAMETER (ndays =  6)
      PARAMETER (mult  = 32)

      INTEGER i, j, k, l, m, nday
      INTEGER skpt(nskile+1), skpt2(mult*nskile)
      REAL dd(nskile+1), ff(nskile+1)
      REAL dir(mult*nskile), dist(mult*nskile)
      REAL lat(mult*nskile), long(mult*nskile)
      REAL sum1, sum2, sdir, sdir2, sdist, sdist2
      REAL deldir

      CHARACTER*60 fname
      CHARACTER*80 header
      CHARACTER*1 trailer

      PRINT *,'What is the name of the skiles1 forecast file?'
      READ (*,9001) fname
      OPEN (10, FILE=fname, STATUS='OLD', FORM='FORMATTED')
      PRINT *,'What is the name of the skiles2 forecast file?'
      READ (*,9001) fname
      OPEN (11, FILE=fname, STATUS='OLD', FORM='FORMATTED')
 9001 FORMAT (A60)
      PRINT *,'What would you like to call the stats file?'
      READ (*,9001) fname
      OPEN (12, FILE=fname, STATUS='UNKNOWN', FORM='FORMATTED')
      PRINT *,'What would you like to call the comparison file?'
      READ (*,9001) fname
      OPEN (13, FILE=fname, STATUS='UNKNOWN', FORM='FORMATTED')
 9020 FORMAT (A80)

      DO 9000 nday = 1, ndays

        READ (11,9020) header 
        READ (11,9020) header 
        READ (11,9020) header 
        READ (11,9020) header 
        READ (11,9020) header 
        DO 1000 i = 1, nskile
          READ (11, *) skpt2(i), dir(i), dist(i)
 1000   CONTINUE
        i = nskile
        READ (11, 9020) header
 1001   CONTINUE
          i = i + 1
          READ (11,9020) header
          READ (header, 9007, ERR=1002)
     1         skpt2(i), long(i), lat(i), dir(i), dist(i)
          IF (skpt2(i) .NE. 0.0) GO TO 1001
 1002   CONTINUE
 9007   FORMAT (I4, 3x, 2F8.3, 2x, 2F6.1)

        npts = i - 1
 
        READ (11,9020) trailer

        READ (10,9020) header
        READ (10,9020) header
        READ (10,9020) header
        READ (10,9020) header
        READ (10,9020) header
        READ (10,9020) header
        DO 1100 i = 1, 52
          k = i + 52
          l = i + 104
          m = i + 156
          READ (10, 9002) skpt(i), dd(i), ff(i),
     1                    skpt(k), dd(k), ff(k),
     2                    skpt(l), dd(l), ff(l),
     3                    skpt(m), dd(m), ff(m)
 1100   CONTINUE
  
 9002   FORMAT (2x, I3, 3x, F5.1, 4x, F5.1, 8x,
     1              I3, 3x, F5.1, 4x, F5.1, 8x,
     2              I3, 3x, F5.1, 4x, F5.1, 8x,
     3              I3, 3x, F5.1, 4x, F5.1        )

        sdist = 0.
        sdir  = 0.
        sdist2 = 0.
        sdir2  = 0.
        sum1   = 0.
        sum2   = 0.
        DO 2000 j = 1, nskile
          sdist = sdist + dist(j) - ff(j)
          sum2 = sum2 + dist(j)
          sum1 = sum1 + ff(j)
          deldir = dir(j) - dd(j)
          IF (deldir .GT. 180.) THEN
            deldir = deldir - 360.
           ELSE IF (deldir .LT. -180.) THEN
            deldir = deldir + 360.
          ENDIF
          WRITE (12, 9004) j, dist(j), ff(j), dist(j)-ff(j),
     1       dir(j), dd(j), deldir
          sdir  = sdir  + deldir
          sdist2 = sdist2 + (dist(j) - ff(j))**2
          sdir2  = sdir2  + deldir**2
 2000   CONTINUE
        sdist = sdist / nskile
        sdir  = sdir  / nskile
        sum1  = sum1  / nskile
        sum2  = sum2  / nskile
        sdist2 = (sdist2 - nskile*sdist*sdist) / (nskile - 1)
        sdir2  = (sdir2  - nskile*sdir*sdir)   / (nskile - 1)
        PRINT *,'means, variances, distance, direction'
        WRITE (*,9003) sum2, sum1, sdist, sdist2, sdir, sdir2
        WRITE (12,9003) sum2, sum1, sdist, sdist2, sdir, sdir2
 9003   FORMAT (3x, 3F7.2, F6.2, F7.1, F9.0)
 9004   FORMAT (I4, 4(F5.1, 2x), F5.1, 2x, F6.1 )

 9000 CONTINUE

C       Write out the last day's information for transmission
        REWIND (11)
        READ (*,9001) fname
        WRITE (13,9001) fname
        READ (11,9001) fname
        READ (11,9001) fname
        READ (11,9001) fname
        WRITE (13,9001) fname

        WRITE (13,9010) 
 9010   FORMAT ('         New            Old ')
        WRITE (13,9011)
 9011   FORMAT ('       Dir   Dist    Dir   Dist ')

        DO 3000 j = 1, nskile
          WRITE (13,9005) j, dir(j), dist(j), dd(j), ff(j)
 9005     FORMAT (I4,4(2x,F5.1))
 3000   CONTINUE

        WRITE (13,9014)
 9014   FORMAT (' ')
        WRITE (13,9013)
 9013   FORMAT ('                Iceline points ')
        WRITE (13,9012)
 9012   FORMAT ('           Long    Lat     Dir    Dist ')
        DO 3100 j = nskile+1, npts
          WRITE (13,9006) skpt2(j), long(j), lat(j), dir(j), dist(j)
 3100   CONTINUE
 9006   FORMAT (I4,4x,2(2x,F5.1), 2x,2(2x,F5.1))

      STOP
      END
