! Function to return a quality indicator for brute force,
!   general indication of whether a lake's annual temperature
!   cycle is plausible.
! Return codes: 
!  -1    -> Results are hopelessly implausible
!   0-10 -> Higher is better, but don't put much stock in small differences
! Current (13 May 2010) version only uses -1, 0, 1, and perhaps 2.
! Arguments:
!   ntemps is expected to be 365 (1 temperature per day)
!   nscores is expected to be 4 (4 checks are currently enables)
!   longitude is currently unused, but may be in the future
!   latitude  is used
!   temps is a real vector of temperatures, Celsius -- input
!   scores is a real vector of scores -- output
! Robert Grumbine 13 May 2010

      REAL FUNCTION lakeqc(lat, lon, temps, ntemps, scores, nscores)
      IMPLICIT none
      INTEGER ntemps, nscores
      REAL temps(ntemps), scores(nscores)
      REAL lat, lon
      REAL tmin, tmax, tfreeze, thot

      scores(1) = tmin(lat, temps)
      scores(2) = tmax(lat, temps)
      scores(3) = tfreeze(lat, temps)
      scores(4) = thot(lat, temps)
      lakeqc = MINVAL(scores)
 
      RETURN
      END

      REAL FUNCTION tmin(lat, temps)
      IMPLICIT none
      REAL lat, temps(365)
      REAL tmpmin
      tmpmin = MINVAL(temps)
      IF (tmpmin .GT. 0 .AND. lat .GT. 45.) THEN
        tmin = -1.0
      ELSE IF (tmpmin .GT. 0 .AND. lat .GT. 40) THEN
        tmin = 0.0
      ELSE 
        tmin = 1.0
      ENDIF 
      RETURN
      END

      REAL FUNCTION tmax(lat, temps)
      IMPLICIT none
      REAL lat, temps(365)
      REAL tmpmax 
      tmpmax = MAXVAL(temps)
      IF (tmpmax .GT. 32) THEN
        tmax = -1.0
      ELSE IF (tmpmax .GT. 30) THEN
        tmax =  0.0
      ELSE IF (tmpmax .LT. 20) THEN
        tmax = -1.0
      ELSE IF (tmpmax .LT. 25 .AND. lat .LT. 55) THEN
        tmax =  0.0
      ELSE
        tmax = 1.0
      ENDIF
      RETURN
      END

      REAL FUNCTION tfreeze(lat, temps)
! Freezing season should increase with latitude, being about 0 at
!   40 N, increasing to 10 months around 65 N (great bear/ great slave)
!   Reference lake, Crystal Lake, IN, 41.235 N, 85.985 W spent 0-30 days
!   frozen in climatological period 1966-1986 (i.e., my youth there).
!   Markedly less in 1979-2009.
      IMPLICIT none
      REAL lat, temps(365)
      INTEGER count, i
      REAL tempor

      count = 0
      tfreeze = 2.0
      DO i = 1, 365
        IF (temps(i) .LE. 0.01) count = count + 1
      ENDDO
      IF (count .GT. 0 .AND. lat .LT. 40) THEN
        tfreeze = 0.0
      ENDIF
      IF (count .GT. 0 .AND. lat .LT. 35) THEN
        tfreeze = -1.0
      ENDIF
      IF (lat .GT. 40) THEN
        tempor = count - (300. / 25.)*(lat - 40.)
        IF (ABS(tempor) .GT. 50) THEN
          tfreeze = -1.0
        ELSE IF (ABS(tempor) .GT. 25) THEN
          tfreeze = 0.0
        ELSE
          tfreeze = 1.0
        ENDIF
      ENDIF
      IF (tfreeze .EQ. 2.0) THEN
        PRINT *,'encountered unexpected case in tfreeze, count lat = ',
     1                count, lat
      ENDIF

      RETURN
      END

      REAL FUNCTION thot(lat, temps)
! Now consider number of 'hot' days on the lake (warmer than 25 C).  
!   Figure that low latitude (below 30 -- Gulf Coast) lakes are always
!   hot, and high latitude lakes (above 65 N) are never hot.  Reference
!   lake, Crystal Lake, IN, 41.235 N, 85.985 W spent 30-50 days
!   this warm in the climatological period 1966-1986 (i.e., my youth there). 
! There are 3 branches here -- low latitude lakes (below 30 N),
!   mid-latitude lakes (30-42 N), and high latitude (above 42 N).  The
!   length of the warm season in lake temperatures does not seem to be
!   plausibly fit by a single linear function.  Two linear functions are
!   used, but probably this should be some nonlinear function.
      IMPLICIT none
      REAL lat, temps(365)
      INTEGER count, i
      REAL tempor
      count = 0
      DO i = 1, 365
        IF (temps(i) .GE. 25) count = count + 1
      ENDDO
      IF (lat .LE. 30) THEN
        IF (count .LE. 335) THEN
          thot = -1.0
        ELSE IF (count .LE. 365) THEN
          thot = 0.0
        ELSE
          thot = 1.0
        ENDIF
        RETURN
      ENDIF

      IF (lat .LE. 42 ) THEN
        tempor = (365 - (365/20)*(lat - 30)) - count
      ELSE
        tempor = (40 - 40*(lat - 42)/23) - count
      ENDIF
      IF (ABS(tempor) .GE. 50) THEN
        thot = -1.0
      ELSE IF (ABS(tempor) .GE. 25) THEN
        thot =  0.0
      ELSE
        thot = 1.0
      ENDIF

      RETURN
      END
