      LOGICAL FUNCTION nearany (sklat, sklon, nskile, toler, 
     1    rlat, rlong, which)
C     Given a table of latitudes and longitudes (sklat, sklon) find
C       if the given point (rlat, rlong) is within toler km of any
C       of them.  If so, also return which element (which) it was.
C     Bob Grumbine 4 April 1995
      IMPLICIT none

      INTEGER which, nskile
      REAL rlat, rlong, sklat(nskile), sklon(nskile)
      REAL toler, arcdis
      INTEGER i
 
      which = 0
      i = 1
      nearany = .FALSE.

 1000 CONTINUE
        IF (ABS(arcdis(sklon(i), sklat(i), rlong, rlat)) .LE. toler) 
     1   THEN
          which = i
          nearany = .TRUE.
          RETURN
        ENDIF
        i = i + 1
        IF (i .LE. nskile) GO TO 1000

 
      RETURN
      END
