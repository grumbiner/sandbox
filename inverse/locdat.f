      SUBROUTINE locdat(w, nday, i0, in, ndat)
C     Locdat locates the start and finish of a time series in which there
C       may be missing points. BG 1/22/94.
C     Bob Grumbine 8 April 1994.

      IMPLICIT none

      INTEGER nday, ndat, i0, in
      REAL w(nday)

      INTEGER i

      ndat = 0
      i0   = 0
      
      DO 1000 i = 1, nday
        IF (w(i) .NE. 0) THEN
          IF (i0 .EQ. 0) i0 = i
          in = i
        ENDIF
 1000 CONTINUE
 
      IF (i0 .EQ. 0) i0 = nday
      IF (in .LT. i0) in = i0 - 1
      ndat = in - i0 + 1
 
      RETURN
      END
