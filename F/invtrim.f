      SUBROUTINE invtrim(yin, yout, i0, in, nday)
C     Transfer series to start and finish with real data.
C     Bob Grumbine 2 June 1994.

      IMPLICIT none

      INTEGER i0, in, nday
      REAL yin(nday), yout(nday)

      INTEGER i

      DO 1000 i = i0, in
        yout(i-i0+1) = yin(i)
 1000 CONTINUE

      RETURN
      END
