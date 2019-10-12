      SUBROUTINE trimmed(y, z, w, i0, in, nday)
C     Transfer series to start and finish with real data.
C     Robert Grumbine
C     Last Modified 27 September 1995
      IMPLICIT none

      INTEGER i0, in, nday
      REAL y(nday), z(nday), w(nday), w2(3000)

      INTEGER i
      SAVE 

      DO 1000 i = i0, in
        z(i-i0+1) = y(i)
        w2(i-i0+1) = w(i)
 1000 CONTINUE
      DO 2000 i = 1, in-i0+1
        w(i) = w2(i)
 2000 CONTINUE

      RETURN
      END
