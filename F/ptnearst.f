      INTEGER FUNCTION nearest(gridx, gridy, nuse, i, j)
C     Function to find the nearest point on the ice line relative
C       to a given point.
C     Bob Grumbine 8 April 1994.

      IMPLICIT none

      INTEGER i, j, nuse
      REAL gridx(nuse), gridy(nuse)

      INTEGER k, pt
      REAL disto, distn

      k = 1
      pt = 1
      disto = SQRT((gridx(k)-i)**2+(gridy(k)-j)**2)
      DO 1000 k = 2, nuse
        distn = SQRT((gridx(k)-i)**2+(gridy(k)-j)**2)
        IF (distn .LT. disto) THEN
          disto = distn
          pt = k
        ENDIF
 1000 CONTINUE

      nearest = pt

      RETURN
      END
