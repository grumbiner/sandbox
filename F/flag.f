      SUBROUTINE flag(y, w, nday, ymin, ymax)
C     Flag (put zero weights in w) those points which are out of
C       range.  Useful for flagging bad data in a series.
C     Bob Grumbine 2 June 1994.

      IMPLICIT none

      INTEGER nday
      REAL ymin, ymax
      REAL y(nday), w(nday)

      INTEGER i

      DO 1000 i = 1, nday
        IF (y(i) .GT. ymax .OR. y(i) .LT. ymin) THEN
          w(i) = 0.0
         ELSE
          w(i) = 1.0
        ENDIF
 1000 CONTINUE

      RETURN
      END
