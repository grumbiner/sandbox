      DOUBLE PRECISION FUNCTION fact(x)
C     Funtion to compute x!.
C     Robert Grumbine 2 May 1995

      IMPLICIT none

      INTEGER x, i

      IF (x .EQ. 0 .OR. x .EQ. 1) THEN
        fact = 1.D0
       ELSE
        fact = 1.D0
        DO 1000 i = 2, x
         fact = fact * DBLE(i)
 1000   CONTINUE
      ENDIF

      RETURN
      END
