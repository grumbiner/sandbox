      SUBROUTINE ledzro(x, name)
C     Given a number (integer), add a leading zero for those which
C       are less than 10.  Useful for forming date groups.
C     Bob Grumbine 6 April 1994.

      IMPLICIT none

      CHARACTER*2 name
      INTEGER x

      IF (x .LT. 10) THEN
        WRITE(name,9001) x
       ELSE
        WRITE (name, 9002) x
      ENDIF

 9001 FORMAT ('0',I1)
 9002 FORMAT (I2)

      RETURN
      END
