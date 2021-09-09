      PROGRAM nantest
C     Demo of working with a NAN -- NANs are neither greather than,
C       nor less than or equal, to 0
C     Robert Grumbine July 30, 2002
      REAL x, y

      y = -5.
      x = acos(y)

      IF (.NOT. ( (x .GT. 0) .OR. (x .LE. 0) ) ) THEN
        PRINT *,'neither ',x
      ELSE
        PRINT *,'else ',x
      ENDIF
 
      STOP
      END
