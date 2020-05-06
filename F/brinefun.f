      REAL FUNCTION brine(theta, salt)
C     Function to evaluate Frankenstein and Garner (1967) brine
C       relation.  Theta is the temperature, salt is the salinity.
C     Robert Grumbine 8 April 1994.
C     Note: Algorithm may not be implemented correctly.

      IMPLICIT none

      REAL theta, salt

      IF (theta .GE. 0.05009) THEN
        PRINT *,'You"ve melted'
        brine = 1000.0/salt
      ENDIF
      IF (theta .LT. -0.5 ) THEN
C       This is a BG addition/approximation to the original paper.
        brine = 50.09/ABS(theta)
      ENDIF
      IF (theta .LT. -2.06) THEN
        brine = -2.28 + 52.56/ABS(theta)
      ENDIF
      IF (theta .LT. -8.2) THEN
        brine = +0.930 + 45.917/ABS(theta)
      ENDIF
      IF (theta .LT. -22.9) THEN
        brine = 1.189+43.795/ABS(theta)
      ENDIF

      brine = brine*salt

      RETURN
      END
