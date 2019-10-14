
      FUNCTION scalars(t)
      REAL t
      INTEGER nmon 
      PARAMETER (nmon = 12)
      REAL alpha(0:nmon), r(0:nmon)
      INTEGER i
      REAL r0, a0, epsi, x, yp, ym

      SAVE alpha, r

      RETURN

      ENTRY sset(t)
      READ (10, 9001) (alpha(i), r(i), i=0, nmon)
      WRITE (*, 9001) (alpha(i), r(i), i=0, nmon)
 9001 FORMAT (F4.2, 2x, F6.3)

      sset = 0.
      RETURN

      ENTRY r0(t)
      IF (t .LT. 0.) THEN
        t = t + FLOAT(nmon)
      ENDIF
      epsi = t - FLOAT(INT(t))
      x = r(INT(t)) + epsi * ( r(INT(t)+1) - r(INT(t)) )
      
      r0 = x
      RETURN

      ENTRY a0(t)
      IF (t .LT. 0.) THEN
        t = t + FLOAT(nmon)
      ENDIF
      epsi = t - FLOAT(INT(t))
      a0 = alpha(INT(t)) + epsi * (alpha(INT(t)+1) - alpha(int(t)))
      RETURN
      END
