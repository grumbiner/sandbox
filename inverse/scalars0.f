
      FUNCTION scalars(t)
      IMPLICIT none
      REAL t
      INTEGER nmon
      PARAMETER (nmon = 639)
      REAL alpha(0:nmon), r(0:nmon)

      INTEGER i
      REAL a0, r0, epsi, tau, scalars, sset

      SAVE alpha, r

      RETURN

      ENTRY sset(t)
      READ (10, 9001) (alpha(i), r(i), i=0, nmon)
CD      WRITE (*, 9001) (alpha(i), r(i), i=0, nmon)
 9001 FORMAT (F6.4, 2x, F9.6)

      sset = 0.
      RETURN
      
      ENTRY r0(t)
      IF (t .LT. 0.) THEN
        tau = t + FLOAT(nmon)
       ELSE
        tau = t
      ENDIF
      IF (tau .GT. nmon) THEN
        tau = nmon-1
      ENDIF
      epsi = tau - FLOAT(INT(tau))
      r0 = r(INT(tau)) + 
     1        epsi * (r(INT(tau)+1) - r(int(tau)))
      RETURN

      ENTRY a0(t)
      IF (t .LT. 0.) THEN
        tau = t + FLOAT(nmon)
       ELSE
        tau = t
      ENDIF
      IF (tau .GT. nmon) THEN
        tau = nmon-1
      ENDIF
      epsi = tau - FLOAT(INT(tau))
      a0 = alpha(INT(tau)) + 
     1        epsi * (alpha(INT(tau)+1) - alpha(int(tau)))
      RETURN
      END
