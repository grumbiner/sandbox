      FUNCTION scalars(t)
      IMPLICIT none
      REAL t
      INTEGER nstep
      PARAMETER (nstep = 160)
      REAL alpha(0:nstep-1), r(0:nstep-1)

      INTEGER i
      REAL a0, r0, epsi, tau, scalars, sset

      SAVE alpha, r

      RETURN

      ENTRY sset(t)
      READ (10, 9001,ERR=9999) (alpha(i), r(i), i=0, nstep-1)
 9001 FORMAT (F6.4, 2x, F9.6)

      sset = 0.
      RETURN
 9999 CONTINUE
      sset = 1.0
      RETURN
      
      ENTRY r0(t)
      IF (t .LT. 0.) THEN
        tau = t + FLOAT(nstep-1)
       ELSE
        tau = t
      ENDIF
      IF (tau .GT. nstep-1) THEN
        tau = nstep-2
      ENDIF
      epsi = tau - FLOAT(INT(tau))
      r0 = r(INT(tau)) + 
     1        epsi * (r(INT(tau)+1) - r(int(tau)))
      RETURN

      ENTRY a0(t)
      IF (t .LT. 0.) THEN
        tau = t + FLOAT(nstep-1)
       ELSE
        tau = t
      ENDIF
      IF (tau .GT. nstep-1) THEN
        tau = nstep-2
      ENDIF
      epsi = tau - FLOAT(INT(tau))
      a0 = alpha(INT(tau)) + 
     1        epsi * (alpha(INT(tau)+1) - alpha(int(tau)))
      RETURN
      END
