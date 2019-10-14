      PROGRAM forin
C     Create a theoretical exact time series for A, g(h), for
C       testing the retrieval of the correct series.
      IMPLICIT none
      REAL fref, dh, dt, h0
      INTEGER mult, days
      PARAMETER (h0   = 0.10)
      PARAMETER (fref = 0.10)
      PARAMETER (dh   = 0.01)
      PARAMETER (days = 8   )
      PARAMETER (mult = 1   )
      PARAMETER (dt   = dh/fref/days/mult)
      INTEGER nthick, nstep
      PARAMETER (nstep  = days/dt )
      PARAMETER (nthick = fref/dh*5+1)
      
      REAL a(nstep), g0(nstep)
      REAL g(nthick, nstep)
      REAL f(nthick), ia, hbar

      INTEGER i, j

      OPEN (10, FILE='gfile', FORM='UNFORMATTED', STATUS='UNKNOWN')
      OPEN (11, FILE='section', FORM='FORMATTED', STATUS='UNKNOWN')
      OPEN (12, FILE='forout', FORM='FORMATTED', STATUS='UNKNOWN')
      OPEN (13, FILE='bering', FORM='FORMATTED', STATUS='UNKNOWN')
      OPEN (14, FILE='freezout',  FORM='FORMATTED', STATUS='OLD')

      PRINT *,'nstep, nthick, dt, dh ', nstep, nthick, dt, dh
CD      PRINT *,'What size frac would you like?'
CD      READ (*,9003) frac
 9003 FORMAT (E13.6)

      CALL fset(f, fref, h0, dh, dt, nthick, 14)
      CALL gset(g, nthick, nstep, f, 14)
      CALL arset(a, nstep, 0.0)
      CALL intega(g, nthick, nstep, dh, 1, ia, h0, hbar)
      WRITE (12,9001) 1, a(1), ia, g0(1), hbar, -a(1)
      WRITE (13, 9005) a(1), -a(1)
 9005 FORMAT (F6.4, 2x, F9.6)
      REWIND (14)
      READ (14, 9009) g0(1)
C     The sign should be switched here for running from the forward
C       program.  Note too that the 9009 format is 37x,E13.6 for the
C       forward version.
      g0(1) = -g0(1)/f(1)/dt
 9009 FORMAT (24x,F8.4)
CD 9009 FORMAT (37x,E13.6)

      DO 1000 i = 2, nstep
        READ (14, 9009) g0(i)
        g0(i) = -g0(i)/f(1)/dt
        CALL aext(a, f, g0(i), dh, nthick, nstep, dt, i)
        CALL gext(g, f, nstep, nthick, dt, dh, g0(i), i)
        CALL intega(g, nthick, nstep, dh, i, ia, h0, hbar)
        WRITE (12,9001) i, a(i), ia, g0(i), hbar,-(a(i)-a(i-1))
        WRITE (13, 9005) a(i), -(a(i)-a(i-1))
        IF (MOD(i,10*mult) .EQ. 0) THEN
          DO 1001 j = 1, nthick
            WRITE (11,9002) i, j, g(j,i)
 1001     CONTINUE
        ENDIF
 1000 CONTINUE
 9001 FORMAT (I5, F6.3, 1X, F6.3, 1X, F8.3, 1X, F8.3, 1x, E13.6)
 9002 FORMAT (2I5,F8.3)

      WRITE (10) g

      CLOSE (10, STATUS='KEEP')
      CLOSE (11, STATUS='KEEP')

      STOP
      END
