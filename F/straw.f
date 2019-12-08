      PROGRAM straw
C     Perform a straw man inversion of concentration and growth rate
C       time series.
C     Bob Grumbine 2/94.

      IMPLICIT none

      REAL fref, dh, dt, h0
      INTEGER outfreq, days
      PARAMETER (h0   = 0.10)
      PARAMETER (fref = 0.10)
      PARAMETER (dh   = 0.01)
      PARAMETER (days = 8   )
      PARAMETER (outfreq = 1   )
      PARAMETER (dt   = dh/fref/2.)

      INTEGER nthick, nstep
      PARAMETER (nstep  = days/dt )
      PARAMETER (nthick = fref/dh*5+1)
      
      REAL a(nstep), g0(nstep), rnew, r0, a0, r0dot 
      REAL g(nthick, nstep)
      REAL f(nthick), ia, hbar

      INTEGER ier, i, j
      REAL sset
      EXTERNAL a0, r0, r0dot

      OPEN (14, FILE='gfile', FORM='UNFORMATTED', STATUS='UNKNOWN')
      OPEN (11, FILE='section', FORM='FORMATTED', STATUS='UNKNOWN')
      OPEN (12, FILE='forout', FORM='FORMATTED', STATUS='UNKNOWN')
      OPEN (13, FILE='bering', FORM='FORMATTED', STATUS='UNKNOWN')
      OPEN (10, FILE='forin.dat',  FORM='FORMATTED', STATUS='OLD')

      PRINT *,'nstep, nthick, dt, dh ', nstep, nthick, dt, dh
 9003 FORMAT (E13.6)

      ier = sset(0.)
      IF (ier .NE. 0) THEN
        PRINT *,'Could not set up scalars file'
        STOP
      ENDIF

CD      PRINT *,'Have read in the scalars'

      CALL fset(f, fref, h0, dh, dt, nthick, 14)
      CALL gset(g, nthick, nstep, f, 14)
      CALL rarst2(a, nstep, 1, 0.0)
      CALL intega(g, nthick, nstep, dh, 1, ia, h0, hbar)
      WRITE (12,9001) 1, a(1), ia, g0(1), hbar, -a(1)
      WRITE (13, 9005) a(1), -a(1)
 9005 FORMAT (F6.4, 2x, F9.6)
      REWIND (14)
      
      PRINT *,' f(1), 1/f1/dt ', f(1), 1/f(1)/dt
CD      PRINT *,'Computing rnew', 0., dt, a0(0.)
      rnew = -r0dot(a0, 0., dt)
C     The sign should be switched here for running from the forward
C       program.  Read in from the forward's bering file.
      g0(1) = -rnew/f(1)/dt
 9009 FORMAT (8x,F9.6)

      DO 1000 i = 2, nstep
CD        PRINT *,'Computing rnew'
CD        PRINT *,'Computing rnew', FLOAT(i-1), dt, a0(FLOAT(i-1))
        rnew = -r0dot(a0, FLOAT(i-1), dt)
        g0(i) = -rnew/f(1)/dt
        CALL aext(a, f, g0(i), dh, nthick, nstep, dt, i)
        CALL gext(g, f, nstep, nthick, dt, dh, g0(i), i)
        CALL intega(g, nthick, nstep, dh, i, ia, h0, hbar)
        WRITE (12,9001) i, a(i), ia, g0(i), hbar,-(a(i)-a(i-1))
        WRITE (13, 9005) a(i), -(a(i)-a(i-1))
        IF (MOD(i,outfreq) .EQ. 0) THEN
          DO 1001 j = 1, nthick
            WRITE (11,9002) i, j, g(j,i)
 1001     CONTINUE
        ENDIF
 1000 CONTINUE
 9001 FORMAT (I5, F6.3, 1X, F6.3, 1X, F8.3, 1X, F8.3, 1x, E13.6)
 9002 FORMAT (2I5,F8.3)

      WRITE (14) g

      CLOSE (10, STATUS='KEEP')
      CLOSE (11, STATUS='KEEP')
      CLOSE (12, STATUS='KEEP')
      CLOSE (13, STATUS='KEEP')
      CLOSE (14, STATUS='KEEP')

      STOP
      END
