      PROGRAM for4
C     Create a theoretical exact time series for A, g(h), for
C       testing the retrieval of the correct series.
      IMPLICIT none
      INCLUDE "inv.inc"

      REAL frac

      REAL a(nstep), g0(nstep)
      REAL g(nthick, nstep)
      REAL f(nthick), ia, hbar, rnew

      INTEGER i, j
      LOGICAL yes, new
C
C     Inputs: 
C       frac: fraction of the remaining ocean cover which begins freezing
C             in the next day.
C     Outputs:
C       10: gfile -- unformatted binary of g(h,t) for every time step.
C       11: section - formatted ASCII of g(h,t) every 10 time steps.
C       12: forout  - formatted, t, A, ?, g0, hbar, ?.
C       13: bering  - formatted, A, r0.
C

      PRINT *,'nstep, nthick, dt, dh ', nstep, nthick, dt, dh
      PRINT *,'Are you running from a variational improvement?'
      IF (yes(.FALSE., 5)) THEN
        new = .FALSE.
        OPEN (14, FILE='freezout', FORM='FORMATTED', STATUS='OLD')
       ELSE
        new = .TRUE.
      ENDIF
      IF (new) THEN
        PRINT *,'What size frac would you like?'
        READ (*,9003) frac
        frac = frac*dt
      ENDIF
 9003 FORMAT (E13.6)

      OPEN (10, FILE='gfile', FORM='UNFORMATTED', STATUS='UNKNOWN')
      OPEN (11, FILE='section', FORM='FORMATTED', STATUS='UNKNOWN')
      OPEN (12, FILE='forout', FORM='FORMATTED', STATUS='UNKNOWN')
      OPEN (13, FILE='bering', FORM='FORMATTED', STATUS='UNKNOWN')

      CALL fset(f, fref, h0, dh, dt, nthick, 14)
      CALL gset(g, nthick, nstep, f, 14)
      CALL rarst2(a, nstep, 1, 0.0)
      CALL intega(g, nthick, nstep, dh, 1, ia, h0, hbar)
      IF (new) THEN
        g0(1) = (1-ia)/dh * frac
       ELSE
        READ (14, 9006) rnew
CD        g0(1) = -rnew/f(1)/dt
        g0(1) = -rnew/f(1)
      ENDIF
      WRITE (12,9001) 1, a(1), ia, g0(1), hbar, -g0(1)*f(1)
      WRITE (13, 9005) a(1), -g0(1)*f(1)
 9005 FORMAT (F6.4, 2x, F9.6)
 9006 FORMAT (24X, F10.6)


      DO 1000 i = 2, nstep
        IF (new) THEN
          g0(i) = (1-ia)/dh * frac
         ELSE
          READ (14, 9006) rnew
CD          g0(i) = -rnew/f(1)/dt
          g0(i) = -rnew/f(1)
        ENDIF

        CALL aext(a, f, g0(i), dh, nthick, nstep, dt, i)
        CALL gext(g, f, nstep, nthick, dt, dh, g0(i), i)
        CALL intega(g, nthick, nstep, dh, i, ia, h0, hbar)

        WRITE (12,9001) i, a(i), ia, g0(i), hbar,-(a(i)-a(i-1))/dt
        WRITE (13, 9005) a(i), -(a(i)-a(i-1))/dt
        IF (MOD(i,INT(10*mult) ) .EQ. 0) THEN
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
      CLOSE (12, STATUS='KEEP')
      CLOSE (13, STATUS='KEEP')
      IF (.NOT. new) CLOSE (14, STATUS='KEEP')

      STOP
      END
