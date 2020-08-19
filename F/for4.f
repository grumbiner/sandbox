      PROGRAM for4
C     Create a theoretical exact time series for A, g(h), for
C       testing the retrieval of the correct series.
      IMPLICIT none
      INCLUDE "inv.inc"

      REAL frac

      REAL a(nx, ny, nstep), g0(nx, ny, nstep)
      REAL g(nthick, nx, ny, nstep)
      REAL f(nthick, nx, ny), ia(nx, ny), hbar(nx, ny), rnew(nx, ny)
      REAL u(nx, ny), v(nx, ny)
      REAL t1, t2

      INTEGER i, j, k, l
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
CD        frac = frac/dt
        PRINT *,'frac = ',frac
      ENDIF
 9003 FORMAT (E13.6)

      OPEN (10, FILE='gfile', FORM='UNFORMATTED', STATUS='UNKNOWN')
      OPEN (11, FILE='section', FORM='FORMATTED', STATUS='UNKNOWN')
      OPEN (12, FILE='forout', FORM='FORMATTED', STATUS='UNKNOWN')
      OPEN (13, FILE='bering', FORM='FORMATTED', STATUS='UNKNOWN')

CD      PRINT *,'Opened files'

      CALL fset(f, fref, h0, dh, dx, dy, dt, nx, ny, nthick, 14)
      CALL gset(g, nx, ny, nthick, nstep, f, 14)
      CALL uset(u, v, nx, ny, 14)
      CALL rarst2(a, nstep*nx*ny, 1, 0.0)
      CALL rarst2(g0, nstep*nx*ny, 1, 0.0)
      CALL intega(g, nx, ny, nthick, nstep, dh, 1, ia,
     1                     h0, hbar)


      IF (new) THEN
        DO 100 j = 1, ny
        DO 100 i = 1, nx
          IF (f(1, i, j) .GT. 0) THEN
CD            g0(i, j, 1) = (1-a(i, j, 1))/dh * frac
            g0(i, j, 1) = (1-ia(i, j))/dh * frac 
CD            PRINT *,'ia = , g0 = ',ia(i,j), g0(i,j, 1)
            g0(i, j, 1) = 0.0
            
          ELSE
            g0(i,j,1) = 0.0
          ENDIF
  100   CONTINUE
       ELSE
        DO 200 j = 1, ny
        DO 200 i = 1, nx
          READ (14, 9006) rnew(i, j)
          g0(i, j, 1) = -rnew(i, j)/f(1, i, j)
  200   CONTINUE
      ENDIF

        i = 1
        CALL gext(g, f, u, v, nx, ny, nstep, nthick, dt, dh, 
     1             dx, dy, g0, i)
        CALL aext(a, f, u, v, g0, g, dh, nx, ny, nthick, nstep, 
     1             dx, dy, dt, i)
        CALL intega(g, nx, ny, nthick, nstep, dh, i, ia,
     1               h0, hbar)

CD      DO 300 j = 1, ny
CD      DO 300 i = 1, nx
        i = nx/2
        j = ny/2
        WRITE (12,9001) 1, i, j, a(i, j, 1), ia(i, j), g0(i, j, 1), 
     1    hbar(i, j), g0(i, j, 1)*f(1, i, j), u(i,j), v(i,j)
CD        WRITE (13, 9005) a(i, j, 1), g0(i, j, 1)*f(1, i, j)
  300 CONTINUE
 9005 FORMAT (F6.4, 2x, F9.6, 2E13.6)
 9006 FORMAT (24X, F10.6)

      DO 1000 i = 2, nstep
        IF (new) THEN
          DO 1100 l = 1, ny
          DO 1100 k = 1, nx
            IF (f(1, k, l) .GT. 0.) THEN
CD              g0(k, l, i) = (1.-a(k, l, i-1))/dh * frac
              g0(k, l, i) = ((1.-ia(k, l))/dh) * frac 
             g0(i, j, 1) = 0.0
            ELSE
              g0(k, l,i) = 0.0
            ENDIF
 1100     CONTINUE
         ELSE
          DO 1200 l = 1, ny
          DO 1200 k = 1, nx
            READ (14, 9006) rnew(k, l)
            g0(k, l, i) = -rnew(k, l)/f(1, k, l)
 1200     CONTINUE
        ENDIF

        CALL gext(g, f, u, v, nx, ny, nstep, nthick, dt, dh, 
     1             dx, dy, g0, i)
        CALL aext(a, f, u, v, g0, g, dh, nx, ny, nthick, nstep, 
     1             dx, dy, dt, i)
        CALL intega(g, nx, ny, nthick, nstep, dh, i, ia,
     1               h0, hbar)

CD        DO 1300 l = 1, ny
CD        DO 1300 k = 1, nx
        l = ny/2
        k = nx/2
          WRITE (12,9001) i, k, l, a(k, l, i), ia(k, l), g0(k, l,i ), 
     1              hbar(k, l), (a(k, l, i)-a(k, l, i-1))
     2    , u(k, l), v(k, l)
        WRITE (13, 9005) a(k, l, i), (a(k, l, i)-a(k, l, i-1))
     2    , u(k, l), v(k, l)
 1300   CONTINUE

        IF (MOD(i,INT(1) ) .EQ. 0) THEN
CD          DO 1400 l = 1, ny
CD          DO 1400 k = 1, nx
            l = ny/2
            k = nx/2
            DO 1001 j = 1, nthick
              WRITE (11,9002) i, j, k, l, g(j, k, l, i)
 1001       CONTINUE
 1400     CONTINUE
        ENDIF

 1000 CONTINUE
 9001 FORMAT (3I4, F6.3, F6.3, F8.3, F8.3, E13.6, 2E11.4)
 9002 FORMAT (4I4,F8.3)

CD      WRITE (10) g
CD      WRITE (10) a

      CLOSE (10, STATUS='KEEP')
      CLOSE (11, STATUS='KEEP')
      CLOSE (12, STATUS='KEEP')
      CLOSE (13, STATUS='KEEP')
      IF (.NOT. new) CLOSE (14, STATUS='KEEP')

      STOP
      END
