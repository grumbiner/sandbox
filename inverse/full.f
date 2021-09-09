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
            g0(i, j, 1) = (1-a(i, j, 1))/dh * frac
CD            g0(i, j, 1) = (1-ia(i, j))/dh * frac
          ELSE
            g0(i,j,1) = 0.0
          ENDIF
  100   CONTINUE
CD        PRINT *,'dh, frac, ia', dh, frac, ia
CD        PRINT *,'g0 after 100 loop', g0
       ELSE
        DO 200 j = 1, ny
        DO 200 i = 1, nx
          READ (14, 9006) rnew(i, j)
          g0(i, j, 1) = -rnew(i, j)/f(1, i, j)
  200   CONTINUE
      ENDIF

      DO 300 j = 1, ny
      DO 300 i = 1, nx
        WRITE (12,9001) 1, i, j, a(i, j, 1), ia(i, j), g0(i, j, 1), 
     1    hbar(i, j), g0(i, j, 1)*f(1, i, j), u(i,j), v(i,j)
        WRITE (13, 9005) a(i, j, 1), g0(i, j, 1)*f(1, i, j)
  300 CONTINUE
 9005 FORMAT (F6.4, 2x, F9.6, 2E13.6)
 9006 FORMAT (24X, F10.6)

      DO 1000 i = 2, nstep
        IF (new) THEN
          DO 1100 l = 1, ny
          DO 1100 k = 1, nx
            IF (f(1, k, l) .GT. 0.) THEN
              g0(k, l, i) = (1.-a(k, l, i-1))/dh * frac
CD              g0(k, l, i) = (1.-ia(k, l))/dh * frac
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

        CALL aext(a, f, u, v, g0, dh, nx, ny, nthick, nstep, 
     1             dx, dy, dt, i)
        CALL gext(g, f, u, v, nx, ny, nstep, nthick, dt, dh, 
     1             dx, dy, g0, i)
        CALL intega(g, nx, ny, nthick, nstep, dh, i, ia,
     1               h0, hbar)

        DO 1300 l = 1, ny
        DO 1300 k = 1, nx
          WRITE (12,9001) i, k, l, a(k, l, i), ia(k, l), g0(k, l,i ), 
     1              hbar(k, l), (a(k, l, i)-a(k, l, i-1))
     2    , u(k, l), v(k, l)
        WRITE (13, 9005) a(k, l, i), (a(k, l, i)-a(k, l, i-1))
     2    , u(k, l), v(k, l)
 1300   CONTINUE

        IF (MOD(i,INT(5) ) .EQ. 0) THEN
          DO 1400 l = 1, ny
          DO 1400 k = 1, nx
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
      SUBROUTINE intega(g, nx, ny, nthick, nstep, dh, tstep, ia, 
     1    h0, hbar)
C     Subroutine to integrate the thickness distribution to get the
C       related ice concentration.
      IMPLICIT none

      INTEGER nx, ny
      REAL ia(nx, ny), dh, h0, hbar(nx, ny)
      INTEGER i, j, k, nthick, nstep, tstep
      REAL g(nthick, nx, ny, nstep)

      DOUBLE PRECISION sum, sum2

      DO 2000 j = 1, ny
      DO 2100 k = 1, nx
        sum  = 0.0
        sum2 = 0.0

        DO 1000 i = 2, nthick-1
CD          sum  = sum  + DBLE(dh*                g(i,k, j, tstep) )
CD          sum2 = sum2 + DBLE(dh* (h0+(i-1)*dh) *g(i,k, j, tstep) )
          sum = sum + DBLE(dh * (g(i,k,j,tstep)+g(i+1,k,j,tstep))/2. )
          sum2 = sum2 + DBLE(dh * (g(i,k,j,tstep)+g(i+1,k,j,tstep))/2.
     1             * (dh*(i-1)+h0 )  )
 1000   CONTINUE
        ia(k,j)   = SNGL(sum)
        hbar(k,j) = SNGL(sum2)
 2100 CONTINUE
 2000 CONTINUE


      RETURN
      END
      SUBROUTINE gext(g, f, u, v, nx, ny, nstep, nthick, dt, dh, 
     1               dx, dy, g0, tstep)
C     Extrapolate the ice thickness distribution for the inversion
C       theory test program.  Use an Euler forward algorithm.
C     Bob Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER nx, ny, nstep, nthick, tstep
      REAL dt, dh, g0(nx, ny, nstep), dx, dy
      REAL g(nthick, nx, ny, nstep), f(nthick, nx, ny)
      REAL u(nx, ny), v(nx, ny)

      INTEGER i, j, k, told
      REAL diffu

      diffu = dh**2 / dt * 0.01
      told = tstep - 1

      i = 1
      DO 100 k = 1, ny
      DO 100 j = 1, nx
        g(i, j, k, tstep) = g0(j, k, tstep)
  100 CONTINUE

      DO 1200 k = 2, ny-1
      DO 1100 j = 2, nx-1
      
CD      i = 2
CDC     Change for the f1g1 term
CD        g(i, j, k, tstep) = g(i, j, k, told) -
CD     1     (dt/dh/2.)* 
CD     2       (f(i+1, j, k)*g(i+1, j, k, told) - 
CD     3        f(i-1, j, k)*g(i-1, j, k, tstep) )
CDCD     2  ( f(i,j,k)*(g(i+1,j,k,told) - g(i-1,j,k,told)) +   !explosively 
CDCD     3    g(i,j,k,told)*(f(i+1,j,k) - f(i-1,j,k) )       ) !unstable
CD     5  + diffu*(dt/dh**2)*(g(i+1, j, k,told)-2.*g(i, j, k,told)+
CD     6                      g(i-1, j, k,told) )
CD     7 - u(j, k)*(g(i, j+1, k, told) - g(i, j-1, k, told))*dt/dx/2.
CD     8 - v(j, k)*(g(i, j, k+1, told) - g(i, j, k-1, told))*dt/dy/2.
CD     9 - g(i,j,k,told)*( (u(j+1,k)-u(j-1,k))/dx/2. 
CD     1                  +(v(j,k+1)-v(j,k-1))/dy/2. )*dt

      DO 1000 i = 2, nthick-1
        g(i, j, k, tstep) = g(i, j, k, told) -
     1     (dt/dh/2.)* 
     2       (f(i+1, j, k)*g(i+1, j, k, told) - 
     3        f(i-1, j, k)*g(i-1, j, k, told) )
CD     2  ( f(i,j,k)*(g(i+1,j,k,told) - g(i-1,j,k,told)) +   !explosively 
CD     3    g(i,j,k,told)*(f(i+1,j,k) - f(i-1,j,k) )       ) !unstable
     5  + diffu*(dt/dh**2)*(g(i+1, j, k,told)-2.*g(i, j, k,told)+
     6                      g(i-1, j, k,told) )
     7 - u(j, k)*(g(i, j+1, k, told) - g(i, j-1, k, told))*dt/dx/2.
     8 - v(j, k)*(g(i, j, k+1, told) - g(i, j, k-1, told))*dt/dy/2.
     9 - g(i,j,k,told)*( (u(j+1,k)-u(j-1,k))/dx/2. 
     1                  +(v(j,k+1)-v(j,k-1))/dy/2. )*dt
 1000 CONTINUE
 1100 CONTINUE
 1200 CONTINUE

C     Have implied boundary condition that no ice is thicker than modelled.
      i = nthick
      DO 3000 j = 1, ny
      DO 3000 k = 1, nx
        g(i, k, j, tstep) = 0.0 
 3000 CONTINUE

       RETURN
       END
      SUBROUTINE aext(a, f, u, v, g0, dh, nx, ny, nthick, nstep, 
     1             dx, dy, dt, tstep)
C     Extrapolate the ice concentration function for the inverse theory
C       test program.  Use simple minded Euler forward.
C     Bob Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER tstep, nthick, nstep, nx, ny
      REAL dh, dt, dx, dy
      REAL a(nx, ny, nstep), f(nthick, nx, ny), g0(nx, ny, nstep)
      REAL u(nx, ny), v(nx, ny)

      INTEGER i, j

      DO 1000 j = 2, ny-1
      DO 1100 i = 2, nx-1
        a(i, j, tstep) = a(i, j, tstep-1) 
     1  + f(1, i, j)*g0(i, j, tstep-1)*dt/2.
     2  - u(i,j)*(a(i+1,j, tstep-1)-a(i-1,j, tstep-1))/dx/2.*dt
     3  - v(i,j)*(a(i,j+1, tstep-1)-a(i,j-1, tstep-1))/dy/2.*dt
     4  - a(i,j, tstep-1)*( (v(i,j+1)-v(i,j))/dy + (u(i+1,j)-u(i,j))/dx
     5         )*dt
 1100 CONTINUE
 1000 CONTINUE
   
      DO 2000 j = 1, ny
        a(1,j, tstep) = a(2,j, tstep)
        a(nx,j, tstep) = a(nx-1,j, tstep)
 2000 CONTINUE
      DO 2100 i = 1, nx
        a(i,ny, tstep) = a(i,ny-1, tstep)
        a(i,1, tstep)  = a(i,2, tstep)
 2100 CONTINUE

      RETURN
      END
