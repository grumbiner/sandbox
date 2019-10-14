      PROGRAM straw
C     Perform a straw man inversion of concentration and growth rate
C       time series.
C     Bob Grumbine 2/94.

      IMPLICIT none
      INCLUDE "inv.inc"

CD      REAL dt
CD      PARAMETER (dt   = dh/fref/days)

CD      INTEGER nstep
CD      PARAMETER (nstep  = days/dt )
      
      REAL a(nstep), g0(nstep), rnew, r0, a0, r0dot 
      REAL g(nthick, nstep)
      REAL f(nthick), ia, hbar

      INTEGER ier, i, j
      REAL sset
      EXTERNAL a0, r0

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
        IF (MOD(i,INT(10*mult)) .EQ. 0) THEN
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
      SUBROUTINE aext(a, f, g0, dh, nthick, nstep, dt, tstep)
C     Extrapolate the ice concentration function for the inverse theory
C       test program.  Use simple minded Euler forward.
C     Bob Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER tstep, nthick, nstep
      REAL dh, dt
      REAL a(nstep), f(nthick), g0

      a(tstep) = a(tstep-1) + f(1)*g0*dt

      RETURN
      END
      SUBROUTINE gext(g, f, nstep, nthick, dt, dh, g0, tstep)
C     Extrapolate the ice thickness distribution for the inversion
C       theory test program.  Use an Euler forward algorithm.
C     Bob Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER nstep, nthick, tstep
      REAL dt, dh, g0
      REAL g(nthick, nstep), f(nthick)

      INTEGER i, told
      REAL diffu

      diffu = dh**2 / dt * 0.03

      i = 1
      g(i,tstep) = g0
      told = tstep - 1

      DO 1000 i = 2, nthick-1
        g(i, tstep) = g(i, told) -
     1     (dt/dh/2.)* (
     2       (f(i+1)*g(i+1, told)- f(i-1)*g(i-1,told) )
     4 )
     5  + diffu*(dt/dh**2)*(g(i+1,told)-2.*g(i,told)+g(i-1,told) )
 1000 CONTINUE

C     Have implied boundary condition that no ice is thicker than modelled.
      i = nthick
      g(i,tstep) = 0.0 

      DO 2000 i = 1, nthick
CD        g(i, tstep) = AMAX1(0., g(i,tstep))
 2000 CONTINUE

C     Alternate advection evaluation -- take d(fg)/dh evaluation alanytically.
CD     2     f(i)*     (g(i+1, tstep-1)-g(i-1,tstep-1) ) +
CD     3  g(i,tstep-1)*(f(i+1)         -f(i-1)         )

       RETURN
       END
      SUBROUTINE fset(f, fref, h0, dh, dt, nthick, iunit)
C     Initialize the freezing rate function for the inversion theory
C       test program.
C     Bob Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER nthick, iunit
      REAL dh, dt, h0, fref, f(nthick)
      REAL h, fdecay

      INTEGER i

      fdecay = h0
      DO 1000 i = 1, nthick
        h    = dh*(i-1)
        f(i) = fref*exp(-h/fdecay)
 1000 CONTINUE

      RETURN
      END
      SUBROUTINE gset(g, nthick, nstep, f, iunit)
C     Initialize the thickness distribution for the inversion theory
C       test program.
C     Bob Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER nthick, nstep, iunit
      REAL g(nthick, nstep), f(nthick)
      INTEGER i, j

      i = 1
      DO 1000 j = 1, nthick
        g(j,i) = 0.0
 1000 CONTINUE

      RETURN
      END
      SUBROUTINE intega(g, nthick, nstep, dh, tstep, ia, h0, hbar)
C     Subroutine to integrate the thickness distribution to get the
C       related ice concentration.
      REAL ia, dh, hbar
      INTEGER i, nthick, nstep, tstep
      REAL g(nthick, nstep)

      DOUBLE PRECISION sum, sum2

      sum  = 0.0
      sum2 = 0.0

      DO 1000 i = 1, nthick
        sum  = sum  + DBLE(dh*                g(i,tstep) )
        sum2 = sum2 + DBLE(dh* (h0+(i-1)*dh) *g(i,tstep) )
 1000 CONTINUE

      ia   = SNGL(sum)
      hbar = SNGL(sum2)

      RETURN
      END
      FUNCTION scalars(t)
      IMPLICIT none
      REAL t
      INTEGER nstep
      PARAMETER (nstep = 640)
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
      REAL FUNCTION r0dot(r0, t, dt)
C     Function to take the derivative of an external function of one
C       variable.  Note that the external function must handle out of
C       bounds arguments.
C     Bob Grumbine 5 April 1994.

      IMPLICIT none

      REAL r0, t, dt
      EXTERNAL r0
      REAL yp, ym

      yp = r0(t+dt)
      ym = r0(t-dt)
      r0dot = (yp-ym)/2./dt

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE rarst2(x, nx, ny, value)
C     Set all elements of array x equal to a scalar value.
C     Bob Grumbine 5 April 1994.

      IMPLICIT none

      INTEGER nx, ny
      REAL x(nx*ny), value

      INTEGER j

      DO 1000 j = 1, ny*nx
          x(j) = value
 1000 CONTINUE

      RETURN
      END
