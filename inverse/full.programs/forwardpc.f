      PROGRAM for4
C     Create a theoretical exact time series for A, g(h), for
C       testing the retrieval of the correct series.
      IMPLICIT none
      INCLUDE "inv.inc"

CD      REAL dt
CD      PARAMETER (dt   = dh/fref/days)
CD      INTEGER nstep
CD      PARAMETER (nstep  = days/dt )

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
      WRITE (12,9001) 1, a(1), ia, g0(1), hbar, -a(1)
      WRITE (13, 9005) a(1), -a(1)
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

        WRITE (12,9001) i, a(i), ia, g0(i), hbar,-(a(i)-a(i-1))
        WRITE (13, 9005) a(i), -(a(i)-a(i-1))
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
      FUNCTION yes(defalt, unit)
C     Function to return .TRUE. if the user responds y, .FALSE. if he
C       says n, and the default value otherwise.
C     Bob Grumbine 5 April 1994.

      IMPLICIT none

      LOGICAL yes, defalt
      INTEGER unit
      CHARACTER resp

      READ (unit,9001) resp
 9001 FORMAT(A1)

      yes = (resp .EQ. 'y') .OR. (defalt .AND. resp .NE. 'y'
     1                                   .AND. resp .NE. 'n')

      RETURN
      END
