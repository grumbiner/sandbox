      SUBROUTINE r0guess(dt, nmon, nstep, r0g, a0)
C     Make an estimate for da/dt given a.  This is simple centered
C       differencing, and assumes that a0 can handle any bounds
C       problems in its argument.
C     Bob Grumbine 2 June 1994.

      IMPLICIT none

      EXTERNAL a0
      INTEGER i, nmon, nstep
      REAL dt, t, a0, r0g(nstep)

      IF (nstep .NE. nmon/dt+0.5) THEN
        PRINT *,'Mismatch between number of steps and time stepping'
        PRINT *,'Nstep, nmon, dt = ',nstep, nmon, dt
        STOP
      ENDIF

      DO 1000 i = 0, INT(nmon/dt+0.5)
        t = i*dt
        r0g(i) = -(a0(t+dt)-a0(t-dt))/2./dt
 1000 CONTINUE
    
      RETURN
      END
