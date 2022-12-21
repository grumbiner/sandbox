
C----------------------------------------------------------------------------
      SUBROUTINE harmonic_extract(x, npts, freq, a, b, todo)
      IMPLICIT none
      INTEGER npts, todo
      REAL x(npts)
      DOUBLE PRECISION freq(todo), a(todo), b(todo)
      DOUBLE PRECISION amplitude(todo), phase(todo)
      REAL dt, pi
      INTEGER i
 
      dt = 1.0
      pi = DABS(DACOS(-1.D0))
  
      CALL harmrm(x , npts, freq, a, b, todo)
      DO i = 1, todo
        amplitude(i) = SQRT(a(i)*a(i)+b(i)*b(i))
        phase(i)     = ATAN2(b(i),a(i))*180./pi
        WRITE (*, *) freq(i)/2./pi/dt,",",
     1               amplitude(i),",",
     2               phase(i),",",  2.D0*pi/freq(i)*dt
      ENDDO

      CALL extract(x , npts, freq, a, b, todo, dt)

      RETURN
      END
