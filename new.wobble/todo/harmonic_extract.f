
C----------------------------------------------------------------------------
      SUBROUTINE harmonic_extract(x, npts, freq, a, b, todo, dt)
      IMPLICIT none
      INTEGER npts, todo
      REAL x(npts)
      DOUBLE PRECISION freq(todo), a(todo), b(todo)
      DOUBLE PRECISION amplitude(todo), phase(todo)
      DOUBLE PRECISION dt, pi
      INTEGER i
 
      pi = DABS(DACOS(-1.D0))
  
      CALL harmrm(x , npts, freq, a, b, todo)
      DO i = 1, todo
        amplitude(i) = SQRT(a(i)*a(i)+b(i)*b(i))
        phase(i)     = ATAN2(b(i),a(i))*180./pi
!        WRITE (*,*) freq(i)/2./pi/dt*365.2422,",",
!     1               amplitude(i),",",
!     2               phase(i),",",  2.D0*pi/freq(i)*dt
        WRITE (*,9001) freq(i)/2./pi/dt*365.2422,
     1               amplitude(i)*1.e6,
     2               phase(i), 2.D0*pi/freq(i)*dt
      ENDDO
 9001 FORMAT (F10.7," , ",F10.3," , ",F7.2," , ",F10.3)

      CALL extract(x , npts, freq, a, b, todo, dt)

      RETURN
      END
