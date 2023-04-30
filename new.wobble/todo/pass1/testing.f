      PROGRAM testing
      IMPLICIT none
      DOUBLE PRECISION PI
!      PARAMETER (PI = 3.141592654)
      PARAMETER (PI = 3.14159265358979323846 )
      INTEGER i, j, n, m
      PARAMETER (n = 36646)
C      PARAMETER (m = 2000)
      DOUBLE PRECISION omega_e_sid, omega_e_anom, omega_e_tropical
      DOUBLE PRECISION omega_v, omega_j, omega_s, omega_u, omega_n
! Sidereal years taken from nssdc.gsfc.nasa.gov/planetary/factsheet/
! except for earth, which is from Astronomical Almanac 2001
      PARAMETER (omega_e_sid  = 3.*2.*PI/365.256363) ! sidereal year
      PARAMETER (omega_e_anom = 3.*2.*PI/365.259635) !anomalistic year
      PARAMETER (omega_e_tropical = 3.*2.*PI/365.24190) !tropical year
      PARAMETER (omega_v = 3.*2.*PI/224.701)
      PARAMETER (omega_j = 3.*2.*PI/4332.589)
      PARAMETER (omega_s = 3.*2.*PI/10759.22)
      PARAMETER (omega_u = 3.*2.*PI/30685.4)
      PARAMETER (omega_n = 3.*2.*PI/60189.)
      REAL omega_prec
      PARAMETER (omega_prec = omega_e_sid / 20940. )
      DOUBLE PRECISION x(n)
      DOUBLE PRECISION omega(0:2000-1), a(0:2000-1), b(0:2000-1)

!Want to go to double precision linpack call
      OPEN (10,FILE="gamma", FORM="FORMATTED")
      DO i = 1, n
        READ (10,*) x(i)
      ENDDO
      omega(0) = 0.0;
      omega(1) = omega_e_anom
      omega(2) = 2.*omega(1)

      m = 3
      x = x - 1.0 
      CALL harmrm(x, n, omega, a, b, m)
      DO i = 0, m-1
        WRITE (*,9001) 3.*2.*PI/omega(i), a(i), b(i),
     1         sqrt(a(i)*a(i)+b(i)*b(i)), atan2(b(i),a(i))
      ENDDO
 9001 FORMAT(F9.3,3E18.8,E24.16)
      DO i = 1, n
        DO j = 0, m-1
          x(i) = x(i) - a(j)*cos(omega(j)*DBLE(i)) - 
     1                  b(j)*sin(omega(j)*DBLE(i))
        ENDDO
        PRINT *,x(i)
      ENDDO

      STOP
      END
