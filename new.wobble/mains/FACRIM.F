      PROGRAM testing
      IMPLICIT none
      DOUBLE PRECISION PI
!      PARAMETER (PI = 3.141592654)
      PARAMETER (PI = 3.14159265358979323846 )
      INTEGER i, j, n, m
      PARAMETER (n = 9091)
      DOUBLE PRECISION omega_e_sid, omega_e_anom, omega_e_tropical
      DOUBLE PRECISION omega_v, omega_j, omega_s, omega_u, omega_n
      DOUBLE PRECISION omega_mars
! Sidereal years taken from nssdc.gsfc.nasa.gov/planetary/factsheet/
! except for earth, which is from Astronomical Almanac 2001
      PARAMETER (omega_e_sid  = 3.*2.*PI/365.256363) ! sidereal year
      PARAMETER (omega_e_anom = 3.*2.*PI/365.259635) !anomalistic year
      PARAMETER (omega_e_tropical = 3.*2.*PI/365.24190) !tropical year
      PARAMETER (omega_mars = 3.*2.*PI/ 686.980)
      PARAMETER (omega_v = 3.*2.*PI/224.701)
      PARAMETER (omega_j = 3.*2.*PI/4332.589)
      PARAMETER (omega_s = 3.*2.*PI/10759.22)
      PARAMETER (omega_u = 3.*2.*PI/30685.4)
      PARAMETER (omega_n = 3.*2.*PI/60189.)
      REAL omega_prec
      PARAMETER (omega_prec = omega_e_sid / 20940. )
      DOUBLE PRECISION x(n)
      DOUBLE PRECISION omega(0:n-1), a(0:n-1), b(0:n-1)
      REAL dum1

!Want to go to double precision linpack call
      OPEN (10,FILE="acrim", FORM="FORMATTED")
      DO i = 1, n
        READ (10,*) dum1, x(i)
      ENDDO
      omega(0) = 0.0;
      omega(1) = 2.*PI/n
      !PRINT *,omega(1)
      !DO i = 2, n/2 - 1
      DO i = 2, 2000
        omega(i) = i*omega(1)
        !PRINT *,i,omega(i)
      ENDDO 
      
      !m = n/2 - 2
      m = 2001
      
      !PRINT *,'calling harmrm'
      CALL harmrm(x, n, omega, a, b, m)
      !PRINT *,'back from calling harmrm'
      DO i = 0, m-1
        WRITE (*,9001) 2.*PI/omega(i), a(i), b(i),
     1         sqrt(a(i)*a(i)+b(i)*b(i)), atan2(b(i),a(i))
      ENDDO
 9001 FORMAT(F10.3,3E18.8,E24.16)
      DO i = 1, n
        DO j = 0, m-1
          x(i) = x(i) - a(j)*cos(omega(j)*DBLE(i)) - 
     1                  b(j)*sin(omega(j)*DBLE(i))
        ENDDO
        PRINT *,x(i)
      ENDDO

      STOP
      END
