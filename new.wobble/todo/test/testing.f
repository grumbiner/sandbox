      PROGRAM testing
      IMPLICIT none
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.141592654)
      INTEGER i, j, n, m
      REAL rj
      PARAMETER (n = 36646)
C      PARAMETER (m = 2000)
      DOUBLE PRECISION omega_e_sid, omega_e_anom, omega_e_tropical
      DOUBLE PRECISION omega_j, omega_s, omega_u, omega_n
      DOUBLE PRECISION omega_day
      PARAMETER (omega_day = 3.*2.*PI/1.0)
      PARAMETER (omega_e_sid  = 3.*2.*PI/365.256363) ! sidereal year
      PARAMETER (omega_e_anom = 3.*2.*PI/365.259635) !anomalistic year
      PARAMETER (omega_e_tropical = 3.*2.*PI/365.24190) !tropical year
      PARAMETER (omega_j = 3.*2.*PI/4332.71)
      PARAMETER (omega_s = 3.*2.*PI/10759.5)
      PARAMETER (omega_u = 3.*2.*PI/30685.0)
      PARAMETER (omega_n = 3.*2.*PI/60190.0)
      DOUBLE PRECISION x(n)
      DOUBLE PRECISION omega(0:2000-1), a(0:2000-1), b(0:2000-1)
      DOUBLE PRECISION delta, step

      OPEN (10,FILE="b", FORM="FORMATTED")
      DO i = 1, n
        READ (10,*) x(i)
      ENDDO
      omega(0) = 0.0;
      omega(1) = omega_e_anom
      omega(2) = 2.*omega(1)
      omega(3) = 3.*omega(1)
      j = 3
      DO i = 1, 6
        omega(i+j) = omega_e_sid - omega_j*i
      ENDDO
      j = i+j-1
      DO i = 1, 6
        omega(i+j) = omega_e_sid - omega_s*i
      ENDDO
      j = i+j-1
      DO i = 1, 6
        omega(i+j) = omega_e_sid - omega_u*i
      ENDDO
      j = i+j-1
      DO i = 1, 6
        omega(i+j) = omega_e_sid - omega_n*i
      ENDDO

      m = 2
      delta = (1.-.95)
      step  = delta/2.5e4
      DO rj = 1.-2.*delta, 1.+2.*delta, step
        omega(1) = (omega_e_sid-omega_j)*rj
        CALL harmrm(x, n, omega, a, b, m)
        DO i = 1, m-1
          WRITE (*,9001) 3.*2.*PI/omega(i), a(i), b(i),
     1         sqrt(a(i)*a(i)+b(i)*b(i)), atan2(b(i),a(i))
        ENDDO
      ENDDO
 9001 FORMAT(F12.5,4E19.8)
      STOP
      DO i = 1, n
        DO j = 0, m-1
          x(i) = x(i) - a(j)*cos(omega(j)*i) - b(j)*sin(omega(j)*i)
        ENDDO
        PRINT *,x(i)
      ENDDO

      STOP
      END
