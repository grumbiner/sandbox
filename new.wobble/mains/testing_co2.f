      PROGRAM testing
      IMPLICIT none
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.141592654)
      INTEGER i, j, n, m
      REAL rj
      PARAMETER (n = 552)
      DOUBLE PRECISION omega_e_sid, omega_e_anom, omega_e_tropical
      DOUBLE PRECISION omega_j, omega_s, omega_u, omega_n
      DOUBLE PRECISION omega_day
      PARAMETER (omega_e_sid  = 2.*PI/12) ! sidereal year
      DOUBLE PRECISION x(n)
      DOUBLE PRECISION omega(0:2000-1), a(0:2000-1), b(0:2000-1)
      REAL dum1


      OPEN (10,FILE="co2.split", FORM="FORMATTED")
      DO i = 1, n
        READ (10,*) dum1, x(i)
        !!READ (10,*) x(i)
      ENDDO

      m = 2
      omega(0) = 0.0
      DO rj = 0.09, 25, 0.005/4
        omega(1) = omega_e_sid/rj
        CALL harmrm(x, n, omega, a, b, m)
        DO i = 1, m-1
          WRITE (*,9001) 2.*PI/omega(i), a(i), b(i),
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
