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
      PARAMETER (omega_v = 3.*2.*PI/224.7)
      PARAMETER (omega_j = 3.*2.*PI/4332.589)
      PARAMETER (omega_s = 3.*2.*PI/10759.22)
      PARAMETER (omega_u = 3.*2.*PI/30685.4)
      PARAMETER (omega_n = 3.*2.*PI/60189.)
      REAL omega_prec
      PARAMETER (omega_prec = omega_e_sid / 20940. )
      DOUBLE PRECISION x(n)
      DOUBLE PRECISION omega(0:2000-1), a(0:2000-1), b(0:2000-1)
      DOUBLE PRECISION c, eccen
      PARAMETER (eccen = 1.6716738e-2)
      INTEGER k, avg
      DOUBLE PRECISION sums, sum, s

!Want to go to double precision linpack call
      OPEN (10,FILE="gamma", FORM="FORMATTED")
      DO i = 1, n
        READ (10,*) x(i)
      ENDDO

      x = x - 1.00014736426
      avg = INT(0.5 + 2.*PI/(omega_e_sid) * 4.0)
      DO i = 1, n
        x(i) = x(i) - eccen*cos(1.*omega_e_sid*(i-1)+3.13444561259)
     1  -       1.406497e-4*cos(2.*omega_e_sid*(i-1)+3.135827664513814)
        PRINT *,x(i)
      ENDDO
      PRINT *,'avg = ',avg
      DO i = 1, n, avg
        c = cos(1.*omega_e_sid*(i-1)+3.13444561259)
        s = sin(1.*omega_e_sid*(i-1)+3.13444561259)
        sum = 0
        sums = 0
        DO k = 1, avg
          sum = sum + x(i)*c
          sums = sums + x(i)*s
        ENDDO 
        sum = sum/avg
        sums = sums/avg
        WRITE (*,9001) sum, sums, 
     1        sqrt(sum*sum + sums*sums),
     2        atan2(sums, sum)
C        IF (ABS(x(i)) .LT. eccen/10.) THEN
C        WRITE (*,9001) x(i),eccen*c, x(i)-c*eccen, 0. 
C        ELSE
C        WRITE (*,9001) x(i),eccen*c, x(i)-c*eccen, 1. - c*eccen/x(i)
C        ENDIF
      ENDDO
 9001 FORMAT (4E19.8)

      STOP
      END
