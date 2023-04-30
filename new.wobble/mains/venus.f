      PROGRAM testing
      IMPLICIT none
      DOUBLE PRECISION PI
!      PARAMETER (PI = 3.141592654)
      PARAMETER (PI = 3.14159265358979323846 )
      INTEGER i, j, k, n, m
      PARAMETER (n = 36646)
C      PARAMETER (m = 2000)
      DOUBLE PRECISION omega_e_sid, omega_e_anom, omega_e_tropical
      DOUBLE PRECISION omega_v, omega_j, omega_s, omega_u, omega_n
! Sidereal years taken from nssdc.gsfc.nasa.gov/planetary/factsheet/
! except for earth, which is from Astronomical Almanac 2001
      REAL stepsize
      PARAMETER (stepsize = 3.)
      PARAMETER (omega_e_sid  = stepsize*2.*PI/365.256363) ! sidereal year
      PARAMETER (omega_e_anom = stepsize*2.*PI/365.259635) !anomalistic year
      PARAMETER (omega_e_tropical = stepsize*2.*PI/365.24190) !tropical year
      PARAMETER (omega_v = stepsize*2.*PI/224.701)
      PARAMETER (omega_j = stepsize*2.*PI/4332.589)
      PARAMETER (omega_s = stepsize*2.*PI/10759.22)
      PARAMETER (omega_u = stepsize*2.*PI/30685.4)
      PARAMETER (omega_n = stepsize*2.*PI/60189.)
      DOUBLE PRECISION  omega_prec
      PARAMETER (omega_prec = omega_e_sid / 20940. )
      DOUBLE PRECISION  omega_moon, omega_perigee, omega_node
      PARAMETER (omega_moon    = stepsize*2.*PI/27.32)
      PARAMETER (omega_perigee = stepsize*2.*PI/365.256/8.85)
      PARAMETER (omega_node    = stepsize*2.*PI/365.256/18.613)
      DOUBLE PRECISION x(n)
      DOUBLE PRECISION omega(0:2000-1), a(0:2000-1), b(0:2000-1)
      REAL period

! Compute periods of simple combinations:
      PRINT *,"venus-jupiter"
      DO i = 1, 6
        DO j = -6, 6
          PRINT *,i,j,2.*PI*stepsize/(i*omega_v + j*omega_j), 
     1                   omega_v*i + j*omega_j
        ENDDO
      ENDDO

      DO i = 1, 8
        DO j = -16, 16
          DO k = -16, 16
            period = 2.*PI*stepsize/
     1            (i*omega_v + j*omega_e_sid + k*omega_j)
            IF (period .GT. 50) THEN
              PRINT *,i,j,k,period, 2.*PI*stepsize/period
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      STOP
      END 
