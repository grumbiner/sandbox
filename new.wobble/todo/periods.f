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

! Compute periods of simple combinations:
      PRINT *,'E-nJ  E-nS  E-nU  E-nN V-nE'
      DO i = -6, 6
        PRINT *,i,3.*2.*PI/(omega_e_sid - i*omega_j),
     1            3.*2.*PI/(omega_e_sid - i*omega_s),
     1            3.*2.*PI/(omega_e_sid - i*omega_u),
     1            3.*2.*PI/(omega_e_sid - i*omega_n)
      ENDDO

      PRINT *,'E-J-nS E-J-nU E-J-nN'
      DO i = -6, 6
        PRINT *,i,
     1            3.*2.*PI/(omega_e_sid - omega_j - i*omega_s),
     1            3.*2.*PI/(omega_e_sid - omega_j - i*omega_u),
     1            3.*2.*PI/(omega_e_sid - omega_j - i*omega_n)
      ENDDO
 
      DO j = 1, 8
      DO i = -16, 16
        PRINT *,j, i,
     1            3.*2.*PI/(j*omega_v - i*omega_e_sid)
      ENDDO
      ENDDO
 

      STOP
      END 
