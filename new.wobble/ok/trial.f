      
      SUBROUTINE trial(residual, i, d1, res1, quality)
      IMPLICIT none

      DOUBLE PRECISION residual, res1
      INTEGER i, d1
      LOGICAL quality

      DOUBLE PRECISION omega_day
      DOUBLE PRECISION omega_e_sidereal, omega_e_anom, omega_e_tropical
      DOUBLE PRECISION omega_prec
      PARAMETER (omega_day = 1./1.0)
      PARAMETER (omega_e_sidereal  = 1./365.256363) ! sidereal year
      PARAMETER (omega_e_anom      = 1./365.259635) !anomalistic year
      PARAMETER (omega_e_tropical  = 1./365.24190) !tropical year
      PARAMETER (omega_prec = omega_e_sidereal / 20940. )

      DOUBLE PRECISION omega_v, omega_mars
      DOUBLE PRECISION omega_j, omega_s, omega_u, omega_n
      PARAMETER (omega_v    = 1./224.701)
      PARAMETER (omega_mars = 1./ 686.980)  
      PARAMETER (omega_j    = 1./4332.589)
      PARAMETER (omega_s    = 1./10759.22)
      PARAMETER (omega_u    = 1./30685.4)
      PARAMETER (omega_n    = 1./60189.0)

      DOUBLE PRECISION omega_moon, omega_perigee, omega_node
      PARAMETER (omega_moon    = 1./27.3217)
      PARAMETER (omega_perigee = 1./365.256363/8.85)
      PARAMETER (omega_node    = 1./365.256363/18.613)

      DOUBLE PRECISION allomega(12)
      DOUBLE PRECISION toler

      allomega(1) = omega_day
      allomega(2) = omega_moon
      allomega(3) = omega_v
      allomega(4) = omega_e_sidereal
      allomega(5) = omega_mars
      allomega(6) = omega_perigee
      allomega(7) = omega_j
      allomega(8) = omega_node
      allomega(9) = omega_s
      allomega(10) = omega_u
      allomega(11) = omega_n
      allomega(12) = omega_prec
      
      toler = 0.85 

      d1 = NINT(residual/allomega(i))
      res1 = residual - d1*allomega(i)
      IF (ABS(res1/residual) .LT. toler) THEN
        quality = .TRUE.
      ELSE
        quality = .FALSE.
      ENDIF
!      PRINT *,i,residual, d1, res1, res1/residual, 1/allomega(i), 
!     1                 residual/allomega(i), quality

      RETURN
      END 
