      SUBROUTINE advdif(deltaz, deltat, phos, kappa, w)
C     Compute a purely advective-diffusive solution, no
C       internal sources or sinks, and no flux boundary conditions.
C     BG 5/17/91.

      IMPLICIT none
      INTEGER nlayer
      PARAMETER (nlayer = 76)

      DOUBLE PRECISION deltaz, deltat
      DOUBLE PRECISION phos(nlayer), kappa(nlayer), w(nlayer)
      DOUBLE PRECISION f2(nlayer), f3(nlayer), fp(nlayer)
	  
	  INTEGER i
	  
CD	  PRINT *,'starting forcing loop'

      DO 1000 i = 3, nlayer-2

        f2(i) = kappa(i)*(-phos(i+2)+16.D0*phos(i+1)
     1      -30.D0*phos(i)-phos(i-2)+16.D0*phos(i-1)  )
     2       /deltaz/deltaz/24.D0
        f3(i) = - w(i)*(phos(i+1)-phos(i))/deltaz
        fp(i) = f2(i) + f3(i)
          
 1000 CONTINUE
 
CD     PRINT *,'outside forcing loop'
      i = nlayer-1
      f2(i) = kappa(i)*(phos(i)-2.*phos(i-1)+phos(i-2))
     1                /deltaz/deltaz
      f3(i) = - w(i)*(phos(i)-phos(i-1))/deltaz
      fp(i) = f2(i) + f3(i)
          
C     Extrapolate the interior
CD     PRINT *,'In extrapolation loop'
      DO 1200 i = 3, nlayer-1
        phos(i)   = phos(i) + deltat * fp(i)
 1200 CONTINUE
 
C     Now apply a no flux diffusion condition
      phos(nlayer-1)   = 0.5D0*(phos(nlayer)+phos(nlayer-1))
      phos(nlayer)     = phos(nlayer-1)

      RETURN
      END