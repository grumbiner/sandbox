C*************************************************----------++++++++++!!
      SUBROUTINE advdif(deltaz, deltat, t, kappa, w)
C     Compute a purely advective-diffusive solution, no
C       internal sources or sinks, and no flux boundary conditions.
C     BG 5/17/91.

      IMPLICIT none
      INTEGER nlayer
      PARAMETER (nlayer = 76)

      DOUBLE PRECISION deltaz, deltat
      DOUBLE PRECISION t(nlayer), kappa(nlayer), w(nlayer)
      DOUBLE PRECISION f2(nlayer), f3(nlayer), fp(nlayer)
        
      INTEGER i
        
      DO 1000 i = 2, nlayer-1

        f2(i) = (   kappa(i+1)          *t(i+1)
     1            -(kappa(i+1)+kappa(i))*t(i)
     2            + kappa(i)            *t(i-1) )
     3    /deltaz/deltaz
     
        f3(i) = -( (t(i+1)-t(i  ))*w(i+1)
     1           + (t(i  )-t(i-1))*w(i  )        )/(2.*deltaz)
     2           + (w(i+1)+w(i))*(w(i+1)+w(i))*.25*deltat*
     3                     (t(i+1)-2.*t(i)+t(i-1))/2./deltaz/deltaz
     
        fp(i) = f2(i) + f3(i)
          
 1000 CONTINUE
           
C     Extrapolate the interior
      DO 1200 i = 2, nlayer-1
        t(i)   = t(i) + deltat * fp(i)
 1200 CONTINUE
 
C     Now apply a no flux diffusion condition
      t(nlayer)   = t(nlayer-1)
      t(1)        = t(2)

      RETURN
      END