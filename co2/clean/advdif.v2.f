C*************************************************----------++++++++++!!
      SUBROUTINE advdif(deltaz, deltat, t, kappa, w, step)
C     Compute a purely advective-diffusive solution, no
C       internal sources or sinks, and no flux boundary conditions.
C     BG 5/17/91.
C     Numerical scheme is a leapfrog in time, staggered in space with 
C       fourth order differencing on the advection, second on diffusion.

      IMPLICIT none
      INTEGER nlayer
      PARAMETER (nlayer = 76)

      DOUBLE PRECISION deltaz, deltat
      DOUBLE PRECISION t(nlayer), kappa(nlayer), w(nlayer)
      DOUBLE PRECISION f2(nlayer), f3(nlayer)
      
      DOUBLE PRECISION tnm1(nlayer), temp(nlayer)
      INTEGER i, step
      
      SAVE tnm1
      
      IF (step-1 .EQ. 0) THEN
        DO 100 i = 1, nlayer
          tnm1(i) = t(i)
  100   CONTINUE
C       Since we are using a leapfrog scheme, we neet to make the first step 
C         with a different scheme.
        CALL advstr(deltaz, deltat, t, kappa, w, step)
        RETURN
      ENDIF
        
      DO 1000 i = 3, nlayer-2

        f2(i) = (   kappa(i+1)          *tnm1(i+1)
     1            -(kappa(i+1)+kappa(i))*tnm1(i)
     2            + kappa(i)            *tnm1(i-1) )
     3    /deltaz/deltaz
     
        f3(i) = -(w(i+1)+w(i))*.5
     1           *(-t(i+2)+8.*t(i+1)-8.*t(i-1)+t(i-2))
     2           /(12.*deltaz)
               
 1000 CONTINUE
      i = 2
        f2(i) = (   kappa(i+1)          *tnm1(i+1)
     1            -(kappa(i+1)+kappa(i))*tnm1(i)
     2            + kappa(i)            *tnm1(i-1) )
     3    /deltaz/deltaz
        f3(i) = -( (t(i+1)-t(i  ))*w(i+1)
     1           + (t(i  )-t(i-1))*w(i  )        )/(2.*deltaz)
     
      i = nlayer-1 
        f2(i) = (   kappa(i+1)          *tnm1(i+1)
     1            -(kappa(i+1)+kappa(i))*tnm1(i)
     2            + kappa(i)            *tnm1(i-1) )
     3    /deltaz/deltaz
        f3(i) = -( (t(i+1)-t(i  ))*w(i+1)
     1           + (t(i  )-t(i-1))*w(i  )        )/(2.*deltaz)

C     Extrapolate the interior
C     First, put the current (n) level into the temporary vector, where
C       on the next step, it will be n-1 level.

      DO 1210 i = 1, nlayer
        temp(i) = t(i)
 1210 CONTINUE
 
      DO 1200 i = 2, nlayer-1
        t(i)   = tnm1(i) + deltat*2.*(f2(i) + f3(i))
 1200 CONTINUE
 
C     Now apply a no flux diffusion condition
      t(nlayer)   = t(nlayer-1)
      t(1)        = t(2)
      
C     Put the temporary data into the saved vector (n-1 on the next step)
      DO 2000 i = 1, nlayer
        tnm1(i) = temp(i)
 2000 CONTINUE

      RETURN
      END