C*************************************************----------++++++++++!!
      SUBROUTINE adv1d2(deltaz, deltat, x, xnm1, kappa, w, q, step,
     1   nlayer, f2, f3, temp)
C     Compute a purely advective-diffusive solution, no
C       internal sources or sinks, and no flux boundary conditions.
C     BG 5/17/91.
C     Numerical scheme is a leapfrog in time, staggered in space with
C       fourth order differencing on the advection, second on diffusion.
C     Internal source/sink term added by 8/4/91. BG.

      IMPLICIT none

       INTEGER nlayer

       	DOUBLE PRECISION deltaz, deltat
       	DOUBLE PRECISION q(nlayer), kappa(nlayer), w(nlayer)
       	DOUBLE PRECISION x(nlayer), xnm1(nlayer)

       	DOUBLE PRECISION f2(nlayer), f3(nlayer), temp(nlayer)

	INTEGER i, step

	IF (step-1 .EQ. 0) THEN
	  DO 100 i = 1, nlayer
	    xnm1(i) = x(i)
  100   CONTINUE
C       Since we are using a leapfrog scheme, we neet to make the first
C         step with a different scheme.  Lax-Wendroff is used.
	  CALL advstr(deltaz, deltat, x, kappa, w, q)
	  RETURN
	ENDIF

	DO 1000 i = 3, nlayer-2

	  f2(i) = (   kappa(i+1)          *xnm1(i+1)
     1            -(kappa(i+1)+kappa(i))*xnm1(i)
     2            + kappa(i)            *xnm1(i-1) )
     3    /deltaz/deltaz

CD        f3(i) = -(w(i+1)+w(i))*.5
CD     1           *(-x(i+2)+8.*x(i+1)-8.*x(i-1)+x(i-2))
CD     2           /(12.*deltaz)

C       Flux form added 12/9/91.  BG
	  f3(i) = - ( -x(i+2)*w(i+2) + 8.*x(i+1)*w(i+1)
     1              +x(i-2)*w(i-2) - 8.*x(i-1)*w(i-1) )
     2            /(12.*deltaz)
 1000 CONTINUE
	i = 2
	  f2(i) = (   kappa(i+1)          *xnm1(i+1)
     1            -(kappa(i+1)+kappa(i))*xnm1(i)
     2            + kappa(i)            *xnm1(i-1) )
     3    /deltaz/deltaz
	  f3(i) = -( (x(i+1)-x(i  ))*w(i+1)
     1           + (x(i  )-x(i-1))*w(i  )        )/(2.*deltaz)

	i = nlayer-1
	  f2(i) = (   kappa(i+1)          *xnm1(i+1)
     1            -(kappa(i+1)+kappa(i))*xnm1(i)
     2            + kappa(i)            *xnm1(i-1) )
     3    /deltaz/deltaz
	  f3(i) = -( (x(i+1)-x(i  ))*w(i+1)
     1           + (x(i  )-x(i-1))*w(i  )        )/(2.*deltaz)

C     Extrapolate the interior
C     First, put the current (n) level into the temporary vector,
C       where on the next step, it will be n-1 level.

	DO 1210 i = 1, nlayer
	  temp(i) = x(i)
 1210 CONTINUE

	DO 1200 i = 2, nlayer-1
	  x(i)   = xnm1(i) + deltat*2.*(f2(i) + f3(i) + q(i)/deltaz)
 1200 CONTINUE

C     Now apply a no flux diffusion condition
	x(nlayer)   = x(nlayer-1)
	x(1)        = x(2)

C     Put the temporary data into the saved vector (n-1 on the next step)
	DO 2000 i = 1, nlayer
	  xnm1(i) = temp(i)
 2000 CONTINUE

	RETURN
	END
