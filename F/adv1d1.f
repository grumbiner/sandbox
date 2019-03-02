C*************************************************----------++++++++++!!
      SUBROUTINE adv1d1(deltaz, deltat, x, kappa, w, q, nlayer,
     1     f2, f3)
C     Compute a purely advective-diffusive solution, no
C       internal sources or sinks, and no flux boundary conditions.
C     BG 5/17/91.
C     Start the extrapolation scheme.  This is forward in time, with
C       Lax-Wendroff on the advection.
C     Internal source/sinks added by 8/4/91. BG
C     Future remove Lax-Wendroff.

      	IMPLICIT none

      	INTEGER nlayer

      DOUBLE PRECISION deltaz, deltat
      	DOUBLE PRECISION kappa(nlayer), w(nlayer), q(nlayer)
      	DOUBLE PRECISION f2(nlayer), f3(nlayer), x(nlayer)

	INTEGER i

	DO 1000 i = 2, nlayer-1

	  f2(i) = (   kappa(i+1)          *x(i+1)
     1            -(kappa(i+1)+kappa(i))*x(i)
     2            + kappa(i)            *x(i-1) )
     3    /deltaz/deltaz

	  f3(i) = -( (x(i+1)-x(i  ))*w(i+1)
     1           + (x(i  )-x(i-1))*w(i  )        )/(2.*deltaz)
     2           + (w(i+1)+w(i))*(w(i+1)+w(i))*.25*deltat*
     3                     (x(i+1)-2.*x(i)+x(i-1))/2./deltaz/deltaz

 1000 CONTINUE

C     Extrapolate the interior
	DO 1200 i = 2, nlayer-1
	  x(i)   = x(i) + deltat * (f2(i) + f3(i) + q(i)/deltaz)
 1200 CONTINUE

C     Now apply a no flux diffusion condition
	x(nlayer)   = x(nlayer-1)
	x(1)        = x(2)

	RETURN
	END
