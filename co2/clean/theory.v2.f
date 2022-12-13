C*************************************************----------++++++++++!!
      SUBROUTINE theory(phos, kappa, w, deltaz, step, deltat)
C
C     Compute the theoretical solution for the evolution of the 
C       cos function initial condition.

      IMPLICIT none
      INTEGER nlayer
      PARAMETER (nlayer = 76)
	  
      DOUBLE PRECISION phos(nlayer), kappa(nlayer), w(nlayer)
      DOUBLE PRECISION deltaz, deltat
      INTEGER i, step
      DOUBLE PRECISION dummy, alpha

      alpha = 1.D0
      DO 100 i = 1, nlayer
        phos(i)   = DBLE(alpha*COS(ACOS(-1.)*
     1                (FLOAT(i)-1.5)/(FLOAT(nlayer)-2.))  )
  100 CONTINUE
  
      dummy = EXP(-alpha*alpha*ACOS(-1.)*ACOS(-1.)*kappa(1)
     1            /FLOAT(nlayer-2)/deltaz/FLOAT(nlayer-2)/deltaz
     2           *FLOAT(step)*deltat )

      DO 200 i = 1, nlayer
        phos(i) = phos(i)*dummy
  200 CONTINUE
  
      RETURN
      END