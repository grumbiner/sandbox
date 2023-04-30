      program a
      INTEGER n
      PARAMETER (n = 75972)
      DOUBLE PRECISION pi, stepsize 
      PARAMETER (pi = 3.141592654)
      PARAMETER (stepsize = 0.25)
      INTEGER i

      DO i = 0, n-1
        PRINT *,1. + 1.e-2*dcos(2.*pi*i*stepsize / 365.25) + 
     1                 1.d-5*dcos(2.*pi*i*stepsize / 27.32)
      ENDDO
      
      END
