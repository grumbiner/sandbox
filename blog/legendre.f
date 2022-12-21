
      REAL FUNCTION legendre(n, x)
      IMPLICIT none
      INTEGER n
      REAL x
      SELECT CASE (n)
        CASE (0)
          legendre = 1
        CASE (1)
          legendre = x
        CASE (2)
          legendre = 0.5*(3*x*x - 1)
        CASE (3)
          legendre = 0.5*(5*x*x*x-3*x)
        CASE (4)
          legendre = (35*x**4 - 30*x**2 + 3)/8
      END SELECT
      RETURN
      END 
