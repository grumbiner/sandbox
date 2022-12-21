C********************************************
      SUBROUTINE resa(lambda, a, b, nx, ny, nt, dx, dy, a1)
C     Compute residuals from dadt equation
      IMPLICIT none

      INTEGER nx, ny, nt
      REAL a1, dx, dy
      REAL lambda(nx, ny, nt), a(nx, ny, nt), b(nx, ny, nt)

      INTEGER i, j, k

      DO 1000 k = 1, nt

        DO 1100 j = 2, ny-1
        DO 1100 i = 2, nx-1
          b(i,j,k) = (lambda(i+1,j,k)-lambda(i-1,j,k))*
     1        (a(i+1,j,k)*a(i+1,j,k) - a(i-1,j,k)*a(i-1,j,k)) 
     2 + (lambda(i,j+1,k) - lambda(i,j-1,k))*
     3        (a(i,j+1,k)*a(i,j+1,k) - a(i,j-1,k)*a(i,j-1,k)) 
     4 + a(i,j,k)*a(i,j,k)*(
     5   lambda(i+1,j,k)+lambda(i,j+1,k)+lambda(i-1,j,k)+lambda(i,j-1,k)
     6     -4.*lambda(i,j,k) )
 1100   CONTINUE

        DO 1200 j = 2, ny-1
        DO 1200 i = 2, nx-1
          b(i,j,k) = b(i,j,k)/2./a1/dx/dx
 1200   CONTINUE

 1000 CONTINUE

      k=15
CD      DO 1300 j = 2, ny-1
CD      DO 1300 i = 2, nx-1
CD        WRITE (*,9001) i,j,b(i,j,k)*86400.
CD 1300 CONTINUE

      RETURN
      END        
