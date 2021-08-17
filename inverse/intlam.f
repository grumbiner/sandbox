C********************************************
      SUBROUTINE intlam(lambda, a, a0, w, beta, nx, ny, nt, dx, dt)
C     Rectangle rule estimation of lambda from initial equation.
      IMPLICIT none

      INTEGER nx, ny, nt
      REAL dx, dt, beta
      REAL lambda(nx, ny, nt), a(nx, ny, nt), a0(nx, ny, nt)
      REAL w(nx, ny, nt)

      INTEGER i, j, k

      k=1
      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        lambda(i,j,k) = 0.
 1000 CONTINUE

      DO 2000 k = 2, nt
      PRINT *,'intlam k = ',k
      DO 2100 j = 2, ny-1
      DO 2200 i = 2, nx-1
        lambda(i,j,k) = lambda(i,j,k-1) + ( 
     1       2.*w(i,j,k)*(a(i,j,k)-a0(i,j,k)) - 
     2       2.*beta*(a(i+1,j,k)+a(i-1,j,k)+a(i,j+1,k)+a(i,j-1,k)
     3                    -4.*a(i,j,k) )/dx**2 ) * dt
 2200 CONTINUE
 2100 CONTINUE
 2000 CONTINUE

      RETURN
      END
