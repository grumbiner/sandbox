
      SUBROUTINE dadt(a, lambda, nx, ny, nt, dt, a3, a4)
C     Compute dA/dt, and use that to estimate lambda
      IMPLICIT none
      INTEGER nx, ny, nt
      REAL dt, a3, a4, a(nx, ny, nt), lambda(nx, ny, nt)
      INTEGER i, j, k

      DO 1000 k = 2, nt-1
      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        lambda(i,j,k) = (a(i,j,k+1) - a(i,j,k-1) ) /2./dt
 1000 CONTINUE

      DO 2000 j = 1, ny
      DO 2000 i = 1, nx
        lambda(i,j,1) = (a(i,j,2) - a(i,j,1) ) /dt
        lambda(i,j,nt) = (a(i,j,nt) - a(i,j,nt-1) ) /dt
 2000 CONTINUE

      DO 3000 k = 1, nt
      DO 3000 j = 1, ny
      DO 3000 i = 1, nx
        lambda(i,j,k) = lambda(i,j,k) / (1./2./a3 + 1./2./a4)
 3000 CONTINUE

      RETURN
      END
