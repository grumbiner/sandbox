C********************************************
      SUBROUTINE resl(lambda, a, a0, b, w, beta, dx, a1, nx, ny, nt)
C     Robert Grumbine
C     Last Modified 28 May 1996 (file)
C     Purpose?  something to do with some variant of weights for inversion

      IMPLICIT none
      INTEGER nx, ny, nt
      REAL beta, dx, a1
      REAL a(nx, ny, nt), b(nx, ny, nt), a0(nx, ny, nt)
      REAL lambda(nx, ny, nt), w(nx, ny, nt)

      INTEGER i, j, k
       
      DO 1000 k = 1, nt
      DO 1100 j = 2, ny-1
      DO 1100 i = 2, nx-1
        b(i,j,k) = 2*w(i,j,k)*(a(i,j,k)-a0(i,j,k)) - 2*beta*
     1  (-4.*a(i,j,k)+a(i+1,j,k)+a(i,j+1,k)+a(i-1,j,k)+a(i,j-1,k) )
     2 - (a(i,j,k)/a1/2./dx/dx/4.)*
     2      ( (lambda(i+1,j,k)-lambda(i-1,j,k))**2  
     3      + (lambda(i,j+1,k)-lambda(i,j-1,k))**2 )
 1100 CONTINUE
 1000 CONTINUE

      RETURN
      END
