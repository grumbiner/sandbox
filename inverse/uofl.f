      SUBROUTINE uofl(lambda, a, u, v, nx, ny, nt, dx, dy, dt, a1) 
C     Given lambda, compute a velocity field, assuming that u0 = 0
C     and given that the velocity points are on the half integer points
C     in lambda.
C     Robert Grumbine
C     Last Modified 28 May 1996

      IMPLICIT none

      INTEGER nx, ny, nt
      REAL dx, dy, dt, a1
      REAL lambda(nx, ny, nt), a(nx, ny, nt)
      REAL u(nx-1, ny-1, nt), v(nx-1, ny-1, nt)
      
      INTEGER i, j, k
     
      DO 1 k = 1, nt
      DO 1000 j = 1, ny-1
      DO 1000 i = 2, nx
        u(i-1, j, k) = a(i,j, k) * (lambda(i,j, k)-lambda(i-1,j, k))
     1      /dx / 2. / a1
 1000 CONTINUE
      DO 2000 j = 2, ny
      DO 2000 i = 1, nx-1
        v(i-1, j, k) = a(i,j, k) * (lambda(i,j, k)-lambda(i,j-1, k))
     1      /dy / 2. / a1
 2000 CONTINUE

    1 CONTINUE

      RETURN
      END
