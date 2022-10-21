      SUBROUTINE laplac(diffus, ss, nnx, nny, dx, dy, diffu)
      IMPLICIT none

      INTEGER nnx, nny
      INCLUDE "grid.inc"
      REAL dx, dy, diffu
      REAL ss(nx, ny), diffus(nx, ny)

      INTEGER i, j
      REAL difu

      difu  = diffu/dx/dx

C     Evaluate the diffusion:
C     Laplacean in the central part of the domain.  Centered in
C       x and y.
      DO 100 j = 2, ny-1
        DO 101 i = 2, nx-1
          diffus(i,j) =
     1    (ss(i+1,j)-4.*ss(i,j)
     2               +ss(i-1,j)   )
CD     3   +(ss(i,j+1)-2.*ss(i,j)
CD     4               +ss(i,j-1)   )
  101   CONTINUE
  100 CONTINUE
      DO 110 j = 2, ny-1
        DO 111 i = 2, nx-1
          diffus(i,j) = diffus(i,j)
     3   +(ss(i,j+1)
     4               +ss(i,j-1)   )
  111   CONTINUE
  110 CONTINUE

C     Diffusion along walls
      DO 200 j = 1, ny
        diffus(1,j) = 0.0
        diffus(nx,j) = 0.0
 200  CONTINUE
      DO 210 i = 1, nx
        diffus(i,1) = 0.0
        diffus(i,ny) = 0.0
  210 CONTINUE

      DO 2000 j = 1, ny
        DO 2100 i = 1, nx
          diffus(i,j) = diffus(i,j)*difu
 2100   CONTINUE
 2000 CONTINUE

      RETURN
      END
