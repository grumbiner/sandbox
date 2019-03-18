C********(*********(*********(*********(*********(*********(*********(!!
      SUBROUTINE laplac(diffus, ss, nx, ny, dx, dy)
C     Evaluate a fourth order accurate laplacean on ss, for a cartesian
C       grid.
C     Robert Grumbine 6 April 1994.

      IMPLICIT none

      INTEGER nx, ny
      REAL diffus(nx, ny), ss(nx, ny)
      REAL dx, dy
      INTEGER i, j
 
C     Zero the laplacean matrix
      DO 10 j = 1, ny
        DO 20 i = 1, nx
          diffus(i,j) = 0.0
  20    CONTINUE
  10  CONTINUE
  
C     Laplacean in the central part of the domain.  Centered in 
C       x and y.
      DO 100 j = 3, ny-2
        DO 101 i = 3, nx-2
          diffus(i,j) =
     1    (-ss(i+2,j)+16.*ss(i+1,j)-30.*ss(i,j)
     2               +16.*ss(i-1,j)-ss(i-2,j)   )/12.
     3   +(-ss(i,j+2)+16.*ss(i,j+1)-30.*ss(i,j)
     4               +16.*ss(i,j-1)-ss(i,j-2)   )/12.
  101   CONTINUE
  100 CONTINUE

C     Diffusion forward in x, centered in y
      DO 102 j = 3, ny-2
        i = 2
        diffus(i,j) = (35.*ss(i,j)-104.*ss(i+1,j)+114.*ss(i+2,j)
     1                -56.*ss(i+3,j)+11.*ss(i+4,j))/12.
     3   +(-ss(i,j+2)+16.*ss(i,j+1)-30.*ss(i,j)
     4               +16.*ss(i,j-1)-ss(i,j-2)   )/12.
        i = nx-1
        diffus(i,j) = (35.*ss(i,j)-104.*ss(i-1,j)+114.*ss(i-2,j)
     1                -56.*ss(i-3,j)+11.*ss(i-4,j))/12.
     3   +(-ss(i,j+2)+16.*ss(i,j+1)-30.*ss(i,j)
     4               +16.*ss(i,j-1)-ss(i,j-2)   )/12.

        i = 1
        diffus(i,j) = (35.*ss(i,j)-104.*ss(i+1,j)+114.*ss(i+2,j)
     1                -56.*ss(i+3,j)+11.*ss(i+4,j))/12.
     3   +(-ss(i,j+2)+16.*ss(i,j+1)-30.*ss(i,j)
     4               +16.*ss(i,j-1)-ss(i,j-2)   )/12.
        i = nx
        diffus(i,j) = (35.*ss(i,j)-104.*ss(i-1,j)+114.*ss(i-2,j)
     1                -56.*ss(i-3,j)+11.*ss(i-4,j))/12.
     3   +(-ss(i,j+2)+16.*ss(i,j+1)-30.*ss(i,j)
     4               +16.*ss(i,j-1)-ss(i,j-2)   )/12.
  102 CONTINUE

C     Now the boundary points in j, with i running on index
C       One point backwards in y, centered in x.
      DO 110 i = 3, nx-2
        j = 2
        diffus(i,j) = (-ss(i+2,j)+16.*ss(i+1,j)-30.*ss(i,j)
     1                 -ss(i-2,j)+16.*ss(i-1,j)            )/12.
     2  +(35.*ss(i,j)-104.*ss(i,j+1)+114.*ss(i,j+2)
     3                -56.*ss(i,j+3)+11.*ss(i,j+4))/12.
        j = 1
        diffus(i,j) = (-ss(i+2,j)+16.*ss(i+1,j)-30.*ss(i,j)
     1                 -ss(i-2,j)+16.*ss(i-1,j)            )/12.
     2  +(35.*ss(i,j)-104.*ss(i,j+1)+114.*ss(i,j+2)
     3                -56.*ss(i,j+3)+11.*ss(i,j+4))/12.
        j = ny-1
        diffus(i,j) = (-ss(i+2,j)+16.*ss(i+1,j)-30.*ss(i,j)
     1                 -ss(i-2,j)+16.*ss(i-1,j)            )/12.
     2  +(35.*ss(i,j)-104.*ss(i,j-1)+114.*ss(i,j-2)
     3                -56.*ss(i,j-3)+11.*ss(i,j-4))/12.
        j = ny
        diffus(i,j) = (-ss(i+2,j)+16.*ss(i+1,j)-30.*ss(i,j)
     1                 -ss(i-2,j)+16.*ss(i-1,j)            )/12.
     2  +(35.*ss(i,j)-104.*ss(i,j-1)+114.*ss(i,j-2)
     3                -56.*ss(i,j-3)+11.*ss(i,j-4))/12.
  110 CONTINUE

C     Now need to take care of corner points.
C     Be (temporarily) lazy, and set equal to zero.
      diffus(1,1) = 0.0
      diffus(2,1) = 0.0
      diffus(1,2) = 0.0
      
      diffus(1,ny) = 0.0
      diffus(2,ny) = 0.0
      diffus(1,ny-1) = 0.0
      
      diffus(nx,1) = 0.0
      diffus(nx,2) = 0.0
      diffus(nx-1,1) = 0.0
      
      diffus(nx,ny) = 0.0
      diffus(nx,ny-1) = 0.0
      diffus(nx-1,ny) = 0.0

      i = 2
      j = 2
        diffus(i,j) = (35.*ss(i,j)-104.*ss(i+1,j)+114.*ss(i+2,j)
     1                -56.*ss(i+3,j)+11.*ss(i+4,j))/12.
     2  +(35.*ss(i,j)-104.*ss(i,j+1)+114.*ss(i,j+2)
     3                -56.*ss(i,j+3)+11.*ss(i,j+4))/12.

      i = 2
      j = ny-1
        diffus(i,j) = (35.*ss(i,j)-104.*ss(i+1,j)+114.*ss(i+2,j)
     1                -56.*ss(i+3,j)+11.*ss(i+4,j))/12.
     2  +(35.*ss(i,j)-104.*ss(i,j-1)+114.*ss(i,j-2)
     3                -56.*ss(i,j-3)+11.*ss(i,j-4))/12.

      i = nx-1
      j = 2
      diffus(nx-1,2) = 0.0
        diffus(i,j) = (35.*ss(i,j)-104.*ss(i-1,j)+114.*ss(i-2,j)
     1                -56.*ss(i-3,j)+11.*ss(i-4,j))/12.
     2  +(35.*ss(i,j)-104.*ss(i,j+1)+114.*ss(i,j+2)
     3                -56.*ss(i,j+3)+11.*ss(i,j+4))/12.

      i = nx-1
      j = ny-1
        diffus(i,j) = (35.*ss(i,j)-104.*ss(i-1,j)+114.*ss(i-2,j)
     1                -56.*ss(i-3,j)+11.*ss(i-4,j))/12.
     2  +(35.*ss(i,j)-104.*ss(i,j-1)+114.*ss(i,j-2)
     3                -56.*ss(i,j-3)+11.*ss(i,j-4))/12.
 
      RETURN
      END
