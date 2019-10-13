      SUBROUTINE vort(zeta, u, v, nx, ny, dx, dy)
C     Compute the vorticity on a cartesian grid with centered 
C       differences.  
C     Robert Grumbine 7-18-90.

      INTEGER nx, ny
      REAL zeta(nx, ny), u(nx, ny), v(nx, ny)
      REAL dx, dy

      INTEGER i, j
      REAL dx2, dy2
      
      dx2 = dx*2.
      dy2 = dy*2.
C     Note that the vorticity is computed for the mid points in the 
C       velocity grid.
      DO 1000 j = 2, ny-1
        DO 1010 i = 2, nx-1
          zeta(i,j) = (v(i+1,j)-v(i-1,j))/dx2 - (u(i,j+1)-u(i,j-1))/dy2
 1010   CONTINUE
 1000 CONTINUE
C     For plotting purposes, set the boundary values to the nearest 
C        internal value. 2-24-89.
      DO 2000 i = 2, nx-1
        zeta(i,1)  = zeta(i,2)
        zeta(i,ny) = zeta(i,ny-1)
 2000 CONTINUE
      DO 2010 j = 2, ny-1
        zeta(1,j)  = zeta(2,j)
        zeta(nx,j) = zeta(nx-1,j)
 2010 CONTINUE
      zeta(1,1)   = zeta(2,2)
      zeta(1,ny)  = zeta(2,ny-1)
      zeta(nx,1)  = zeta(nx-1,2)
      zeta(nx,ny) = zeta(nx-1,ny-1)

      RETURN
      END
