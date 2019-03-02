C*************************************************----------++++++++++!!
      SUBROUTINE stmfc4(psi, ut, vt, dx, dy, nx, ny)
C     Given a psi function, compute the velocities for a stream function.
C     Use fourth order accurate differencing.
C     Robert Grumbine 7 April 1994.

      IMPLICIT none

      INTEGER nx, ny
      REAL psi(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL dx, dy

      INTEGER i, j
      DO 1000 j = 1, ny
        i = 1
C       Use a forward fourth order difference.
        vt(i,j) = (-11.*psi(i  ,j)+18.*psi(i+1,j)
     1              -9.*psi(i+2,j)+2. *psi(i+3,j))/6./dx
        i = 2
        vt(i,j) = (-11.*psi(i  ,j)+18.*psi(i+1,j)
     1              -9.*psi(i+2,j)+2. *psi(i+3,j))/6./dx
        DO 1010 i = 3, nx-2
C     Implement a 4th order accurate first derivative in x
C       in order to get a decent treatment of v near the boundaries.
          vt(i,j) = (-psi(i+2,j)+8.*psi(i+1,j)
     1                          -8.*psi(i-1,j)+psi(i-2,j))/12./dx
 1010   CONTINUE
C       Use traditional forms for the eastern boundary, where
C         there is less structure.
        i = nx-1
        vt(i,j) = (-11.*psi(i-3,j)+18.*psi(i-2,j)
     1              -9.*psi(i-1,j)+2. *psi(i  ,j))/6./dx
        i = nx
        vt(i,j) = (-11.*psi(i-3,j)+18.*psi(i-2,j)
     1              -9.*psi(i-1,j)+2. *psi(i  ,j))/6./dx
 1000 CONTINUE

C     Use ordinary centered differences for u velocity.
      DO 4000 j = 3, ny-2
        DO 4010 i = 1, nx
          ut(i,j) = -(-psi(i,j+2)+8.*psi(i,j+1)
     1                +psi(i,j-2)-8.*psi(i,j-1) )/12./dy
 4010   CONTINUE
 4000 CONTINUE
      DO 4030 i = 1, nx
        j = 1
        ut(i,j) = -(psi(i,j+1)-psi(i,j))/dy
        j = 2
        ut(i,j) = -(psi(i,j+1)-psi(i,j))/dy
        j = ny
        ut(i,j) = -(psi(i,j)-psi(i,j-1))/dy
        j = ny-1
        ut(i,j) = -(psi(i,j)-psi(i,j-1))/dy
 4030 CONTINUE

C     Give values to the corner points
      ut(1,1)    = 0.0
      ut(1,ny)   = 0.0
      ut(nx,1)   = 0.0
      ut(nx, ny) = 0.0
      vt(1,1)    = 0.0
      vt(1,ny)   = 0.0
      vt(nx,1)   = 0.0
      vt(nx, ny) = 0.0

      RETURN
      END
