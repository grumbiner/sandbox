
C*************************************************----------++++++++++!!
      SUBROUTINE stmfc4(psi, h, ut, vt, dx, dy, nx, ny)
C     Now, given a psi function, compute the velocities.
C     Now should have a complete soln for psi.
      IMPLICIT none

      INTEGER nx, ny
      REAL psi(nx, ny), h(nx, ny)
      REAL ut(nx, ny), vt(nx, ny)
      REAL dx, dy
      INTEGER i, j
C*************************************************----------++++++++++!!
      DO 1000 j = 1, ny
        i = 1
C       Use a forward fourth order difference.
        vt(i,j) = (-11.*psi(i  ,j)+18.*psi(i+1,j)
     1              -9.*psi(i+2,j)+2. *psi(i+3,j))/6./dx
        i = 2
        vt(i,j) = (-11.*psi(i  ,j)+18.*psi(i+1,j)
     1              -9.*psi(i+2,j)+2. *psi(i+3,j))/6./dx

        DO 1010 i = 3, nx-2
C         Implement a 4th order accurate first derivative in x
C         in order to get a decent treatment of v near the boundaries.
          vt(i,j) = ( -psi(i+2,j)+8.*psi(i+1,j)
     1                +psi(i-2,j)-8.*psi(i-1,j) )/12./dx
CD        vt(i,j) = (psi(i+1,j)-psi(i-1,j))/2./dx
 1010   CONTINUE

C       Use traditional forms for the eastern boundary, where
C         there is less structure.
        i = nx-1
        vt(i,j) = (psi(i+1,j)-psi(i-1,j))/2./dx
        i = nx
        vt(i,j) = (psi(i,j)-psi(i-1,j))/dx
 1000 CONTINUE
C*************************************************----------++++++++++!!
C     Use ordinary centered differences for u velocity.
      DO 4000 j = 2, ny-1
        DO 4010 i = 1, nx
          ut(i,j) = -(psi(i,j+1)-psi(i,j-1))/2./dy
 4010   CONTINUE
 4000 CONTINUE
      DO 4030 i = 1, nx
        j = 1
        ut(i,j) = -(psi(i,j+1)-psi(i,j)  )/dy
        j = ny
        ut(i,j) = -(psi(i,j) - psi(i,j-1))/dy
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

C     Now divide by height to give velocity
      DO 5000 j = 2, ny
        DO 5100 i = 1, nx
          ut(i,j) = ut(i,j)/(h(i,j)+h(i,j-1))*2.
 5100   CONTINUE
 5000 CONTINUE
      DO 5110 i = 1, nx
        ut(i,1) = ut(i,1)/h(i,1)
 5110 CONTINUE

      DO 5200 j = 1, ny
        DO 5300 i = 2, nx
          vt(i,j) = vt(i,j)/(h(i,j)+h(i-1,j))*2.
 5300   CONTINUE
 5200 CONTINUE
      DO 5310 j = 1, ny
        vt(1,j) = vt(1,j)/h(1,j)
 5310 CONTINUE

      DO 6000 j = 1, ny
        DO 6100 i = 1, nx
          IF (h(i,j) .LE. 10.) THEN
            ut(i,j) = 0.0
            vt(i,j) = 0.0
          ENDIF
 6100   CONTINUE
 6000 CONTINUE

      RETURN
      END
