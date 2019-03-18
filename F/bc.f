C********(*********(*********(*********(*********(*********(---------!++
      SUBROUTINE ubc(u, v, nx, ny)
C     Apply boundary conditions on the velocity field
      REAL u(nx, ny), v(nx, ny)

      INTEGER i, j

C     No normal gradient (using same differencing scheme as diffusion)
CD      DO 1000 j = 1, ny
CD        g(1,j) = g(2,j)
CD        g(nx,j) = g(nx-1,j)
CD 1000 CONTINUE
CD      DO 1100 i = 1, nx
CD        g(i,1) = g(i,2)
CD        g(i,ny) = g(i,ny-1)
CD        g(i,ny) = norval
CD 1100 CONTINUE

C     No slip, and no normal flow
      DO 1000 j = 1, ny
        u(1,j)  = 0.0
        u(nx,j) = 0.0
        v(1,j)  = 0.0
        v(nx,j) = 0.0
 1000 CONTINUE
      DO 1100 i =1, nx
        u(i,1)  = 0.0
        u(i,ny) = 0.0
        v(i,1)  = 0.0
        v(i,ny) = 0.0
 1100 CONTINUE

      RETURN
      END
C********(*********(*********(*********(*********(*********(---------!++
      SUBROUTINE sbc(g, nx, ny)
C     Apply boundary conditions on the scalar fields
      INTEGER nx, ny
      REAL g(nx, ny)

      INTEGER i, j

C     No normal gradient (using same differencing scheme as diffusion)
      DO 1000 j = 1, ny
        g(1,j) = g(2,j)
        g(nx,j) = g(nx-1,j)
 1000 CONTINUE
      DO 1100 i = 1, nx
        g(i,1) = g(i,2)
        g(i,ny) = g(i,ny-1)
CD        g(i,ny) = norval
 1100 CONTINUE

      RETURN
      END
