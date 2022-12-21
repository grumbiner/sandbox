      SUBROUTINE adv2d2(u, v, q, gnew, g, gold, nx, ny,
     1                  adv, diffo, diffu, dx, dy, dt)
C     2-d advective-diffusive solution, with sources, on Cartesian
C       grid, centered in time.
C     Robert Grumbine 6 June 1994.

      IMPLICIT none

      REAL dx, dy, dt, diffu
      INTEGER nx, ny

      REAL u(nx, ny), v(nx, ny), gnew(nx, ny), g(nx, ny), gold(nx, ny)
      REAL q(nx, ny), adv(nx, ny), diffo(nx,ny)

      INTEGER i, j
      REAL dt2, dx2i, dy2i

      REAL cenx, ceny, bakx, baky, forx, fory

C     Statement functions for computing differences
      cenx(i,j) =
     1         (u(i+1,j)*g(i+1,j)-u(i-1,j)*g(i-1,j))*dx2i
      ceny(i,j) =
     1         (v(i,j+1)*g(i,j+1)-v(i,j-1)*g(i,j-1))*dy2i
      bakx(i,j) =
     1     (+u(i,j)*g(i,j)-u(i-1,j)*g(i-1,j))/dx
      baky(i,j) =
     1     (+v(i,j)*g(i,j)-v(i,j-1)*g(i,j-1))/dy
      forx(i,j) =
     1     (-u(i,j)*g(i,j)+u(i+1,j)*g(i+1,j))/dx
      fory(i,j) =
     1     (-v(i,j)*g(i,j)+v(i,j+1)*g(i,j+1))/dy

      CALL laplac(diffo, gold, nx, ny, dx, dy, diffu)
CD      CALL laplac(diffo, g, nx, ny, dx, dy, diffu)

      dt2 = 2.*dt
      dx2i = 1./2./dx
      dy2i = 1./2./dy
C     Now compute the advection
C     First zero the array.
      DO 100 j = 1, ny
        DO 110 i = 1, nx
          adv(i,j) = 0.0
  110   CONTINUE
  100 CONTINUE

      DO 2000 j = 2, ny-1
        DO 2100 i = 2, nx-1
          adv(i,j) =
     1       cenx(i,j)
 2100   CONTINUE
 2000 CONTINUE
      DO 2010 j = 2, ny-1
        DO 2110 i = 2, nx-1
          adv(i,j) = adv(i,j) +
     1         ceny(i,j)
 2110   CONTINUE
 2010 CONTINUE

C     Now compute the near-boundary advection.
      DO 2200 j = 2, ny-1
          i = 1
          adv(i,j) =
     1       ( forx(i,j) + ceny(i,j) )
          i = nx
          adv(i,j) =
     1       ( bakx(i,j) + ceny(i,j) )
 2200 CONTINUE
      DO 2300 i = 2, nx-1
          j = 1
          adv(i,j) =
     1   ( cenx(i,j) + fory(i,j) )
          j = ny
          adv(i,j) =
     1       ( cenx(i,j) + baky(i,j) )
 2300   CONTINUE

C     Add the various sub-terms first (for precision considerations)
      DO 1000 j = 1, ny
        DO 1100 i  = 1, nx
          diffo(i,j) = diffo(i,j)-adv(i,j)+q(i,j)
 1100   CONTINUE
 1000 CONTINUE

      DO 2400 j = 1, ny
        DO 2500 i = 1, nx
          gnew(i,j) = gold(i,j) + diffo(i,j)*dt2
 2500   CONTINUE
 2400 CONTINUE

C     Re-order arrays/update for next time step.
      DO 3000 j = 1, ny
        DO 3100 i = 1, nx
          gold(i,j) = g(i,j)
 3100   CONTINUE
 3000 CONTINUE
      DO 3200 j = 1, ny
        DO 3300 i = 1, nx
          g(i,j) = gnew(i,j)
 3300   CONTINUE
 3200 CONTINUE

C     Apply boundary conditions in main program

      RETURN
      END
