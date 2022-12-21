

      SUBROUTINE adv2d2(u, v, q, gnew, g, gold, nnx, nny,
     1                           diffu, dx, dy, dt)
      IMPLICIT none
      INCLUDE "grid.inc"
      REAL dx, dy, dt, diffu
      INTEGER nnx, nny

      REAL u(nx, ny), v(nx, ny), gnew(nx, ny), g(nx, ny), gold(nx, ny)
      REAL q(nx, ny), adv(nx, ny), diffo(nx,ny)

      INTEGER i, j
      REAL dt2, dx2i, dy2i

      INTEGER m,n
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

      dt2 = 2.*dt
      dx2i = 1./2./dx
      dy2i = 1./2./dy
C     Now compute the advection
      DO 2000 j = 2, ny-1
        DO 2100 i = 2, nx-1
          adv(i,j) =
     1         (u(i+1,j)*g(i+1,j)-u(i-1,j)*g(i-1,j))*dx2i
CD     1       cenx(i,j)
 2100   CONTINUE
 2000 CONTINUE
      DO 2010 j = 2, ny-1
        DO 2110 i = 2, nx-1
          adv(i,j) = adv(i,j) +
CD     1         ceny(i,j)
     1         (v(i,j+1)*g(i,j+1)-v(i,j-1)*g(i,j-1))*dy2i
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
C     Corner points
      adv(1,1) = 0.0
      adv(1,ny) = 0.0
      adv(nx,1) = 0.0
      adv(nx,ny) = 0.0

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
      SUBROUTINE adv2d1(u, v, q, g, gold, nnx, nny,
     1                           diffu, dx, dy, dt)
C     Fordard differencing in time.

      IMPLICIT none
      INTEGER nnx, nny
      INCLUDE "grid.inc"
      REAL dx, dy, dt, diffu
      REAL u(nx, ny), v(nx, ny), g(nx, ny), gold(nx, ny)
      REAL q(nx, ny), adv(nx, ny), diffo(nx,ny)

      INTEGER i, j

      INTEGER m,n
      REAL cenx, ceny, bakx, baky, forx, fory
      REAL dx2i, dy2i

C     Statement functions for computing differences
      cenx(m,n) =
     1         (u(m+1,n)*gold(m+1,n)-u(m-1,n)*gold(m-1,n))*dx2i
      ceny(m,n) =
     1         (v(m,n+1)*gold(m,n+1)-v(m,n-1)*gold(m,n-1))*dy2i
      bakx(m,n) =
     1         (u(m,n)*gold(m,n)-u(m-1,n)*gold(m-1,n))/dx
      baky(m,n) =
     1         (v(m,n)*gold(m,n)-v(m,n-1)*gold(m,n-1))/dy
      forx(m,n) =
     1     (-u(m,n)*gold(m,n)+u(m+1,n)*gold(m+1,n))/dx
      fory(m,n) =
     1     (-v(m,n)*gold(m,n)+v(m,n+1)*gold(m,n+1))/dy

C     Put g into gold.
      DO 10 j = 1, ny
        DO 11 i = 1, ny
          gold(i,j) = g(i,j)
   11   CONTINUE
   10 CONTINUE

C     Compute the diffusive forcing.
      CALL laplac(diffo, gold, nx, ny, dx, dy, diffu)

      dx2i = 1./2./dx
      dy2i = 1./2./dy

C     Now add in the advection
      DO 2000 j = 2, ny-1
        DO 2100 i = 2, nx-1
          adv(i,j) = -( cenx(i,j) + ceny(i,j) )
 2100   CONTINUE
 2000 CONTINUE

C     Now compute the near-boundary advection.
      DO 2200 j = 2, ny-1
          i = 1
          adv(i,j) =( forx(i,j) + ceny(i,j) )
          i = nx
          adv(i,j) =( bakx(i,j) + ceny(i,j) )
 2200 CONTINUE
      DO 2300 i = 2, nx-1
          j = 1
          adv(i,j) =( cenx(i,j) + fory(i,j) )
          j = ny
          adv(i,j) =( cenx(i,j) + baky(i,j) )
 2300   CONTINUE

C     Carry out the extrapolation
      DO 3000 j = 1, ny
        DO 3100 i = 1, nx
          g(i,j) = gold(i,j) + dt*(adv(i,j)+diffo(i,j)+q(i,j))
 3100   CONTINUE
 3000 CONTINUE

C     Re-order arrays/update for next time step.  Not needed in forward.

C     Apply boundary conditions in main program

      RETURN
      END
