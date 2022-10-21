      SUBROUTINE adv2d1(u, v, q, g, gold, nx, ny,
     1                      adv, diffo, diffu, dx, dy, dt)
C     2-d advective-diffusive solution, with sources, on a
C       Cartesian grid.
C     Forward differencing in time.
C     Robert Grumbine 6 June 1994.

      IMPLICIT none

      INTEGER nx, ny
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
C     First zero the array.
      DO 100 j = 1, ny
        DO 110 i = 1, nx
          adv(i,j) = 0.0
  110   CONTINUE
  100 CONTINUE

      DO 2000 j = 2, ny-1
        DO 2100 i = 2, nx-1
          adv(i,j) = ( cenx(i,j) + ceny(i,j) )
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
          g(i,j) = gold(i,j) + dt*(-adv(i,j)+diffo(i,j)+q(i,j))
 3100   CONTINUE
 3000 CONTINUE

C     Re-order arrays/update for next time step.  Not needed in forward.

C     Apply boundary conditions in main program

      RETURN
      END
