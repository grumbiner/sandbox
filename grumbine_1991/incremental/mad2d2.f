      SUBROUTINE mad2d2(unew, vnew, u, v, uold, vold, p, rho, qi, qj,
     1                h, f, beta, diffu, dx, dy, dt, nx, ny,
     2 advi, advj, diffi, diffj)
      IMPLICIT none

      REAL dx, dy, dt, diffu
      REAL f, beta

      INTEGER nx, ny
      REAL unew(nx, ny), vnew(nx, ny), u(nx, ny), v(nx, ny)
      REAL uold(nx, ny), vold(nx, ny), p(nx, ny), rho
      REAL h(nx, ny), qi(nx, ny), qj(nx, ny)

      REAL advi(nx, ny), advj(nx, ny), diffi(nx,ny), diffj(nx, ny)
      INTEGER i, j, jcen
      REAL dxi, dyi, dt2, dx2i, dy2i

      INTEGER m,n
      REAL cenx, ceny, bakx, baky, forx, fory, fimp, delta

C     Statement functions for computing differences
C     I component
C********(*********(*********(*********(*********(*********(*********(!!
      cenx(i,j) =
     1((v(i+1,j)-v(i-1,j))*dx2i - (u(i,j+1)-u(i,j-1))*dy2i)
     1  * (-v(i,j))
     2  + (u(i+1,j)*u(i+1,j) - u(i-1,j)*u(i-1,j))*dx2i*0.5
     3  + (v(i+1,j)*v(i+1,j) - v(i-1,j)*v(i-1,j))*dx2i*0.5
      forx(i,j) =
     1( (v(i+1,j)-v(i,j))*dxi - (u(i,j+1)-u(i,j))*dyi )
     1  * (-v(i,j))
     2  + (u(i+1,j)*u(i+1,j) - u(i,j)*u(i,j))*dxi*0.5
     3  + (v(i+1,j)*v(i+1,j) - v(i,j)*v(i,j))*dxi*0.5
      bakx(i,j) =
     1( (v(i,j)-v(i-1,j))*dxi - (u(i,j)-u(i,j-1))*dyi )
     1  * (-v(i,j))
     2  + (u(i,j)*u(i,j) - u(i-1,j)*u(i-1,j))*dxi*0.5
     3  + (v(i,j)*v(i,j) - v(i-1,j)*v(i-1,j))*dxi*0.5

C********(*********(*********(*********(*********(*********(*********(!!
C     J component
      ceny(i,j) =
     1((v(i+1,j)-v(i-1,j))*dx2i - (u(i,j+1)-u(i,j-1))*dy2i)
     1  * ( u(i,j))
     2  + (u(i,j+1)*u(i,j+1) - u(i,j-1)*u(i,j-1))*dy2i*0.5
     3  + (v(i,j+1)*v(i,j+1) - v(i,j-1)*v(i,j-1))*dy2i*0.5
      fory(i,j) =
     1( (v(i+1,j)-v(i,j))*dxi - (u(i,j+1)-u(i,j))*dyi )
     1  * ( u(i,j))
     2  + (u(i,j+1)*u(i,j+1) - u(i,j)*u(i,j))*dyi*0.5
     3  + (v(i,j+1)*v(i,j+1) - v(i,j)*v(i,j))*dyi*0.5
      baky(i,j) =
     1( (v(i,j)-v(i-1,j))*dxi - (u(i,j)-u(i,j-1))*dyi )
     1  * ( u(i,j))
     2  + (u(i,j)*u(i,j) - u(i,j-1)*u(i,j-1))*dyi*0.5
     3  + (v(i,j)*v(i,j) - v(i,j-1)*v(i,j-1))*dyi*0.5

C********(*********(*********(*********(*********(*********(*********(!!
C     Compute diffusion in i component
      CALL laplac(diffi, uold, nx, ny, dx, dy, diffu)
      CALL laplac(diffj, vold, nx, ny, dx, dy, diffu)

      jcen = (1+ny)/2
      dt2 = 2.*dt
      dx2i = 1./2./dx
      dy2i = 1./2./dy
      dxi  = 1./dx
      dyi  = 1./dy
C     Now compute the advection
      DO 2000 j = 2, ny-1
        DO 2100 i = 2, nx-1
          advi(i,j) =
     1       cenx(i,j)
     2     - (f+beta*(j-jcen)*dy)*v(i,j)
 2100   CONTINUE
 2000 CONTINUE
      DO 2010 j = 2, ny-1
        DO 2110 i = 2, nx-1
          advj(i,j) =
     1       ceny(i,j)
     2    + (f+beta*(j-jcen)*dy)*u(i,j)
 2110   CONTINUE
 2010 CONTINUE

C     Add the various sub-terms first (for precision considerations)
      DO 1000 j = 1, ny
        DO 1100 i  = 1, nx
          diffi(i,j) = diffi(i,j)-advi(i,j)+qi(i,j)
          diffj(i,j) = diffj(i,j)-advj(i,j)+qj(i,j)
 1100   CONTINUE
 1000 CONTINUE

      DO 2400 j = 1, ny
        DO 2500 i = 1, nx
          unew(i,j) = uold(i,j) + diffi(i,j)*dt2
          vnew(i,j) = vold(i,j) + diffj(i,j)*dt2
 2500   CONTINUE
 2400 CONTINUE

C     Re-order arrays/update for next time step.
      DO 3000 j = 1, ny
        DO 3100 i = 1, nx
          uold(i,j) = u(i,j)
          vold(i,j) = v(i,j)
 3100   CONTINUE
 3000 CONTINUE

      DO 2600 j = 1, ny
        DO 2700 i = 1, nx
          u(i,j) = unew(i,j)
          v(i,j) = vnew(i,j)
 2700   CONTINUE
 2600 CONTINUE

C     Apply boundary conditions in main program

      RETURN
      END
