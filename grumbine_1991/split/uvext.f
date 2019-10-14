C***********************************************************----------!!
      SUBROUTINE uvext (uc, vc, ss, href,
     1                  rhoref, g, f, beta, ahm, delx, dely, nx, ny)
C     Extrapolate u, v to the next time level
C     Computation of common constants added prior to 5-26-88.
C     Version rewritten for geostrophy, a la derivation. 4-5-89.
C       Much commented program deleted 4-5-89.
C     Unused variables deleted 7-16-90.
C     Per reviewer notes, overspecified boundary conditions removed.--
C       Note though that the bc. on SS effectively produce the same
C       result.  7-16-90.
C     Rho computation done via statement function, cuts about 98%
C       of computational time (overhead is tremendous!) 11/30/92
      IMPLICIT none
      INTEGER nx, ny

      REAL uc(nx, ny), vc(nx, ny)
      REAL ss(nx, ny)
      REAL rhoref, f, g, beta, ahm, href
      REAL delx, dely

      REAL rhos1p, rhos(nx, ny), bcorr
      INTEGER i, j, k, l

C     Params for speedier numerics:
      REAL dx2, dy2, g8rref

C     Density statement function
CD      REAL alpha, beta, gamma
      REAL gamma
CD      PARAMETER (alpha = -4.5795E-2)
CD      PARAMETER (beta  = -6.8927E-3)
      PARAMETER (gamma =  0.80908  )
C     reference values are T= -0.5, S=34.6, P = 0.0
CD      rhos1p(i,j) = t(i,j)*(alpha + beta*t(i,j)) + gamma*ss(i,j)
      rhos1p(i,j) = gamma*ss(i,j)

C     Compute params for speedier numerics:
      dx2    = 2.*delx
      dy2    = 2.*dely
      g8rref = g*href/4./rhoref/f

C     Compute the density field before entering the extrapolation.
C     This reduces the number of calls to the density function by
C       almost a factor of 4.  8-4-88.
      DO 900 l = 1, ny
        DO 910 k = 1, nx
            rhos(k,l) = rhos1p( k, l)
  910   CONTINUE
  900 CONTINUE

C     Compute the geostrophic velocity
      DO 1000 j = 2, ny-1
        DO 1010 i = 2, nx-1

          uc(i,j) = +g8rref*( rhos(i,j+1) - rhos(i,j-1) )/dy2
          vc(i,j) = -g8rref*( rhos(i+1,j) - rhos(i-1,j) )/dx2

 1010   CONTINUE
 1000 CONTINUE

C     Now compute the free slip velocities along boundaries.
      j = 1
      DO 1100 i = 2, nx-1
        uc(i,j) = +g8rref*( rhos(i,j+1) - rhos(i,j) )/dely
        vc(i,j) = -g8rref*( rhos(i+1,j) - rhos(i-1,j) )/dx2
 1100 CONTiNUE
      j = ny
      DO 1200 i = 2, nx-1
        uc(i,j) = +g8rref*( rhos(i,j) - rhos(i,j-1) )/dely
        vc(i,j) = -g8rref*( rhos(i+1,j) - rhos(i-1,j) )/dx2
 1200 CONTiNUE
      i = 1
      DO 1300 j = 2, ny-1
        uc(i,j) = +g8rref*( rhos(i,j+1) - rhos(i,j-1) )/dy2
        vc(i,j) = -g8rref*( rhos(i+1,j) - rhos(i,j) )/delx
 1300 CONTiNUE
      i = nx
      DO 1400 j = 2, ny-1
        uc(i,j) = +g8rref*( rhos(i,j+1) - rhos(i,j-1) )/dy2
        vc(i,j) = -g8rref*( rhos(i,j) - rhos(i-1,j) )/delx
 1400 CONTiNUE
      uc(1,1)   = +g8rref*( rhos(1 ,2)  - rhos(1 ,1) )/dely
      uc(1,ny)  = +g8rref*( rhos(1 ,ny) - rhos(1 ,ny-1) )/dely
      uc(nx,1)  = +g8rref*( rhos(nx,2)  - rhos(nx,1) )/dely
      uc(nx,ny) = +g8rref*( rhos(nx,ny) - rhos(nx,ny-1) )/dely
      vc(1,1)   = -g8rref*( rhos(2 ,1)  - rhos(1 ,1) )/delx
      vc(1,ny)  = -g8rref*( rhos(2 ,ny) - rhos(1 ,ny) )/delx
      vc(nx,1)  = -g8rref*( rhos(nx,1)  - rhos(nx-1,1) )/delx
      vc(nx,ny) = -g8rref*( rhos(nx,ny) - rhos(nx-1,ny) )/delx

C     Apply the beta correction to the velocities.
      DO 2000 j = 1, ny
        bcorr = 1. + beta*FLOAT(ny/2-j)*dely/f
        DO 2100 i = 1, nx
          uc(i,j) = uc(i,j)/bcorr
          vc(i,j) = vc(i,j)/bcorr
 2100   CONTINUE
 2000 CONTINUE

C     Now consider the boundary conditions:
C       7-16-90:
C         At i = 1         u = 0.0
C         At i = nx        u = 0.0
C         At j = 1         v = 0.0
C      8-2-90
C         At j = ny        v = 0.0

      DO 3000 i = 1, nx
C       BC on v at the y boundaries
        vc(i,1)  = 0.0
        vc(i,ny) = 0.0
 3000 CONTINUE

      DO 3010 j = 1, ny
C       u = 0.0 implemented 1-26-89
        uc(1,  j) = 0.0
        uc(nx, j) = 0.0
 3010 CONTINUE

      RETURN
      END
