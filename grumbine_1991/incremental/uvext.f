C***********************************************************----------!!
      SUBROUTINE uvext (ub, vb, ss, sd, h, nx, ny,
     1                  rhoref, g, f, ahm, avm, delx, dely, delt)
C     Extrapolate u, v to the next time level
C     Computation of common constants added prior to 5-26-88.
C     Version rewritten for geostrophy, a la derivation. 4-5-89.
C       Much commented program deleted 4-5-89.

      INTEGER nx, ny
      REAL ub(nx, ny), vb(nx, ny)
      REAL ss(nx, ny), sd(nx, ny)
      REAL h(nx, ny)
      REAL rhoref, f, g, ahm, avm
      REAL delx, dely, delt

      INTEGER nnx, nny
      PARAMETER (nnx = 36)
      PARAMETER (nny = 36)
      REAL rhos1p, rhos(nnx, nny)
      INTEGER i, j, k, l
C     Params for speedier numerics:
      REAL dx2, dy2, g8rref

C     Compute params for speedier numerics:
      dx2    = 2.*delx
      dy2    = 2.*dely
      g8rref = g*h(1,1)/4./rhoref/f

C     Compute the density field before entering the extrapolation.
C     This reduces the number of calls to the density function by
C       almost a factor of 4.  8-4-88.
      DO 900 l = 1, ny
        DO 910 k = 1, nx
            rhos(k,l) = rhos1p( ss(k,l), 0.0, 0.0)
  910   CONTINUE
  900 CONTINUE

C     Compute the geostrophic velocity
      DO 1000 j = 2, ny-1
        DO 1010 i = 2, nx-1

          ub(i,j) = +g8rref*( rhos(i,j+1) - rhos(i,j-1) )/dy2
          vb(i,j) = -g8rref*( rhos(i+1,j) - rhos(i-1,j) )/dx2

 1010   CONTINUE
 1000 CONTINUE

C     Now consider the boundary conditions:
C       1-26-89:
C         At i = 1         u = v = 0.0
C         At i = nx        u = v = 0.0
C         At j = 1         u = v = 0.0
C         At j = ny normal deriv = 0.0

      DO 2000 i = 1, nx
C       BC on v at the y boundaries
        vb(i,1)  = 0.0
        vb(i,ny) = vb(i, ny-1)
C       BC on u at the y boundaries
        ub(i,1)  = 0.0
        ub(i,ny) = ub(i, ny-1)
 2000 CONTINUE

      DO 2010 j = 1, ny
C       v = 0.0 implemented 1-26-89
        vb(1,  j) = 0.0
        vb(nx, j) = 0.0
C       u = 0.0 implemented 1-26-89
        ub(1,  j) = 0.0
        ub(nx, j) = 0.0
 2010 CONTINUE

      RETURN
      END
