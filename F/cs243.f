      SUBROUTINE cs243(u, v, radius, dx, dy, nx, ny, curl)
C     Compute the k component of the curl on the entire sphere,
C       given both the i and j components of the vector field.
C     Use centered differencing on a staggered grid.  BG 4-20-90.
C     Assume that j=1 corresponds to southernmost point,
C       = -90+dy/2 in degrees.
C     Scheme of 4-20-90 was d(Ucos(theta))/dtheta.
C     Change to explicit derivative of cos, --> dU/dtheta - Usin(theta)
C     Fix the definition of curl, need to divide dv/dx by cos theta.
C     Staggered grid means must average i, i+1 value of d/dy to get
C       right d/dy, similarly for d/dx.
C     Revised for speed, ~30% increase (on Mac) by precomputing scalars
C       and trig function  
C     Robert Grumbine 4-22-90

      IMPLICIT NONE
      INTEGER nx, ny
      REAL u(nx, ny), v(nx, ny), curl(nx, ny)
      REAL radius, dx, dy
      
      REAL theta
      REAL dx2, dy2, cosmdx, qtan
      REAL pi, pi2
      PARAMETER (pi = 3.141592654)
      INTEGER i, j
CD      SAVE u, v, curl

C     Precompute some scalars:
      dx2 = dx*2.
      dy2 = dy*2.
      pi2 = pi/2.

C     Compute the curl of the interior points
      DO 1000 j = 1, ny-1
        theta   = FLOAT(j)*dy - pi2
        cosmdx  = COS(theta)*dx2
        qtan    = TAN(theta)/4.0
        DO 1010 i = 1, nx-1

          curl(i,j) =( (v(i+1,j  )-v(i,j  ))
     1                +(v(i+1,j+1)-v(i,j+1)) )/cosmdx
     2       - ( u(i,j+1)-u(i  ,j)   + u(i+1,j+1)-u(i+1,j))/dy2
     4       + ( u(i,j+1)+u(i+1,j+1) + u(i,j    )+u(i+1,j))*qtan

 1010   CONTINUE
 1000 CONTINUE
 
C     Compute the curl for perimeter points.
C     Note that for the centered differencing, staggered grid,
C      j= ny corresponds to the north pole.  j=1 is at -90+dy
C      in the curl field.
C     Set curl equal to zero at the north pole. BG 4-20-90.
      DO 1200 i = 1, nx
        curl(i,ny) = 0.0
 1200 CONTINUE
C     Assume that i=1 is dx/2 and i=nx is -dx/2. (in the stress field)
C      i = 1 -- dx in the curl field.
      DO 2000 j = 1, ny-1
        theta   = FLOAT(j)*dy - pi2
        cosmdx  = COS(theta)*dx2
        qtan    = TAN(theta)/4.0
        i = nx
        curl(i,j) =( (v(1,j  )-v(i,j  ))
     1              +(v(1,j+1)-v(i,j+1)) )/cosmdx
     2     - ( u(i,j+1)-u(i,j  ) + u(1,j+1)-u(1,j))/dy2
     4     + ( u(i,j+1)+u(1,j+1) + u(i,j  )+u(1,j))*qtan

 2000 CONTINUE
 
C     Divide by radius
      DO 1100 j = 1, ny-1
        DO 1110 i = 1, nx
          curl(i,j) = curl(i,j)/radius
 1110   CONTINUE
 1100 CONTINUE

      RETURN
      END      
