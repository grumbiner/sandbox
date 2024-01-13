      SUBROUTINE init(u, v, gnew, g, gold, nx, ny, nstep, outfrq,
     1               forfrq, diffu, q0, period)
      IMPLICIT none

      INTEGER nx, ny, nstep, outfrq, forfrq
      REAL u(nx, ny), v(nx, ny), g(nx, ny), we(nx, ny)
      REAL gnew(nx, ny), gold(nx, ny), diffu

      INTEGER i, j, xcen, ycen, x1, x2, y1, y2
      REAL u0, v0, g0, q0, period
      REAL pi

      PRINT *,'How many time steps to run?'
      READ (*,9001) nstep
      PRINT *,'How often do you want the output?'
      READ (*,9001) outfrq
      PRINT *,'How often do you want the forward step?'
      READ (*,9001) forfrq
 9001 FORMAT (I7)

      pi = ATAN(1.0)*4.
      xcen = (1+nx)/2
      ycen = (1+ny)/2
      x1 = nx/3
      x2 = 2*nx/3
      y1 = ny/3
      y2 = 2*ny/3
CD      PRINT *,x1, x2, y1, y2, xcen, ycen

      PRINT *,'What is the reference u velocity?'
      READ (*,9002) u0
      PRINT *,'What is the reference v velocity?'
      READ (*,9002) v0
      PRINT *,'What is the reference g value?'
      READ (*,9002) g0
      PRINT *,'What is the diffusivity?'
      READ (*,9002) diffu
      PRINT *,'What is the forcing magnitude?'
      READ (*,9002) q0
      PRINT *,'What is the period of the forcing?'
      READ (*,9002) period
 9002 FORMAT (E13.6)

C     Initialize velocity to an elliptical field
      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
CD          u(i,j) = -u0*FLOAT(j-ycen)/FLOAT(ny-ycen)
CD          v(i,j) =  v0*FLOAT(i-xcen)/FLOAT(nx-xcen)
          g(i,j) = 0.0
          gold(i,j) = 0.0
          gnew(i,j) = 0.0
          we(i,j)  = q0*SIN(2.*pi*(j-2)/(ny-2))
 1100   CONTINUE
 1000 CONTINUE

      CALL uvtrop(u, v, we, 500., 2.E4, 2.E4, -1.4E-4, 7.E-12, 1.25E3)
C     Set boundary velocities to zero for mass conservation (!!)
      DO 1200 j = 1, ny
        u(1,j) = 0.0
        u(nx,j) = 0.0
        u(2,j) = 0.0
        u(nx-1,j) = 0.0
        v(1,j) = 0.0
        v(nx,j) = 0.0
        v(2,j) = 0.0
        v(nx-1,j) = 0.0
 1200 CONTINUE
      DO 1300 i = 1, nx
        v(i,1) = 0.0
        v(i,ny) = 0.0
        v(i,2) = 0.0
        v(i,ny-1) = 0.0
        u(i,1) = 0.0
        u(i,ny) = 0.0
        u(i,2) = 0.0
        u(i,ny-1) = 0.0
 1300 CONTINUE

C     Initialize g into a box
      DO 2000 j = y1, y2
        DO 2100 i = x1, x2
          g(i,j) = g0*
     1 FLOAT(i-x1)*FLOAT(i-x2)*FLOAT(j-y1)*FLOAT(j-y2)/
     2 FLOAT(xcen-x1)/FLOAT(xcen-x2)/FLOAT(ycen-y1)/FLOAT(ycen-y2)
 2100   CONTINUE
 2000 CONTINUE

      RETURN
      END
