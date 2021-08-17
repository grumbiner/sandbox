      PROGRAM t
C     Given spatially smoothed fields for A, and the original A0,
C     Start in with evolution of lambda, A.

      INTEGER nx, ny, nt
      PARAMETER (nx = 77)
      PARAMETER (ny = 93)
      PARAMETER (nt = 30)
    
      REAL dlim, beta, dx, dy
      PARAMETER (dx = 5*25.4E3)
      PARAMETER (dy = 5*25.4E3)
      PARAMETER (beta = 0.04*dx**2)
      PARAMETER (dlim = 0.005)
      REAL dt, a1, a2, a3, a4
      PARAMETER (dt = 8.64E4)
      PARAMETER (a2 = 10.) !from f0 order 0.1 A
CD      PARAMETER (a3 = 0.05) !from g0 order f0
      PARAMETER (a3 = dt*beta/dx/dx) !from dldt vs dadt
      PARAMETER (a1 = 1.E-9*a3) !from 10 cm/s drift being order f0
CD      PARAMETER (a4 = 0.005) !from psi order 0.1*g0
      PARAMETER (a4 = 0.1*a3) !from psi order 0.1*g0

      INTEGER i, j, k
      REAL dmax
      REAL a0(nx, ny, nt), a(nx, ny, nt)
      REAL lambda(nx, ny, nt),  w(nx, ny, nt)
      REAL u(nx-1, ny-1, nt), v(nx-1, ny-1, nt)
 
      DO 1 k = 1, nt
      DO 1 j = 1, ny
      DO 1 i = 1, nx 
        w(i,j,k) = 0.
        a(i,j,k) = 0.
        a0(i,j,k) = 0.
 1    CONTINUE
 
      CALL readin(a0, a, nx, ny, nt)
      CALL findw(a0, w, nx, ny, nt)

      CALL dadt(a, lambda, nx, ny, nt, dt, a3, a4)  
      CALL uofl(lambda, a, u, v, nx, ny, nt, dx, dy, dt, a1) 

      DO 1000 j = 1, ny-1
      DO 1000 i = 1, nx-1
        WRITE (*,9001) i,j,lambda(i,j,15), lambda(i,j,15)*dt/a3/2.,
     1       u(i,j,15), v(i,j,15)
 1000 CONTINUE
 9001 FORMAT (2I3, E13.6, F7.3, 2F8.3) 

      CALL intlam(lambda, a, a0, w, beta, nx, ny, nt, dx, dt)
      CALL uofl(lambda, a, u, v, nx, ny, nt, dx, dy, dt, a1) 
      DO 2000 j = 1, ny-1
      DO 2000 i = 1, nx-1
        WRITE (*,9001) i,j,lambda(i,j,15), lambda(i,j,15)/a3*86400.,
     1       u(i,j,15), v(i,j,15)
 2000 CONTINUE

      STOP
      END
