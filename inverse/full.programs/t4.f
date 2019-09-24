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
      REAL b(nx, ny, nt)
 
      DO 1 k = 1, nt
      DO 1 j = 1, ny
      DO 1 i = 1, nx 
        w(i,j,k) = 0.
        a(i,j,k) = 0.
        b(i,j,k) = 0.
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

C     Compute the residual in the dadt equation
      CALL resa(lambda, a, b, nx, ny, nt, dx, dy, a1)
      DO 3000 j = 2, ny-1
      DO 3000 i = 2, nx-1
        WRITE (*,9002) i, j, lambda(i,j,15)/a3*dt, b(i,j,15)*dt
 3000 CONTINUE 
 9002 FORMAT (2I3, 2F8.5)

C     Compute the residual in the dldt equation
      CALL resl(lambda, a, a0, b, w, beta, dx, a1, nx, ny, nt)
      DO 4000 j = 2, ny-1
      DO 4000 i = 2, nx-1
        WRITE (*,9002) i, j, lambda(i,j,15)/a3*dt, b(i,j,15)*dt
 4000 CONTINUE 

CD      CALL intlam(lambda, a, a0, w, beta, nx, ny, nt, dx, dt)
CD      CALL uofl(lambda, a, u, v, nx, ny, nt, dx, dy, dt, a1) 
CD      DO 2000 j = 1, ny-1
CD      DO 2000 i = 1, nx-1
CD        WRITE (*,9001) i,j,lambda(i,j,15), lambda(i,j,15)/a3*86400.,
CD     1       u(i,j,15), v(i,j,15)
CD 2000 CONTINUE

      STOP
      END
C********************************************
      SUBROUTINE resl(lambda, a, a0, b, w, beta, dx, a1, nx, ny, nt)

      IMPLICIT none
      INTEGER nx, ny, nt
      REAL beta, dx, a1
      REAL a(nx, ny, nt), b(nx, ny, nt), a0(nx, ny, nt)
      REAL lambda(nx, ny, nt), w(nx, ny, nt)

      INTEGER i, j, k
       
      DO 1000 k = 1, nt
      DO 1100 j = 2, ny-1
      DO 1100 i = 2, nx-1
        b(i,j,k) = 2*w(i,j,k)*(a(i,j,k)-a0(i,j,k)) - 2*beta*
     1  (-4.*a(i,j,k)+a(i+1,j,k)+a(i,j+1,k)+a(i-1,j,k)+a(i,j-1,k) )
     2 - (a(i,j,k)/a1/2./dx/dx/4.)*
     2      ( (lambda(i+1,j,k)-lambda(i-1,j,k))**2  
     3      + (lambda(i,j+1,k)-lambda(i,j-1,k))**2 )
 1100 CONTINUE
 1000 CONTINUE

      RETURN
      END
C********************************************
      SUBROUTINE resa(lambda, a, b, nx, ny, nt, dx, dy, a1)
C     Compute residuals from dadt equation
      IMPLICIT none

      INTEGER nx, ny, nt
      REAL a1, dx, dy
      REAL lambda(nx, ny, nt), a(nx, ny, nt), b(nx, ny, nt)

      INTEGER i, j, k

      DO 1000 k = 1, nt

        DO 1100 j = 2, ny-1
        DO 1100 i = 2, nx-1
          b(i,j,k) = (lambda(i+1,j,k)-lambda(i-1,j,k))*
     1        (a(i+1,j,k)*a(i+1,j,k) - a(i-1,j,k)*a(i-1,j,k)) 
     2 + (lambda(i,j+1,k) - lambda(i,j-1,k))*
     3        (a(i,j+1,k)*a(i,j+1,k) - a(i,j-1,k)*a(i,j-1,k)) 
     4 + a(i,j,k)*a(i,j,k)*(
     5   lambda(i+1,j,k)+lambda(i,j+1,k)+lambda(i-1,j,k)+lambda(i,j-1,k)
     6     -4.*lambda(i,j,k) )
 1100   CONTINUE

        DO 1200 j = 2, ny-1
        DO 1200 i = 2, nx-1
          b(i,j,k) = b(i,j,k)/2./a1/dx/dx
 1200   CONTINUE

 1000 CONTINUE

      k=15
CD      DO 1300 j = 2, ny-1
CD      DO 1300 i = 2, nx-1
CD        WRITE (*,9001) i,j,b(i,j,k)*86400.
CD 1300 CONTINUE

      RETURN
      END        

C********************************************
      SUBROUTINE intlam(lambda, a, a0, w, beta, nx, ny, nt, dx, dt)
C     Rectangle rule estimation of lambda from initial equation.
      IMPLICIT none

      INTEGER nx, ny, nt
      REAL dx, dt, beta
      REAL lambda(nx, ny, nt), a(nx, ny, nt), a0(nx, ny, nt)
      REAL w(nx, ny, nt)

      INTEGER i, j, k

      k=1
      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        lambda(i,j,k) = 0.
 1000 CONTINUE

      DO 2000 k = 2, nt
      PRINT *,'intlam k = ',k
      DO 2100 j = 2, ny-1
      DO 2200 i = 2, nx-1
        lambda(i,j,k) = lambda(i,j,k-1) + ( 
     1       2.*w(i,j,k)*(a(i,j,k)-a0(i,j,k)) - 
     2       2.*beta*(a(i+1,j,k)+a(i-1,j,k)+a(i,j+1,k)+a(i,j-1,k)
     3                    -4.*a(i,j,k) )/dx**2 ) * dt
 2200 CONTINUE
 2100 CONTINUE
 2000 CONTINUE

      RETURN
      END

      SUBROUTINE dadt(a, lambda, nx, ny, nt, dt, a3, a4)
C     Compute dA/dt, and use that to estimate lambda
      IMPLICIT none
      INTEGER nx, ny, nt
      REAL dt, a3, a4, a(nx, ny, nt), lambda(nx, ny, nt)
      INTEGER i, j, k

      DO 1000 k = 2, nt-1
      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        lambda(i,j,k) = (a(i,j,k+1) - a(i,j,k-1) ) /2./dt
 1000 CONTINUE

      DO 2000 j = 1, ny
      DO 2000 i = 1, nx
        lambda(i,j,1) = (a(i,j,2) - a(i,j,1) ) /dt
        lambda(i,j,nt) = (a(i,j,nt) - a(i,j,nt-1) ) /dt
 2000 CONTINUE

      DO 3000 k = 1, nt
      DO 3000 j = 1, ny
      DO 3000 i = 1, nx
        lambda(i,j,k) = lambda(i,j,k) / (1./2./a3 + 1./2./a4)
 3000 CONTINUE

      RETURN
      END

      SUBROUTINE uofl(lambda, a, u, v, nx, ny, nt, dx, dy, dt, a1) 
C     Given lambda, compute a velocity field, assuming that u0 = 0
C     and given that the velocity points are on the half integer points
C     in lambda.
      INTEGER nx, ny, nt
      REAL dx, dy, dt, a1
      REAL lambda(nx, ny, nt), a(nx, ny, nt)
      REAL u(nx-1, ny-1, nt), v(nx-1, ny-1, nt)
      
      INTEGER i, j, k
     
      DO 1 k = 1, nt
      DO 1000 j = 1, ny-1
      DO 1000 i = 2, nx
        u(i-1, j, k) = a(i,j, k) * (lambda(i,j, k)-lambda(i-1,j, k))
     1      /dx / 2. / a1
 1000 CONTINUE
      DO 2000 j = 2, ny
      DO 2000 i = 1, nx-1
        v(i-1, j, k) = a(i,j, k) * (lambda(i,j, k)-lambda(i,j-1, k))
     1      /dy / 2. / a1
 2000 CONTINUE

    1 CONTINUE

      RETURN
      END
       

      SUBROUTINE iter(a, a0, w, b, beta, nx, ny, dx, dy, dmax)
C     Relaxation solution to beta*lapl(a) = w*(a-a0) 
      INTEGER i, j, nx, ny
      REAL beta, dx, dy
      REAL a(nx, ny), a0(nx, ny), w(nx, ny), b(nx, ny)
      REAL dmax

      dmax = 0.
      DO 1000 j = 2, ny-1
      DO 1000 i = 2, nx-1
        b(i,j) = (a(i+1,j) + a(i-1,j) + a(i,j+1) + a(i,j-1) 
     1            + w(i,j)*dx**2/beta*a0(i,j) ) 
     2          / (4+w(i,j)*dx**2/beta )
     3          - a(i,j)
        dmax = AMAX1(dmax, ABS(b(i,j)) )
        a(i,j) = a(i,j) + b(i,j)
 1000 CONTINUE
CD      PRINT *,'dmax = ',dmax

      RETURN
      END

      SUBROUTINE findw(a0, w, nx, ny, nt)
C     Find the weighting function for a0
      INTEGER nx, ny, i, j, k
      REAL a0(nx,ny, nt), w(nx, ny, nt)
    
      DO 1000 k = 1, nt
      DO 1000 j = 1, ny
      DO 1000 i = 1, nx
        IF (a0(i,j, k) .GT. 1.28) THEN
          w(i,j, k) = 0.0
        ELSE IF (a0(i,j, k) .GT. 1.00) THEN
CD          a0(i,j, k) = 1.0
          w(i,j, k) = 1.0
        ELSE
          w(i,j, k) = 1.0
        ENDIF
 1000 CONTINUE

      RETURN
      END
C********************************************
      SUBROUTINE readin(a0, a, nx, ny, nt)
      INTEGER nx, ny, nt, i, j, k
      CHARACTER*60 fname
      REAL a0(nx, ny, nt)
      REAL a(nx, ny, nt)
      
      READ (*,9001) fname
 9001 FORMAT (A60)
      OPEN (10, FILE=fname, FORM="FORMATTED", STATUS="OLD")

      DO 1002 k = 1, nt
      DO 1000 j = 2, ny-1
      DO 1001 i = 2, nx-1
        READ (10,*) d1, d2, a0(d1,d2,k), a(d1, d2, k)
 1001 CONTINUE
 1000 CONTINUE
 1002 CONTINUE

      RETURN
      END 
C********(*********(*********(*********(*********(*********(*********(!!
      SUBROUTINE laplac(diffus, ss, nx, ny, dx, dy, diffu)
      IMPLICIT none

      INTEGER nx, ny
      REAL diffus(nx, ny), ss(nx, ny)
      REAL dx, dy, diffu

      INTEGER i, j
      REAL difu, value
 
      difu  = diffu/dx/dx

C     Evaluate the diffusion:
C     Laplacean in the central part of the domain.  Centered in 
C       x and y.
      DO 100 j = 2, ny-1
        DO 101 i = 2, nx-1
          diffus(i,j) =
     1    ss(i+1,j) -4.*ss(i,j) +ss(i-1,j)
     3  + ss(i,j+1) +ss(i,j-1)
  101   CONTINUE
  100 CONTINUE

C     Diffusion along walls
      DO 200 j = 1, ny
        diffus(1,j) = 0.0
        diffus(nx,j) = 0.0
 200  CONTINUE
      DO 210 i = 1, nx
        diffus(i,1) = 0.0
        diffus(i,ny) = 0.0
  210 CONTINUE

      DO 2000 j = 1, ny
        DO 2100 i = 1, nx
          diffus(i,j) = diffus(i,j)*difu
 2100   CONTINUE
 2000 CONTINUE


CD      PRINT *,'laplac'

      RETURN
      END
