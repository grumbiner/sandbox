C*************************************************----------++++++++++!!
      SUBROUTINE vexact(vt, we, psi, href, dx, f, beta, ahm, nx, ny)
C     Analytic evaluation of soln.
      IMPLICIT none
      INTEGER nx, ny
      REAL vt(nx, ny), we(nx, ny), psi(nx, ny)
      REAL dx, f, beta, ahm, href

      REAL num, lb, wave, phi, dx2
      INTEGER i, j

      dx2 = 2.*dx
C     COMPUTE THE INTERIOR SOLUTION
      DO 1001 i = 1, nx
        psi(i,1) = 0.0
 1001 CONTINUE
      DO 1000 j = 2, ny
CD        psi(nx, j) = we(nx, j)
        psi(nx, j)  = 0.0
        psi(nx-1,j) = 0.0
        DO 1010 i = nx-2, 1, -1
          psi(i, j) = psi(i+1,j) + we(i+1,j) + we(i,j)
 1010   CONTINUE
 1000 CONTINUE
      num = f*dx/2./beta/href
      DO 1020 j = 1, ny
        DO 1030 i= 1, nx
          psi(i,j) = -psi(i,j) * num
 1030   CONTINUE
 1020 CONTINUE

C     APPLY THE BOUNDARY LAYER CORRECTIONS.
      lb = (ahm/beta)**(1./3.)
      wave = SQRT(3.)*dx/2./lb
      DO 2000 j = 1, ny
        num = lb*(psi(nx,j)-psi(nx-1,j))/dx
        DO 2010 i = 2, nx-1
          phi = wave*(i-1)
          vt(i,j) = ((psi(i+1,j)-psi(i-1,j))/dx2)*
     1   (1.-EXP(-(i-1)*dx/2./lb)*
     2                 (COS(phi) + SIN(phi)/SQRT(3.) ))
     3   + psi(i,j)*EXP(-(i-1)*dx/2./lb)*SIN(phi)*(4./SQRT(3.))/2./lb
     4              - num*EXP( (i-nx)*dx/lb)/lb
 2010   CONTINUE
 2000 CONTINUE
 
      RETURN
      END
