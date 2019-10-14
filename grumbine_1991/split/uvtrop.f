C***********************************************************----------!!
      SUBROUTINE uvtrop(ut, vt, we, h, dx, dy, f, beta, am, nx, ny)
C     Barotropic solution.
      IMPLICIT none

      INTEGER nx, ny
      REAL ut(nx, ny), vt(nx, ny), we(nx, ny)
      REAL dx, dy, f, beta, am, h

      REAL psi(nx, ny)
      REAL num, lb, wave
      INTEGER i, j

C     Compute the interior solution
      DO 1001 i = 1, nx
        psi(i,1) = 0.0
 1001 CONTINUE
      DO 1000 j = 1, ny
        psi(nx, j)  = 0.0
        psi(nx-1,j) = 0.0
        DO 1010 i = nx-2, 1, -1
          psi(i, j) = psi(i+1,j) + we(i+1,j) + we(i,j)
 1010   CONTINUE
 1000 CONTINUE
      num = f*dx/2./beta/h
      DO 1020 j = 1, ny
        DO 1030 i= 1, nx
          psi(i,j) = -psi(i,j) * num
 1030   CONTINUE
 1020 CONTINUE

C     Apply the boundary layer corrections.
      lb = (am/beta)**(1./3.)
      wave = SQRT(3.)*dx/2./lb
      DO 2000 j = 1, ny
        num = lb*(psi(nx,j)-psi(nx-1,j))/dx
        DO 2010 i = 1, nx
          psi(i,j) = psi(i,j)*(1.-EXP(-(i-1)*dx/2./lb)*
     1                 (COS(wave*(i-1)) + SIN(wave*(i-1))/SQRT(3.) ))
     2              - num*EXP( (i-nx)*dx/lb)
 2010   CONTINUE
 2000 CONTINUE

C     Now that we have psi, compute ut, vt.
      CALL stmfc4(psi, ut, vt, dx, dy, nx, ny)

      RETURN
      END
