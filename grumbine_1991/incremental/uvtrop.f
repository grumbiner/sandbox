C***********************************************************----------!!
      SUBROUTINE uvtrop(ut, vt, we, h, nx, ny, dx, dy, f, beta, am)
C     Barotropic solution.
      INTEGER nx, ny
      REAL ut(nx, ny), vt(nx, ny), we(nx, ny), h(nx, ny)
      REAL dx, dy, f, beta, am

      INTEGER nnx, nny
      PARAMETER (nnx = 36)
      PARAMETER (nny = 36)
      REAL psi(nnx, nny)
      REAL num, lb, wave
      INTEGER i, j

C     Compute the interior solution
      DO 1000 j = 1, ny
        psi(nx, j) = we(nx, j)
        DO 1010 i = nx-1, 1, -1
          psi(i, j) = psi(i+1,j) + we(i+1,j) + we(i,j)
 1010   CONTINUE
 1000 CONTINUE
      num = f*dx/2./beta/h(1,1)
      DO 1020 j = 1, ny
        DO 1030 i= 1, nx
          psi(i,j) = -psi(i,j) * num
 1030   CONTINUE
 1020 CONTINUE

C     Apply the boundary layer corrections.
      lb = (am/beta)**(1./3.)
      wave = SQRT(3.)*dx/2./lb
      DO 2000 j = 1, ny
        num = f*we(nx,j)*lb/beta/h(1,j)
        DO 2010 i = 1, nx
          psi(i,j) = psi(i,j)*(1.-exp(-(i-1)*dx/2./lb)*
     1                 (cos(wave*(i-1)) + sin(wave*(i-1))/SQRT(3.) ))
     2              - num*exp( (i-nx)*dx/lb)
 2010   CONTINUE
 2000 CONTINUE

C     Now that we have psi, compute ut, vt.
      DO 3000 j = 2, ny-1
        DO 3010 i = 2, nx-1
          ut(i,j) = -(psi(i,j+1)-psi(i,j-1))/2./dy
          vt(i,j) =  (psi(i+1,j)-psi(i-1,j))/2./dx
 3010   CONTINUE
 3000 CONTINUE
      DO 3020 j = 2, ny-1
        i = 1
        ut(i,j) = -(psi(i,j+1)-psi(i,j-1))/2./dy
        vt(i,j) =  (psi(i+1,j)-psi(i  ,j))   /dx
        i = nx
        ut(i,j) = -(psi(i,j+1)-psi(i,j-1))/2./dy
        vt(i,j) =  (psi(i  ,j)-psi(i-1,j))   /dx
 3020 CONTINUE
      DO 3030 i = 1, nx
        j = 1
        ut(i,j) = -(psi(i,j+1)-psi(i,j))/dy
        vt(i,j) =  (psi(i+1,j)-psi(i-1,j))/2./dx
        j = ny
        ut(i,j) = -(psi(i,j)-psi(i,j-1))/dy
        vt(i,j) =  (psi(i+1,j)-psi(i-1,j))/2./dx
 3030 CONTINUE

      RETURN
      END
