      PROGRAM ekman
C     generate a ekman pumping field for the barotropic flow soln.

      INTEGER nx, ny
      PARAMETER (nx = 20)
      PARAMETER (ny = 20)
      REAL u(nx, ny), v(nx, ny), we(nx, ny), s(nx, ny)

      INTEGER i, j
      REAL dx, dy
      REAL uref, vref, xlen, ylen
      INTEGER type
      CHARACTER*60 fname
      REAL pi
      PARAMETER (pi = 3.141592654)
      REAL coef, cd, rhoa, rhoo, f
      PARAMETER (cd = 2.E-3)
      PARAMETER (rhoa = 1.2)
      PARAMETER (rhoo = 1028)
      PARAMETER (f    = -1.4E-4)
      PARAMETER (coef = cd*rhoa/rhoo/f)

      PRINT *,'What is delta x?'
      READ (*,9002) dx
      PRINT *,'What is delta y?'
      READ (*,9002) dy
      PRINT *,'What is the reference forcing in u?'
      READ (*,9002) uref
      PRINT *,'What is the wavelength in x?'
      READ (*,9002) xlen
      PRINT *,'What is the wavelength in y?'
      READ (*,9002) ylen
      DO 1000 j = 1, ny
        DO 1010 i = 1, nx
          u(i,j) = uref*cos(2.*pi*(j-1)*dy/ylen)*
     1                  sin(2.*pi*(i-1)*dx/xlen)
 1010   CONTINUE
 1000 CONTINUE

      PRINT *,'What is the reference wind speed in v?'
      READ (*,9002) vref
      PRINT *,'What is the wavelength in x?'
      READ (*,9002) xlen
      PRINT *,'What is the wavelength in y?'
      READ (*,9002) ylen
      DO 2000 j = 1, ny
        DO 2010 i = 1, nx
          v(i,j) = vref*sin(2.*pi*(j-1)*dy/ylen)*
     1                  cos(2.*pi*(i-1)*dx/xlen)
 2010   CONTINUE
 2000 CONTINUE
      

      DO 5000 j = 1, ny
        DO 5010 i = 1, nx
          s(i,j) = SQRT(u(i,j)**2 + v(i,j)**2)
 5010   CONTINUE
 5000 CONTINUE

C     Now compute ekman pumping:
      DO 6000 j = 2, ny-1
        DO 6010 i = 2, nx-1
          we(i,j) = coef*(
     1 (u(i,j)*(s(i,j+1)-s(i,j-1))/2./dy
     2 +s(i,j)*(u(i,j+1)-u(i,j-1))/2./dy ) -
     3 (v(i,j)*(s(i+1,j)-s(i-1,j))/2./dx
     4 +s(i,j)*(v(i+1,j)-v(i-1,j))/2./dx ) )
 6010   CONTINUE
 6000 CONTINUE
      DO 6020 j = 1, ny
        we(1,j)  = 0.0
        we(nx,j) = 0.0
 6020 CONTINUE
      DO 6030 i = 1, nx
        we(i,1)  = 0.0
        we(i,ny) = 0.0
 6030 CONTINUE

      PRINT *,'What would you like to call the forcing file?'
      READ (*,9003) fname
      OPEN (10, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'What would you like to call the u file?'
      READ (*,9003) fname
      OPEN (11, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'What would you like to call the v file?'
      READ (*,9003) fname
      OPEN (12, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')

      WRITE (10) we

      DO 9999 i = 1, 10
        WRITE (11) u
        WRITE (12) v
 9999 CONTINUE

 9001 FORMAT (I1)

 9002 FORMAT (E13.7)

 9003 FORMAT (A60)

      CLOSE (10, STATUS='KEEP')
      CLOSE (11, STATUS='KEEP')
      CLOSE (12, STATUS='KEEP')
      END
