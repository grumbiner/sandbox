      SUBROUTINE sinext(ss, sd, nx, ny, xlen, ylen, tlen,
     1                  mag, i, delt, delx, dely)

C      Generate a sinusoidal function in space and time.  8-9-88
C     Robert Grumbine

      IMPLICIT none

      INTEGER nx, ny, xlen, ylen, tlen, i
      REAL ss(nx, ny), sd(nx, ny)
      REAL mag, delt, delx, dely
      REAL pi
      PARAMETER (pi = 3.141592654)
      INTEGER l, m

      DO 1000 m = 1, ny
        DO 1010 l = 1, nx
          ss(l, m) = cos(2.*pi*l*delx/xlen)*cos(2.*pi*m*dely/ylen)
     1                * sin(2.*pi*i*delt/tlen)*mag
 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
