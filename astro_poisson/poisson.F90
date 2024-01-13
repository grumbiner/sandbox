SUBROUTINE poisson(rho, pot, nx, ny, nz, dx)
IMPLICIT none
INTEGER nx, ny, nz
REAL dx
DOUBLE PRECISION rho(nx, ny, nz), pot(nx, ny, nz)
DOUBLE PRECISION G
PARAMETER (G = 6.673e-11)
DOUBLE PRECISION pi
DOUBLE PRECISION rhs(nx, ny, nz)
DOUBLE PRECISION tmp(nx, ny, nz), delta(nx, ny, nz)
DOUBLE PRECISION toler
INTEGER iter, itmax
INTEGER i, j, k
DOUBLE PRECISION dpot(nx, ny, nz)

pi = ACOS(-1.)
toler = 1.e-6
itmax = nx*nx*nx
dpot = pot

rhs = -(4.*pi*G*rho )*dx*dx
iter = 1

DO WHILE (iter .LT. itmax)
  tmp = 0
  delta = 0
  DO k = 2, nz-1
  DO j = 2, ny-1
  DO i = 2, nx-1
    tmp(i,j,k) = (dpot(i+1,j,k)+dpot(i-1,j,k) ) + &
                 (dpot(i,j+1,k)+dpot(i,j-1,k) ) + &
                 (dpot(i,j,k+1)+dpot(i,j,k-1) ) - rhs(i,j,k)
    delta(i,j,k) = tmp(i,j,k) - 6.*dpot(i,j,k)
  ENDDO
  ENDDO
  ENDDO

  IF (MOD(iter, 10000) .EQ. 0) THEN
    PRINT *,'delta, pot ',iter, MAXVAL(delta), MINVAL(delta), MAXVAL(dpot)
  ENDIF
  IF (ABS(MAXVAL(ABS(delta))/MAXVAL(ABS(dpot))) < toler ) THEN
    WRITE (*,9002) iter, MAXVAL(delta), MINVAL(delta), MAXVAL(dpot)
    WHERE (dpot .NE. 0) delta = delta / dpot
    PRINT *,"relative ",MAXVAL(delta), MINVAL(delta)
    pot = dpot
    RETURN
  ELSE
    iter = iter + 1
    dpot  = tmp/6.
  ENDIF
9002 FORMAT ("final pot ",I6,3E15.6)

END DO
PRINT *,'no convergence ',iter, MAXVAL(delta), MINVAL(delta), MAXVAL(pot)

RETURN

END SUBROUTINE
