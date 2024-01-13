SUBROUTINE divergence(u, v, w, rho, div, nx, ny, nz)
IMPLICIT none
INTEGER nx, ny, nz
DOUBLE PRECISION u(nx, ny, nz), v(nx, ny, nz), w(nx, ny, nz)
DOUBLE PRECISION rho(nx, ny, nz)
DOUBLE PRECISION div(nx, ny, nz)

INTEGER i, j, k
div = 0
DO k = 2, nz-1
DO j = 2, ny-1
DO i = 2, nx-1
  div(i,j,k) = (u(i+1,j,k)*rho(i+1,j,k) - u(i-1,j,k)*rho(i-1,j,k)) + &
               (v(i,j+1,k)*rho(i,j+1,k) - v(i,j-1,k)*rho(i,j-1,k)) + &
               (w(i,j,k+1)*rho(i,j,k+1) - w(i,j,k-1)*rho(i,j,k-1))
ENDDO
ENDDO
ENDDO

RETURN
END SUBROUTINE
