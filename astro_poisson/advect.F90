SUBROUTINE advect(u,v,w,x,nx, ny, nz, dx, dy, dz, nl)
IMPLICIT none
INTEGER nx, ny, nz
REAL dx, dy, dz
DOUBLE PRECISION u(nx, ny, nz), v(nx, ny, nz), w(nx, ny, nz), x(nx, ny, nz), nl(nx, ny, nz)
INTEGER i, j, k
DO k = 2, nz-1
DO j = 2, ny-1
DO i = 2, nx-1
  nl(i,j,k) = u(i,j,k)*(x(i+1,j,k)-x(i-1,j,k) )/2./dx +&
              v(i,j,k)*(x(i,j+1,k)-x(i,j-1,k) )/2./dy +&
              w(i,j,k)*(x(i,j,k+1)-x(i,j,k-1) )/2./dz
ENDDO
ENDDO
ENDDO
RETURN
END
