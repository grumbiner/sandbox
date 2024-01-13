DOUBLE PRECISION FUNCTION integral(x, nx, ny, nz)
IMPLICIT none
INTEGER nx, ny, nz
DOUBLE PRECISION x(nx, ny, nz)
INTEGER i, j, k
DOUBLE PRECISION sum

sum = 0
DO k = 1, ny
DO j = 1, ny
DO i = 1, nx
  sum = sum + x(i,j,k)
ENDDO
ENDDO
ENDDO

integral = sum
RETURN
END
