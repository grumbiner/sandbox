  PROGRAM grid_foundation
    IMPLICIT none
!Note, hardwired file names: gfs.ftn, latlon.ftn,
!Note: this is doing things at main program level, rather than component
!          model/module level
    INTEGER :: nx, ny
    PARAMETER (nx = 3072, ny = 1536)
    REAL :: dydi(nx, ny), dydj(nx, ny), dxdi(nx, ny), dxdj(nx, ny)
    REAL :: jacobian(nx, ny), dx(nx, ny), dy(nx, ny)
    REAL :: lat(nx, ny), lon(nx, ny)
    LOGICAL :: landmask(nx, ny)

! Local:
    REAL landin(nx, ny)
    INTEGER i,j
    DOUBLE PRECISION summing

! Establish the grid:
    OPEN (11, FILE="latlon.ftn", FORM="UNFORMATTED", STATUS="OLD")
    READ (11) lat
    READ (11) lon
    CLOSE (11)
    PRINT *,"lat  max min ",MAXVAL(lat), MINVAL(lat)
    PRINT *,"lon  max min ",MAXVAL(lon), MINVAL(lon)
    CALL geometry(lat, lon, dydi, dydj, dxdi, dxdj, jacobian, dx, dy, nx, ny)
    PRINT *,"jacobian  max min ",MAXVAL(jacobian)/1.e6, MINVAL(jacobian)/1.e6

    summing = 0
    DO j = 1, ny
    DO i = 1, nx
      summing = summing + jacobian(i,j)
    ENDDO
    ENDDO
    PRINT *,'total = ',summing/1e6/1e6


END PROGRAM grid_foundation

SUBROUTINE geometry(lat, lon, dydi, dydj, dxdi, dxdj, jacobian, dx, dy, nx, ny)
!suitable for module
      IMPLICIT none

      INTEGER, intent(in) :: nx, ny
      REAL, intent(in)  :: lat(nx, ny), lon(nx, ny)
      REAL, intent(out) :: dydi(nx, ny), dydj(nx, ny)
      REAL, intent(out) :: dxdi(nx, ny), dxdj(nx, ny)
      REAL, intent(out) :: dx(nx, ny), dy(nx, ny)
      REAL, intent(out) :: jacobian(nx, ny)

!follow example of resops.h
      REAL arcdis
      INTEGER i, ip, im, j, jp, jm

! finding dx, dy:
      DO j = 1, ny
      DO i = 1, nx-1
        ip = i + 1
        dx(i,j)     = 1000.*arcdis(lon(i,j), lat(i,j), lon(ip,j), lat(ip,j))
      ENDDO
      ENDDO
      i = nx
      im = i - 1
      DO j = 1, ny
        dx(i,j)     = 1000.*arcdis(lon(i,j), lat(i,j), lon(im,j), lat(im,j))
      ENDDO

!
      DO j = 1, ny - 1
        jp = j + 1
      DO i = 1, nx
        dy(i,j) = 1000.*arcdis(lon(i,j),lat(i,j),lon(i,jp),lat(i,jp))
      ENDDO
      ENDDO
      j = ny
      jm = j - 1
      DO i = 1, nx
        dy(i,j) = 1000.*arcdis(lon(i,j),lat(i,j),lon(i,jm),lat(i,jm))
      ENDDO

! d/di terms:
      DO j = 1, ny
      DO i = 1, nx-1
        ip = i + 1
        dydi(i,j) = dy(ip,j) - dy(i,j)
        dxdi(i,j) = dx(ip,j) - dx(i,j)
      ENDDO
      ENDDO
      i = nx
      im = i - 1
      DO j = 1, ny
        dydi(i,j) = dy(i,j) - dy(im,j)
        dxdi(i,j) = dx(i,j) - dx(im,j)
      ENDDO

! d/dj terms
      DO j = 1, ny - 1
        jp = j + 1
      DO i = 1, nx
        dydj(i,j) = dy(i,jp) - dy(i,j)
        dxdj(i,j) = dx(i,jp) - dx(i,j)
      ENDDO
      ENDDO
      j = ny
      jm = j - 1
      DO i = 1, nx
        dydj(i,j) = dy(i,j) - dy(i,jm)
        dxdj(i,j) = dx(i,j) - dx(i,jm)
      ENDDO

! Compute Jacobian of the transformation and inspect for zero values
      DO j = 1, ny
      DO i = 1, nx
        jacobian(i,j) = dxdi(i,j)*dydj(i,j) - dydi(i,j)*dxdj(i,j)
      ENDDO
      ENDDO

      jacobian = jacobian + dx*dy
      DO j = 1, ny
      DO i = 1, nx
        IF (jacobian(i,j) .EQ. 0 ) THEN
          PRINT *,'zero jacobian ',i,j,jacobian(i,j)
        ELSE IF ((jacobian(i,j)) .LE. 7.5e-4) THEN
          PRINT *,'small jacobian ',i,j,jacobian(i,j)
        ENDIF
      ENDDO
      ENDDO

      RETURN
END SUBROUTINE geometry
