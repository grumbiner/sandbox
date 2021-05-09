PROGRAM fv

  IMPLICIT none

  INTEGER ratio
  PARAMETER (ratio = 64)
  INTEGER nx, ny, nz
  PARAMETER (nx = 1*ratio)
  PARAMETER (ny = 1*ratio)
  PARAMETER (nz = 1*ratio)

  REAL cv, r, k
  PARAMETER (cv = 1004.)
  PARAMETER (r  = 289.)
  PARAMETER (k  = 0.026) !thermal conductivity of air
  REAL dt

! From -Wall, allocatable permits proper treatment of larger arrays
  REAL, allocatable :: dx(:, :, :), dy(:, :, :), dz(:, :, :)
  REAL, allocatable :: ax(:, :, :), ay(:, :, :), az(:, :, :), vol(:, :, :)
  REAL, allocatable :: u(:,:,:), v(:,:,:), w(:,:,:), rho(:,:,:)

  INTEGER i
  REAL mass
  
  ALLOCATE(dx(nx, ny, nz), dy(nx, ny, nz), dz(nx, ny, nz) )
  dx = 1.e-0 !meters
  dy = 1.e-0
  dz = 1.e-0

! Precomputing area of faces and volumes of the cells (efficiency of computation)
! Precompute volume of cells (ditto)
! Then Deallocate to free memory
  ALLOCATE(ax(nx, ny, nz), ay(nx, ny, nz), az(nx, ny, nz), vol(nx, ny, nz))
  CALL precompute(dx, dy, dz, ax, ay, az, vol, nx, ny, nz)
  DEALLOCATE(dx, dy, dz)

  ALLOCATE(u(nx, ny, nz), v(nx, ny, nz), w(nx, ny, nz), rho(nx, ny, nz))
  u = 0.0
  v = 0.0
  w = 0.0
  CALL initial_vels(u,v,w,nx,ny,nz)

  rho = 1.0  ! kg/m^3
  rho(nx/2, ny/2, nz/2) = 1. + 1./16.
  PRINT *,0,mass(rho, vol, nx, ny, nz), MAXVAL(rho), MINVAL(rho), MAXLOC(rho)

  !dt = 1.e-1
  dt = 1./16.

  OPEN(10, FILE="fout", FORM="UNFORMATTED", STATUS="UNKNOWN", ACCESS="STREAM")
  OPEN(11, FILE="slices", FORM="UNFORMATTED", STATUS="UNKNOWN", ACCESS="STREAM")
  DO i = 1, 12801
    CALL step(u, v, w, ax, ay, az, vol, rho, dt, nx, ny, nz)
    IF (MOD(i-1, 16) == 0) THEN
      PRINT *,i,mass(rho, vol, nx, ny, nz), MAXVAL(rho), MINVAL(rho), MAXLOC(rho)
      WRITE (10) rho(:,32, 32)
      WRITE (11) rho(:,:,32)
    ENDIF
  ENDDO
  PRINT *,i-1,mass(rho, vol, nx, ny, nz), MAXVAL(rho), MINVAL(rho), MAXLOC(rho)

END
REAL FUNCTION mass(rho, vol, nx, ny, nz)
  IMPLICIT none
  INTEGER, intent(in) ::  nx, ny, nz
  REAL, intent(in)    :: rho(nx, ny, nz), vol(nx, ny, nz)
  REAL(SELECTED_REAL_KIND(24,10)) :: tot
  INTEGER i, j, k
  tot = 0.0
  DO k = 1, nz
    DO j = 1, ny
      DO i = 1, nx
        tot = tot + rho(i,j,k)*vol(i,j,k)
      ENDDO
    ENDDO
  ENDDO

!  tmp = rho*vol
! faster than above (cuts ~80%):
!  mass =  SUM(rho*vol)
! ... but wrong, as accumulating 100,000,000 sums in single precision is not pretty.
  mass = tot
END FUNCTION mass
  
SUBROUTINE initial_vels(u,v,w,nx,ny,nz)
  IMPLICIT none
  INTEGER, intent(in) ::  nx, ny, nz
  REAL, intent(out)   :: u(nx, ny, nz), v(nx, ny, nz), w(nx, ny, nz)
  
  INTEGER i, j, k
  REAL x,y,z
  REAL cx, cy, cz
  REAL dx, sigma, u0

  u = 0.05
  !u(nx/2,ny/2,nz/2) = 0.025
  v = 0.05
  w = 0.05
  RETURN

  u0 = 0.05  ! for CFL condition, dt = 0.01, dx = 1.e-3
  dx = 1.e-3
  PRINT *,'half-crossing time ', nx/2.*dx/u0
  sigma = FLOAT(nx)/6.*dx
  cz = FLOAT(nz)/2.*dx
  cy = FLOAT(ny)/2.*dx
  cx = FLOAT(nx)/2.*dx
  DO k = 1, nz
    z = FLOAT(k)*dx
    DO j = 1, ny
      y = FLOAT(j)*dx
      DO i = 1, nx
        x = FLOAT(i)*dx
        u(i,j,k) = u0*exp(-(  (x-cx)**2 + (y-cy)**2 + (z-cz)**2 )/2./sigma**2)
        v(i,j,k) = u(i,j,k)
        w(i,j,k) = v(i,j,k)
      ENDDO
    ENDDO
  ENDDO

  RETURN
END SUBROUTINE initial_vels


SUBROUTINE precompute(dx, dy, dz, ax, ay, az, vol, nx, ny, nz)
  IMPLICIT none
  INTEGER, intent(in) ::  nx, ny, nz
  REAL, intent(in)  :: dx(nx, ny, nz), dy(nx, ny, nz), dz(nx, ny, nz)
  REAL, intent(out) :: ax(nx, ny, nz), ay(nx, ny, nz), az(nx, ny, nz)
  REAL, intent(out) :: vol(nx, ny, nz)

  ax  = dy*dz
  ay  = dx*dz
  az  = dx*dy
  vol = dx*dy*dz

  RETURN
END SUBROUTINE precompute

SUBROUTINE step(u, v, w, ax, ay, az, vol, rho, dt, nx, ny, nz) 
  IMPLICIT none
  INTEGER nx, ny, nz
  REAL ax(nx, ny, nz), ay(nx, ny, nz), az(nx, ny, nz)
  REAL u(nx, ny, nz), v(nx, ny, nz), w(nx, ny, nz)
  REAL vol(nx, ny, nz), rho(nx, ny, nz)
  REAL dt

  INTEGER i, j, k
  REAL drho(nx, ny, nz)

  drho = 0.0

! u
  DO k = 2, nz-1
    DO j = 2, ny-1
      DO i = 2, nx-1
        drho(i,j,k) =                                               &
! (a)
!  -ax(i,j,k)* ( (rho(i+1,j,k)-rho(i-1,j,k))/2. * u(i,j,k) )
! (1)
!  -ax(i,j,k)* ( (rho(i+1,j,k)-rho(i-1,j,k))/2. * u(i+1,j,k) )
! (2)
!  -ax(i,j,k)* ( (rho(i+1,j,k)-rho(i-1,j,k))/2. * (u(i+1,j,k)+u(i,j,k))/2. )

! (3)
           -( (rho(i+1,j,k)+rho(i,j,k))/2. * u(i+1,j,k)*ax(i+1,j,k) - &
              (rho(i-1,j,k)+rho(i,j,k))/2. * u(i  ,j,k)*ax(i  ,j,k)   )
! (4)
!!           -( rho(i,j,k) * u(i+1,j,k)*ax(i+1,j,k) - &
!!              rho(i,j,k) * u(i  ,j,k)*ax(i  ,j,k)   )
      ENDDO
    ENDDO
  ENDDO

! v
!  DO k = 2, nz-1
!    DO j = 2, ny-1
!      DO i = 2, nx-1
!        drho(i,j,k) = drho(i,j,k)  - ( &
!            (rho(i,j+1,k)+rho(i,j,k))/2. * v(i,j+1,k)*ay(i,j+1,k) - &
!            (rho(i,j-1,k)+rho(i,j,k))/2. * v(i,j,k)*  ay(i,j,k)     )
!      ENDDO
!    ENDDO
!  ENDDO

! w
!  DO k = 2, nz-1
!    DO j = 2, ny-1
!      DO i = 2, nx-1
!        drho(i,j,k) = drho(i,j,k) - ( &
!            (rho(i,j,k+1)+rho(i,j,k))/2. * w(i,j,k+1)*az(i,j,k+1) - &
!            (rho(i,j,k-1)+rho(i,j,k))/2. * w(i,j,k  )*az(i,j,k  ) )
!      ENDDO
!    ENDDO
!  ENDDO

  rho = rho + drho*dt/vol

!(5)
  rho(nx, :, :) = rho(nx-1,:,:)
!(5b)
  rho(1,:,:) = rho(2,:,:)

  RETURN
END SUBROUTINE step
