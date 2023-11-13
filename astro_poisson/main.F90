PROGRAM a

IMPLICIT none

INTEGER nx, ny, nz
PARAMETER (nx = 32*1)
PARAMETER (ny = 32*1)
PARAMETER (nz = 32*1)
DOUBLE PRECISION rho(nx, ny, nz), u(nx, ny, nz), v(nx, ny, nz), w(nx, ny, nz)
DOUBLE PRECISION fx(nx, ny, nz), fy(nx, ny, nz), fz(nx, ny, nz), pot(nx, ny, nz)
DOUBLE PRECISION div(nx, ny, nz), nl(nx, ny, nz)
DOUBLE PRECISION G
PARAMETER (G = 6.673e-11)
REAL dx, dy, dz, dt
PARAMETER (dx = 100.*150e9 / FLOAT(nx) ) ! meters, 100 au box
PARAMETER (dy = dx)
PARAMETER (dz = dx)
REAL sun
PARAMETER (sun = 2.E30/1000)

INTEGER count, step, i, j, k
REAL r, umax
DOUBLE PRECISION sum, tmp, time, integral

OPEN(10,FILE="rho", FORM="FORMATTED")

rho = sun / FLOAT(nx*ny*nz) / dx**3
rho(1,:,:) = 0
rho(nx,:,:) = 0
rho(:,1,:) = 0
rho(:,ny,:) = 0
rho(:,:,nz) = 0
rho(:,:,1) = 0

u = 0
v = 0
w = 0
fx = 0
fy = 0
fz = 0
pot = 6e6/1000. !do this to avoid division by 0 later
dt = (86400.0*365.25)/16. !seconds, ~1 year
time = 0

!lapl(pot) = 4*pi*G*rho
!first guess = paraboloid, 2*pi*G
CALL poisson(rho, pot, nx, ny, nz, dx)


DO step = 1, 16*10000
  time = time + dt

  DO k = 2, nz-1
  DO j = 2, ny-1
  DO i = 2, nx-1
    fx(i,j,k) = (pot(i+1,j,k)-pot(i-1,j,k) )
    fy(i,j,k) = (pot(i,j+1,k)-pot(i,j-1,k) )
    fz(i,j,k) = (pot(i,j,k+1)-pot(i,j,k-1) )
  ENDDO
  ENDDO
  ENDDO
  fx = fx / dx/2.
  fy = fy / dx/2.
  fz = fz / dx/2.

!dt -> dx / |u|
!dt -> u / |F|
!u = u + F*dt
!rho -> drho/dt = - div(rho*u_vec)

  umax = MAXVAL(ABS(u))
  IF (umax .GT. 3.e7) THEN
    PRINT *,'umax is too close to speed of light! ',umax
    STOP
  ENDIF

  IF (dx < 0.25*dt*umax) THEN
    tmp = dt / (4.*dx/umax)
    dt  = dt / CEILING(tmp)
    PRINT *,'had to downscale dt ',dt, dx, dx/umax
  !  STOP
  ELSE
  !  PRINT *,'max dt = ',0.25*dx/MAX(0.1,MAXVAL(ABS(u)))
  ENDIF

  CALL divergence(u, v, w, rho, div, nx, ny, nz)
  rho = rho - dt*div/2./dx
  IF (MINVAL(rho) .LT. 0) THEN
    sum = 0
    count = 0
    DO k = 1,nz
    DO j = 1,ny
    DO i = 1,nx
      IF (rho(i,j,k) .LT. 0) THEN
        count = count + 1
        sum = sum + rho(i,j,k)
      ENDIF
    ENDDO
    ENDDO
    ENDDO

    PRINT *,'mass ',step, count, -sum*dx*dy*dz/sun
    WHERE (rho .LT. 0) rho = 0
    WHERE (rho .GT. 0) rho = rho + sum/DBLE(nx*ny*nz-count)
    WHERE (rho .EQ. 0) u = 0
    WHERE (rho .EQ. 0) v = 0
    WHERE (rho .EQ. 0) w = 0

    IF (count .GE. nx*ny*nz/2) STOP
  ENDIF

  CALL advect(u,v,w,u,nx, ny, nz, dx, dy, dz, nl)
  fx = fx - nl
  CALL advect(u,v,w,v,nx, ny, nz, dx, dy, dz, nl)
  fy = fy - nl
  CALL advect(u,v,w,w,nx, ny, nz, dx, dy, dz, nl)
  fz = fz - nl

  u = u + fx*dt
  v = v + fy*dt
  w = w + fz*dt

  !lapl(pot) = 4*pi*G*rho
  !CALL poisson(rho, pot, nx, ny, nz, dx)

  WRITE (*,9001) time/(8.64e4*365.25), MAXVAL(u), MAXVAL(v), MAXVAL(w), MINVAL(u), MINVAL(v), MINVAL(w)
9001 FORMAT (F13.8, ' uvw ',3E13.6, '  ',3E14.6)
  WRITE (*,9002) time/(8.64e4*365.25), MAXVAL(rho), MINVAL(rho), rho(nx/2, ny/2, nz/2), integral(rho, nx, ny, nz)*dx*dy*dz
9002 FORMAT (F13.8, ' rho ',4E14.5)
  IF (MOD(step,32) .EQ. 0) THEN
    DO i = 1, nx
      WRITE (10,*) time/(8.64e4*365.25), rho(i,ny/2, nz/2), u(i,ny/2, nz/2), pot(i,ny/2, nz/2)
    ENDDO
  ENDIF

ENDDO

END
