PROGRAM aerodynamics
IMPLICIT none
INTEGER nx, ny
PARAMETER (nx =  256*4)
PARAMETER (ny =  256*1)

REAL u1(nx, ny), v1(nx, ny), rho1(nx, ny)
REAL u2(nx, ny), v2(nx, ny), rho2(nx, ny)
REAL alpha, nu, dt, dx
PARAMETER (alpha = 7./5.)  ! adiabatic constant, P = PO*rho^alpha
PARAMETER (nu    = 1.5e-5) ! viscosity
PARAMETER (dx    = 1.5e-0) ! grid spacing 
PARAMETER (dt    = dx/500.)

INTEGER i, j
INTEGER freq, NSTEP
REAL uref
PARAMETER (uref  =  10.0)
PARAMETER (freq  =    60)
PARAMETER (NSTEP = 15000)          !PARAMETER (NSTEP = dx*NX/uref/dt )

!LOGICAL wing(nx, ny), edge(nx, ny)

rho1 = 0.0
rho2 = 0.0
v1   = 0.0
v2   = 0.0
u1   = -uref
u2   = -uref
!wing = .FALSE.

DO j = -ny/3, ny/3
DO i = -nx/3, nx/3
  rho1(nx/2 + i, ny/2+j) = rho1(nx/2 + i, ny/2+j) +   .003*exp( -(i*i+j*j)/2./9. )
ENDDO
ENDDO
u2 = u1

OPEN (10, FILE="fout", FORM="UNFORMATTED", STATUS="UNKNOWN")

DO i = 1, NSTEP
  !CALL drhodt(nx, ny, dx, dt, wing, edge, u1, v1, rho1, rho2)
  CALL drhodt(nx, ny, dx, dt, u1, v1, rho1, rho2)

  CALL dudt  (nx, ny, dx, dt, alpha, nu, -uref, u1, v1, rho2, u2, v2)

  IF (MOD(i-1,freq) .EQ. 0) THEN
    WRITE (10) rho2
    WRITE (10) u2
    WRITE (10) v2
    PRINT *,i-1,'maxes ',MAXVAL(rho2), MAXVAL(u2), MAXVAL(v2), MINVAL(u2), MINVAL(v2)
  ENDIF

  CALL drhodt(nx, ny, dx, dt, u2, v2, rho2, rho1)

  CALL dudt  (nx, ny, dx, dt, alpha, nu, -uref, u2, v2, rho1, u1, v1)

ENDDO
PRINT *,'Real time run length = ',NSTEP*dt
PRINT *,'advective time across screen ',dx*NX/uref
PRINT *,'number of steps ',NSTEP
PRINT *,'nx ny = ',nx, ny
END
!----------------------------------------------------------------------
SUBROUTINE dudt(nx, ny, dx, dt, alpha, nu, uref, u1, v1, rho1, u2, v2)
IMPLICIT none
INTEGER nx, ny
REAL dx, dt, alpha, nu, uref
REAL u1(nx, ny), v1(nx, ny), rho1(nx, ny)
REAL u2(nx, ny), v2(nx, ny)

REAL P0
PARAMETER (P0 = 1.01325e5) ! pressure when rho = 1

INTEGER i, j
REAL usq(nx, ny), vsq(nx, ny), zeta(nx, ny)
REAL tmp(nx, ny)

tmp = P0*alpha*((1.+rho1)**(alpha-2.))

usq = u1*u1
vsq = v1*v1

DO j = 2, ny - 1
DO i = 2, nx - 1
!  u(i,j) = u1(i,j) + dt * (-.5*grad(u^2) - curl(u)Xu + nu*lapl(u) - grad(P)/rho )
! grad(P)/rho = alpha * grad(rho)/rho * rho**(alpha-1)
! => alpha*grad(rho)*rho**(alpha-2)

!  zeta(i,j) = (u1(i,j+1)-u1(i,j-1) + v1(i+1,j) - v1(i-1,j))/2./dx

  u2(i,j) = u1(i,j) + dt * ( &
            - ( (rho1(i+1,j)-rho1(i-1,j))*tmp(i,j)/(2.*dx) )  &
!            - 0.5*(usq(i+1,j) - usq(i-1,j) + vsq(i+1,j) - vsq(i-1,j) ) / (2.*dx) & ! i
!            + zeta(i,j)*v1(i,j) &
            + (nu*(u1(i+1,j) + u1(i-1,j) + u1(i,j+1) + u1(i,j-1) - 4.*u1(i,j))/dx/dx ) &
            )
  v2(i,j) = v1(i,j) + dt * ( &
            - ( (rho1(i,j+1)-rho1(i,j-1))*tmp(i,j)/(2.*dx) )  &
!            - 0.5*(usq(i,j+1) - usq(i,j-1) + vsq(i,j+1) - vsq(i,j-1) ) / (2.*dx) & ! j
!            - zeta(i,j)*u1(i,j) &
            + (nu*(v1(i+1,j) + v1(i-1,j) + v1(i,j+1) + v1(i,j-1) - 4.*v1(i,j))/dx/dx) &
            )
ENDDO
ENDDO

RETURN
END

!SUBROUTINE drhodt(nx, ny, dx, dt, wing, edge, u1, v1, rho1, rho2)
SUBROUTINE drhodt(nx, ny, dx, dt, u1, v1, rho1, rho2)
! advance drhodt = -div(u*rho)
IMPLICIT none
INTEGER nx, ny
REAL dx, dt
REAL u1(nx, ny), v1(nx, ny), rho1(nx, ny)
REAL rho2(nx, ny)
!LOGICAL wing(nx, ny), edge(nx, ny)

INTEGER i, j

  DO j = 2, ny-1
  DO i = 2, nx-1
    !IF (.NOT. wing(i,j)) THEN

!full problem      rho2(i,j) = rho1(i,j) - dt*( u1(i+1,j)*rho1(i+1,j) - u1(i-1,j)*rho1(i-1,j) + &
!full problem                                   v1(i,j+1)*rho1(i,j+1) - v1(i,j-1)*rho1(i,j-1) ) / (2.*dx) &
!full problem                            - dt*( u1(i+1,j) - u1(i-1,j) + v1(i,j+1) - v1(i,j-1) ) / (2.*dx) ! unit density reference

!Linearized problem:
      rho2(i,j) = rho1(i,j) - dt*( u1(i+1,j) - u1(i-1,j) + v1(i,j+1) - v1(i,j-1) ) / (2.*dx) ! unit density reference
    !ENDIF
  ENDDO
  ENDDO
 
RETURN
END

SUBROUTINE insert_wing(nx, ny, wing, edge, u, v, rho)
IMPLICIT none
INTEGER nx, ny
LOGICAL wing(nx, ny), edge(nx, ny)
REAL u(nx, ny), v(nx, ny), rho(nx, ny)

INTEGER i, j
REAL x1, y1, radius

wing = .FALSE.
edge = .FALSE.
RETURN

x1 = 2.*nx/3.
y1 =    ny/2.
radius = 10.

DO j = 1, ny
DO i = 1, nx
  IF (sqrt((i-x1)**2+(j-y1)**2) .LE. radius) wing(nx, ny) = .FALSE.
ENDDO
ENDDO

DO j = 1, ny
DO i = 1, nx
  IF ( wing(i,j) ) THEN
    edge(i,j) = wing(i+1,j) .AND. wing(i-1,j) .AND. wing(i,j+1) .AND. wing(i,j-1)
    edge(i,j) = .NOT. edge(i,j)
    IF (.NOT. edge(i,j) ) THEN
      u(i,j) = 0.0
      v(i,j) = 0.0
      rho(i,j) = 0.0
    ENDIF
  ENDIF
ENDDO
ENDDO

RETURN
END
