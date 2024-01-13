PROGRAM alpha

  USE types

  IMPLICIT none

  INTEGER nx, ny
  PARAMETER (nx = 360)
  PARAMETER (ny = 180)

  TYPE(cell) field(nx, ny), temp(nx, ny)
!really wants a type on tops of cell, which includes nx, ny
!  ... and dt

! initialize:
  REAL dt
  REAL runlen
  PARAMETER (runlen = 3600.0)
  INTEGER i, nstep

  field%h   = 4000.
  field%u   = 0.
  field%v   = 0.
  field%eta = 0.
  field%f   = 1.e-4
  field%dx  = 2.5e3
  field%dy  = 2.5e3

!  PRINT *,MAXVAL(field%dx) / r_earth * nx 

  dt = MIN ( MINVAL(field%dx)/sqrt(g_earth*MAXVAL(field%h)),  &
             MINVAL(field%dy)/sqrt(g_earth*MAXVAL(field%h)) )
  dt = dt / sqrt(2.)
  PRINT *,'dt = , nstep = ',dt, runlen/dt

! find a decent dt that divides in to runlen evenly while < dt start
  nstep = CEILING(runlen/dt)
  PRINT *,'nstep = ',nstep, runlen/nstep, LOG(FLOAT(nstep))/LOG(2.)
  PRINT *,'power of 2 step, dt ',CEILING(LOG(FLOAT(nstep))/LOG(2.))  &
              ,runlen/2**CEILING(LOG(FLOAT(nstep))/LOG(2.))          &
              ,2**CEILING(LOG(FLOAT(nstep))/LOG(2.))
  nstep = 2**CEILING(LOG(FLOAT(nstep))/LOG(2.))
  dt    = runlen/nstep
  PRINT *,'dt, nstep = ',dt, nstep
 
  temp = field
  field(nx/2, ny/2)%eta = 1.0
  DO i = 1, nstep/2
    CALL advance(field, temp, nx, ny, dt)
    CALL advance(temp, field, nx, ny, dt)
  ENDDO
  PRINT *,MAXVAL(field%eta), MAXLOC(field%eta), MAXVAL(field%u), MAXVAL(field%v)
  


END PROGRAM alpha

SUBROUTINE advance(field, temp, nx, ny, dt)
  USE types
  IMPLICIT none
  INTEGER nx, ny
  REAL dt
  TYPE(cell) field(nx, ny)
  TYPE(cell) temp(nx, ny)
  INTEGER i, j

  DO j = 2, ny-1
  DO i = 2, nx-1
!    fn   = f + dt * (-fkxU) - dt*g*grad(eta) 
!    temp(i,j)%u   = field(i,j)%u - dt*g_earth*(field(i+1,j)%eta-field(i-1,j)%eta)/2./field(i,j)%dx
    temp(i,j)%v   = field(i,j)%v - dt*g_earth*(field(i,j+1)%eta-field(i,j-1)%eta)/2./field(i,j)%dy
    temp(i,j)%eta = field(i,j)%eta &
    - dt * (field(i+1,j)%u - field(i-1,j)%u)*field(i,j)%h /2./field(i,j)%dx &
    - dt * (field(i,j+1)%v - field(i,j-1)%v)*field(i,j)%h /2./field(i,j)%dy

  ENDDO
  ENDDO

END SUBROUTINE advance
