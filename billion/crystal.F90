!====================================================================
SUBROUTINE iterate(outfreq, nsteps, m, dt, dx, dy, dz, ref, bodies, npts) 
  USE things
  IMPLICIT none

  REAL(crystal_kind), intent(in) :: dx, dy, dz, dt, m
  INTEGER, intent(in) :: npts, ref, nsteps, outfreq

  TYPE(thing), intent(inout) :: bodies(npts)

! local:
  TYPE(triplet) :: initial_loc
  REAL(crystal_kind) :: lennard_jones
  TYPE(triplet) lennard_jones_gradient
  TYPE(triplet) force
  INTEGER count, i, j
  CHARACTER(1) tab 
  REAL(crystal_kind) :: minpot, fraction, dt_suggest
  LOGICAL short
  REAL, ALLOCATABLE ::  tmp_pot(:)
  INTEGER           :: active(npts)
  INTEGER nt

  tab = ACHAR(9)
  fraction = 3.16e-4
  short    = .FALSE.
  initial_loc = bodies(ref)%x / dx

  PRINT *,'nsteps, outfreq = ',nsteps, outfreq
  IF (.NOT. short) THEN
    active = 0
    CALL scan(bodies, npts, ref, fraction, minpot, active, count, dx, m, dt_suggest)
    short = .TRUE.
    IF (dt > dt_suggest) THEN
      nt = nsteps*CEILING(dt/dt_suggest)
      PRINT *,'change dt, ',dt, dt_suggest, nsteps, nt
      FLUSH(6)
     ELSE
      nt = nsteps
    ENDIF 
  ENDIF

  DO i = 1, nt

    IF (.NOT. short) THEN
      active = 0
      CALL scan(bodies, npts, ref, fraction, minpot, active, count, dx, m, dt_suggest)
!D      WRITE (6,*) 'back from scan, count = ',i, count
!D      FLUSH(6)
      short = .TRUE.
    ENDIF

    CALL initialize(force)
    IF (count .NE. 0) THEN
      DO j = 1, count
        force = force + lennard_jones_gradient(bodies(ref), bodies(active(j)) )
      ENDDO
    ENDIF

    IF (MOD(i-1,outfreq) .EQ. 0) THEN
      WRITE (6,9000) i*dt_suggest, tab, &
         bodies(ref)%x%x/dx - initial_loc%x,  tab, bodies(ref)%u%x,  tab, force%x/m,  tab,&
         bodies(ref)%x%y/dy - initial_loc%y,  tab, bodies(ref)%u%y,  tab, force%y/m,  tab, &
         bodies(ref)%x%z/dz - initial_loc%z,  tab, bodies(ref)%u%z,  tab, force%z/m
      FLUSH(6)
    ENDIF

    CALL accelerate(bodies(ref), dt_suggest, force, m)
    CALL time_step (bodies(ref), dt_suggest, force, m)

    IF (MOD(i,100) .EQ. 0) THEN
      short = .false.
    ENDIF
  ENDDO
9000 FORMAT (10(E14.6, A1), E14.6)

END SUBROUTINE iterate

!===============================================
! F = -grad(V)
TYPE(triplet) FUNCTION lennard_jones_gradient(p1, p2) result (force)
  USE things
  IMPLICIT none
  TYPE(thing) p1, p2
  REAL(crystal_kind) :: sigma, epsilon
  REAL(crystal_kind) :: r

  sigma   =  3.393e-10       ! first zero in potential, material property
  epsilon = -1.602e-19 * 0.002757    ! minimum potential, -1 ev, say

  r = dist(p1%x, p2%x)

  force%x = - 4.*epsilon*( -12.*(sigma/r)**12 + 6.*(sigma/r)**6)/r
  force%y = force%x
  force%z = force%x

  force%x = + force%x * (p2%x%x - p1%x%x)/r
  force%y = + force%y * (p2%x%y - p1%x%y)/r
  force%z = + force%z * (p2%x%z - p1%x%z)/r

END
!===============================================
REAL(crystal_kind) FUNCTION lennard_jones(p1, p2) result (vlj)
  USE things
  IMPLICIT none
  TYPE(thing) p1, p2
  REAL(crystal_kind) :: r, sigma, epsilon
  sigma   = 0.3393e-9         ! first zero in potential, material property
  epsilon = -1.602e-19*0.002757    ! minimum potential, -1 ev, say
  r = dist(p1%x, p2%x)
  vlj = 4.*epsilon*(sigma/r)**6*((sigma/r)**6 - 1) 
END 
!===============================================
SUBROUTINE scan(bodies, npts, ref, fraction, minpot, active, count, &
      dx, m, dt_suggest)
! Scan for points with high enough potential to include

  USE things
  IMPLICIT none

  INTEGER, intent(in)            :: npts, ref
  REAL(crystal_kind), intent(in) :: fraction, dx, m
  TYPE(thing), intent(in)        :: bodies(npts)

  INTEGER, intent(inout) :: active(npts)
  INTEGER, intent(out)              :: count
  REAL(crystal_kind), intent(out)   :: minpot, dt_suggest

  REAL(crystal_kind), ALLOCATABLE :: tmp_pot(:)
  REAL(crystal_kind) :: lennard_jones

  REAL(crystal_kind) :: vmax, absmin
  INTEGER j

  vmax = 0.0
  absmin = 1.e-26

! allocate tmp_pot
    ALLOCATE(tmp_pot(npts))
    tmp_pot = 0.0
    DO j = 1, npts
      vmax = MAX(vmax, ABS(bodies(ref)%u%x), ABS(bodies(ref)%u%y), &
                   ABS(bodies(ref)%u%z) )
      IF (j .NE. ref ) THEN
        tmp_pot(j) = ABS(lennard_jones(bodies(ref), bodies(j)))
      ENDIF
    ENDDO
    minpot = fraction * MAXVAL(tmp_pot)
!D    WRITE (6,*) 'vmax, dt_v a, dt_a ',vmax, 0.1*dx/vmax, &
!D           ABS(MAXVAL(tmp_pot))/m/dx, 0.01*dx*sqrt(m / ABS(MAXVAL(tmp_pot)))
!D    FLUSH(6)
    IF (vmax .NE. 0.) THEN
      dt_suggest = MIN(0.1*dx/vmax, 0.01*dx*sqrt(m / ABS(MAXVAL(tmp_pot)))  )
    ELSE
      dt_suggest =  0.01*dx*sqrt(m / ABS(MAXVAL(tmp_pot)))
    ENDIF

    count = 0
    DO j = 1, npts
      IF (j .NE. ref .AND. tmp_pot(j) > minpot .AND. tmp_pot(j) > absmin) THEN
        count = count + 1
      ENDIF
    ENDDO

    count = 0
    DO j = 1, npts
      IF (j .NE. ref .AND. tmp_pot(j) > minpot .AND. tmp_pot(j) > absmin) THEN
        count = count + 1
        active(count) = j
      ENDIF
    ENDDO

! free up tmp_pot
    DEALLOCATE(tmp_pot)

END SUBROUTINE scan
