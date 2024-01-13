!------------------ Main program ----------------------
PROGRAM solar_system
  USE types
  USE astronomy
  USE point_mass
  IMPLICIT none

  INTEGER isun, iearth, nbody, nmassless
  type(body), allocatable :: system(:)
  type(body), allocatable :: massless(:)
  REAL(dp), allocatable :: r0(:), ke0(:)

  REAL(dp) :: ratio, dt, dr, tmpf

  REAL(dp) :: mom(3), tmom(3), tcom(3)
  REAL(dp) :: zero_vec(3)

  INTEGER step, freq, nyear
  INTEGER i, j, k
  REAL(dp), parameter :: zero = 0.0

  i     = 0
  isun  = 0
  iearth = 1
  nbody = 2
  ratio = 4*8640.        !Steps per mean solar day of 86400 seconds
  freq  = INT(4*ratio)   !Output frequency, number of steps

  dt = mean_solar_day/ratio

  ALLOCATE(system(0:nbody))
  ALLOCATE(r0(0:nbody))
  ALLOCATE(ke0(0:nbody))
  zero_vec = zero

  system(isun)%m = m_sun
  system(isun)%k = m_sun*G
! Initialize sun location, velocity below, to zero center of mass and 
!    momentum for solar system

  i = iearth
    system(i)%m = m_earth*1.e-0
    system(i)%k = G*system(i)%m
    CALL init_loc(system(i), zero, au*1., zero)
    CALL init_vel(system(i), kepler(system(i), system(isun) ) , zero, zero )
  i = 2
    system(i)%m = m_jupiter
    system(i)%k = G*system(i)%m
    CALL init_loc(system(i), 5.2038*au, zero, zero)
    tmpf = -kepler(system(i), system(isun) ) * (1. - system(i)%m / system(isun)%m)
    CALL init_vel(system(i), zero, tmpf, zero )

!  nmassless = 13
!  ALLOCATE(massless(nmassless))
!  DO i = 1, nmassless
!! L1 is ~0.99 au
!    CALL init_loc(massless(i), zero, au*(.981+i*0.002), zero)
!    CALL init_vel(massless(i), kepler(massless(i), system(isun) ) , zero, zero )
!  ENDDO

!--------------------------------------------------------
! Sun's initial position s.t. center of mass is at 0,0,0
!       initial velocity s.t. total momentum = 0,0,0
  tcom = zero
  DO i = 1, nbody
    tcom = tcom + system(i)%m*system(i)%x
  ENDDO
  tcom = -tcom / system(0)%m
  !debug PRINT *,tcom/au
  CALL init_loc(system(0), tcom(1), tcom(2), tcom(3))

  tmom = zero
  mom  = zero
  DO i = 1, nbody
    CALL momentum(system(i), tmom)
    mom = mom + tmom
  ENDDO
  mom = -mom / system(0)%m
  !debug PRINT *,mom, tmom
  CALL init_vel(system(0), mom(1), mom(2), mom(3))

!--------------------------------------------------------
  DO i = 0, nbody
    r0(i) = dist(zero_vec, system(i)%x)
    ke0(i) = ke(system(i))
  ENDDO
! ----- Done initializing solar system -----

!M  OPEN(10, FILE="massless", FORM="FORMATTED", STATUS="UNKNOWN")

! Main loop
! Split steps per year and number of years to avoid going over MAXINT
  DO nyear = 0, 10000-1
  DO step = 0, int(366*ratio)

    IF (MOD(step,freq) .EQ. 0) THEN
!M      WRITE (10,9009) nyear+step*dt/mean_solar_day/366.,     &
!M             system(iearth)%x(1)/au, system(iearth)%x(2)/au, &
!M             (massless(j)%x(1)/au,j=1,nmassless),(massless(j)%x(2)/au,j=1,nmassless)
      i = iearth
      j = 2
      k = 0
      WRITE(*,*) nyear+step*dt/mean_solar_day/366.,     &
      system(i)%x(1)/au, system(i)%x(2)/au,  & 
!         (ke0(i) - ke(system(i)))/ke0(i),       &
      1e6*(dist(system(i)%x, zero_vec)/au - r0(i)/au), &
      system(j)%x(1)/au, system(j)%x(2)/au,  & 
!         (ke0(j) - ke(system(j)))/ke0(j),       &
      1e6*(dist(system(j)%x, zero_vec)/au - r0(j)/au), &
      1e6*(dist(system(isun)%x, zero_vec)/au - r0(isun)/au)
    ENDIF
!alt -- solar distance:      1e6*(dist(system(j)%x, system(isun)%x)/au - r0(j)/au), &
    
! Gravitational interactions of massive bodies
    DO j = 0, nbody
    DO k = 0, nbody
      IF (k .NE. j) CALL gravity(system(j), system(k))
    ENDDO
    ENDDO

!M    For massless bodies:
!M    DO j = 1, nmassless
!M      DO k = 0, nbody
!M        CALL gravity(massless(j), system(k) )
!M      ENDDO
!M      CALL update_loc(massless(j), dt)
!M      CALL update_vel(massless(j), dt)
!M    ENDDO
! Update location, position of massive bodies
    DO j = 0, nbody
      CALL update_loc(system(j), dt)
      CALL update_vel(system(j), dt)
    ENDDO

  ENDDO 
  ENDDO 
! End main loops
 9009 FORMAT(E13.6,2x,2(F9.6,1x),2x, 40(F9.6,1x))

  i = iearth
  j = 2
  k = 0
      WRITE(*,*) -1+nyear+(step-1)*dt/mean_solar_day/366.,     &
      system(i)%x(1)/au, system(i)%x(2)/au,  & 
      1e6*(dist(system(i)%x, zero_vec)/au - r0(i)/au), &
      system(j)%x(1)/au, system(j)%x(2)/au,  & 
      1e6*(dist(system(j)%x, zero_vec)/au - r0(j)/au), &
      1e6*(dist(system(isun)%x, zero_vec)/au - r0(isun)/au)


 9001 FORMAT(F8.3, 2F10.6, 2F9.1, 2E14.6)

END
