MODULE types
PUBLIC
  INTEGER, parameter :: dp = SELECTED_REAL_KIND(15,300)
END MODULE types

MODULE astronomy
USE types
PUBLIC
  REAL(dp) ,parameter :: m_earth = 5.9722d24 
  REAL(dp) ,parameter :: m_jupiter = m_earth*317.8
  REAL(dp) ,parameter :: m_sun   = 1.98847d30 
  REAL(dp) ,parameter :: au      = 1.49597870700d11 
  REAL(dp) ,parameter :: rmoon   = 3.84e8
  REAL(dp) ,parameter :: ly      = 9.46d15 
  REAL(dp) ,parameter :: parsec  = 3.26d0*ly 
  REAL(dp) ,parameter :: G       = 6.67d-11
  REAL(dp) ,parameter :: mean_solar_day = 86400.
END MODULE astronomy

MODULE point_mass
USE types
USE astronomy

  IMPLICIT none
  PUBLIC
  TYPE body
    REAL(dp) :: x(3), u(3), a(3)
    REAL(dp) :: m, k
  END TYPE body
  type(body) :: self

  PUBLIC init, init_loc, init_vel
  PUBLIC update_loc, update_vel
  PUBLIC show, ke, dist, gravity, kepler 

  CONTAINS
    SUBROUTINE init(self)
    type(body) :: self
      self%x = 0
      self%u = 0
      self%a = 0
      self%m = 0
      self%k = 0
    RETURN
    END
    SUBROUTINE init_loc(self, loc1, loc2, loc3)
      type(body) :: self
      REAL(dp) :: loc1, loc2, loc3
      self%x(1) = loc1
      self%x(2) = loc2
      self%x(3) = loc3
    RETURN
    END
    SUBROUTINE init_vel(self, vel1, vel2, vel3)
      type(body) :: self
      REAL(dp) :: vel1, vel2, vel3
      self%u(1) = vel1
      self%u(2) = vel2
      self%u(3) = vel3
    RETURN
    END

    SUBROUTINE update_loc(self, dt)
      type(body) :: self
      REAL(dp) :: dt
      self%x = self%x + dt*self%u + 0.5*dt*dt*self%a
    RETURN
    END
    SUBROUTINE update_vel(self, dt)
      type(body) :: self
      REAL(dp) :: dt
      self%u = self%u + dt*self%a
      self%a = 0.
    RETURN
    END

    SUBROUTINE show(self, l )
      TYPE(body) :: self
      REAL(dp),optional :: l
      REAL(dp) :: scale
      IF (present(l)) THEN
        scale = l
      ELSE
        scale = 1.0
      ENDIF
      PRINT *, self%x / l, self%u, self%m
    END SUBROUTINE show
     
    SUBROUTINE momentum(self, mom)
      type(body) self
      REAL(dp) mom(3)
      mom = self%u * self%m
      RETURN
    END SUBROUTINE momentum

    REAL(dp) FUNCTION ke(self)
      type(body) self
      ke = 0.5*self%m* (self%u(1)**2 + self%u(2)**2 + self%u(3)**2 )
    END
    REAL(dp) FUNCTION dist(x, y)
      REAL(dp) :: x(3), y(3), dx(3)
      dx = y - x
      dist = sqrt(dx(1)*dx(1) + dx(2)*dx(2) + dx(3)*dx(3))
    END

!keplerian speed for distance around body y:
  REAL(dp) FUNCTION kepler(x, y)
    type(body) x, y
    REAL(dp) :: dx(3)
    REAL(dp) :: r
    dx = y%x - x%x
    r = sqrt(dx(1)*dx(1) + dx(2)*dx(2) + dx(3)*dx(3))
    kepler = sqrt(y%k / r)
  END FUNCTION kepler
  
  SUBROUTINE gravity(self, y)
    TYPE(body) :: self, y
    REAL(dp) :: dx(3), a0, r
    dx = y%x - self%x
    r = sqrt(dx(1)*dx(1) + dx(2)*dx(2) + dx(3)*dx(3))
    IF (r < rmoon ) THEN
      PRINT *,'close approach ',r/au,r/rmoon
    ENDIF
    a0 = y%k / r**3
    self%a = self%a + a0*dx
    RETURN
  END 
  REAL(dp) FUNCTION pe(self, remote)
    type(body) self, remote
    pe = 0.0
  END
    
END MODULE point_mass

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
  freq  =   8640
  ratio = 4*8640.

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
    system(i)%m = m_earth*317.8
    system(i)%k = G*system(i)%m
    CALL init_loc(system(i), 5.2000*au, zero, zero)
    tmpf = -kepler(system(i), system(isun) ) * (1. - system(i)%m / system(isun)%m)
    CALL init_vel(system(i), zero, tmpf, zero )

  nmassless = 13
  ALLOCATE(massless(nmassless))
  DO i = 1, nmassless
! L1 is ~0.99 au
    CALL init_loc(massless(i), zero, au*(.981+i*0.002), zero)
    CALL init_vel(massless(i), kepler(massless(i), system(isun) ) , zero, zero )
  ENDDO

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
  OPEN(10, FILE="massless", FORM="FORMATTED", STATUS="UNKNOWN")

! Main loop
  !DO nyear = 0, 71
  DO nyear = 0, 11
  DO step = 0, int(366*ratio)

    IF (MOD(step,freq) .EQ. 0) THEN
      WRITE (10,9009) nyear+step*dt/mean_solar_day/366.,     &
             system(iearth)%x(1)/au, system(iearth)%x(2)/au, &
             (massless(j)%x(1)/au,j=1,nmassless),(massless(j)%x(2)/au,j=1,nmassless)
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
    
    DO j = 0, nbody
    DO k = 0, nbody
      IF (k .NE. j) CALL gravity(system(j), system(k))
    ENDDO
    ENDDO

    DO j = 1, nmassless
      DO k = 0, nbody
        CALL gravity(massless(j), system(k) )
      ENDDO
      CALL update_loc(massless(j), dt)
      CALL update_vel(massless(j), dt)
    ENDDO
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
