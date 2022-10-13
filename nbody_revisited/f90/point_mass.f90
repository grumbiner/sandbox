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
  REAL(dp) ,parameter :: G       = 6.6743d-11
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

