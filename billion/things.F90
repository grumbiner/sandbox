MODULE things
  USE billion_kinds
  USE triplets
  IMPLICIT none

  PUBLIC
    TYPE thing
      TYPE(triplet) x
      TYPE(triplet) u
    END TYPE thing

  INTERFACE initialize
    module procedure initialize_thing
  END INTERFACE initialize

  INTERFACE time_step
    module procedure time_step_euler
    module procedure time_step_euler_force
  END INTERFACE time_step
  INTERFACE accelerate
    module procedure accelerate_euler
  END INTERFACE accelerate

CONTAINS
  SUBROUTINE initialize_thing(zzz)
    IMPLICIT none
    TYPE(thing) zzz
    CALL initialize(zzz%x)
    CALL initialize(zzz%u)
    RETURN
  END SUBROUTINE initialize_thing
      
  SUBROUTINE show(zzz)
    IMPLICIT none
    TYPE(thing) zzz
    PRINT *,'position ',zzz%x%x, zzz%x%y, zzz%x%z
    PRINT *,'velocity ',zzz%u%x, zzz%u%y, zzz%u%z
  END SUBROUTINE show

  SUBROUTINE accelerate_euler(zzz, dt, force, m)
    IMPLICIT none
    REAL(crystal_kind), intent(in) :: dt, m
    TYPE(triplet), intent(in) :: force
    TYPE(thing) zzz

    zzz%u%x = zzz%u%x + dt*(force%x/m)
    zzz%u%y = zzz%u%y + dt*(force%y/m)
    zzz%u%z = zzz%u%z + dt*(force%z/m)
  END SUBROUTINE accelerate_euler

  SUBROUTINE time_step_euler(zzz, dt)
    IMPLICIT none
    REAL(crystal_kind), intent(in) :: dt
    TYPE(thing) zzz

    !use triplet notation
    zzz%x = zzz%x + dt*zzz%u

    !zzz%x%x = zzz%x%x + zzz%u%u*dt
    !zzz%x%y = zzz%x%y + zzz%u%v*dt
    !zzz%x%z = zzz%x%z + zzz%u%w*dt

  END SUBROUTINE time_step_euler 

  SUBROUTINE time_step_euler_force(zzz, dt, force, m)
    IMPLICIT none
    REAL(crystal_kind) :: dt, m
    TYPE(thing) zzz
    TYPE(triplet) force

    zzz%x = zzz%x + dt*zzz%u + force*(dt*dt/m/2.)

    !zzz%x%x = zzz%x%x + zzz%u%u*dt + force%x*(dt*dt/m/2.)
    !zzz%x%y = zzz%x%y + zzz%u%v*dt + force%y*(dt*dt/m/2.)
    !zzz%x%z = zzz%x%z + zzz%u%w*dt + force%z*(dt*dt/m/2.)
  END SUBROUTINE time_step_euler_force 

END MODULE things
