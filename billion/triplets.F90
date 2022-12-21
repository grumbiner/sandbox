MODULE triplets
  USE billion_kinds

  IMPLICIT none

  PUBLIC
    TYPE triplet
      REAL(crystal_kind) :: x, y, z
    END TYPE

  INTERFACE operator (*)
    MODULE PROCEDURE scalar_mult
    MODULE PROCEDURE scalar_mult_pre
  END INTERFACE
  INTERFACE operator (+)
    MODULE PROCEDURE add
  END INTERFACE
  INTERFACE operator (/)
    MODULE PROCEDURE div
  END INTERFACE
  
  INTERFACE initialize
    module procedure initialize
  END INTERFACE initialize
CONTAINS

  TYPE(triplet) FUNCTION div(xxx, x) result (zzz)
    TYPE(triplet), intent(in) :: xxx
    REAL(crystal_kind), intent(in) :: x
    zzz%x = xxx%x / x
    zzz%y = xxx%y / x
    zzz%z = xxx%z / x
  END FUNCTION div

  TYPE(triplet) FUNCTION add(xxx, yyy) result (zzz)
    TYPE(triplet), intent(in) :: xxx, yyy
    zzz%x = xxx%x + yyy%x
    zzz%y = xxx%y + yyy%y
    zzz%z = xxx%z + yyy%z
  END FUNCTION add

  TYPE(triplet) FUNCTION scalar_mult_pre(dt, zzz) result (yyy)
    TYPE(triplet), intent(in)  :: zzz
    REAL(crystal_kind), intent(in) :: dt
    yyy%x = zzz%x * dt
    yyy%y = zzz%y * dt
    yyy%z = zzz%z * dt
  END FUNCTION scalar_mult_pre

  TYPE(triplet) FUNCTION scalar_mult(zzz, dt) result (yyy)
    TYPE(triplet), intent(in)  :: zzz
    REAL(crystal_kind), intent(in) :: dt
    yyy%x = zzz%x * dt
    yyy%y = zzz%y * dt
    yyy%z = zzz%z * dt
  END FUNCTION scalar_mult

  SUBROUTINE initialize(zzz)
    TYPE(triplet) zzz
    zzz%x = 0.0
    zzz%y = 0.0
    zzz%z = 0.0
    RETURN
  END SUBROUTINE initialize
  REAL(crystal_kind) FUNCTION dist(p1, p2) result (r)
    TYPE(triplet), intent(in) :: p1, p2
    REAL(crystal_kind) :: dx, dy, dz
    dx = p1%x - p2%x
    dy = p1%y - p2%y
    dz = p1%z - p2%z
    r = sqrt(dx**2 + dy**2 + dz**2)
  END FUNCTION dist

END MODULE triplets
