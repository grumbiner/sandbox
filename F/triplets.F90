MODULE triplets
  IMPLICIT none
  PUBLIC
    TYPE triplet
      REAL x, y, z
    END TYPE

   INTERFACE initialize
     module procedure initialize
   END INTERFACE initialize
CONTAINS

  SUBROUTINE initialize(zzz)
    TYPE(triplet) zzz
    zzz%x = 0.0
    zzz%y = 0.0
    zzz%z = 0.0
    RETURN
  END SUBROUTINE initialize

END MODULE triplets
