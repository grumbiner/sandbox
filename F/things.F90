MODULE things
  USE triplets
  IMPLICIT none
  PUBLIC
    TYPE thing
      TYPE(triplet) x
      TYPE(triplet) u
    END TYPE
  INTERFACE initialize_things
    module procedure initialize_things
  END INTERFACE initialize_things

CONTAINS
  SUBROUTINE show(zzz)
    IMPLICIT none
    TYPE(thing) zzz
    PRINT *,'position ',zzz%x%x, zzz%x%y, zzz%x%z
    PRINT *,'velocity ',zzz%u%x, zzz%u%y, zzz%u%z
  END SUBROUTINE show

  SUBROUTINE initialize_things(zzz)
    IMPLICIT none
    TYPE(thing) zzz
    CALL initialize(zzz%x)
    CALL initialize(zzz%u)
    RETURN
  END SUBROUTINE initialize_things
      
END MODULE things

PROGRAM alpha
  USE things
  IMPLICIT none
  TYPE(thing) first

  CALL initialize_things(first)
  first%x%x = 1.0
  first%u%x = 2.0
  CALL show(first)

END PROGRAM alpha
