      PROGRAM nametest
      REAL x, y, z
      namelist /tester/ x, y, z
      COMMON /seaice/ x, y, z

      OPEN(UNIT=10, FILE="nametest")
      REWIND(10)
      READ(10,tester)

      PRINT *,x, y, z

      CALL stupid

      STOP
      END
      SUBROUTINE stupid
      REAL x, y, z
      COMMON /seaice/ x, y, z
      PRINT *,x, y, z
      RETURN
      END
