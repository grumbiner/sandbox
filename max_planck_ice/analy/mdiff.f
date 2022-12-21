      PROGRAM mdiff
      INTEGER nx, ny
      PARAMETER (nx = 77)
      PARAMETER (ny = 93)

      CHARACTER*60 fname
      REAL a(nx, ny), b(nx, ny)

      READ (10) a
      READ (11) b
      a = a - b

      WRITE (12) a

      STOP
      END
