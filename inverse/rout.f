      SUBROUTINE rout(r, k)
      INTEGER k, nx, ny
      PARAMETER (nx = 385)
      PARAMETER (ny = 465)
      REAL r(nx, ny)

      WRITE (100+k) r

      RETURN
      END
