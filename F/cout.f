      SUBROUTINE cout(c, k)
      INTEGER k, nx, ny
      PARAMETER (nx = 385)
      PARAMETER (ny = 465)
      CHARACTER*1 c(nx, ny)

      WRITE (10+k) c

      RETURN
      END
