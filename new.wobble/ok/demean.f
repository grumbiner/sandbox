      SUBROUTINE demean(x, mean, npts)
      IMPLICIT none
      INTEGER npts
      REAL x(npts), mean
      DOUBLE PRECISION sum
      INTEGER i
      sum = 0
      DO i = 1, npts
        sum = sum + x(i)
      ENDDO
      mean = sum / DBLE(npts)
      x = x - mean
      RETURN
      END
