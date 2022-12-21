C----------------------------------------------------------------------------
      SUBROUTINE linear_extract(x, y, npts, name)
      IMPLICIT none
      INTEGER i, npts
      REAL x(npts), y(npts)
      CHARACTER*3 name
      REAL mean
      DOUBLE PRECISION  intercept, slope, correl

      CALL demean(y, mean, npts)
      y = y - mean

      CALL fit(x, y, npts, intercept, slope, correl)
      WRITE (*,9002) name, mean, intercept, slope, correl
      DO i = 1, npts
        y(i) = y(i) - intercept - slope*x(i)
      ENDDO
 9002 FORMAT (A3," mean ",F11.4," regress a+bx ",F12.5," ", E11.4,
     1             " correlation ",F8.5)
      RETURN
      END
