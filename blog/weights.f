      PROGRAM weights
      IMPLICIT none

      INTEGER i, j, n, nmax, ny
      PARAMETER (ny = 354)
      PARAMETER (nmax = 4)

      REAL dum, y, t, x, dx, legendre, temp(ny)
      DOUBLE PRECISION sum(5)

      OPEN (10, FILE="c3", FORM="FORMATTED", STATUS="OLD")
      DO i = 1, ny
        READ (10,*) y, t
        temp(i) = t
      ENDDO
      dx = 1./15.

      DO j = 1, ny-30
      !DO j = 1, 2
        sum = 0.0
        DO i = 1, 31
          x = (FLOAT(i) - 16.)/15.
          IF (x .EQ. -1 .OR. x .EQ. 1.0) THEN
            DO n = 0, nmax
              sum(n+1) = sum(n+1) + legendre(n,x)*temp(j+i-1)/2
            ENDDO
          ELSE
            DO n = 0, nmax
              sum(n+1) = sum(n+1) + legendre(n,x)*temp(j+i-1)
            ENDDO
          ENDIF
        ENDDO
        DO n = 0, nmax
          sum(n+1) = sum(n+1)*((2.*n+1.)/2.)*dx
        ENDDO
        WRITE (*,9001) j+1658+15, sum(1), sum(2), sum(3), sum(4), sum(5)

      ENDDO
 9001 FORMAT (I4,F7.3,4F8.4)

      END
