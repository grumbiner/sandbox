      PROGRAM weights
      IMPLICIT none

      INTEGER i, j, k, n, nmax, ny
      PARAMETER (ny = 1613)
      PARAMETER (nmax = 4)

      REAL dum, y, t, x, dx, legendre, temp(ny)
      REAL slope(ny), accel(ny)
      DOUBLE PRECISION sum(nmax+1)

      OPEN (10, FILE="1880-2014.csv", FORM="FORMATTED", STATUS="OLD")
      DO i = 1, ny
        READ (10,*) y, t
        temp(i) = t
      ENDDO

      slope = 0.
      accel = 0.
      k = 15
      dx = 1./FLOAT(k)

      DO j = k+1, ny-k
        sum = 0.0
        DO i = -k, k
          x = FLOAT(i) / FLOAT(k)
          IF (x .EQ. -1 .OR. x .EQ. 1.0) THEN
            sum(1) = sum(1) +  temp(j+i-1)/2
          ELSE
            sum(1) = sum(1) +  temp(j+i-1)
          ENDIF
        ENDDO
        sum = sum * dx  /2.
        slope(j) = temp(j) - sum(1)
      ENDDO
        

 9001 FORMAT (I4,5F9.4,2x,3F9.4)

      END
