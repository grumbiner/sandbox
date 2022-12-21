C----------------------------------------------------------------------------
      SUBROUTINE extract(x, n, omega, a, b, m, dt)
      INTEGER n, m
      DOUBLE PRECISION x(n), omega(0:m-1), a(0:m-1), b(0:m-1)

      DO i = 1, n
        DO j = 0, m-1
          x(i) = x(i) - a(j)*cos(omega(j)*DBLE(i)) 
     1                - b(j)*sin(omega(j)*DBLE(i))
        ENDDO
      ENDDO

      RETURN
      END
