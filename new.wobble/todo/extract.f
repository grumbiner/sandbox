      SUBROUTINE extract(x, n, omega, a, b, m, stepsize)
      IMPLICIT none
      INTEGER n, m
      REAL stepsize
      DOUBLE PRECISION x(n), a(0:m-1), b(0:m-1), omega(0:m-1)
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.14159265358979323846 )
      INTEGER i, j

      CALL harmrm(x, n, omega, a, b, m)
      DO i = 0, m-1
        IF (omega(i) .NE. 0) THEN
        WRITE (*,*) stepsize*2.*PI/omega(i), omega(i), a(i), b(i),
     1         sqrt(a(i)*a(i)+b(i)*b(i)), atan2(b(i),a(i))
        ELSE
        WRITE (*,*) "mean", omega(i), a(i), b(i),
     1         sqrt(a(i)*a(i)+b(i)*b(i)), atan2(b(i),a(i))
        ENDIF
      ENDDO
      DO i = 1, n
        DO j = 0, m-1
          x(i) = x(i) - a(j)*cos(omega(j)*DBLE(i)) - 
     1                  b(j)*sin(omega(j)*DBLE(i))
        ENDDO
      ENDDO

 9001 FORMAT (6E22.13)

      RETURN
      END
