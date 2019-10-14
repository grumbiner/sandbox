
      FUNCTION sumxy(x, y, n)
C     Compute the sum of x(i)*y(i)
      INTEGER n
      REAL x(n), y(n), sumxy

      INTEGER i
      REAL sum

      sum = 0.0
      DO 1000 i = 1, n
        sum = sum + x(i)*y(i)
 1000 CONTINUE

      sumxy = sum
      RETURN
      END
