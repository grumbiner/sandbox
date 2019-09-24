
      FUNCTION sumx2(x, n)
      IMPLICIT none
      INTEGER n
      REAL x(n)
      REAL sumx2

      DOUBLE PRECISION sum
      INTEGER i
      sum = 0.0
      DO 2000 i = 1, n
        sum = sum + DBLE(x(i)*x(i))
 2000 CONTINUE
      sumx2 = SNGL(sum)

      RETURN
      END
