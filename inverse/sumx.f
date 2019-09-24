      FUNCTION sumx(x,n)
      IMPLICIT none
      INTEGER n
      REAL x(n)
      REAL sumx

      DOUBLE PRECISION sum
      INTEGER i
      sum = 0.
      DO 1000 i = 1, n
        sum = sum + DBLE(x(i))
 1000 CONTINUE
      sumx = SNGL(sum)
      RETURN
      END
