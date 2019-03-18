      SUBROUTINE nopiv(a, n)
C     Routine for decomposing a matrix into its LU form, without
C       using partial pivoting.
      REAL a(n, n), m
      INTEGER i, j, k, n

      DO 10 k = 1, n
        DO 20 i = k+1, n
          a(i, k) = a(i, k)/a(k, k)
          m       = a(i, k)
          DO 30 j = k+1, n
            a(i, j) = a(i, j) - m*a(k, j)
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE

      RETURN
      END
