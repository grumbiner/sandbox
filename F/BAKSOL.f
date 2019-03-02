C*************************************************----------++++++++++!!
      SUBROUTINE BAKSOL(k, n, a, b, w)
C     Subroutine to do the back substitution for a matrix already
C       factored into its LU form.

      INTEGER k, n
      DOUBLE PRECISION a(n,-k:k), b(n), w(n)

C     Local variables.
      INTEGER i, j
     
C     Forward through L.
      w(1) = b(1)
      DO 1000 i = 2, n
        w(i) = b(i)
        DO 1010 j = 1, MIN(i-1, k)
          w(i) = w(i) - a(i,-j)*w(i-j)
 1010   CONTINUE
 1000 CONTINUE

C     Back through U
      b(n) = w(n) / a(n, 0)
      DO 2000 i = n-1, 1, -1
        b(i) = w(i)
        DO 2010 j = 1, MIN(n-i, k)
          b(i) = b(i) - b(i+j)*a(i,j)
 2010   CONTINUE
        b(i) = b(i) /a(i,0)
 2000 CONTINUE

      RETURN
      END
