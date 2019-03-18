      SUBROUTINE  hilbert (h, n)
C     Generate a hilbert matrix of degree n.
      INTEGER n, i, j
      REAL h(n, n)

      DO 10 i = 1, n
        DO 20 j = 1, n
          h(i, j) = 1./(i+j-1)
  20    CONTINUE
  10  CONTINUE

      RETURN
      END
