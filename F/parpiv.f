      SUBROUTINE parpiv(a, b, n, buffer, err)
C     routine for use in decomposing a matrix for finding an invers.
      INTEGER n, k, i, j, maxrow
      REAL a(n, n), b(n), buffer, err, m

      DO 10 k = 1, n-1
C       Find the row with the largest element in the column.
        maxrow = k
        DO 100 l = k+1, n
          IF ( abs(a(maxrow,k)) .LT. abs(a(l, k)) ) maxrow = l
  100   CONTINUE
        IF (abs(a(maxrow, k)) .LT. buffer) THEN
C         All attempted pivots are too close to zero to use.
          err = 1.0
          WRITE (6, 1) k
    1     FORMAT (' There is a zero pivot on step ', I3)
          RETURN
        ENDIF
       
        IF (k .NE. maxrow) THEN
C         Must swap rows.
          DO 300 l = 1, n
            dummy = a(k, l)
            a(k, l) = a(maxrow, l)
            a(maxrow, l) = dummy
  300     CONTINUE
          dummy  = b(k)
          b(k)   = b(maxrow)
          b(maxrow) = dummy
C         Finished swapping.
        ENDIF

        DO 20 i = k+1, n
          a(i, k) = a(i, k) / a(k,k)
          m = a(i, k)
          DO 30 j = k+1, n
            a(i, j) = a(i, j) - m*a(k, j)
   30     CONTINUE
          b(i) = b(i) - m*b(k)
   20   CONTINUE

   10   CONTINUE

        RETURN
        END
