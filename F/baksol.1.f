      SUBROUTINE BAKSOL (A, B, X, N, BUFFER, ERR)
C     Robert Grumbine 1985

      INTEGER n, i, j
      REAL a(n,n), b(n), x(n), buffer, err, r
       
      err = 0.0
      DO 10 i = n, 1, -1
        r = b(i)
        DO 20 j = i+1, n
          r = r-a(i, j)*x(j)
   20   CONTINUE
        IF (abs(a(i,i)) .GT. buffer) THEN
          x(i) = r/ a(i,i)
         ELSEIF (abs(r) .LE. buffer) THEN
          x(i) = 0.0
         ELSE
          err = 2.0
          WRITE (6, 1) i
    1     FORMAT (' An inconsistant equation appeared in row',I3)
          RETURN
        ENDIF
   10 CONTINUE

      RETURN
      END
