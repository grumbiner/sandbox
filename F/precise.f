      PROGRAM precise
C     Test the precision of invertible arithmetic by solving
C       for x in ax+b = y, where a, b, y are known.  Use 
C       two different orders of operations to test the associativity
C       of the operation.
      IMPLICIT none
      INTEGER i, n
      PARAMETER (n = 100000)
      REAL a, b, y, c1(n), c2(n), x1(n), x2(n)
      REAL m, delta
     
      delta = 1.E-15
      a = 3.  
      b = 1.
      DO 1000 i = 1, n
        y = 1.+(delta*i)
        c1(i) = y/a
        c2(i) = b/a
        x2(i) = (y-b)/a
 1000 CONTINUE
      DO 2000 i = 1, n
        x1(i) = c1(i)-c2(i)
 2000 CONTINUE
      DO 3000 i = 1, n
        IF (ABS(x1(i)-x2(i)) .GT. delta/a .AND. x1(i) .NE. 0.) THEN
          PRINT *,i, (x1(i) - x2(i))/x1(i)
         ENDIF
 3000 CONTINUE

      STOP
      END
